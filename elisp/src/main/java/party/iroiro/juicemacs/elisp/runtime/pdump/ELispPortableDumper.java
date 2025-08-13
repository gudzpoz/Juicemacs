package party.iroiro.juicemacs.elisp.runtime.pdump;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.strings.MutableTruffleString;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.utilities.CyclicAssumption;
import org.apache.fory.Fory;
import org.apache.fory.config.Language;
import org.apache.fory.io.ForyStreamReader;
import org.apache.fory.logging.LoggerFactory;
import org.apache.fory.serializer.ExternalizableSerializer;
import party.iroiro.juicemacs.elisp.forms.*;
import party.iroiro.juicemacs.elisp.forms.BuiltInCcl.CclProgram;
import party.iroiro.juicemacs.elisp.forms.coding.ELispCodings;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical.Captured;
import party.iroiro.juicemacs.elisp.nodes.local.ELispLexical.Scope;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispContext.ContextSerializer;
import party.iroiro.juicemacs.elisp.runtime.ELispGlobals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.array.ELispConsSerializer;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.objects.AbstractELispClosure.ClosureCommons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer.MarkerMoveNotifier;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispHashtable.ELispWeakHashtable;
import party.iroiro.juicemacs.elisp.runtime.pdump.serializers.*;
import party.iroiro.juicemacs.elisp.runtime.pdump.serializers.PieceTreeSerializer.Intervals;
import party.iroiro.juicemacs.elisp.runtime.pdump.serializers.PieceTreeSerializer.Marks;
import party.iroiro.juicemacs.elisp.runtime.scopes.FunctionStorage;
import party.iroiro.juicemacs.elisp.runtime.scopes.ThreadLocalStorage;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage.*;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.piecetree.PieceTreeBase;
import party.iroiro.juicemacs.piecetree.meta.IntervalPieceTree;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree.Marker;

import java.io.OutputStream;
import java.nio.channels.SeekableByteChannel;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInBaseNode.JAVA_SOURCE;

/// Portable dumper for ELisp
public final class ELispPortableDumper {
    // TODO: support pdump with native images until fory fixes issues
    private static final Fory FORY;

    static {
        LoggerFactory.disableLogging();
        Fory fory = Fory.builder()
                .withLanguage(Language.JAVA)
                .withRefTracking(true)
                .requireClassRegistration(true)
                .ignoreBasicTypesRef(true)
                .registerGuavaTypes(false)
                .build();

        // Symbols
        fory.registerSerializer(ELispObarray.class, new ELispObarraySerializer(fory));
        fory.registerSerializer(ELispSymbol.class, new ELispSymbolSerializer(fory));
        // Symbol values
        fory.registerSerializer(ThreadLocalStorage.class,
                DumpUtils.stateless(fory, ThreadLocalStorage.class, () -> new ThreadLocalStorage(false)));
        fory.registerSerializer(ValueStorage.class, new ExternalizableSerializer<>(fory, ValueStorage.class));

        // Objects
        fory.registerSerializer(ELispHashtable.class, new ELispHashtableSerializer(fory));
        fory.registerSerializer(ELispWeakHashtable.class, new ELispHashtableSerializer(fory));
        fory.registerSerializer(ELispCons.class, new ELispConsSerializer(fory));

        // Buffers
        fory.registerSerializer(ELispMarker.class, new ELispMarkerSerializer(fory));
        fory.registerSerializer(Marker.class, DumpUtils.never(fory, Marker.class));
        fory.registerSerializer(PieceTreeBase.class, new PieceTreeSerializer(fory));
        fory.registerSerializer(IntervalPieceTree.class, new Intervals(fory));
        fory.registerSerializer(MarkerPieceTree.class, new Marks(fory));

        // Truffle internals
        fory.registerSerializer(CyclicAssumption.class, new CyclicAssumptionSerializer(fory));
        fory.registerSerializer(Assumption.class, DumpUtils.never(fory, Assumption.class));
        Class<? extends MaterializedFrame> frameClass = Truffle.getRuntime()
                .createMaterializedFrame(new Object[0]).getClass(); // FrameWithoutBoxing
        fory.registerSerializer(frameClass, new MaterializedFrameSerializer(fory));
        fory.registerSerializer(ELispSubroutine.class, DumpUtils.never(fory, ELispSubroutine.class));
        fory.registerSerializer(JAVA_SOURCE.getClass(), new SourceSerializer(fory)); // SourceImpl

        Class<?>[] classes = {
                // Symbols
                PlainValue.class,
                Forwarded.class,
                ForwardedBool.class,
                ForwardedLong.class,
                ForwardedPerKboard.class,
                ForwardedPerBuffer.class,
                VarAlias.class,
                BufferLocal.class,
                TrappedWrite.class,
                FunctionStorage.class,
                // Objects
                ELispString.class,
                ELispCharTable.class,
                ELispCharTable.SubTable.class,
                ELispCharTable.CompressedUnipropSubTable.class,
                ELispBoolVector.class,
                ELispVector.class,
                ELispRecord.class,
                ELispInterpretedClosure.class,
                ELispBytecode.class,
                // Buffers
                ELispBuffer.class,
                MarkerMoveNotifier.class,
                // Lexical
                ELispLexical.class,
                Scope.class,
                Captured.class,
                ClosureCommons.class,
                // Truffle internals
                FrameDescriptor.class,
                TruffleString.class,
                MutableTruffleString.class,
                // Context
                ELispBuiltIns[].class,
                BuiltInAlloc.class,
                BuiltInBuffer.class,
                BuiltInBytecode.class,
                BuiltInCallInt.class,
                BuiltInCallProc.class,
                BuiltInCaseFiddle.class,
                BuiltInCaseTab.class,
                BuiltInCategory.class,
                BuiltInCcl.class, CclProgram.class,
                BuiltInCharSet.class,
                BuiltInCharTab.class,
                BuiltInCharacter.class,
                BuiltInCmds.class,
                BuiltInCoding.class,
                BuiltInComp.class,
                BuiltInComposite.class,
                BuiltInData.class,
                BuiltInDired.class,
                BuiltInDispNew.class,
                BuiltInDoc.class,
                BuiltInEditFns.class,
                BuiltInEmacs.class,
                BuiltInEval.class,
                BuiltInFileIO.class,
                BuiltInFileLock.class,
                BuiltInFloatFns.class,
                BuiltInFns.class,
                BuiltInFrame.class,
                BuiltInIndent.class,
                BuiltInKeyboard.class,
                BuiltInKeymap.class,
                BuiltInLRead.class,
                BuiltInMacros.class,
                BuiltInMarker.class,
                BuiltInMiniBuf.class,
                BuiltInPdumper.class,
                BuiltInPrint.class,
                BuiltInProcess.class,
                BuiltInSearch.class,
                BuiltInSyntax.class,
                BuiltInTerminal.class,
                BuiltInTextProp.class,
                BuiltInTimeFns.class,
                BuiltInTreesit.class,
                BuiltInWindow.class,
                BuiltInXDisp.class,
                BuiltInXFaces.class,
        };
        for (Class<?> builtIn : classes) {
            fory.register(builtIn, false);
        }
        BuiltInSearch.registerSerializer(fory);
        ELispCodings.registerSerializer(fory, false);

        for (Class<?> builtIn : classes) {
            fory.register(builtIn, true);
        }
        ELispCodings.registerSerializer(fory, true);

        // No other objects should store refs to ELispGlobals and ELispContext
        fory.register(ELispGlobals.class, false);
        fory.registerSerializer(ELispContext.class, new ContextSerializer(fory));
        fory.register(ELispGlobals.class, true);

        FORY = fory;
    }

    public static void serializeFromContext(OutputStream output, ELispContext context) {
        try {
            ELispContext.ContextSerializer.setCurrentContext(context);
            FORY.serializeJavaObject(output, context);
            FORY.reset();
        } finally {
            ELispContext.ContextSerializer.setCurrentContext(null);
        }
    }

    public static void deserializeIntoContext(SeekableByteChannel channel, ELispContext context) {
        try {
            ELispContext.ContextSerializer.setCurrentContext(context);
            FORY.deserializeJavaObject(ForyStreamReader.of(channel), ELispContext.class);
            FORY.reset();
        } finally {
            ELispContext.ContextSerializer.setCurrentContext(null);
        }
    }
}
