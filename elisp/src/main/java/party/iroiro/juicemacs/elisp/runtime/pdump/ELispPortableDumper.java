package party.iroiro.juicemacs.elisp.runtime.pdump;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.utilities.CyclicAssumption;
import org.apache.fury.Fury;
import org.apache.fury.config.Language;
import org.apache.fury.io.FuryStreamReader;
import org.apache.fury.logging.LoggerFactory;
import org.apache.fury.serializer.ExternalizableSerializer;
import party.iroiro.juicemacs.elisp.forms.ELispBuiltIns;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispGlobals;
import party.iroiro.juicemacs.elisp.runtime.objects.*;
import party.iroiro.juicemacs.elisp.runtime.pdump.serializers.*;
import party.iroiro.juicemacs.elisp.runtime.scopes.FunctionStorage;
import party.iroiro.juicemacs.elisp.runtime.scopes.ThreadLocalStorage;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.mule.*;
import party.iroiro.juicemacs.piecetree.PieceTreeBase;
import party.iroiro.juicemacs.piecetree.meta.IntervalPieceTree;
import party.iroiro.juicemacs.piecetree.meta.MarkerPieceTree;

import java.io.OutputStream;
import java.nio.channels.SeekableByteChannel;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInBaseNode.JAVA_SOURCE;

/// Portable dumper for ELisp
public final class ELispPortableDumper {
    private static Fury getFury(ELispContext context) {
        LoggerFactory.disableLogging();
        Fury fury = Fury.builder()
                .withLanguage(Language.JAVA)
                .withAsyncCompilation(true)
                .withRefTracking(true)
                .requireClassRegistration(false)
                .suppressClassRegistrationWarnings(true)
                .ignoreBasicTypesRef(true)
                .ignoreStringRef(true)
                .registerGuavaTypes(false)
                .build();

        // TODO: Register *all* classes (even external private ones?)
        //   and make fury a static field to support GraalVM.

        registerSerializers(fury);

        // Context
        fury.registerSerializer(ELispContext.class, new ELispContext.ContextSerializer(fury, context));
        fury.register(ELispGlobals.class);
        fury.register(ELispBuiltIns.class);
        for (ELispBuiltIns builtIn : context.globals().getBuiltIns()) {
            fury.register(builtIn.getClass());
        }
        return fury;
    }

    static void registerSerializers(Fury fury) {
        // Symbols
        MuleStringSerializer serializer = new MuleStringSerializer(fury);
        fury.registerSerializer(MuleByteArrayString.class, serializer);
        fury.registerSerializer(MuleTruffleString.class, serializer);
        fury.registerSerializer(MuleIntArrayString.class, serializer);
        fury.registerSerializer(MuleStringBuffer.class, serializer);
        fury.registerSerializer(ELispObarray.class, new ELispObarraySerializer(fury));
        fury.registerSerializer(ELispSymbol.class, new ELispSymbolSerializer(fury));
        // Symbol values
        fury.registerSerializer(ThreadLocalStorage.class,
                DumpUtils.stateless(fury, ThreadLocalStorage.class, () -> new ThreadLocalStorage(false)));
        fury.registerSerializer(ValueStorage.class, new ExternalizableSerializer<>(fury, ValueStorage.class));
        fury.register(ValueStorage.PlainValue.class);
        fury.register(ValueStorage.Forwarded.class);
        fury.register(ValueStorage.ForwardedBool.class);
        fury.register(ValueStorage.ForwardedLong.class);
        fury.register(ValueStorage.ForwardedPerKboard.class);
        fury.register(ValueStorage.ForwardedPerBuffer.class);
        fury.register(ValueStorage.BufferLocal.class);
        fury.register(ValueStorage.TrappedWrite.class);
        fury.register(FunctionStorage.class);

        // Objects
        fury.registerSerializer(ELispHashtable.class, new ELispHashtableSerializer(fury));
        fury.registerSerializer(ELispHashtable.ELispWeakHashtable.class, new ELispHashtableSerializer(fury));
        fury.registerSerializer(ELispCons.class, new ELispConsSerializer(fury));
        fury.register(ELispString.class);
        fury.register(ELispCharTable.class);
        fury.register(ELispCharTable.SubTable.class);
        fury.register(ELispBoolVector.class);
        fury.register(ELispVector.class);
        fury.register(ELispRecord.class);
        fury.register(ELispInterpretedClosure.class);
        fury.register(ELispBytecode.class);

        // Buffers
        fury.register(ELispBuffer.class);
        fury.register(ELispBuffer.MarkerMoveNotifier.class);
        fury.registerSerializer(ELispMarker.class, new ELispMarkerSerializer(fury));
        fury.registerSerializer(MarkerPieceTree.Marker.class, DumpUtils.never(fury, MarkerPieceTree.Marker.class));
        fury.registerSerializer(PieceTreeBase.class, new PieceTreeSerializer(fury));
        fury.registerSerializer(IntervalPieceTree.class, new PieceTreeSerializer.Intervals(fury));
        fury.registerSerializer(MarkerPieceTree.class, new PieceTreeSerializer.Marks(fury));

        // Truffle internals
        fury.registerSerializer(CyclicAssumption.class, new CyclicAssumptionSerializer(fury));
        fury.registerSerializer(Assumption.class, DumpUtils.never(fury, Assumption.class));
        Class<? extends MaterializedFrame> frameClass = Truffle.getRuntime().createMaterializedFrame(new Object[0]).getClass();
        fury.registerSerializer(frameClass, new MaterializedFrameSerializer(fury));
        fury.registerSerializer(ELispSubroutine.class, DumpUtils.never(fury, ELispSubroutine.class));
        fury.registerSerializer(JAVA_SOURCE.getClass(), new SourceSerializer(fury));
    }

    public static void serializeFromContext(OutputStream output, ELispContext context) {
        Fury fury = getFury(context);
        fury.serializeJavaObject(output, context);
    }

    public static void deserializeIntoContext(SeekableByteChannel channel, ELispContext context) {
        Fury fury = getFury(context);
        fury.deserializeJavaObject(FuryStreamReader.of(channel), ELispContext.class);
    }
}
