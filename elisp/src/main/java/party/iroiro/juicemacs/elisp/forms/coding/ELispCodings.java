package party.iroiro.juicemacs.elisp.forms.coding;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import org.apache.fory.Fory;
import org.apache.fory.serializer.EnumSerializer;
import org.jspecify.annotations.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInEval;
import party.iroiro.juicemacs.elisp.forms.BuiltInFns;
import party.iroiro.juicemacs.elisp.forms.coding.CodingSystemRawText.RawCoding;
import party.iroiro.juicemacs.elisp.forms.coding.CodingSystemUndecided.DetectingCodingSystem;
import party.iroiro.juicemacs.elisp.forms.coding.CodingSystemUtf8.BomDetectingCodingSystem;
import party.iroiro.juicemacs.elisp.forms.coding.CodingSystemUtf8.Utf8Codec;
import party.iroiro.juicemacs.elisp.forms.coding.ELispCharset.CharsetMethod;
import party.iroiro.juicemacs.elisp.forms.coding.ELispCodingSystem.Spec;
import party.iroiro.juicemacs.elisp.forms.coding.ELispCodingSystemType.EolDetectingCodingSystem;
import party.iroiro.juicemacs.elisp.forms.coding.EolAwareStringBuilder.EndOfLine;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;

import java.io.IOException;
import java.nio.channels.SeekableByteChannel;
import java.util.*;

import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

/// A utility class for handling Emacs coding logic implemented in this
/// [party.iroiro.juicemacs.elisp.forms.coding] package
///
/// ## Prerequisites
///
/// To understand how Emacs codings and this package function, one is recommended to
/// first read some Emacs docs before continue reading this doc.
///
/// - [Emacs Coding System](https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Systems.html)
///   explains how one can use coding systems with ELisp code, but barely touches how it is implemented.
/// - The docstring of [`define-coding-system`](http://doc.endlessparentheses.com/Fun/define-coding-system.html)
///   explains how one can define a coding system and can give you a rough impression of what is a coding system.
/// - Comments in [`src/coding.c`](https://github.com/emacs-mirror/emacs/blob/master/src/coding.c) provides
///   some details about how decoders/encoders of coding systems are implemented.
///
/// And the following documentation serves to put all these together to have high-level understanding of
/// Emacs codings and how we can re-implement them in Java in an OOP way.
///
/// ## Global variables
///
/// There are some global variables that are worth mentioning:
/// - `Vcoding_system_hash_table`: Mapping between coding system symbol (used by ELisp code)
///   to the specs of defined coding systems, including attrs, aliases and eol types.
/// - `coding_priorities` and `coding_categories`: These two are used together to determine which
///   coding system to prioritize when detecting coding system.
///
/// ## Coding detection
///
/// Emacs provides support for several coding types. Strictly speaking, the "coding detection"
/// in Emacs does not detect coding systems but rather detect the specific "coding type" of
/// a buffer. To go from a coding type to a specific coding system, Emacs uses its coding system
/// priorities (set by `set-coding-system-priority`).
///
/// Also, there are several other things to facilitate coding detection:
/// - The `:bom` attribute can contain a cons, which the UTF-8/16 detector uses to find the
///   corresponding LE/BE variants.
/// - The `undecided` pseudo-coding system has several other attributes that serve as some sort of
///   priorities, e.g. `:prefer-utf-8`.
///
/// Eol types are also detected in this process.
///
/// ### Updating buffer codings
///
/// `(insert-file-contents FILE VISIT)` also sets the coding of the current buffer to the detected
/// coding, making things a bit more complicated: `insert-file-contents` must be aware of the coding
/// detection process.
///
/// ## Decoding/Encoding
///
/// TODO
public final class ELispCodings {
    private static final Map<ELispSymbol, ELispCodingSystemType> CODING_SYSTEM_TYPES;
    static final CodingSystemCharset CODING_SYSTEM_CHARSET = new CodingSystemCharset();
    static final CodingSystemCcl CODING_SYSTEM_CCL = new CodingSystemCcl();
    static final CodingSystemUtf16 CODING_SYSTEM_UTF_16 = new CodingSystemUtf16();
    static final CodingSystemIso2022 CODING_SYSTEM_ISO_2022 = new CodingSystemIso2022();
    static final CodingSystemEmacsMule CODING_SYSTEM_EMACS_MULE = new CodingSystemEmacsMule();
    static final CodingSystemShiftJis CODING_SYSTEM_SHIFT_JIS = new CodingSystemShiftJis();
    static final CodingSystemBig5 CODING_SYSTEM_BIG_5 = new CodingSystemBig5();
    static final CodingSystemRawText CODING_SYSTEM_RAW_TEXT = new CodingSystemRawText();
    static final CodingSystemUtf8 CODING_SYSTEM_UTF_8 = new CodingSystemUtf8();
    static final CodingSystemUndecided CODING_SYSTEM_UNDECIDED = new CodingSystemUndecided();

    static {
        ELispCodingSystemType[] types = {
                CODING_SYSTEM_CHARSET,
                CODING_SYSTEM_CCL,
                CODING_SYSTEM_UTF_16,
                CODING_SYSTEM_ISO_2022,
                CODING_SYSTEM_EMACS_MULE,
                CODING_SYSTEM_SHIFT_JIS,
                CODING_SYSTEM_BIG_5,
                CODING_SYSTEM_RAW_TEXT,
                CODING_SYSTEM_UTF_8,
                CODING_SYSTEM_UNDECIDED,
        };
        Map<ELispSymbol, ELispCodingSystemType> entries = new HashMap<>(types.length);
        for (ELispCodingSystemType type : types) {
            entries.put(type.codingType(), type);
        }
        CODING_SYSTEM_TYPES = Map.copyOf(entries);
    }

    private final HashMap<ELispSymbol, ELispCodingSystem> codingSpecTable = new HashMap<>();
    private final ELispCodingDetector detector = new ELispCodingDetector();
    @Nullable
    public ELispCodingSystem safeTerminalCoding = null;

    ELispCodingDetector getDetector() {
        return detector;
    }

    public int initExtraAttrs(ELispSymbol codingType, ELispVector attrs, Object[] args, Object charsetList) {
        ELispCodingSystemType type = getCodingSystemType(codingType);
        if (type == null) {
            throw ELispSignals.error("Invalid coding system type");
        }
        return type.initExtraAttrs(attrs, args, charsetList);
    }

    @TruffleBoundary
    public void addCodingSystem(ELispSymbol codingType, ELispSymbol name, ELispCodingSystem.Spec spec,
                                int prioritizedCategory) {
        ELispCodingSystemType type = Objects.requireNonNull(getCodingSystemType(codingType));
        codingSpecTable.put(name, createCodingSystem(spec, type));

        CODING_SYSTEM_LIST.setValue(ELispCons.cons(name, CODING_SYSTEM_LIST.getValue()));
        ELispString nameString = name.name();
        Object assoc = BuiltInFns.FAssoc.assocEqual(nameString, CODING_SYSTEM_ALIST.getValue());
        if (isNil(assoc)) {
            CODING_SYSTEM_ALIST.setValue(ELispCons.cons(
                    ELispCons.listOf(nameString),
                    CODING_SYSTEM_ALIST.getValue()
            ));
        }

        if (prioritizedCategory != -1) {
            detector.tryInitPriority(prioritizedCategory, name);
        }
    }

    private static ELispCodingSystem createCodingSystem(ELispCodingSystem.Spec spec, ELispCodingSystemType type) {
        ELispSymbol[] eolTypes = spec.eolTypes();
        if (eolTypes.length == 1) {
            ELispSymbol eolType = eolTypes[0];
            EolAwareStringBuilder.EndOfLine eol =
                    eolType == DOS ? EolAwareStringBuilder.EndOfLine.CR_LF
                            : (eolType == MAC ? EolAwareStringBuilder.EndOfLine.CR
                            : EolAwareStringBuilder.EndOfLine.LF);
            return type.create(spec, eol);
        }
        return new ELispCodingSystemType.EolDetectingCodingSystem(spec, eolTypes);
    }

    @TruffleBoundary
    @Nullable
    public ELispCodingSystem getCodingSystem(ELispSymbol name) {
        return codingSpecTable.get(name);
    }

    @TruffleBoundary
    public ELispCodingSystem resolveCodingSystem(ELispSymbol name) {
        ELispCodingSystem system = codingSpecTable.get(name);
        system = system == null ? codingSpecTable.get(UNDECIDED) : system;
        if (system == null) {
            throw BuiltInEval.FSignal.signal(CODING_SYSTEM_ERROR, name);
        }
        return system;
    }

    @TruffleBoundary
    @Nullable
    private ELispCodingSystemType getCodingSystemType(ELispSymbol name) {
        return CODING_SYSTEM_TYPES.get(name);
    }

    @TruffleBoundary
    public ELispString.Builder decode(
            ELispCodingSystem coding,
            SeekableByteChannel channel,
            long start, long end,
            ValueStorage.@Nullable Forwarded codingStorage) throws IOException {
        while (true) {
            try {
                return coding.decode(this, new ByteIterator(channel, start, end));
            } catch (ELispCodingSystem.OtherCodingDetectedException other) {
                coding = Objects.requireNonNull(getCodingSystem(other.getCoding()));
            } finally {
                if (codingStorage != null) {
                    codingStorage.setValue(coding.getSpec().name());
                }
            }
        }
    }

    @SuppressWarnings({"rawtypes", "unchecked", "GetClassOnEnum"})
    public static void registerSerializer(Fory fory) {
        for (CharsetMethod method : CharsetMethod.values()) {
            fory.register(method.getClass());
        }
        Class<?> clazz = CharsetMethod.class;
        // TODO: until fory fixes abstract enum serialization;
        //       remember to remove the @Suppress annotation then.
        //       https://github.com/apache/fory/issues/2695
        fory.registerSerializer(CharsetMethod.class, new EnumSerializer(fory, (Class<Enum>) clazz));
        for (Class<?> system : new Class<?>[]{
                // CharsetMethod.class,
                ELispCodings.class,
                ELispCharset.class,
                ELispCodingDetector.class,
                Spec.class,

                BomDetectingCodingSystem.class,
                DetectingCodingSystem.class,
                EolDetectingCodingSystem.class,
                RawCoding.class,
                Utf8Codec.class,

                EndOfLine.class,
        }) {
            fory.register(system);
        }
        for (ELispCodingSystemType type : CODING_SYSTEM_TYPES.values()) {
            fory.register(type.getClass());
        }
    }
}
