package party.iroiro.juicemacs.elisp.forms.coding;

import com.oracle.truffle.api.nodes.ControlFlowException;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;

import java.io.IOException;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asLong;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public abstract class ELispCodingSystem {
    private final ELispCodingSystemType type;
    private final Spec spec;
    private final EolAwareStringBuilder.EndOfLine eol;

    ELispCodingSystem(
            ELispCodingSystemType type,
            Spec spec,
            EolAwareStringBuilder.EndOfLine eol
    ) {
        this.type = type;
        this.spec = spec;
        this.eol = eol;
    }

    public Spec getSpec() {
        return spec;
    }

    ELispCodingSystemType getType() {
        return type;
    }

    EolAwareStringBuilder newStringBuilder() {
        return new EolAwareStringBuilder(eol);
    }

    abstract ELispString.Builder decode(ELispCodings codings, ByteIterator input) throws OtherCodingDetectedException, IOException;

    public static final class OtherCodingDetectedException extends ControlFlowException {
        private final ELispSymbol coding;

        public OtherCodingDetectedException(ELispSymbol coding) {
            this.coding = coding;
        }

        public ELispSymbol getCoding() {
            return coding;
        }
    }

    public record Spec(ELispSymbol name, ELispVector attrs, ELispCons aliases, ELispSymbol[] eolTypes) {
        public Spec(ELispSymbol name, ELispVector attrs, ELispCons aliases, ELispSymbol eolType) {
            this(name, attrs, aliases, new ELispSymbol[]{eolType});
        }
        public Object type() {
            return attrs.get(CODING_ATTR_TYPE);
        }
        public Object getBaseName() {
            return attrs.get(CODING_ATTR_BASE_NAME);
        }
        public Object getPlist() {
            return attrs.get(CODING_ATTR_PLIST);
        }
        public boolean preferUtf8() {
            return !isNil(attrs.get(CODING_ATTR_UNDECIDED_PREFER_UTF_8));
        }
        public boolean inhibitNullByteDetection() {
            return !isNil(attrs.get(CODING_ATTR_UNDECIDED_INHIBIT_NULL_BYTE_DETECTION));
        }
        public boolean inhibitIsoEscapeDetection() {
            return !isNil(attrs.get(CODING_ATTR_UNDECIDED_INHIBIT_ISO_ESCAPE_DETECTION));
        }
        public long isoFlags() {
            return asLong(attrs.get(CODING_ATTR_ISO_FLAGS));
        }
        public boolean isAsciiCompat() {
            return !isNil(attrs.get(CODING_ATTR_ASCII_COMPAT));
        }
        public Object charsetList() {
            return attrs.get(CODING_ATTR_CHARSET_LIST);
        }
        public Object safeCharsets() {
            return attrs.get(CODING_ATTR_SAFE_CHARSETS);
        }
        public Object utfBom() {
            return attrs.get(CODING_ATTR_UTF_BOM);
        }
        public long category() {
            return asLong(attrs.get(CODING_ATTR_CATEGORY));
        }
    }
}
