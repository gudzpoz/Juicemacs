package party.iroiro.juicemacs.elisp.forms.regex;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.strings.AbstractTruffleString;
import com.oracle.truffle.api.strings.TruffleString;
import com.oracle.truffle.api.strings.TruffleStringBuilderUTF32;
import com.oracle.truffle.api.strings.TruffleStringIterator;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;

public abstract class ELispRegExp {

    public static final class RegExpFunctionNode extends RootNode {

        @SuppressWarnings("FieldMayBeFinal")
        @Child
        private ELispRegExpNode node;
        private final String regexp;

        private RegExpFunctionNode(TruffleLanguage<?> language, ELispRegExpNode node, String regexp) {
            super(language, ELispRegExpNode.REGEXP_FRAME_DESCRIPTOR);
            this.node = node;
            this.regexp = regexp;
            adoptChildren();
        }

        @Override
        public Object execute(VirtualFrame frame) {
            return node.execute(frame);
        }

        @Override
        public String getName() {
            return regexp;
        }

        @Override
        public String toString() {
            return "RegExpFunctionNode[" + getName() + "]";
        }
    }

    public record CompiledRegExp(RootCallTarget callTarget) {
        public Object call(AbstractTruffleString value, boolean search, int from, int end, Object buffer) {
            return callTarget.call(value, search, from, end, buffer); // NOPMD
        }
        public Object call(ELispBuffer value, boolean search, int from, int end) {
        return callTarget.call(value, search, from, end, value); // NOPMD
        }
    }

    /// Returns a callable regex object
    ///
    /// The call signature is `(StringLikeObject string, boolean search, int start, int end, ELispBuffer buffer)`,
    /// where `start` and `end` are both in codepoint offsets.
    ///
    /// The buffer argument can be null, as long as one does not use category/case/syntax table-related regex.
    @CompilerDirectives.TruffleBoundary
    public static CompiledRegExp compile(TruffleLanguage<?> language,
                                         AbstractTruffleString string,
                                         @Nullable AbstractTruffleString whitespaceRegExp,
                                         TruffleString.Encoding encoding,
                                         @Nullable ELispCharTable canon) {
        ELispRegExpCompiler.Compiled compiled = getCompiled(string, whitespaceRegExp, encoding, canon);
        ELispRegExpNode node = new ELispRegExpNode(compiled, canon != null);
        RegExpFunctionNode root = new RegExpFunctionNode(language, node, string.toString());
        return new CompiledRegExp(root.getCallTarget());
    }

    public static ELispString quote(AbstractTruffleString string, TruffleString.Encoding encoding) {
        int length = string.codePointLengthUncached(encoding);
        TruffleStringIterator i = string.createCodePointIteratorUncached(encoding);
        TruffleStringBuilderUTF32 quoted = TruffleStringBuilderUTF32.createUTF32(length);
        while (i.hasNext()) {
            int c = i.nextUncached();
            boolean escapeNeeded = switch (c) {
                case '^', '$', '+', '?', '*', '.', '[', '\\' -> true;
                default -> false;
            };
            if (escapeNeeded) {
                quoted.appendCodePointUncached('\\');
            }
            quoted.appendCodePointUncached(c);
        }
        return new ELispString(quoted.toStringUncached());
    }

    static ELispRegExpCompiler.Compiled getCompiled(AbstractTruffleString string,
                                                    @Nullable AbstractTruffleString whitespaceRegExp,
                                                    TruffleString.Encoding encoding,
                                                    @Nullable ELispCharTable canon) {
        ELispRegExpParser parser = new ELispRegExpParser(string, whitespaceRegExp, encoding);
        ELispRegExpParser.REAst ast = parser.parse();
        return ELispRegExpCompiler.compile(ast, parser.getMaxGroup(), canon);
    }

}
