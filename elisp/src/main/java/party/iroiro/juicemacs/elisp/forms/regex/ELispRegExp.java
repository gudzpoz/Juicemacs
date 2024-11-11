package party.iroiro.juicemacs.elisp.forms.regex;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.strings.AbstractTruffleString;
import com.oracle.truffle.api.strings.TruffleString;
import org.eclipse.jdt.annotation.Nullable;

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

    /// Returns a callable regex object
    ///
    /// The call signature is `(StringLikeObject string, boolean search, int start, int end, ELispBuffer buffer)`,
    /// where `start` and `end` are both in codepoint offsets.
    ///
    /// The buffer argument can be null, as long as one does not use category/case/syntax table-related regex.
    @CompilerDirectives.TruffleBoundary
    public static RootCallTarget compile(TruffleLanguage<?> language,
                                         AbstractTruffleString string, TruffleString.Encoding encoding) {
        ELispRegExpCompiler.Compiled compiled = getCompiled(string, encoding);
        ELispRegExpNode node = new ELispRegExpNode(compiled);
        RegExpFunctionNode root = new RegExpFunctionNode(language, node, string.toString());
        return root.getCallTarget();
    }

    static ELispRegExpCompiler.Compiled getCompiled(AbstractTruffleString string, TruffleString.Encoding encoding) {
        @Nullable AbstractTruffleString whitespaceRegExp = null; // TODO
        ELispRegExpParser parser = new ELispRegExpParser(string, whitespaceRegExp, encoding);
        ELispRegExpParser.REAst ast = parser.parse();
        return ELispRegExpCompiler.compile(ast, parser.getMaxGroup());
    }

}
