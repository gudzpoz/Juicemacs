package party.iroiro.juicemacs.elisp.forms.regex;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.strings.AbstractTruffleString;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.string.MuleStringBuilder;
import party.iroiro.juicemacs.elisp.runtime.string.StringSupport;
import party.iroiro.juicemacs.piecetree.StringNodes;

public abstract class ELispRegExp {

    public static final class RegExpFunctionNode extends RootNode {

        @SuppressWarnings("FieldMayBeFinal")
        @Child
        private ELispRegExpNode node;
        private final String regexp;

        RegExpFunctionNode(@Nullable TruffleLanguage<?> language, ELispRegExpNode node, String regexp) {
            super(language, ELispRegExpNode.REGEXP_FRAME_DESCRIPTOR);
            this.node = node;
            this.regexp = regexp;
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
        public Object call(ELispString value, boolean search, long from, long end, Object buffer) {
            return callTarget.call(value, search, from, end, buffer);
        }
        public Object call(ELispBuffer value, boolean search, long from, long end) {
            return callTarget.call(value, search, from, end, value);
        }
    }

    /// Returns a callable regex object
    ///
    /// The call signature is `(StringLikeObject string, boolean search, int start, int end, ELispBuffer buffer)`,
    /// where `start` and `end` are both in codepoint offsets.
    ///
    /// The buffer argument can be null, as long as one does not use category/case/syntax table-related regex.
    @TruffleBoundary
    public static CompiledRegExp compile(TruffleLanguage<?> language,
                                         ELispString string,
                                         @Nullable ELispString whitespaceRegExp,
                                         @Nullable ELispCharTable canon) {
        ELispRegExpCompiler.Compiled compiled = getCompiled(string, whitespaceRegExp, canon);
        ELispRegExpNode node = new ELispRegExpNode(compiled, canon != null);
        RegExpFunctionNode root = new RegExpFunctionNode(language, node, string.toString());
        return new CompiledRegExp(root.getCallTarget());
    }

    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private static final int[] ESCAPE_NEEDED = {'^', '$', '+', '?', '*', '.', '[', '\\'};
    public static ELispString quote(ELispString string) {
        AbstractTruffleString inner = string.value();
        int length = StringNodes.length(inner);
        int at = 0, prev = 0;
        MuleStringBuilder quoted = new MuleStringBuilder();
        while (prev < length) {
            at = StringSupport.indexOfAny(inner, ESCAPE_NEEDED, at, length);
            int end = at < 0 ? length : at;
            quoted.append(StringNodes.substring(inner, prev, end - prev), string.state());
            prev = end;
            if (at < 0) {
                break;
            }
            quoted.appendCodePoint('\\');
            at++;
        }
        return new ELispString(quoted.build());
    }

    static ELispRegExpCompiler.Compiled getCompiled(ELispString string,
                                                    @Nullable ELispString whitespaceRegExp,
                                                    @Nullable ELispCharTable canon) {
        ELispRegExpParser parser = new ELispRegExpParser(string, whitespaceRegExp);
        REAst ast = parser.parse();
        return ELispRegExpCompiler.compile(ast, parser.getMaxGroup(), canon);
    }

}
