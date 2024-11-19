package party.iroiro.juicemacs.elisp.forms.regex;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.mule.MuleString;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import java.util.PrimitiveIterator;

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
        public Object call(MuleString value, boolean search, int from, int end, Object buffer) {
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
                                         MuleString string,
                                         @Nullable MuleString whitespaceRegExp,
                                         @Nullable ELispCharTable canon) {
        ELispRegExpCompiler.Compiled compiled = getCompiled(string, whitespaceRegExp, canon);
        ELispRegExpNode node = new ELispRegExpNode(compiled, canon != null);
        RegExpFunctionNode root = new RegExpFunctionNode(language, node, string.toString());
        return new CompiledRegExp(root.getCallTarget());
    }

    public static ELispString quote(MuleString string) {
        PrimitiveIterator.OfInt i = string.iterator(0);
        MuleStringBuffer quoted = new MuleStringBuffer();
        while (i.hasNext()) {
            int c = i.nextInt();
            boolean escapeNeeded = switch (c) {
                case '^', '$', '+', '?', '*', '.', '[', '\\' -> true;
                default -> false;
            };
            if (escapeNeeded) {
                quoted.append('\\');
            }
            quoted.append(c);
        }
        return new ELispString(quoted.build());
    }

    static ELispRegExpCompiler.Compiled getCompiled(MuleString string,
                                                    @Nullable MuleString whitespaceRegExp,
                                                    @Nullable ELispCharTable canon) {
        ELispRegExpParser parser = new ELispRegExpParser(string, whitespaceRegExp);
        ELispRegExpParser.REAst ast = parser.parse();
        return ELispRegExpCompiler.compile(ast, parser.getMaxGroup(), canon);
    }

}
