package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import party.iroiro.juicemacs.elisp.nodes.ELispExpressionNode;
import party.iroiro.juicemacs.elisp.nodes.ReadFunctionArgNode;

@NodeChild(value = "arguments", type = ReadFunctionArgNode[].class)
public abstract class ELispBuiltInBaseNode extends ELispExpressionNode {
    public static final Source JAVA_SOURCE = Source.newBuilder("java", "", "<built-in>")
            .content(Source.CONTENT_NONE)
            .internal(true)
            .build();

    @Override
    public SourceSection getSourceSection() {
        return JAVA_SOURCE.createUnavailableSection();
    }
}
