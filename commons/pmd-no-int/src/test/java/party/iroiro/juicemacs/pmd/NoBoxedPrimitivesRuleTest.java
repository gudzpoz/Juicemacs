package party.iroiro.juicemacs.pmd;

import net.sourceforge.pmd.test.SimpleAggregatorTst;

class NoBoxedPrimitivesRuleTest extends SimpleAggregatorTst {
    @Override
    protected void setUp() {
        addRule("party/iroiro/juicemacs/pmd/NoBoxedPrimitivesRule.xml", "NoBoxedPrimitivesRule");
    }
}
