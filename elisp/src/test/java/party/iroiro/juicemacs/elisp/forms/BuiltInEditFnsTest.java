package party.iroiro.juicemacs.elisp.forms;

public class BuiltInEditFnsTest extends BaseFormTest {
    private final static Object[] TESTS = {
            "(equal (system-name) (progn (setq system-name \"blahblah\") (system-name)))", false,
            "(equal \"blahblah\" (progn (setq system-name \"blahblah\") (system-name)))", true,
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }
}
