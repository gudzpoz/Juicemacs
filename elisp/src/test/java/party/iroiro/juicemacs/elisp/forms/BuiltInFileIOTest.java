package party.iroiro.juicemacs.elisp.forms;

public class BuiltInFileIOTest extends BaseFormTest {
    private static final Object[] TESTS = new Object[]{
            "(equal (expand-file-name \"~\") \"" + System.getProperty("user.home") + "\")", true,
            "(equal (expand-file-name \"\") (expand-file-name \".\"))", true,
            "(equal (expand-file-name \"/\") \"/\")", true,
            "(equal (expand-file-name \"usr\" \"/\") \"/usr\")", true,
            "(file-directory-p \"\")", true,
            "(file-directory-p \".\")", true,
            "(file-directory-p (expand-file-name \"~\"))", true,
    };

    @Override
    protected Object[] entries() {
        return TESTS;
    }
}
