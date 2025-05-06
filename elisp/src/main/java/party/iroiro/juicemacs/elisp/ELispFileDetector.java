package party.iroiro.juicemacs.elisp;

import java.io.IOException;
import java.nio.charset.Charset;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleFile.FileTypeDetector;
import org.eclipse.jdt.annotation.Nullable;

public class ELispFileDetector implements FileTypeDetector {
    public static final String EXTENSION = ".el";
    public static final String BYTECODE_EXTENSION = ".elc";

    @Nullable
    @Override
    public String findMimeType(TruffleFile file) {
                String name = file.getName();
        if (name != null && (name.endsWith(EXTENSION) || name.endsWith(BYTECODE_EXTENSION))) {
            return ELispLanguage.MIME_TYPE;
        }
        return null;
    }

    @Nullable
    @Override
    public Charset findEncoding(TruffleFile file) {
        return null;
    }

}
