package party.iroiro.juicemacs.elisp;

import java.io.IOException;
import java.nio.charset.Charset;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleFile.FileTypeDetector;
import org.eclipse.jdt.annotation.Nullable;

public class ELispFileDetector implements FileTypeDetector {
    public static final String EXTENSION = ".el";

    @Nullable
    @Override
    public String findMimeType(TruffleFile file) throws IOException {
                String name = file.getName();
        if (name != null && name.endsWith(EXTENSION)) {
            return ELispLanguage.MIME_TYPE;
        }
        return null;
    }

    @Nullable
    @Override
    public Charset findEncoding(TruffleFile file) throws IOException {
        return null;
    }

}
