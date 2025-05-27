package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.forms.coding.ELispCodingSystem;
import party.iroiro.juicemacs.elisp.forms.coding.ELispCodings;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;
import party.iroiro.juicemacs.mule.MuleString;
import party.iroiro.juicemacs.mule.MuleStringBuffer;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SeekableByteChannel;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.Set;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInUtils.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInFileIO extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInFileIOFactory.getFactories();
    }

    private static TruffleFile generateTempFileName(TruffleFile dir, String prefix, String suffix) {
        String hexString = Long.toHexString(Integer.toUnsignedLong(new Random().nextInt()));
        hexString = "0".repeat(8 - hexString.length()) + hexString;
        for (int i = 0; i < Integer.MAX_VALUE; i++) {
            TruffleFile resolved = dir.resolve(prefix + hexString + i + suffix);
            if (!resolved.exists()) {
                return resolved;
            }
        }
        throw ELispSignals.error("unable to create file");
    }

    /**
     * <pre>
     * Return FILENAME's handler function for OPERATION, if it has one.
     * Otherwise, return nil.
     * A file name is handled if one of the regular expressions in
     * `file-name-handler-alist' matches it.
     *
     * If OPERATION equals `inhibit-file-name-operation', then ignore
     * any handlers that are members of `inhibit-file-name-handlers',
     * but still do run any other handlers.  This lets handlers
     * use the standard functions without calling themselves recursively.
     * </pre>
     */
    @ELispBuiltIn(name = "find-file-name-handler", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFindFileNameHandler extends ELispBuiltInBaseNode {
        @Specialization
        public static Object findFileNameHandler(Object filename, Object operation) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Return the directory component in file name FILENAME.
     * Return nil if FILENAME does not include a directory.
     * Otherwise return a directory name.
     * Given a Unix syntax file name, returns a string ending in slash.
     * </pre>
     */
    @ELispBuiltIn(name = "file-name-directory", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileNameDirectory extends ELispBuiltInBaseNode {
        @Specialization
        public Object fileNameDirectory(ELispString filename) {
            TruffleLanguage.Env env = getContext().truffleEnv();
            String sep = env.getFileNameSeparator();
            String name = filename.toString();
            if (seemsLikeDirectory(name, sep)) {
                return filename;
            }
            TruffleFile parent = env.getPublicTruffleFile(name).getParent();
            if (parent == null) {
                return false;
            }
            String dir = parent.toString();
            return new ELispString(seemsLikeDirectory(dir, sep) ? dir : parent + sep);
        }

        private static boolean seemsLikeDirectory(String name, String sep) {
            return name.endsWith(sep) || name.endsWith("/") || name.endsWith("\\");
        }
    }

    /**
     * <pre>
     * Return file name FILENAME sans its directory.
     * For example, in a Unix-syntax file name,
     * this is everything after the last slash,
     * or the entire name if it contains no slash.
     * </pre>
     */
    @ELispBuiltIn(name = "file-name-nondirectory", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileNameNondirectory extends ELispBuiltInBaseNode {
        @Specialization
        public ELispString fileNameNondirectory(ELispString filename) {
            TruffleLanguage.Env env = getContext().truffleEnv();
            String sep = env.getFileNameSeparator();
            String name = filename.toString();
            if (name.endsWith(sep) || name.endsWith("/") || name.endsWith("\\")) {
                return new ELispString("");
            }
            return new ELispString(Objects.requireNonNullElse(env.getPublicTruffleFile(name).getName(), ""));
        }
    }

    /**
     * <pre>
     * Return a directly usable directory name somehow associated with FILENAME.
     * A `directly usable' directory name is one that may be used without the
     * intervention of any file name handler.
     * If FILENAME is a directly usable file itself, return
     * \(file-name-as-directory FILENAME).
     * If FILENAME refers to a file which is not accessible from a local process,
     * then this should return nil.
     * The `call-process' and `start-process' functions use this function to
     * get a current directory to run processes in.
     * </pre>
     */
    @ELispBuiltIn(name = "unhandled-file-name-directory", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUnhandledFileNameDirectory extends ELispBuiltInBaseNode {
        @Specialization
        public static Void unhandledFileNameDirectory(Object filename) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a string representing the file name FILE interpreted as a directory.
     * This operation exists because a directory is also a file, but its name as
     * a directory is different from its name as a file.
     * The result can be used as the value of `default-directory'
     * or passed as second argument to `expand-file-name'.
     * For a Unix-syntax file name, just appends a slash unless a trailing slash
     * is already present.
     * </pre>
     */
    @ELispBuiltIn(name = "file-name-as-directory", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileNameAsDirectory extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString fileNameAsDirectory(Object file) {
            return new ELispString(new MuleStringBuffer()
                    .append(asStr(file).value())
                    .append(MuleString.fromString(File.separator))
                    .build());
        }
    }

    /**
     * <pre>
     * Return non-nil if NAME ends with a directory separator character.
     * </pre>
     */
    @ELispBuiltIn(name = "directory-name-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDirectoryNameP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void directoryNameP(Object name) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Returns the file name of the directory named DIRECTORY.
     * This is the name of the file that holds the data for the directory DIRECTORY.
     * This operation exists because a directory is also a file, but its name as
     * a directory is different from its name as a file.
     * In Unix-syntax, this function just removes the final slash.
     * </pre>
     */
    @ELispBuiltIn(name = "directory-file-name", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDirectoryFileName extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString directoryFileName(ELispString directory) {
            return new ELispString(Path.of(directory.toString()).toString());
        }
    }

    /**
     * <pre>
     * Generate a new file whose name starts with PREFIX, a string.
     * Return the name of the generated file.  If DIR-FLAG is zero, do not
     * create the file, just its name.  Otherwise, if DIR-FLAG is non-nil,
     * create an empty directory.  The file name should end in SUFFIX.
     * Do not expand PREFIX; a non-absolute PREFIX is relative to the Emacs
     * working directory.  If TEXT is a string, insert it into the newly
     * created file.
     *
     * Signal an error if the file could not be created.
     *
     * This function does not grok magic file names.
     * </pre>
     */
    @ELispBuiltIn(name = "make-temp-file-internal", minArgs = 4, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FMakeTempFileInternal extends ELispBuiltInBaseNode {
        @Specialization
        public ELispString makeTempFileInternal(ELispString prefix, Object dirFlag, Object suffix, Object text) {
            TruffleLanguage.Env env = getContext().truffleEnv();
            TruffleFile file = env.getPublicTruffleFile(prefix.toString()).getAbsoluteFile();
            TruffleFile parent = Objects.requireNonNull(file.getParent());
            String suffixStr = isNil(suffix) ? "" : asStr(suffix).toString();
            if (dirFlag instanceof Long l && l == 0) {
                TruffleFile tempFile = generateTempFileName(parent, file.getName(), suffixStr);
                return new ELispString(tempFile.toString());
            }
            try {
                TruffleFile temp;
                if (isNil(dirFlag)) {
                    temp = env.createTempFile(parent, file.getName(), suffixStr);
                    if (!isNil(text)) {
                        try (BufferedWriter writer = temp.newBufferedWriter()) {
                            writer.write(asStr(text).toString());
                        }
                    }
                } else {
                    temp = generateTempFileName(parent, file.getName(), suffixStr);
                    temp.createDirectory();
                }
                return new ELispString(temp.toString());
            } catch (IOException e) {
                throw ELispSignals.reportFileError(e, prefix);
            }
        }
    }

    /**
     * <pre>
     * Generate temporary file name (string) starting with PREFIX (a string).
     *
     * This function tries to choose a name that has no existing file.
     * For this to work, PREFIX should be an absolute file name, and PREFIX
     * and the returned string should both be non-magic.
     *
     * There is a race condition between calling `make-temp-name' and
     * later creating the file, which opens all kinds of security holes.
     * For that reason, you should normally use `make-temp-file' instead.
     * </pre>
     */
    @ELispBuiltIn(name = "make-temp-name", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeTempName extends ELispBuiltInBaseNode {
        @Specialization
        public static Void makeTempName(Object prefix) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Append COMPONENTS to DIRECTORY and return the resulting string.
     * Each element in COMPONENTS must be a string or nil.
     * DIRECTORY or the non-final elements in COMPONENTS may or may not end
     * with a slash -- if they don't end with a slash, a slash will be
     * inserted before concatenating.
     * usage: (file-name-concat DIRECTORY &amp;rest COMPONENTS)
     * </pre>
     */
    @ELispBuiltIn(name = "file-name-concat", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FFileNameConcat extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fileNameConcat(Object directory, Object[] components) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Convert filename NAME to absolute, and canonicalize it.
     * Second arg DEFAULT-DIRECTORY is directory to start with if NAME is relative
     * \(does not start with slash or tilde); both the directory name and
     * a directory's file name are accepted.  If DEFAULT-DIRECTORY is nil or
     * missing, the current buffer's value of `default-directory' is used.
     * NAME should be a string that is a valid file name for the underlying
     * filesystem.
     *
     * File name components that are `.' are removed, and so are file name
     * components followed by `..', along with the `..' itself; note that
     * these simplifications are done without checking the resulting file
     * names in the file system.
     *
     * Multiple consecutive slashes are collapsed into a single slash, except
     * at the beginning of the file name when they are significant (e.g., UNC
     * file names on MS-Windows.)
     *
     * An initial \"~\" in NAME expands to your home directory.
     *
     * An initial \"~USER\" in NAME expands to USER's home directory.  If
     * USER doesn't exist, \"~USER\" is not expanded.
     *
     * To do other file name substitutions, see `substitute-in-file-name'.
     *
     * For technical reasons, this function can return correct but
     * non-intuitive results for the root directory; for instance,
     * \(expand-file-name ".." "/") returns "/..".  For this reason, use
     * \(directory-file-name (file-name-directory dirname)) to traverse a
     * filesystem tree, not (expand-file-name ".." dirname).  Note: make
     * sure DIRNAME in this example doesn't end in a slash, unless it's
     * the root directory.
     * </pre>
     */
    @ELispBuiltIn(name = "expand-file-name", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FExpandFileName extends ELispBuiltInBaseNode {
        @CompilerDirectives.TruffleBoundary
        @Specialization
        public static ELispString expandFileName(ELispString name, Object defaultDirectory) {
            return new ELispString(MuleString.fromString(
                    expandFileNamePath(name, defaultDirectory).toAbsolutePath().toString()
            ));
        }

        public static Path expandFileNamePath(ELispString name, Object defaultDirectory) {
            String path = name.toString();
            if (path.startsWith("~")) {
                path = System.getProperty("user.home") + path.substring(1);
            } else if (!path.startsWith("/")) {
                return Path.of(isNil(defaultDirectory)
                        ? System.getProperty("user.home") // TODO: default-directory
                        : defaultDirectory.toString(), path);
            }
            return Path.of(path);
        }
    }

    /**
     * <pre>
     * Substitute environment variables referred to in FILENAME.
     * `$FOO' where FOO is an environment variable name means to substitute
     * the value of that variable.  The variable name should be terminated
     * with a character not a letter, digit or underscore; otherwise, enclose
     * the entire variable name in braces.
     *
     * If FOO is not defined in the environment, `$FOO' is left unchanged in
     * the value of this function.
     *
     * If `/~' appears, all of FILENAME through that `/' is discarded.
     * If `//' appears, everything up to and including the first of
     * those `/' is discarded.
     * </pre>
     */
    @ELispBuiltIn(name = "substitute-in-file-name", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSubstituteInFileName extends ELispBuiltInBaseNode {
        @Specialization
        public static Void substituteInFileName(Object filename) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Copy FILE to NEWNAME.  Both args must be strings.
     * If NEWNAME is a directory name, copy FILE to a like-named file under
     * NEWNAME.  For NEWNAME to be recognized as a directory name, it should
     * end in a slash.
     *
     * This function always sets the file modes of the output file to match
     * the input file.
     *
     * The optional third argument OK-IF-ALREADY-EXISTS specifies what to do
     * if file NEWNAME already exists.  If OK-IF-ALREADY-EXISTS is nil,
     * signal a `file-already-exists' error without overwriting.  If
     * OK-IF-ALREADY-EXISTS is an integer, request confirmation from the user
     * about overwriting; this is what happens in interactive use with M-x.
     * Any other value for OK-IF-ALREADY-EXISTS means to overwrite the
     * existing file.
     *
     * Fourth arg KEEP-TIME non-nil means give the output file the same
     * last-modified time as the old one.  (This works on only some systems.)
     *
     * A prefix arg makes KEEP-TIME non-nil.
     *
     * If PRESERVE-UID-GID is non-nil, try to transfer the uid and gid of
     * FILE to NEWNAME.
     *
     * If PRESERVE-PERMISSIONS is non-nil, copy permissions of FILE to NEWNAME;
     * this includes the file modes, along with ACL entries and SELinux
     * context if present.  Otherwise, if NEWNAME is created its file
     * permission bits are those of FILE, masked by the default file
     * permissions.
     * </pre>
     */
    @ELispBuiltIn(name = "copy-file", minArgs = 2, maxArgs = 6)
    @GenerateNodeFactory
    public abstract static class FCopyFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Void copyFile(Object file, Object newname, Object okIfAlreadyExists, Object keepTime, Object preserveUidGid, Object preservePermissions) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Create a new directory named DIRECTORY.
     * </pre>
     */
    @ELispBuiltIn(name = "make-directory-internal", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeDirectoryInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Void makeDirectoryInternal(Object directory) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Delete the directory named DIRECTORY.  Does not follow symlinks.
     * </pre>
     */
    @ELispBuiltIn(name = "delete-directory-internal", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDeleteDirectoryInternal extends ELispBuiltInBaseNode {
        @Specialization
        public boolean deleteDirectoryInternal(ELispString directory) {
            TruffleFile file = getContext().truffleEnv().getPublicTruffleFile(directory.toString());
            try {
                file.delete();
            } catch (IOException e) {
                throw ELispSignals.reportFileError(e, directory);
            }
            return true;
        }
    }

    /**
     * <pre>
     * Delete file named FILENAME; internal use only.
     * If it is a symlink, remove the symlink.
     * If file has multiple names, it continues to exist with the other names.
     * </pre>
     */
    @ELispBuiltIn(name = "delete-file-internal", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDeleteFileInternal extends ELispBuiltInBaseNode {
        @Specialization
        public boolean deleteFileInternal(ELispString filename) {
            Path path = FExpandFileName.expandFileNamePath(filename, false);
            TruffleFile file = getContext().truffleEnv().getPublicTruffleFile(path.toString());
            try {
                file.delete();
            } catch (IOException e) {
                throw ELispSignals.reportFileError(e, filename);
            }
            return true;
        }
    }

    /**
     * <pre>
     * Return t if file FILENAME is on a case-insensitive filesystem.
     * Return nil if FILENAME does not exist or is not on a case-insensitive
     * filesystem, or if there was trouble determining whether the filesystem
     * is case-insensitive.
     * </pre>
     */
    @ELispBuiltIn(name = "file-name-case-insensitive-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileNameCaseInsensitiveP extends ELispBuiltInBaseNode {
        @Specialization
        public boolean fileNameCaseInsensitiveP(ELispString filename) {
            TruffleFile file = getContext().truffleEnv().getPublicTruffleFile(filename.toString());
            return file.exists();
        }
    }

    /**
     * <pre>
     * Rename FILE as NEWNAME.  Both args must be strings.
     * If file has names other than FILE, it continues to have those names.
     * If NEWNAME is a directory name, rename FILE to a like-named file under
     * NEWNAME.  For NEWNAME to be recognized as a directory name, it should
     * end in a slash.
     *
     * Signal a `file-already-exists' error if a file NEWNAME already exists
     * unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
     * An integer third arg means request confirmation if NEWNAME already exists.
     * This is what happens in interactive use with M-x.
     * </pre>
     */
    @ELispBuiltIn(name = "rename-file", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FRenameFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Void renameFile(Object file, Object newname, Object okIfAlreadyExists) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Give FILE additional name NEWNAME.  Both args must be strings.
     * If NEWNAME is a directory name, give FILE a like-named new name under
     * NEWNAME.
     *
     * Signal a `file-already-exists' error if a file NEWNAME already exists
     * unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
     * An integer third arg means request confirmation if NEWNAME already exists.
     * This is what happens in interactive use with M-x.
     * </pre>
     */
    @ELispBuiltIn(name = "add-name-to-file", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FAddNameToFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Void addNameToFile(Object file, Object newname, Object okIfAlreadyExists) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Make a symbolic link to TARGET, named LINKNAME.
     * If LINKNAME is a directory name, make a like-named symbolic link under
     * LINKNAME.
     *
     * Signal a `file-already-exists' error if a file LINKNAME already exists
     * unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
     * An integer third arg means request confirmation if LINKNAME already
     * exists, and expand leading "~" or strip leading "/:" in TARGET.
     * This happens for interactive use with M-x.
     * </pre>
     */
    @ELispBuiltIn(name = "make-symbolic-link", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FMakeSymbolicLink extends ELispBuiltInBaseNode {
        @Specialization
        public static Void makeSymbolicLink(Object target, Object linkname, Object okIfAlreadyExists) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if FILENAME is an absolute file name.
     * On Unix, absolute file names start with `/'.  In Emacs, an absolute
     * file name can also start with an initial `~' or `~USER' component,
     * where USER is a valid login name.
     * </pre>
     */
    @ELispBuiltIn(name = "file-name-absolute-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileNameAbsoluteP extends ELispBuiltInBaseNode {
        @Specialization
        public boolean fileNameAbsoluteP(ELispString filename) {
            return getContext().truffleEnv().getPublicTruffleFile(filename.toString()).isAbsolute();
        }
    }

    /**
     * <pre>
     * Return t if file FILENAME exists (whether or not you can read it).
     * Return nil if FILENAME does not exist, or if there was trouble
     * determining whether the file exists.
     * See also `file-readable-p' and `file-attributes'.
     * This returns nil for a symlink to a nonexistent file.
     * Use `file-symlink-p' to test for such links.
     * </pre>
     */
    @ELispBuiltIn(name = "file-exists-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileExistsP extends ELispBuiltInBaseNode {
        @Specialization
        public boolean fileExistsP(ELispString filename) {
            // TODO: TRAMP and other hooks
            Path path = FExpandFileName.expandFileNamePath(filename, false);
            return getContext().truffleEnv().getPublicTruffleFile(path.toString()).exists();
        }
    }

    /**
     * <pre>
     * Return t if FILENAME can be executed by you.
     * For a directory, this means you can access files in that directory.
     * \(It is generally better to use `file-accessible-directory-p' for that
     * purpose, though.)
     * </pre>
     */
    @ELispBuiltIn(name = "file-executable-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileExecutableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fileExecutableP(Object filename) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if file FILENAME exists and you can read it.
     * See also `file-exists-p' and `file-attributes'.
     * </pre>
     */
    @ELispBuiltIn(name = "file-readable-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileReadableP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean fileReadableP(ELispString filename) {
            return ELispContext.get(null).truffleEnv().getPublicTruffleFile(filename.asString()).isReadable();
        }
    }

    /**
     * <pre>
     * Return t if file FILENAME can be written or created by you.
     * </pre>
     */
    @ELispBuiltIn(name = "file-writable-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileWritableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fileWritableP(Object filename) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Access file FILENAME, and get an error if that does not work.
     * The second argument STRING is prepended to the error message.
     * If there is no error, returns nil.
     * </pre>
     */
    @ELispBuiltIn(name = "access-file", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FAccessFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Void accessFile(Object filename, Object string) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if file FILENAME is the name of a symbolic link.
     * The value is the link target, as a string.
     * Return nil if FILENAME does not exist or is not a symbolic link,
     * of there was trouble determining whether the file is a symbolic link.
     *
     * This function does not check whether the link target exists.
     * </pre>
     */
    @ELispBuiltIn(name = "file-symlink-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileSymlinkP extends ELispBuiltInBaseNode {
        @Specialization
        public boolean fileSymlinkP(ELispString filename) {
            TruffleLanguage.Env env = getContext().truffleEnv();
            TruffleFile file = env.getPublicTruffleFile(filename.toString());
            return file.isSymbolicLink();
        }
    }

    /**
     * <pre>
     * Return t if FILENAME names an existing directory.
     * Return nil if FILENAME does not name a directory, or if there
     * was trouble determining whether FILENAME is a directory.
     *
     * As a special case, this function will also return t if FILENAME is the
     * empty string (\"\").  This quirk is due to Emacs interpreting the
     * empty string (in some cases) as the current directory.
     *
     * Symbolic links to directories count as directories.
     * See `file-symlink-p' to distinguish symlinks.
     * </pre>
     */
    @ELispBuiltIn(name = "file-directory-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileDirectoryP extends ELispBuiltInBaseNode {
        @CompilerDirectives.TruffleBoundary
        @Specialization
        public static boolean fileDirectoryP(ELispString filename) {
            String path = filename.toString();
            TruffleFile file = ELispContext.get(null).truffleEnv().getPublicTruffleFile(path.isEmpty() ? "." : path);
            return file.isDirectory();
        }
    }

    /**
     * <pre>
     * Return t if FILENAME names a directory you can open.
     * This means that FILENAME must specify the name of a directory, and the
     * directory must allow you to open files in it.  If this isn't the case,
     * return nil.
     *
     * FILENAME can either be a directory name (eg. \"/tmp/foo/\") or the
     * file name of a file which is a directory (eg. \"/tmp/foo\", without
     * the final slash).
     *
     * In order to use a directory as a buffer's current directory, this
     * predicate must return true.
     * </pre>
     */
    @ELispBuiltIn(name = "file-accessible-directory-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileAccessibleDirectoryP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean fileAccessibleDirectoryP(ELispString filename) {
            TruffleFile file = ELispContext.get(null).truffleEnv()
                    .getPublicTruffleFile(FExpandFileName.expandFileNamePath(filename, false).toString());
            return file.isDirectory() && file.isReadable();
        }
    }

    /**
     * <pre>
     * Return t if FILENAME names a regular file.
     * This is the sort of file that holds an ordinary stream of data bytes.
     * Return nil if FILENAME does not exist or is not a regular file,
     * or there was trouble determining whether FILENAME is a regular file.
     * Symbolic links to regular files count as regular files.
     * See `file-symlink-p' to distinguish symlinks.
     * </pre>
     */
    @ELispBuiltIn(name = "file-regular-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileRegularP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fileRegularP(Object filename) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return SELinux context of file named FILENAME.
     * The return value is a list (USER ROLE TYPE RANGE), where the list
     * elements are strings naming the user, role, type, and range of the
     * file's SELinux security context.
     *
     * Return (nil nil nil nil) if the file is nonexistent,
     * or if SELinux is disabled, or if Emacs lacks SELinux support.
     * </pre>
     */
    @ELispBuiltIn(name = "file-selinux-context", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileSelinuxContext extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fileSelinuxContext(Object filename) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set SELinux context of file named FILENAME to CONTEXT.
     * CONTEXT should be a list (USER ROLE TYPE RANGE), where the list
     * elements are strings naming the components of a SELinux context.
     *
     * Value is t if setting of SELinux context was successful, nil otherwise.
     *
     * This function does nothing and returns nil if SELinux is disabled,
     * or if Emacs was not compiled with SELinux support.
     * </pre>
     */
    @ELispBuiltIn(name = "set-file-selinux-context", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetFileSelinuxContext extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setFileSelinuxContext(Object filename, Object context) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return ACL entries of file named FILENAME.
     * The entries are returned in a format suitable for use in `set-file-acl'
     * but is otherwise undocumented and subject to change.
     * Return nil if file does not exist.
     * </pre>
     */
    @ELispBuiltIn(name = "file-acl", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileAcl extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fileAcl(Object filename) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set ACL of file named FILENAME to ACL-STRING.
     * ACL-STRING should contain the textual representation of the ACL
     * entries in a format suitable for the platform.
     *
     * Value is t if setting of ACL was successful, nil otherwise.
     *
     * Setting ACL for local files requires Emacs to be built with ACL
     * support.
     * </pre>
     */
    @ELispBuiltIn(name = "set-file-acl", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetFileAcl extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setFileAcl(Object filename, Object aclString) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return mode bits of file named FILENAME, as an integer.
     * Return nil if FILENAME does not exist.  If optional FLAG is `nofollow',
     * do not follow FILENAME if it is a symbolic link.
     * </pre>
     */
    @ELispBuiltIn(name = "file-modes", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFileModes extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fileModes(Object filename, Object flag) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set mode bits of file named FILENAME to MODE (an integer).
     * Only the 12 low bits of MODE are used.  If optional FLAG is `nofollow',
     * do not follow FILENAME if it is a symbolic link.
     *
     * Interactively, prompt for FILENAME, and read MODE with
     * `read-file-modes', which accepts symbolic notation, like the `chmod'
     * command from GNU Coreutils.
     * </pre>
     */
    @ELispBuiltIn(name = "set-file-modes", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetFileModes extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setFileModes(Object filename, Object mode, Object flag) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set the file permission bits for newly created files.
     * The argument MODE should be an integer; only the low 9 bits are used.
     * On Posix hosts, this setting is inherited by subprocesses.
     *
     * This function works by setting the Emacs's file mode creation mask.
     * Each bit that is set in the mask means that the corresponding bit
     * in the permissions of newly created files will be disabled.
     *
     * Note that when `write-region' creates a file, it resets the
     * execute bit, even if the mask set by this function allows that bit
     * by having the corresponding bit in the mask reset.
     *
     * See also `with-file-modes'.
     * </pre>
     */
    @ELispBuiltIn(name = "set-default-file-modes", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetDefaultFileModes extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean setDefaultFileModes(Object mode) {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Return the default file protection for created files.
     * The value is an integer.
     * </pre>
     */
    @ELispBuiltIn(name = "default-file-modes", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FDefaultFileModes extends ELispBuiltInBaseNode {
        @Specialization
        public static long defaultFileModes() {
            // TODO: platform-wise?
            //noinspection OctalInteger
            return 0755L;
        }
    }

    /**
     * <pre>
     * Set times of file FILENAME to TIMESTAMP.
     * If optional FLAG is `nofollow', do not follow FILENAME if it is a
     * symbolic link.  Set both access and modification times.  Return t on
     * success, else nil.  Use the current time if TIMESTAMP is nil.
     * TIMESTAMP is in the format of `current-time'.
     * </pre>
     */
    @ELispBuiltIn(name = "set-file-times", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetFileTimes extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setFileTimes(Object filename, Object timestamp, Object flag) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Tell Unix to finish all pending disk updates.
     * </pre>
     */
    @ELispBuiltIn(name = "unix-sync", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FUnixSync extends ELispBuiltInBaseNode {
        @Specialization
        public static Void unixSync() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if file FILE1 is newer than file FILE2.
     * If FILE1 does not exist, the return value is nil;
     * if FILE2 does not exist, the return value is t.
     * For existing files, this compares their last-modified times.
     * </pre>
     */
    @ELispBuiltIn(name = "file-newer-than-file-p", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFileNewerThanFileP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fileNewerThanFileP(Object file1, Object file2) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Insert contents of file FILENAME after point.
     * Returns list of absolute file name and number of characters inserted.
     * If second argument VISIT is non-nil, the buffer's visited filename and
     * last save file modtime are set, and it is marked unmodified.  If
     * visiting and the file does not exist, visiting is completed before the
     * error is signaled.
     *
     * The optional third and fourth arguments BEG and END specify what portion
     * of the file to insert.  These arguments count bytes in the file, not
     * characters in the buffer.  If VISIT is non-nil, BEG and END must be nil.
     *
     * When inserting data from a special file (e.g., /dev/urandom), you
     * can't specify VISIT or BEG, and END should be specified to avoid
     * inserting unlimited data into the buffer from some special files
     * which otherwise could supply infinite amounts of data.
     *
     * If optional fifth argument REPLACE is non-nil and FILENAME names a
     * regular file, replace the current buffer contents (in the accessible
     * portion) with the file's contents.  This is better than simply
     * deleting and inserting the whole thing because (1) it preserves some
     * marker positions (in unchanged portions at the start and end of the
     * buffer) and (2) it puts less data in the undo list.  When REPLACE is
     * non-nil, the second element of the return value is the number of
     * characters that replace the previous buffer contents.
     *
     * If FILENAME is not a regular file and REPLACE is `if-regular', erase
     * the accessible portion of the buffer and insert the new contents.  Any
     * other non-nil value of REPLACE will signal an error if FILENAME is not
     * a regular file.
     *
     * This function does code conversion according to the value of
     * `coding-system-for-read' or `file-coding-system-alist', and sets the
     * variable `last-coding-system-used' to the coding system actually used.
     *
     * In addition, this function decodes the inserted text from known formats
     * by calling `format-decode', which see.
     * </pre>
     */
    @ELispBuiltIn(name = "insert-file-contents", minArgs = 1, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FInsertFileContents extends ELispBuiltInBaseNode {
        @Specialization
        public ELispCons insertFileContents(ELispString filename, boolean visit, Object beg, Object end, Object replace) {
            ELispContext context = getContext();
            TruffleLanguage.Env env = context.truffleEnv();

            ELispBuffer buffer = currentBuffer();
            if (visit) {
                if (!isNil(beg) || !isNil(end)) {
                    throw ELispSignals.error("Attempt to visit less than an entire file");
                }
                if (buffer.pointMax() != 1) {
                    throw ELispSignals.error("Cannot do file visiting in a non-empty buffer");
                }
            }
            TruffleFile file = env.getPublicTruffleFile(filename.toString());
            try (SeekableByteChannel channel = file.newByteChannel(Set.of(StandardOpenOption.READ))) {
                long start = notNilOr(beg, 0L);
                long limit = Math.min(notNilOr(end, Long.MAX_VALUE), file.size());

                Object codingSystem = detectCodingSystem(context, filename, channel, file.size());
                BuiltInCoding.FCheckCodingSystem.checkCodingSystem(codingSystem);
                ELispCodings codings = context.globals().getCodings();
                ELispCodingSystem coding = codings.resolveCodingSystem(asSym(codingSystem));

                ValueStorage.Forwarded container = new ValueStorage.Forwarded();
                buffer.insert(codings.decode(coding, channel, start, limit, container));
                if (visit) {
                    BUFFER_FILE_CODING_SYSTEM.setValue(container.getValue());
                    BUFFER_FILE_NAME.setValue(new ELispString(file.getName()));
                    BUFFER_FILE_TRUENAME.setValue(new ELispString(file.getAbsoluteFile().toString()));
                }
                return ELispCons.listOf(new ELispString(file.getAbsoluteFile().toString()), limit - start);
            } catch (IOException e) {
                throw ELispSignals.reportFileError(e, filename);
            }
        }

        private Object detectCodingSystem(ELispContext context, ELispString filename,
                                          SeekableByteChannel file, long size) throws IOException {
            Object codingSystem = getContext().getValue(CODING_SYSTEM_FOR_READ);
            if (!isNil(codingSystem)) {
                return codingSystem;
            }
            codingSystem = callSetAutoCodingSystem(context, filename, file, size);
            if (!isNil(codingSystem)) {
                return codingSystem;
            }
            codingSystem = callFindOperationCodingSystem();
            if (isNil(codingSystem)) {
                codingSystem = UNDECIDED;
            }
            return codingSystem;
        }

        private Object callSetAutoCodingSystem(ELispContext context, ELispString filename,
                                               SeekableByteChannel file, long size)
                throws IOException {
            Object autoCodingFunction = context.getValue(SET_AUTO_CODING_FUNCTION);
            if (isNil(autoCodingFunction)) {
                return false;
            }
            file.position(0);
            ByteBuffer headAndTail = ByteBuffer.allocate(4096);
            if (size <= 4096) {
                file.position(0).read(headAndTail);
            } else {
                headAndTail.limit(1024);
                file.position(0).read(headAndTail);
                headAndTail.limit(4096);
                file.position(size - 3072).read(headAndTail);
            }
            long read = headAndTail.position();
            headAndTail.flip();
            try (CurrentBufferScope current = withInternalBufferReset(" *code-conversion-work*")) {
                ELispBuffer buffer = current.current();
                buffer.setEnableMultibyteCharacters(false);
                buffer.insert(MuleString.fromRaw(headAndTail));
                buffer.setPoint(1);
                return BuiltInEval.FFuncall.funcall(this, autoCodingFunction, filename, read);
            }
        }

        private Object callFindOperationCodingSystem() {
            // TODO
            return false;
        }
    }

    /**
     * <pre>
     * Write current region into specified file.
     * When called from a program, requires three arguments:
     * START, END and FILENAME.  START and END are normally buffer positions
     * specifying the part of the buffer to write.
     * If START is nil, that means to use the entire buffer contents; END is
     * ignored.
     * If START is a string, then output that string to the file
     * instead of any buffer contents; END is ignored.
     *
     * Optional fourth argument APPEND if non-nil means
     *   append to existing file contents (if any).  If it is a number,
     *   seek to that offset in the file before writing.
     * Optional fifth argument VISIT, if t or a string, means
     *   set the last-save-file-modtime of buffer to this file's modtime
     *   and mark buffer not modified.
     * If VISIT is t, the buffer is marked as visiting FILENAME.
     * If VISIT is a string, it is a second file name;
     *   the output goes to FILENAME, but the buffer is marked as visiting VISIT.
     *   VISIT is also the file name to lock and unlock for clash detection.
     * If VISIT is neither t nor nil nor a string, or if Emacs is in batch mode,
     *   do not display the \"Wrote file\" message.
     * The optional sixth arg LOCKNAME, if non-nil, specifies the name to
     *   use for locking and unlocking, overriding FILENAME and VISIT.
     * The optional seventh arg MUSTBENEW, if non-nil, insists on a check
     *   for an existing file with the same name.  If MUSTBENEW is `excl',
     *   that means to get an error if the file already exists; never overwrite.
     *   If MUSTBENEW is neither nil nor `excl', that means ask for
     *   confirmation before overwriting, but do go ahead and overwrite the file
     *   if the user confirms.
     *
     * This does code conversion according to the value of
     * `coding-system-for-write', `buffer-file-coding-system', or
     * `file-coding-system-alist', and sets the variable
     * `last-coding-system-used' to the coding system actually used.
     *
     * This calls `write-region-annotate-functions' at the start, and
     * `write-region-post-annotation-function' at the end.
     * </pre>
     */
    @ELispBuiltIn(name = "write-region", minArgs = 3, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FWriteRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void writeRegion(Object start, Object end, Object filename, Object append, Object visit, Object lockname, Object mustbenew) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if (car A) is numerically less than (car B).
     * </pre>
     */
    @ELispBuiltIn(name = "car-less-than-car", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCarLessThanCar extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean carLessThanCar(ELispCons a, ELispCons b) {
            return BuiltInData.compareTo(a.car(), b.car()) < 0;
        }
    }

    /**
     * <pre>
     * Return t if last mod time of BUF's visited file matches what BUF records.
     * This means that the file has not been changed since it was visited or saved.
     * If BUF is omitted or nil, it defaults to the current buffer.
     * See Info node `(elisp)Modification Time' for more details.
     * </pre>
     */
    @ELispBuiltIn(name = "verify-visited-file-modtime", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FVerifyVisitedFileModtime extends ELispBuiltInBaseNode {
        @Specialization
        public static Void verifyVisitedFileModtime(Object buf) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the current buffer's recorded visited file modification time.
     * Return a Lisp timestamp (as in `current-time') if the current buffer
     * has a recorded file modification time, 0 if it doesn't, and -1 if the
     * visited file doesn't exist.
     * See Info node `(elisp)Modification Time' for more details.
     * </pre>
     */
    @ELispBuiltIn(name = "visited-file-modtime", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FVisitedFileModtime extends ELispBuiltInBaseNode {
        @Specialization
        public static Void visitedFileModtime() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Update buffer's recorded modification time from the visited file's time.
     * Useful if the buffer was not read from the file normally
     * or if the file itself has been changed for some known benign reason.
     * An argument specifies the modification time value to use
     * \(instead of that of the visited file), in the form of a time value as
     * in `current-time' or an integer flag as returned by `visited-file-modtime'.
     * </pre>
     */
    @ELispBuiltIn(name = "set-visited-file-modtime", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetVisitedFileModtime extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setVisitedFileModtime(Object timeFlag) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Auto-save all buffers that need it.
     * This auto-saves all buffers that have auto-saving enabled and
     * were changed since last auto-saved.
     *
     * Auto-saving writes the buffer into a file so that your edits are
     * not lost if the system crashes.
     *
     * The auto-save file is not the file you visited; that changes only
     * when you save.
     *
     * Normally, run the normal hook `auto-save-hook' before saving.
     *
     * A non-nil NO-MESSAGE argument means do not print any message if successful.
     *
     * A non-nil CURRENT-ONLY argument means save only current buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "do-auto-save", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDoAutoSave extends ELispBuiltInBaseNode {
        @Specialization
        public static Void doAutoSave(Object noMessage, Object currentOnly) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Mark current buffer as auto-saved with its current text.
     * No auto-save file will be written until the buffer changes again.
     * </pre>
     */
    @ELispBuiltIn(name = "set-buffer-auto-saved", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FSetBufferAutoSaved extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setBufferAutoSaved() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Clear any record of a recent auto-save failure in the current buffer.
     * </pre>
     */
    @ELispBuiltIn(name = "clear-buffer-auto-save-failure", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FClearBufferAutoSaveFailure extends ELispBuiltInBaseNode {
        @Specialization
        public static Void clearBufferAutoSaveFailure() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if current buffer has been auto-saved recently.
     * More precisely, if it has been auto-saved since last read from or saved
     * in the visited file.  If the buffer has no visited file,
     * then any auto-save counts as "recent".
     * </pre>
     */
    @ELispBuiltIn(name = "recent-auto-save-p", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FRecentAutoSaveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void recentAutoSaveP() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return t if a call to `read-file-name' will use a dialog.
     * The return value is only relevant for a call to `read-file-name' that happens
     * before any other event (mouse or keypress) is handled.
     * </pre>
     */
    @ELispBuiltIn(name = "next-read-file-uses-dialog-p", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FNextReadFileUsesDialogP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void nextReadFileUsesDialogP() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Switch STREAM to binary I/O mode or text I/O mode.
     * STREAM can be one of the symbols `stdin', `stdout', or `stderr'.
     * If MODE is non-nil, switch STREAM to binary mode, otherwise switch
     * it to text mode.
     *
     * As a side effect, this function flushes any pending STREAM's data.
     *
     * Value is the previous value of STREAM's I/O mode, nil for text mode,
     * non-nil for binary mode.
     *
     * On MS-Windows and MS-DOS, binary mode is needed to read or write
     * arbitrary binary data, and for disabling translation between CR-LF
     * pairs and a single newline character.  Examples include generation
     * of text files with Unix-style end-of-line format using `princ' in
     * batch mode, with standard output redirected to a file.
     *
     * On Posix systems, this function always returns non-nil, and has no
     * effect except for flushing STREAM's data.
     * </pre>
     */
    @ELispBuiltIn(name = "set-binary-mode", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetBinaryMode extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setBinaryMode(Object stream, Object mode) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return storage information about the file system FILENAME is on.
     * Value is a list of numbers (TOTAL FREE AVAIL), where TOTAL is the total
     * storage of the file system, FREE is the free storage, and AVAIL is the
     * storage available to a non-superuser.  All 3 numbers are in bytes.
     * If the underlying system call fails, value is nil.
     * </pre>
     */
    @ELispBuiltIn(name = "file-system-info", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileSystemInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fileSystemInfo(Object filename) {
            throw new UnsupportedOperationException();
        }
    }
}
