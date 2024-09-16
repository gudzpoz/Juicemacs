package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.io.File;
import java.nio.file.Path;
import java.util.List;

public class BuiltInFileIO extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInFileIOFactory.getFactories();
    }

    @ELispBuiltIn(name = "find-file-name-handler", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFindFileNameHandler extends ELispBuiltInBaseNode {
        @Specialization
        public static Object findFileNameHandler(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-name-directory", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileNameDirectory extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileNameDirectory(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-name-nondirectory", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileNameNondirectory extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileNameNondirectory(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "unhandled-file-name-directory", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FUnhandledFileNameDirectory extends ELispBuiltInBaseNode {
        @Specialization
        public static Object unhandledFileNameDirectory(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-name-as-directory", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileNameAsDirectory extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileNameAsDirectory(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "directory-name-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDirectoryNameP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object directoryNameP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "directory-file-name", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDirectoryFileName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object directoryFileName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-temp-file-internal", minArgs = 4, maxArgs = 4)
    @GenerateNodeFactory
    public abstract static class FMakeTempFileInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeTempFileInternal(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-temp-name", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeTempName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeTempName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-name-concat", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FFileNameConcat extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileNameConcat(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "expand-file-name", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FExpandFileName extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString expandFileName(ELispString a, Object b) {
            String path = a.toString();
            if (path.startsWith("~")) {
                path = System.getProperty("user.home") + path.substring(1);
            } else if (!path.startsWith("/")) {
                return new ELispString(ELispString.from(
                        Path.of(ELispSymbol.isNil(b) ? System.getProperty("user.home") : b.toString(), path)
                                .toAbsolutePath().toString()
                ));
            }
            return new ELispString(ELispString.from(Path.of(path).toAbsolutePath().toString()));
        }
    }

    @ELispBuiltIn(name = "substitute-in-file-name", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSubstituteInFileName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object substituteInFileName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "copy-file", minArgs = 2, maxArgs = 6)
    @GenerateNodeFactory
    public abstract static class FCopyFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Object copyFile(Object a, Object b, Object c, Object d, Object e, Object f) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-directory-internal", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FMakeDirectoryInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeDirectoryInternal(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-directory-internal", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDeleteDirectoryInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteDirectoryInternal(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-file-internal", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FDeleteFileInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteFileInternal(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-name-case-insensitive-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileNameCaseInsensitiveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileNameCaseInsensitiveP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "rename-file", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FRenameFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Object renameFile(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "add-name-to-file", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FAddNameToFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Object addNameToFile(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-symbolic-link", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FMakeSymbolicLink extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeSymbolicLink(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-name-absolute-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileNameAbsoluteP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileNameAbsoluteP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-exists-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileExistsP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileExistsP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-executable-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileExecutableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileExecutableP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-readable-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileReadableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileReadableP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-writable-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileWritableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileWritableP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "access-file", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FAccessFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Object accessFile(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-symlink-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileSymlinkP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileSymlinkP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-directory-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileDirectoryP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean fileDirectoryP(ELispString a) {
            String path = a.toString();
            File file = new File(path.isEmpty() ? "." : path);
            return file.isDirectory();
        }
    }

    @ELispBuiltIn(name = "file-accessible-directory-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileAccessibleDirectoryP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileAccessibleDirectoryP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-regular-p", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileRegularP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileRegularP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-selinux-context", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileSelinuxContext extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileSelinuxContext(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-file-selinux-context", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetFileSelinuxContext extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setFileSelinuxContext(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-acl", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileAcl extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileAcl(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-file-acl", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetFileAcl extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setFileAcl(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-modes", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFileModes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileModes(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-file-modes", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetFileModes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setFileModes(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-default-file-modes", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetDefaultFileModes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setDefaultFileModes(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "default-file-modes", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FDefaultFileModes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defaultFileModes() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-file-times", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FSetFileTimes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setFileTimes(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "unix-sync", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FUnixSync extends ELispBuiltInBaseNode {
        @Specialization
        public static Object unixSync() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-newer-than-file-p", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFileNewerThanFileP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileNewerThanFileP(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert-file-contents", minArgs = 1, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FInsertFileContents extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insertFileContents(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "write-region", minArgs = 3, maxArgs = 7)
    @GenerateNodeFactory
    public abstract static class FWriteRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object writeRegion(Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "car-less-than-car", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCarLessThanCar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object carLessThanCar(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "verify-visited-file-modtime", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FVerifyVisitedFileModtime extends ELispBuiltInBaseNode {
        @Specialization
        public static Object verifyVisitedFileModtime(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "visited-file-modtime", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FVisitedFileModtime extends ELispBuiltInBaseNode {
        @Specialization
        public static Object visitedFileModtime() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-visited-file-modtime", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetVisitedFileModtime extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setVisitedFileModtime(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "do-auto-save", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FDoAutoSave extends ELispBuiltInBaseNode {
        @Specialization
        public static Object doAutoSave(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-buffer-auto-saved", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FSetBufferAutoSaved extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setBufferAutoSaved() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "clear-buffer-auto-save-failure", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FClearBufferAutoSaveFailure extends ELispBuiltInBaseNode {
        @Specialization
        public static Object clearBufferAutoSaveFailure() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "recent-auto-save-p", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FRecentAutoSaveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object recentAutoSaveP() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "next-read-file-uses-dialog-p", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FNextReadFileUsesDialogP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nextReadFileUsesDialogP() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-binary-mode", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FSetBinaryMode extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setBinaryMode(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-system-info", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFileSystemInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileSystemInfo(Object a) {
            throw new UnsupportedOperationException();
        }
    }
}
