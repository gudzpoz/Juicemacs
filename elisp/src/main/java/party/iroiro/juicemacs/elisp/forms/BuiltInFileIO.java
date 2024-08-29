package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;

import java.util.List;

public class BuiltInFileIO extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInFileIOFactory.getFactories();
    }

    @ELispBuiltIn(name = "find-file-name-handler", minArgs = 2, maxArgs = 2, doc = "Return FILENAME's handler function for OPERATION, if it has one.\nOtherwise, return nil.\nA file name is handled if one of the regular expressions in\n`file-name-handler-alist' matches it.\n\nIf OPERATION equals `inhibit-file-name-operation', then ignore\nany handlers that are members of `inhibit-file-name-handlers',\nbut still do run any other handlers.  This lets handlers\nuse the standard functions without calling themselves recursively.")
    @GenerateNodeFactory
    public abstract static class FFindFileNameHandler extends ELispBuiltInBaseNode {
        @Specialization
        public static Object findFileNameHandler(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-name-directory", minArgs = 1, maxArgs = 1, doc = "Return the directory component in file name FILENAME.\nReturn nil if FILENAME does not include a directory.\nOtherwise return a directory name.\nGiven a Unix syntax file name, returns a string ending in slash.")
    @GenerateNodeFactory
    public abstract static class FFileNameDirectory extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileNameDirectory(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-name-nondirectory", minArgs = 1, maxArgs = 1, doc = "Return file name FILENAME sans its directory.\nFor example, in a Unix-syntax file name,\nthis is everything after the last slash,\nor the entire name if it contains no slash.")
    @GenerateNodeFactory
    public abstract static class FFileNameNondirectory extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileNameNondirectory(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "unhandled-file-name-directory", minArgs = 1, maxArgs = 1, doc = "Return a directly usable directory name somehow associated with FILENAME.\nA `directly usable' directory name is one that may be used without the\nintervention of any file name handler.\nIf FILENAME is a directly usable file itself, return\n\\(file-name-as-directory FILENAME).\nIf FILENAME refers to a file which is not accessible from a local process,\nthen this should return nil.\nThe `call-process' and `start-process' functions use this function to\nget a current directory to run processes in.")
    @GenerateNodeFactory
    public abstract static class FUnhandledFileNameDirectory extends ELispBuiltInBaseNode {
        @Specialization
        public static Object unhandledFileNameDirectory(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-name-as-directory", minArgs = 1, maxArgs = 1, doc = "Return a string representing the file name FILE interpreted as a directory.\nThis operation exists because a directory is also a file, but its name as\na directory is different from its name as a file.\nThe result can be used as the value of `default-directory'\nor passed as second argument to `expand-file-name'.\nFor a Unix-syntax file name, just appends a slash unless a trailing slash\nis already present.")
    @GenerateNodeFactory
    public abstract static class FFileNameAsDirectory extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileNameAsDirectory(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "directory-name-p", minArgs = 1, maxArgs = 1, doc = "Return non-nil if NAME ends with a directory separator character.")
    @GenerateNodeFactory
    public abstract static class FDirectoryNameP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object directoryNameP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "directory-file-name", minArgs = 1, maxArgs = 1, doc = "Returns the file name of the directory named DIRECTORY.\nThis is the name of the file that holds the data for the directory DIRECTORY.\nThis operation exists because a directory is also a file, but its name as\na directory is different from its name as a file.\nIn Unix-syntax, this function just removes the final slash.")
    @GenerateNodeFactory
    public abstract static class FDirectoryFileName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object directoryFileName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-temp-file-internal", minArgs = 4, maxArgs = 4, doc = "Generate a new file whose name starts with PREFIX, a string.\nReturn the name of the generated file.  If DIR-FLAG is zero, do not\ncreate the file, just its name.  Otherwise, if DIR-FLAG is non-nil,\ncreate an empty directory.  The file name should end in SUFFIX.\nDo not expand PREFIX; a non-absolute PREFIX is relative to the Emacs\nworking directory.  If TEXT is a string, insert it into the newly\ncreated file.\n\nSignal an error if the file could not be created.\n\nThis function does not grok magic file names.")
    @GenerateNodeFactory
    public abstract static class FMakeTempFileInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeTempFileInternal(Object a, Object b, Object c, Object d) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-temp-name", minArgs = 1, maxArgs = 1, doc = "Generate temporary file name (string) starting with PREFIX (a string).\n\nThis function tries to choose a name that has no existing file.\nFor this to work, PREFIX should be an absolute file name, and PREFIX\nand the returned string should both be non-magic.\n\nThere is a race condition between calling `make-temp-name' and\nlater creating the file, which opens all kinds of security holes.\nFor that reason, you should normally use `make-temp-file' instead.")
    @GenerateNodeFactory
    public abstract static class FMakeTempName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeTempName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-name-concat", minArgs = 1, maxArgs = 1, varArgs = true, doc = "Append COMPONENTS to DIRECTORY and return the resulting string.\nEach element in COMPONENTS must be a string or nil.\nDIRECTORY or the non-final elements in COMPONENTS may or may not end\nwith a slash -- if they don't end with a slash, a slash will be\ninserted before concatenating.\nusage: (file-name-concat DIRECTORY &rest COMPONENTS)")
    @GenerateNodeFactory
    public abstract static class FFileNameConcat extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileNameConcat(Object a, Object[] args) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "expand-file-name", minArgs = 1, maxArgs = 2, doc = "Convert filename NAME to absolute, and canonicalize it.\nSecond arg DEFAULT-DIRECTORY is directory to start with if NAME is relative\n\\(does not start with slash or tilde); both the directory name and\na directory's file name are accepted.  If DEFAULT-DIRECTORY is nil or\nmissing, the current buffer's value of `default-directory' is used.\nNAME should be a string that is a valid file name for the underlying\nfilesystem.\n\nFile name components that are `.' are removed, and so are file name\ncomponents followed by `..', along with the `..' itself; note that\nthese simplifications are done without checking the resulting file\nnames in the file system.\n\nMultiple consecutive slashes are collapsed into a single slash, except\nat the beginning of the file name when they are significant (e.g., UNC\nfile names on MS-Windows.)\n\nAn initial \\\"~\\\" in NAME expands to your home directory.\n\nAn initial \\\"~USER\\\" in NAME expands to USER's home directory.  If\nUSER doesn't exist, \\\"~USER\\\" is not expanded.\n\nTo do other file name substitutions, see `substitute-in-file-name'.\n\nFor technical reasons, this function can return correct but\nnon-intuitive results for the root directory; for instance,\n\\(expand-file-name \"..\" \"/\") returns \"/..\".  For this reason, use\n\\(directory-file-name (file-name-directory dirname)) to traverse a\nfilesystem tree, not (expand-file-name \"..\" dirname).  Note: make\nsure DIRNAME in this example doesn't end in a slash, unless it's\nthe root directory.")
    @GenerateNodeFactory
    public abstract static class FExpandFileName extends ELispBuiltInBaseNode {
        @Specialization
        public static ELispString expandFileName(ELispString a, ELispString b) {
            // TODO
            return new ELispString(ELispString.from(""));
        }
    }

    @ELispBuiltIn(name = "substitute-in-file-name", minArgs = 1, maxArgs = 1, doc = "Substitute environment variables referred to in FILENAME.\n`$FOO' where FOO is an environment variable name means to substitute\nthe value of that variable.  The variable name should be terminated\nwith a character not a letter, digit or underscore; otherwise, enclose\nthe entire variable name in braces.\n\nIf FOO is not defined in the environment, `$FOO' is left unchanged in\nthe value of this function.\n\nIf `/~' appears, all of FILENAME through that `/' is discarded.\nIf `//' appears, everything up to and including the first of\nthose `/' is discarded.")
    @GenerateNodeFactory
    public abstract static class FSubstituteInFileName extends ELispBuiltInBaseNode {
        @Specialization
        public static Object substituteInFileName(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "copy-file", minArgs = 2, maxArgs = 6, doc = "Copy FILE to NEWNAME.  Both args must be strings.\nIf NEWNAME is a directory name, copy FILE to a like-named file under\nNEWNAME.  For NEWNAME to be recognized as a directory name, it should\nend in a slash.\n\nThis function always sets the file modes of the output file to match\nthe input file.\n\nThe optional third argument OK-IF-ALREADY-EXISTS specifies what to do\nif file NEWNAME already exists.  If OK-IF-ALREADY-EXISTS is nil,\nsignal a `file-already-exists' error without overwriting.  If\nOK-IF-ALREADY-EXISTS is an integer, request confirmation from the user\nabout overwriting; this is what happens in interactive use with M-x.\nAny other value for OK-IF-ALREADY-EXISTS means to overwrite the\nexisting file.\n\nFourth arg KEEP-TIME non-nil means give the output file the same\nlast-modified time as the old one.  (This works on only some systems.)\n\nA prefix arg makes KEEP-TIME non-nil.\n\nIf PRESERVE-UID-GID is non-nil, try to transfer the uid and gid of\nFILE to NEWNAME.\n\nIf PRESERVE-PERMISSIONS is non-nil, copy permissions of FILE to NEWNAME;\nthis includes the file modes, along with ACL entries and SELinux\ncontext if present.  Otherwise, if NEWNAME is created its file\npermission bits are those of FILE, masked by the default file\npermissions.")
    @GenerateNodeFactory
    public abstract static class FCopyFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Object copyFile(Object a, Object b, Object c, Object d, Object e, Object f) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-directory-internal", minArgs = 1, maxArgs = 1, doc = "Create a new directory named DIRECTORY.")
    @GenerateNodeFactory
    public abstract static class FMakeDirectoryInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeDirectoryInternal(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-directory-internal", minArgs = 1, maxArgs = 1, doc = "Delete the directory named DIRECTORY.  Does not follow symlinks.")
    @GenerateNodeFactory
    public abstract static class FDeleteDirectoryInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteDirectoryInternal(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "delete-file-internal", minArgs = 1, maxArgs = 1, doc = "Delete file named FILENAME; internal use only.\nIf it is a symlink, remove the symlink.\nIf file has multiple names, it continues to exist with the other names.")
    @GenerateNodeFactory
    public abstract static class FDeleteFileInternal extends ELispBuiltInBaseNode {
        @Specialization
        public static Object deleteFileInternal(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-name-case-insensitive-p", minArgs = 1, maxArgs = 1, doc = "Return t if file FILENAME is on a case-insensitive filesystem.\nReturn nil if FILENAME does not exist or is not on a case-insensitive\nfilesystem, or if there was trouble determining whether the filesystem\nis case-insensitive.")
    @GenerateNodeFactory
    public abstract static class FFileNameCaseInsensitiveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileNameCaseInsensitiveP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "rename-file", minArgs = 2, maxArgs = 3, doc = "Rename FILE as NEWNAME.  Both args must be strings.\nIf file has names other than FILE, it continues to have those names.\nIf NEWNAME is a directory name, rename FILE to a like-named file under\nNEWNAME.  For NEWNAME to be recognized as a directory name, it should\nend in a slash.\n\nSignal a `file-already-exists' error if a file NEWNAME already exists\nunless optional third argument OK-IF-ALREADY-EXISTS is non-nil.\nAn integer third arg means request confirmation if NEWNAME already exists.\nThis is what happens in interactive use with M-x.")
    @GenerateNodeFactory
    public abstract static class FRenameFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Object renameFile(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "add-name-to-file", minArgs = 2, maxArgs = 3, doc = "Give FILE additional name NEWNAME.  Both args must be strings.\nIf NEWNAME is a directory name, give FILE a like-named new name under\nNEWNAME.\n\nSignal a `file-already-exists' error if a file NEWNAME already exists\nunless optional third argument OK-IF-ALREADY-EXISTS is non-nil.\nAn integer third arg means request confirmation if NEWNAME already exists.\nThis is what happens in interactive use with M-x.")
    @GenerateNodeFactory
    public abstract static class FAddNameToFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Object addNameToFile(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "make-symbolic-link", minArgs = 2, maxArgs = 3, doc = "Make a symbolic link to TARGET, named LINKNAME.\nIf LINKNAME is a directory name, make a like-named symbolic link under\nLINKNAME.\n\nSignal a `file-already-exists' error if a file LINKNAME already exists\nunless optional third argument OK-IF-ALREADY-EXISTS is non-nil.\nAn integer third arg means request confirmation if LINKNAME already\nexists, and expand leading \"~\" or strip leading \"/:\" in TARGET.\nThis happens for interactive use with M-x.")
    @GenerateNodeFactory
    public abstract static class FMakeSymbolicLink extends ELispBuiltInBaseNode {
        @Specialization
        public static Object makeSymbolicLink(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-name-absolute-p", minArgs = 1, maxArgs = 1, doc = "Return t if FILENAME is an absolute file name.\nOn Unix, absolute file names start with `/'.  In Emacs, an absolute\nfile name can also start with an initial `~' or `~USER' component,\nwhere USER is a valid login name.")
    @GenerateNodeFactory
    public abstract static class FFileNameAbsoluteP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileNameAbsoluteP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-exists-p", minArgs = 1, maxArgs = 1, doc = "Return t if file FILENAME exists (whether or not you can read it).\nReturn nil if FILENAME does not exist, or if there was trouble\ndetermining whether the file exists.\nSee also `file-readable-p' and `file-attributes'.\nThis returns nil for a symlink to a nonexistent file.\nUse `file-symlink-p' to test for such links.")
    @GenerateNodeFactory
    public abstract static class FFileExistsP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileExistsP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-executable-p", minArgs = 1, maxArgs = 1, doc = "Return t if FILENAME can be executed by you.\nFor a directory, this means you can access files in that directory.\n\\(It is generally better to use `file-accessible-directory-p' for that\npurpose, though.)")
    @GenerateNodeFactory
    public abstract static class FFileExecutableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileExecutableP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-readable-p", minArgs = 1, maxArgs = 1, doc = "Return t if file FILENAME exists and you can read it.\nSee also `file-exists-p' and `file-attributes'.")
    @GenerateNodeFactory
    public abstract static class FFileReadableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileReadableP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-writable-p", minArgs = 1, maxArgs = 1, doc = "Return t if file FILENAME can be written or created by you.")
    @GenerateNodeFactory
    public abstract static class FFileWritableP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileWritableP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "access-file", minArgs = 2, maxArgs = 2, doc = "Access file FILENAME, and get an error if that does not work.\nThe second argument STRING is prepended to the error message.\nIf there is no error, returns nil.")
    @GenerateNodeFactory
    public abstract static class FAccessFile extends ELispBuiltInBaseNode {
        @Specialization
        public static Object accessFile(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-symlink-p", minArgs = 1, maxArgs = 1, doc = "Return non-nil if file FILENAME is the name of a symbolic link.\nThe value is the link target, as a string.\nReturn nil if FILENAME does not exist or is not a symbolic link,\nof there was trouble determining whether the file is a symbolic link.\n\nThis function does not check whether the link target exists.")
    @GenerateNodeFactory
    public abstract static class FFileSymlinkP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileSymlinkP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-directory-p", minArgs = 1, maxArgs = 1, doc = "Return t if FILENAME names an existing directory.\nReturn nil if FILENAME does not name a directory, or if there\nwas trouble determining whether FILENAME is a directory.\n\nAs a special case, this function will also return t if FILENAME is the\nempty string (\\\"\\\").  This quirk is due to Emacs interpreting the\nempty string (in some cases) as the current directory.\n\nSymbolic links to directories count as directories.\nSee `file-symlink-p' to distinguish symlinks.")
    @GenerateNodeFactory
    public abstract static class FFileDirectoryP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileDirectoryP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-accessible-directory-p", minArgs = 1, maxArgs = 1, doc = "Return t if FILENAME names a directory you can open.\nThis means that FILENAME must specify the name of a directory, and the\ndirectory must allow you to open files in it.  If this isn't the case,\nreturn nil.\n\nFILENAME can either be a directory name (eg. \\\"/tmp/foo/\\\") or the\nfile name of a file which is a directory (eg. \\\"/tmp/foo\\\", without\nthe final slash).\n\nIn order to use a directory as a buffer's current directory, this\npredicate must return true.")
    @GenerateNodeFactory
    public abstract static class FFileAccessibleDirectoryP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileAccessibleDirectoryP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-regular-p", minArgs = 1, maxArgs = 1, doc = "Return t if FILENAME names a regular file.\nThis is the sort of file that holds an ordinary stream of data bytes.\nReturn nil if FILENAME does not exist or is not a regular file,\nor there was trouble determining whether FILENAME is a regular file.\nSymbolic links to regular files count as regular files.\nSee `file-symlink-p' to distinguish symlinks.")
    @GenerateNodeFactory
    public abstract static class FFileRegularP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileRegularP(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-selinux-context", minArgs = 1, maxArgs = 1, doc = "Return SELinux context of file named FILENAME.\nThe return value is a list (USER ROLE TYPE RANGE), where the list\nelements are strings naming the user, role, type, and range of the\nfile's SELinux security context.\n\nReturn (nil nil nil nil) if the file is nonexistent,\nor if SELinux is disabled, or if Emacs lacks SELinux support.")
    @GenerateNodeFactory
    public abstract static class FFileSelinuxContext extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileSelinuxContext(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-file-selinux-context", minArgs = 2, maxArgs = 2, doc = "Set SELinux context of file named FILENAME to CONTEXT.\nCONTEXT should be a list (USER ROLE TYPE RANGE), where the list\nelements are strings naming the components of a SELinux context.\n\nValue is t if setting of SELinux context was successful, nil otherwise.\n\nThis function does nothing and returns nil if SELinux is disabled,\nor if Emacs was not compiled with SELinux support.")
    @GenerateNodeFactory
    public abstract static class FSetFileSelinuxContext extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setFileSelinuxContext(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-acl", minArgs = 1, maxArgs = 1, doc = "Return ACL entries of file named FILENAME.\nThe entries are returned in a format suitable for use in `set-file-acl'\nbut is otherwise undocumented and subject to change.\nReturn nil if file does not exist.")
    @GenerateNodeFactory
    public abstract static class FFileAcl extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileAcl(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-file-acl", minArgs = 2, maxArgs = 2, doc = "Set ACL of file named FILENAME to ACL-STRING.\nACL-STRING should contain the textual representation of the ACL\nentries in a format suitable for the platform.\n\nValue is t if setting of ACL was successful, nil otherwise.\n\nSetting ACL for local files requires Emacs to be built with ACL\nsupport.")
    @GenerateNodeFactory
    public abstract static class FSetFileAcl extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setFileAcl(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-modes", minArgs = 1, maxArgs = 2, doc = "Return mode bits of file named FILENAME, as an integer.\nReturn nil if FILENAME does not exist.  If optional FLAG is `nofollow',\ndo not follow FILENAME if it is a symbolic link.")
    @GenerateNodeFactory
    public abstract static class FFileModes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileModes(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-file-modes", minArgs = 2, maxArgs = 3, doc = "Set mode bits of file named FILENAME to MODE (an integer).\nOnly the 12 low bits of MODE are used.  If optional FLAG is `nofollow',\ndo not follow FILENAME if it is a symbolic link.\n\nInteractively, prompt for FILENAME, and read MODE with\n`read-file-modes', which accepts symbolic notation, like the `chmod'\ncommand from GNU Coreutils.")
    @GenerateNodeFactory
    public abstract static class FSetFileModes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setFileModes(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-default-file-modes", minArgs = 1, maxArgs = 1, doc = "Set the file permission bits for newly created files.\nThe argument MODE should be an integer; only the low 9 bits are used.\nOn Posix hosts, this setting is inherited by subprocesses.\n\nThis function works by setting the Emacs's file mode creation mask.\nEach bit that is set in the mask means that the corresponding bit\nin the permissions of newly created files will be disabled.\n\nNote that when `write-region' creates a file, it resets the\nexecute bit, even if the mask set by this function allows that bit\nby having the corresponding bit in the mask reset.\n\nSee also `with-file-modes'.")
    @GenerateNodeFactory
    public abstract static class FSetDefaultFileModes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setDefaultFileModes(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "default-file-modes", minArgs = 0, maxArgs = 0, doc = "Return the default file protection for created files.\nThe value is an integer.")
    @GenerateNodeFactory
    public abstract static class FDefaultFileModes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object defaultFileModes() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-file-times", minArgs = 1, maxArgs = 3, doc = "Set times of file FILENAME to TIMESTAMP.\nIf optional FLAG is `nofollow', do not follow FILENAME if it is a\nsymbolic link.  Set both access and modification times.  Return t on\nsuccess, else nil.  Use the current time if TIMESTAMP is nil.\nTIMESTAMP is in the format of `current-time'.")
    @GenerateNodeFactory
    public abstract static class FSetFileTimes extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setFileTimes(Object a, Object b, Object c) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "unix-sync", minArgs = 0, maxArgs = 0, doc = "Tell Unix to finish all pending disk updates.")
    @GenerateNodeFactory
    public abstract static class FUnixSync extends ELispBuiltInBaseNode {
        @Specialization
        public static Object unixSync() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-newer-than-file-p", minArgs = 2, maxArgs = 2, doc = "Return t if file FILE1 is newer than file FILE2.\nIf FILE1 does not exist, the answer is nil;\notherwise, if FILE2 does not exist, the answer is t.")
    @GenerateNodeFactory
    public abstract static class FFileNewerThanFileP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileNewerThanFileP(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "insert-file-contents", minArgs = 1, maxArgs = 5, doc = "Insert contents of file FILENAME after point.\nReturns list of absolute file name and number of characters inserted.\nIf second argument VISIT is non-nil, the buffer's visited filename and\nlast save file modtime are set, and it is marked unmodified.  If\nvisiting and the file does not exist, visiting is completed before the\nerror is signaled.\n\nThe optional third and fourth arguments BEG and END specify what portion\nof the file to insert.  These arguments count bytes in the file, not\ncharacters in the buffer.  If VISIT is non-nil, BEG and END must be nil.\n\nWhen inserting data from a special file (e.g., /dev/urandom), you\ncan't specify VISIT or BEG, and END should be specified to avoid\ninserting unlimited data into the buffer from some special files\nwhich otherwise could supply infinite amounts of data.\n\nIf optional fifth argument REPLACE is non-nil and FILENAME names a\nregular file, replace the current buffer contents (in the accessible\nportion) with the file's contents.  This is better than simply\ndeleting and inserting the whole thing because (1) it preserves some\nmarker positions (in unchanged portions at the start and end of the\nbuffer) and (2) it puts less data in the undo list.  When REPLACE is\nnon-nil, the second element of the return value is the number of\ncharacters that replace the previous buffer contents.\n\nIf FILENAME is not a regular file and REPLACE is `if-regular', erase\nthe accessible portion of the buffer and insert the new contents.  Any\nother non-nil value of REPLACE will signal an error if FILENAME is not\na regular file.\n\nThis function does code conversion according to the value of\n`coding-system-for-read' or `file-coding-system-alist', and sets the\nvariable `last-coding-system-used' to the coding system actually used.\n\nIn addition, this function decodes the inserted text from known formats\nby calling `format-decode', which see.")
    @GenerateNodeFactory
    public abstract static class FInsertFileContents extends ELispBuiltInBaseNode {
        @Specialization
        public static Object insertFileContents(Object a, Object b, Object c, Object d, Object e) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "write-region", minArgs = 3, maxArgs = 7, doc = "Write current region into specified file.\nWhen called from a program, requires three arguments:\nSTART, END and FILENAME.  START and END are normally buffer positions\nspecifying the part of the buffer to write.\nIf START is nil, that means to use the entire buffer contents; END is\nignored.\nIf START is a string, then output that string to the file\ninstead of any buffer contents; END is ignored.\n\nOptional fourth argument APPEND if non-nil means\n  append to existing file contents (if any).  If it is a number,\n  seek to that offset in the file before writing.\nOptional fifth argument VISIT, if t or a string, means\n  set the last-save-file-modtime of buffer to this file's modtime\n  and mark buffer not modified.\nIf VISIT is t, the buffer is marked as visiting FILENAME.\nIf VISIT is a string, it is a second file name;\n  the output goes to FILENAME, but the buffer is marked as visiting VISIT.\n  VISIT is also the file name to lock and unlock for clash detection.\nIf VISIT is neither t nor nil nor a string, or if Emacs is in batch mode,\n  do not display the \\\"Wrote file\\\" message.\nThe optional sixth arg LOCKNAME, if non-nil, specifies the name to\n  use for locking and unlocking, overriding FILENAME and VISIT.\nThe optional seventh arg MUSTBENEW, if non-nil, insists on a check\n  for an existing file with the same name.  If MUSTBENEW is `excl',\n  that means to get an error if the file already exists; never overwrite.\n  If MUSTBENEW is neither nil nor `excl', that means ask for\n  confirmation before overwriting, but do go ahead and overwrite the file\n  if the user confirms.\n\nThis does code conversion according to the value of\n`coding-system-for-write', `buffer-file-coding-system', or\n`file-coding-system-alist', and sets the variable\n`last-coding-system-used' to the coding system actually used.\n\nThis calls `write-region-annotate-functions' at the start, and\n`write-region-post-annotation-function' at the end.")
    @GenerateNodeFactory
    public abstract static class FWriteRegion extends ELispBuiltInBaseNode {
        @Specialization
        public static Object writeRegion(Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "car-less-than-car", minArgs = 2, maxArgs = 2, doc = "Return t if (car A) is numerically less than (car B).")
    @GenerateNodeFactory
    public abstract static class FCarLessThanCar extends ELispBuiltInBaseNode {
        @Specialization
        public static Object carLessThanCar(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "verify-visited-file-modtime", minArgs = 0, maxArgs = 1, doc = "Return t if last mod time of BUF's visited file matches what BUF records.\nThis means that the file has not been changed since it was visited or saved.\nIf BUF is omitted or nil, it defaults to the current buffer.\nSee Info node `(elisp)Modification Time' for more details.")
    @GenerateNodeFactory
    public abstract static class FVerifyVisitedFileModtime extends ELispBuiltInBaseNode {
        @Specialization
        public static Object verifyVisitedFileModtime(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "visited-file-modtime", minArgs = 0, maxArgs = 0, doc = "Return the current buffer's recorded visited file modification time.\nReturn a Lisp timestamp (as in `current-time') if the current buffer\nhas a recorded file modification time, 0 if it doesn't, and -1 if the\nvisited file doesn't exist.\nSee Info node `(elisp)Modification Time' for more details.")
    @GenerateNodeFactory
    public abstract static class FVisitedFileModtime extends ELispBuiltInBaseNode {
        @Specialization
        public static Object visitedFileModtime() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-visited-file-modtime", minArgs = 0, maxArgs = 1, doc = "Update buffer's recorded modification time from the visited file's time.\nUseful if the buffer was not read from the file normally\nor if the file itself has been changed for some known benign reason.\nAn argument specifies the modification time value to use\n\\(instead of that of the visited file), in the form of a time value as\nin `current-time' or an integer flag as returned by `visited-file-modtime'.")
    @GenerateNodeFactory
    public abstract static class FSetVisitedFileModtime extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setVisitedFileModtime(Object a) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "do-auto-save", minArgs = 0, maxArgs = 2, doc = "Auto-save all buffers that need it.\nThis auto-saves all buffers that have auto-saving enabled and\nwere changed since last auto-saved.\n\nAuto-saving writes the buffer into a file so that your edits are\nnot lost if the system crashes.\n\nThe auto-save file is not the file you visited; that changes only\nwhen you save.\n\nNormally, run the normal hook `auto-save-hook' before saving.\n\nA non-nil NO-MESSAGE argument means do not print any message if successful.\n\nA non-nil CURRENT-ONLY argument means save only current buffer.")
    @GenerateNodeFactory
    public abstract static class FDoAutoSave extends ELispBuiltInBaseNode {
        @Specialization
        public static Object doAutoSave(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-buffer-auto-saved", minArgs = 0, maxArgs = 0, doc = "Mark current buffer as auto-saved with its current text.\nNo auto-save file will be written until the buffer changes again.")
    @GenerateNodeFactory
    public abstract static class FSetBufferAutoSaved extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setBufferAutoSaved() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "clear-buffer-auto-save-failure", minArgs = 0, maxArgs = 0, doc = "Clear any record of a recent auto-save failure in the current buffer.")
    @GenerateNodeFactory
    public abstract static class FClearBufferAutoSaveFailure extends ELispBuiltInBaseNode {
        @Specialization
        public static Object clearBufferAutoSaveFailure() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "recent-auto-save-p", minArgs = 0, maxArgs = 0, doc = "Return t if current buffer has been auto-saved recently.\nMore precisely, if it has been auto-saved since last read from or saved\nin the visited file.  If the buffer has no visited file,\nthen any auto-save counts as \"recent\".")
    @GenerateNodeFactory
    public abstract static class FRecentAutoSaveP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object recentAutoSaveP() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "next-read-file-uses-dialog-p", minArgs = 0, maxArgs = 0, doc = "Return t if a call to `read-file-name' will use a dialog.\nThe return value is only relevant for a call to `read-file-name' that happens\nbefore any other event (mouse or keypress) is handled.")
    @GenerateNodeFactory
    public abstract static class FNextReadFileUsesDialogP extends ELispBuiltInBaseNode {
        @Specialization
        public static Object nextReadFileUsesDialogP() {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "set-binary-mode", minArgs = 2, maxArgs = 2, doc = "Switch STREAM to binary I/O mode or text I/O mode.\nSTREAM can be one of the symbols `stdin', `stdout', or `stderr'.\nIf MODE is non-nil, switch STREAM to binary mode, otherwise switch\nit to text mode.\n\nAs a side effect, this function flushes any pending STREAM's data.\n\nValue is the previous value of STREAM's I/O mode, nil for text mode,\nnon-nil for binary mode.\n\nOn MS-Windows and MS-DOS, binary mode is needed to read or write\narbitrary binary data, and for disabling translation between CR-LF\npairs and a single newline character.  Examples include generation\nof text files with Unix-style end-of-line format using `princ' in\nbatch mode, with standard output redirected to a file.\n\nOn Posix systems, this function always returns non-nil, and has no\neffect except for flushing STREAM's data.")
    @GenerateNodeFactory
    public abstract static class FSetBinaryMode extends ELispBuiltInBaseNode {
        @Specialization
        public static Object setBinaryMode(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    @ELispBuiltIn(name = "file-system-info", minArgs = 1, maxArgs = 1, doc = "Return storage information about the file system FILENAME is on.\nValue is a list of numbers (TOTAL FREE AVAIL), where TOTAL is the total\nstorage of the file system, FREE is the free storage, and AVAIL is the\nstorage available to a non-superuser.  All 3 numbers are in bytes.\nIf the underlying system call fails, value is nil.")
    @GenerateNodeFactory
    public abstract static class FFileSystemInfo extends ELispBuiltInBaseNode {
        @Specialization
        public static Object fileSystemInfo(Object a) {
            throw new UnsupportedOperationException();
        }
    }
}
