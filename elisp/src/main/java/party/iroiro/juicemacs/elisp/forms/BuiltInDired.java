package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.regex.ELispRegExp;
import party.iroiro.juicemacs.elisp.nodes.local.Dynamic;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.array.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.string.MuleStringBuilder;

import java.io.IOException;
import java.nio.file.attribute.PosixFilePermission;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Stream;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInUtils.currentBuffer;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

public class BuiltInDired extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInDiredFactory.getFactories();
    }

    record CompletionRegexpFilter(ELispContext context, ArrayList<ELispRegExp.CompiledRegExp> regexps)
            implements Predicate<TruffleFile> {
        static CompletionRegexpFilter create(ELispContext context) {
            ArrayList<ELispRegExp.CompiledRegExp> regexps = new ArrayList<>();
            for (Object regexp : ELispCons.iterate(context.getValue(COMPLETION_REGEXP_LIST))) {
                regexps.add(BuiltInSearch.compileRegExp(
                        context.language(),
                        asStr(regexp),
                        null
                ));
            }
            return new CompletionRegexpFilter(context, regexps);
        }

        @Override
        public boolean test(TruffleFile truffleFile) {
            ELispString name = new ELispString(truffleFile.getName());
            return regexps.stream().noneMatch((regexp) ->
                    isNil(regexp.call(name, false, 0, Long.MAX_VALUE, context.currentBuffer())));
        }
    }

    @TruffleBoundary
    public static Object fileNameCompletion(ELispContext context, ELispString file, ELispString dirname, boolean all, Object predicate) {
        boolean ignoreCase = !isNil(COMPLETION_IGNORE_CASE.getValue());
        try (Dynamic _ = Dynamic.pushDynamic(DEFAULT_DIRECTORY, dirname);
             Dynamic _ = Dynamic.pushDynamic(CASE_FOLD_SEARCH, ignoreCase)) {
            // TODO: encode file names
            TruffleFile d = context.getFileExpanded(dirname);
            Collection<TruffleFile> listing = d.list();

            Object allCompletion = listing.stream().filter((entry) -> { // TODO: NOPMD
                        String javaName = entry.getName();
                        if (javaName.length() < file.length()) {
                            return false;
                        }
                        ELispString name = new ELispString(javaName);
                        if (!isT(BuiltInFns.FCompareStrings.compareStrings(
                                name, 0L, (long) file.length(),
                                file, 0L, (long) file.length(),
                                ignoreCase
                        ))) {
                            return false;
                        }
                        if (!all) {
                            // TODO: !all
                        }
                        return true;
                    }).filter(CompletionRegexpFilter.create(context))
                    .map((entry) -> {
                        if (entry.isDirectory()) {
                            return new MuleStringBuilder()
                                    .appendString(new ELispString(entry.getName()))
                                    .appendCodePoint('/')
                                    .buildString();
                        } else {
                            return new ELispString(entry.getPath());
                        }
                    })
                    .collect(ELispCons.ListBuilder.collector());
            // TODO: !all
            return allCompletion;
        } catch (IOException e) {
            throw ELispSignals.reportFileError(e, dirname);
        }
    }

    /**
     * <pre>
     * Return a list of names of files in DIRECTORY.
     * There are four optional arguments:
     * If FULL is non-nil, return absolute file names.  Otherwise return names
     *  that are relative to the specified directory.
     * If MATCH is non-nil, mention only file names whose non-directory part
     *  matches the regexp MATCH.
     * If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
     *  Otherwise, the list returned is sorted with `string-lessp'.
     *  NOSORT is useful if you plan to sort the result yourself.
     * If COUNT is non-nil and a natural number, the function will return
     *  COUNT number of file names (if so many are present).
     * </pre>
     */
    @ELispBuiltIn(name = "directory-files", minArgs = 1, maxArgs = 5)
    @GenerateNodeFactory
    public abstract static class FDirectoryFiles extends ELispBuiltInBaseNode {
        @TruffleBoundary
        @Specialization
        public Object directoryFiles(ELispString directory, boolean full, Object match, boolean nosort, Object count) {
            long limit = notNilOr(count, -1);
            ELispRegExp.@Nullable CompiledRegExp matcher = isNil(match)
                    ? null
                    : BuiltInSearch.compileRegExp(getLanguage(), asStr(match), null);
            TruffleFile dir = getContext().getFileExpanded(directory);
            try {
                Stream<TruffleFile> stream = dir.list().stream();
                if (matcher != null) {
                    ELispBuffer buffer = currentBuffer();
                    stream = stream.filter((file) ->
                            !isNil(matcher.call(new ELispString(file.getName()), true, 0, -1, buffer)));
                }
                if (!nosort) {
                    stream = stream.sorted(Comparator.comparing(TruffleFile::getName));
                }
                if (limit >= 0) {
                    stream = stream.limit(limit);
                }
                Object collect = (full // TODO: NOPMD
                        ? stream.map((file) -> new ELispString(file.getAbsoluteFile().toString()))
                        : stream.map((file) -> new ELispString(file.getName())))
                        .collect(ELispCons.ListBuilder.collector());
                return collect;
            } catch (IOException e) {
                throw ELispSignals.reportFileError(e, directory);
            }
        }
    }

    /**
     * <pre>
     * Return a list of names of files and their attributes in DIRECTORY.
     * Value is a list of the form:
     *
     *   ((FILE1 . FILE1-ATTRS) (FILE2 . FILE2-ATTRS) ...)
     *
     * where each FILEn-ATTRS is the attributes of FILEn as returned
     * by `file-attributes'.
     *
     * This function accepts five optional arguments:
     * If FULL is non-nil, return absolute file names.  Otherwise return names
     *  that are relative to the specified directory.
     * If MATCH is non-nil, mention only file names whose non-directory part
     *  matches the regexp MATCH.
     * If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
     *  NOSORT is useful if you plan to sort the result yourself.
     * ID-FORMAT specifies the preferred format of attributes uid and gid, see
     *  `file-attributes' for further documentation.
     * If COUNT is non-nil and a natural number, the function will return
     *  COUNT number of file names (if so many are present).
     * On MS-Windows, performance depends on `w32-get-true-file-attributes',
     * which see.
     * </pre>
     */
    @ELispBuiltIn(name = "directory-files-and-attributes", minArgs = 1, maxArgs = 6)
    @GenerateNodeFactory
    public abstract static class FDirectoryFilesAndAttributes extends ELispBuiltInBaseNode {
        @Specialization
        public static Void directoryFilesAndAttributes(Object directory, Object full, Object match, Object nosort, Object idFormat, Object count) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Complete file name FILE in directory DIRECTORY.
     * Returns the longest string
     * common to all file names in DIRECTORY that start with FILE.
     * If there is only one and FILE matches it exactly, returns t.
     * Returns nil if DIRECTORY contains no name starting with FILE.
     *
     * If PREDICATE is non-nil, call PREDICATE with each possible
     * completion (in absolute form) and ignore it if PREDICATE returns nil.
     *
     * This function ignores some of the possible completions as determined
     * by the variables `completion-regexp-list' and
     * `completion-ignored-extensions', which see.  `completion-regexp-list'
     * is matched against file and directory names relative to DIRECTORY.
     * </pre>
     */
    @ELispBuiltIn(name = "file-name-completion", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FFileNameCompletion extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fileNameCompletion(Object file, Object directory, Object predicate) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of all completions of file name FILE in directory DIRECTORY.
     * These are all file names in directory DIRECTORY which begin with FILE.
     *
     * This function ignores some of the possible completions as determined
     * by `completion-regexp-list', which see.  `completion-regexp-list'
     * is matched against file and directory names relative to DIRECTORY.
     * </pre>
     */
    @ELispBuiltIn(name = "file-name-all-completions", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFileNameAllCompletions extends ELispBuiltInBaseNode {
        @Specialization
        public Object fileNameAllCompletions(ELispString file, ELispString directory) {
            directory = BuiltInFileIO.FExpandFileName.expandFileName(directory, false);
            Object handler = BuiltInFileIO.FFindFileNameHandler.findFileNameHandler(directory, FILE_NAME_ALL_COMPLETIONS, this);
            if (isNil(handler)) {
                handler = BuiltInFileIO.FFindFileNameHandler.findFileNameHandler(file, FILE_NAME_ALL_COMPLETIONS, this);
            }
            if (!isNil(handler)) {
                return BuiltInEval.FFuncall.funcall(this, handler, FILE_NAME_ALL_COMPLETIONS, file, directory);
            }
            return fileNameCompletion(getContext(), file, directory, true, false);
        }
    }

    /**
     * <pre>
     * Return a list of attributes of file FILENAME.
     * Value is nil if specified file does not exist.
     *
     * ID-FORMAT specifies the preferred format of attributes uid and gid (see
     * below) - valid values are `string' and `integer'.  The latter is the
     * default, but we plan to change that, so you should specify a non-nil value
     * for ID-FORMAT if you use the returned uid or gid.
     *
     * To access the elements returned, the following access functions are
     * provided: `file-attribute-type', `file-attribute-link-number',
     * `file-attribute-user-id', `file-attribute-group-id',
     * `file-attribute-access-time', `file-attribute-modification-time',
     * `file-attribute-status-change-time', `file-attribute-size',
     * `file-attribute-modes', `file-attribute-inode-number', and
     * `file-attribute-device-number'.
     *
     * Elements of the attribute list are:
     *  0. t for directory, string (name linked to) for symbolic link, or nil.
     *  1. Number of links to file.
     *  2. File uid as a string or (if ID-FORMAT is `integer' or a string value
     *   cannot be looked up) as an integer.
     *  3. File gid, likewise.
     *  4. Last access time, in the style of `current-time'.
     *   (See a note below about access time on FAT-based filesystems.)
     *  5. Last modification time, likewise.  This is the time of the last
     *   change to the file's contents.
     *  6. Last status change time, likewise.  This is the time of last change
     *   to the file's attributes: owner and group, access mode bits, etc.
     *  7. Size in bytes, as an integer.
     *  8. File modes, as a string of ten letters or dashes as in ls -l.
     *  9. An unspecified value, present only for backward compatibility.
     * 10. inode number, as a nonnegative integer.
     * 11. Filesystem device identifier, as an integer or a cons cell of integers.
     *
     * Large integers are bignums, so `eq' might not work on them.
     * On most filesystems, the combination of the inode and the device
     * identifier uniquely identifies the file.  This unique file identification
     * is provided by the access function `file-attribute-file-identifier'.
     *
     * On MS-Windows, performance depends on `w32-get-true-file-attributes',
     * which see.
     *
     * On some FAT-based filesystems, only the date of last access is recorded,
     * so last access time will always be midnight of that day.
     * </pre>
     */
    @ELispBuiltIn(name = "file-attributes", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFileAttributes extends ELispBuiltInBaseNode {
        @TruffleBoundary
        @Specialization
        public Object fileAttributes(ELispString filename, Object idFormat) {
            // TODO: id-format
            TruffleFile file = getContext().getFileExpanded(filename);
            if (!file.exists()) {
                return false;
            }
            try {
                return ELispCons.listOf(
                        // #0: directory?
                        file.isDirectory()
                                ? true
                                : file.isSymbolicLink()
                                ? new ELispString(file.readSymbolicLink().toString())
                                : false,
                        // #1: TODO: links
                        1L,
                        // #2: uid
                        new ELispString(file.getOwner().getName()),
                        // #3: gid
                        new ELispString(file.getGroup().getName()),
                        // #4: last access time
                        BuiltInTimeFns.ELispTimeFnsNode.toTimeCons(file.getLastAccessTime().toInstant()),
                        // #5: modification time
                        BuiltInTimeFns.ELispTimeFnsNode.toTimeCons(file.getLastModifiedTime().toInstant()),
                        // #6: TODO: status change time
                        BuiltInTimeFns.ELispTimeFnsNode.toTimeCons(file.getCreationTime().toInstant()),
                        // #7: size
                        file.size(),
                        // #8: mode
                        new ELispString(permissionString(file)),
                        // #9: ?
                        false,
                        // #10: TODO: inode
                        1L,
                        // #11: TODO: device
                        0L
                );
            } catch (IOException e) {
                throw ELispSignals.reportFileError(e, filename);
            }
        }

        @TruffleBoundary
        private byte[] permissionString(TruffleFile file) throws IOException {
            byte[] string = new byte[10];
            string[0] = (byte) (file.isDirectory() ? 'd' : file.isSymbolicLink() ? 'l' : '-');
            Set<PosixFilePermission> permissions = file.getPosixPermissions();
            string[1] = (byte) (permissions.contains(PosixFilePermission.OWNER_READ) ? 'r' : '-');
            string[2] = (byte) (permissions.contains(PosixFilePermission.OWNER_WRITE) ? 'w' : '-');
            string[3] = (byte) (permissions.contains(PosixFilePermission.OWNER_EXECUTE) ? 'x' : '-');
            string[4] = (byte) (permissions.contains(PosixFilePermission.GROUP_READ) ? 'r' : '-');
            string[5] = (byte) (permissions.contains(PosixFilePermission.GROUP_WRITE) ? 'w' : '-');
            string[6] = (byte) (permissions.contains(PosixFilePermission.GROUP_EXECUTE) ? 'x' : '-');
            string[7] = (byte) (permissions.contains(PosixFilePermission.OTHERS_READ) ? 'r' : '-');
            string[8] = (byte) (permissions.contains(PosixFilePermission.OTHERS_WRITE) ? 'w' : '-');
            string[9] = (byte) (permissions.contains(PosixFilePermission.OTHERS_EXECUTE) ? 'x' : '-');
            return string;
        }
    }

    /**
     * <pre>
     * Return t if first arg file attributes list is less than second.
     * Comparison is in lexicographic order and case is significant.
     * </pre>
     */
    @ELispBuiltIn(name = "file-attributes-lessp", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FFileAttributesLessp extends ELispBuiltInBaseNode {
        @Specialization
        public static Void fileAttributesLessp(Object f1, Object f2) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of user names currently registered in the system.
     * If we don't know how to determine that on this platform, just
     * return a list with one element, taken from `user-real-login-name'.
     * </pre>
     */
    @ELispBuiltIn(name = "system-users", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FSystemUsers extends ELispBuiltInBaseNode {
        @Specialization
        public static Void systemUsers() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return a list of user group names currently registered in the system.
     * The value may be nil if not supported on this platform.
     * </pre>
     */
    @ELispBuiltIn(name = "system-groups", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FSystemGroups extends ELispBuiltInBaseNode {
        @Specialization
        public static Void systemGroups() {
            throw new UnsupportedOperationException();
        }
    }
}
