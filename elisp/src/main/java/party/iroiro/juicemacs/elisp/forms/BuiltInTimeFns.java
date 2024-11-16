package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;

import java.time.Instant;
import java.util.List;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public class BuiltInTimeFns extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInTimeFnsFactory.getFactories();
    }

    /**
     * <pre>
     * Return the sum of two time values A and B, as a time value.
     * See `format-time-string' for the various forms of a time value.
     * For example, nil stands for the current time.
     * </pre>
     */
    @ELispBuiltIn(name = "time-add", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTimeAdd extends ELispBuiltInBaseNode {
        @Specialization
        public static Void timeAdd(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the difference between two time values A and B, as a time value.
     * You can use `float-time' to convert the difference into elapsed seconds.
     * See `format-time-string' for the various forms of a time value.
     * For example, nil stands for the current time.
     * </pre>
     */
    @ELispBuiltIn(name = "time-subtract", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTimeSubtract extends ELispBuiltInBaseNode {
        @Specialization
        public static Void timeSubtract(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if time value A is less than time value B.
     * See `format-time-string' for the various forms of a time value.
     * For example, nil stands for the current time.
     * </pre>
     */
    @ELispBuiltIn(name = "time-less-p", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTimeLessP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void timeLessP(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return non-nil if A and B are equal time values.
     * See `format-time-string' for the various forms of a time value.
     * </pre>
     */
    @ELispBuiltIn(name = "time-equal-p", minArgs = 2, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTimeEqualP extends ELispBuiltInBaseNode {
        @Specialization
        public static Void timeEqualP(Object a, Object b) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the current time, as a float number of seconds since the epoch.
     * If SPECIFIED-TIME is given, it is a time value to convert to float
     * instead of the current time.  See `format-time-string' for the various
     * forms of a time value.
     *
     * WARNING: Since the result is floating point, it may not be exact.
     * If precise time stamps are required, use either `time-convert',
     * or (if you need time as a string) `format-time-string'.
     * </pre>
     */
    @ELispBuiltIn(name = "float-time", minArgs = 0, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FFloatTime extends ELispBuiltInBaseNode {
        @Specialization
        public static Void floatTime(Object specifiedTime) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Use FORMAT-STRING to format the time value TIME.
     * A time value that is omitted or nil stands for the current time,
     * a number stands for that many seconds, an integer pair (TICKS . HZ)
     * stands for TICKS/HZ seconds, and an integer list (HI LO US PS) stands
     * for HI*2**16 + LO + US/10**6 + PS/10**12 seconds.  This function
     * treats seconds as time since the epoch of 1970-01-01 00:00:00 UTC.
     *
     * The optional ZONE is omitted or nil for Emacs local time, t for
     * Universal Time, `wall' for system wall clock time, or a string as in
     * the TZ environment variable.  It can also be a list (as from
     * `current-time-zone') or an integer (as from `decode-time') applied
     * without consideration for daylight saving time.
     *
     * The value is a copy of FORMAT-STRING, but with certain constructs replaced
     * by text that describes the specified date and time in TIME:
     *
     * %Y is the year, %y year without century, %C the century.
     * %G is the year corresponding to the ISO week, %g year corresponding
     *  to the ISO week, without century.
     * %m is the numeric month.
     * %b and %h are the locale's abbreviated month name, %B the full name.
     *  (%h is not supported on MS-Windows.)
     * %d is the day of the month, zero-padded, %e is blank-padded.
     * %u is the numeric day of week from 1 (Monday) to 7, %w from 0 (Sunday) to 6.
     * %a is the locale's abbreviated name of the day of week, %A the full name.
     * %U is the week number starting on Sunday, %W starting on Monday,
     *  %V the week number according to ISO 8601.
     * %j is the day of the year.
     *
     * %H is the hour on a 24-hour clock, %I is on a 12-hour clock, %k is like %H
     *  only blank-padded, %l is like %I blank-padded.
     * %p is the locale's equivalent of either AM or PM.
     * %q is the calendar quarter (1–4).
     * %M is the minute (00-59).
     * %S is the second (00-59; 00-60 on platforms with leap seconds)
     * %s is the number of seconds since 1970-01-01 00:00:00 +0000.
     * %N is the nanosecond, %6N the microsecond, %3N the millisecond, etc.
     * %Z is the time zone abbreviation, %z is the numeric form.
     *
     * %c is the locale's date and time format.
     * %x is the locale's "preferred" date format.
     * %D is like "%m/%d/%y".
     * %F is the ISO 8601 date format (like "%+4Y-%m-%d").
     *
     * %R is like "%H:%M", %T is like "%H:%M:%S", %r is like "%I:%M:%S %p".
     * %X is the locale's "preferred" time format.
     *
     * Finally, %n is a newline, %t is a tab, %% is a literal %, and
     * unrecognized %-sequences stand for themselves.
     *
     * A %-sequence can contain optional flags, field width, and a modifier
     * (in that order) after the `%'.  The flags are:
     *
     * `-' Do not pad the field.
     * `_' Pad with spaces.
     * `0' Pad with zeros.
     * `+' Pad with zeros and put `+' before nonnegative year numbers with &gt;4 digits.
     * `^' Use upper case characters if possible.
     * `#' Use opposite case characters if possible.
     *
     * A field width N is an unsigned decimal integer with a leading digit
     * nonzero.  %NX is like %X, but takes up at least N positions.  The
     * field width is (on GNU/Linux and some other systems) in measured in
     * bytes, not characters.  It depends on the locale what the width (in
     * characters) %NX will end up being, especially when there are non-ASCII
     * characters in %X.
     *
     * The modifiers are:
     *
     * `E' Use the locale's alternative version.
     * `O' Use the locale's number symbols.
     *
     * For example, to produce full ISO 8601 format, use "%FT%T%z".
     *
     * usage: (format-time-string FORMAT-STRING &amp;optional TIME ZONE)
     * </pre>
     */
    @ELispBuiltIn(name = "format-time-string", minArgs = 1, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FFormatTimeString extends ELispBuiltInBaseNode {
        @Specialization
        public static Void formatTimeString(Object formatString, Object time, Object zone) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Decode a timestamp into (SEC MINUTE HOUR DAY MONTH YEAR DOW DST UTCOFF).
     * The optional TIME is the time value to convert.  See
     * `format-time-string' for the various forms of a time value.
     *
     * The optional ZONE is omitted or nil for Emacs local time, t for
     * Universal Time, `wall' for system wall clock time, or a string as in
     * the TZ environment variable.  It can also be a list (as from
     * `current-time-zone') or an integer (the UTC offset in seconds) applied
     * without consideration for daylight saving time.
     *
     * The optional FORM specifies the form of the SEC member.  If `integer',
     * SEC is an integer; if t, SEC is an integer or (TICKS . HZ) timestamp
     * with the same precision as TIME.  An omitted or nil FORM is currently
     * treated like `integer', but this may change in future Emacs versions.
     *
     * To access (or alter) the elements in the time value, the
     * `decoded-time-second', `decoded-time-minute', `decoded-time-hour',
     * `decoded-time-day', `decoded-time-month', `decoded-time-year',
     * `decoded-time-weekday', `decoded-time-dst' and `decoded-time-zone'
     * accessors can be used.
     *
     * The list has the following nine members: SEC is an integer or
     * Lisp timestamp representing a nonnegative value less than 60
     * \(or less than 61 if the operating system supports leap seconds).
     * MINUTE is an integer between 0 and 59.  HOUR is an integer
     * between 0 and 23.  DAY is an integer between 1 and 31.  MONTH is an
     * integer between 1 and 12.  YEAR is the year number, an integer; 0
     * represents 1 BC.  DOW is the day of week, an integer between 0 and 6,
     * where 0 is Sunday.  DST is t if daylight saving time is in effect,
     * nil if it is not in effect, and -1 if daylight saving information is
     * not available.  UTCOFF is an integer indicating the UTC offset in
     * seconds, i.e., the number of seconds east of Greenwich.  (Note that
     * Common Lisp has different meanings for DOW and UTCOFF, and its
     * SEC is always an integer between 0 and 59.)
     *
     * usage: (decode-time &amp;optional TIME ZONE FORM)
     * </pre>
     */
    @ELispBuiltIn(name = "decode-time", minArgs = 0, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FDecodeTime extends ELispBuiltInBaseNode {
        @Specialization
        public static Void decodeTime(Object time, Object zone, Object form) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Convert TIME to a timestamp.
     *
     * TIME is a list (SECOND MINUTE HOUR DAY MONTH YEAR IGNORED DST ZONE)
     * in the style of `decode-time', so that (encode-time (decode-time ...)) works.
     * In this list, ZONE can be nil for Emacs local time, t for Universal
     * Time, `wall' for system wall clock time, or a string as in the TZ
     * environment variable.  ZONE can also be a list (as from
     * `current-time-zone') or an integer (as from `decode-time') applied
     * without consideration for daylight saving time.  If ZONE specifies a
     * time zone with daylight-saving transitions, DST is t for daylight
     * saving time, nil for standard time, and -1 to cause the daylight
     * saving flag to be guessed.
     *
     * TIME can also be a list (SECOND MINUTE HOUR DAY MONTH YEAR), which is
     * equivalent to (SECOND MINUTE HOUR DAY MONTH YEAR nil -1 nil).
     *
     * As an obsolescent calling convention, if this function is called with
     * 6 or more arguments, the first 6 arguments are SECOND, MINUTE, HOUR,
     * DAY, MONTH, and YEAR, and specify the components of a decoded time.
     * If there are more than 6 arguments the *last* argument is used as ZONE
     * and any other extra arguments are ignored, so that (apply
     * #\\='encode-time (decode-time ...)) works.  In this obsolescent
     * convention, DST is -1 and ZONE defaults to nil.
     *
     * The range of supported years is at least 1970 to the near future.
     * Out-of-range values for SECOND through MONTH are brought into range
     * via date arithmetic.  This can be tricky especially when combined with
     * DST; see Info node `(elisp)Time Conversion' for details and caveats.
     *
     * usage: (encode-time TIME &amp;rest OBSOLESCENT-ARGUMENTS)
     * </pre>
     */
    @ELispBuiltIn(name = "encode-time", minArgs = 1, maxArgs = 1, varArgs = true)
    @GenerateNodeFactory
    public abstract static class FEncodeTime extends ELispBuiltInBaseNode {
        @Specialization
        public static Void encodeTime(Object time, Object[] obsolescentArguments) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Convert TIME value to a Lisp timestamp of the given FORM.
     * Truncate the returned value toward minus infinity.
     *
     * If FORM is a positive integer, return a pair of integers (TICKS . FORM),
     * where TICKS is the number of clock ticks and FORM is the clock frequency
     * in ticks per second.
     *
     * If FORM is t, return (TICKS . PHZ), where PHZ is a suitable clock
     * frequency in ticks per second.
     *
     * If FORM is `integer', return an integer count of seconds.
     *
     * If FORM is `list', return an integer list (HIGH LOW USEC PSEC), where
     * HIGH has the most significant bits of the seconds, LOW has the least
     * significant 16 bits, and USEC and PSEC are the microsecond and
     * picosecond counts.
     *
     * If FORM is nil, the behavior depends on `current-time-list',
     * but new code should not rely on it.
     * </pre>
     */
    @ELispBuiltIn(name = "time-convert", minArgs = 1, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FTimeConvert extends ELispBuiltInBaseNode {
        @Specialization
        public static Void timeConvert(Object time, Object form) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the current time, as the number of seconds since 1970-01-01 00:00:00.
     * If the variable `current-time-list' is nil, the time is returned as a
     * pair of integers (TICKS . HZ), where TICKS counts clock ticks and HZ
     * is the clock ticks per second.  Otherwise, the time is returned as a
     * list of integers (HIGH LOW USEC PSEC) where HIGH has the most
     * significant bits of the seconds, LOW has the least significant 16
     * bits, and USEC and PSEC are the microsecond and picosecond counts.
     *
     * You can use `time-convert' to get a particular timestamp form
     * regardless of the value of `current-time-list'.
     * </pre>
     */
    @ELispBuiltIn(name = "current-time", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCurrentTime extends ELispBuiltInBaseNode {
        @Specialization
        public static Object currentTime() {
            if (isNil(ELispContext.CURRENT_TIME_LIST.getValue())) {
                return new ELispCons(System.currentTimeMillis(), 1000L);
            }
            ELispCons.ListBuilder builder = new ELispCons.ListBuilder();
            Instant now = Instant.now();
            long seconds = now.getEpochSecond();
            builder.add(seconds >> 16).add(seconds & 0xffff);
            long nano = now.getNano();
            builder.add(nano / 1000).add(nano % 1000 * 1000);
            return builder.build();
        }
    }

    /**
     * <pre>
     * Return the current CPU time along with its resolution.
     * The return value is a pair (CPU-TICKS . TICKS-PER-SEC).
     * The CPU-TICKS counter can wrap around, so values cannot be meaningfully
     * compared if too much time has passed between them.
     * </pre>
     */
    @ELispBuiltIn(name = "current-cpu-time", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FCurrentCpuTime extends ELispBuiltInBaseNode {
        @Specialization
        public static Void currentCpuTime() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the current local time, as a human-readable string.
     * Programs can use this function to decode a time,
     * since the number of columns in each field is fixed
     * if the year is in the range 1000-9999.
     * The format is `Sun Sep 16 01:03:52 1973'.
     * However, see also the functions `decode-time' and `format-time-string'
     * which provide a much more powerful and general facility.
     *
     * If SPECIFIED-TIME is given, it is the time value to format instead of
     * the current time.  See `format-time-string' for the various forms of a
     * time value.
     *
     * The optional ZONE is omitted or nil for Emacs local time, t for
     * Universal Time, `wall' for system wall clock time, or a string as in
     * the TZ environment variable.  It can also be a list (as from
     * `current-time-zone') or an integer (as from `decode-time') applied
     * without consideration for daylight saving time.
     * </pre>
     */
    @ELispBuiltIn(name = "current-time-string", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCurrentTimeString extends ELispBuiltInBaseNode {
        @Specialization
        public static Void currentTimeString(Object specifiedTime, Object zone) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Return the offset and name for the local time zone.
     * This returns a list of the form (OFFSET NAME).
     * OFFSET is an integer number of seconds ahead of UTC (east of Greenwich).
     *     A negative value means west of Greenwich.
     * NAME is a string giving the name of the time zone.
     * If SPECIFIED-TIME is given, the time zone offset is determined from it
     * instead of using the current time.  The argument should be a Lisp
     * time value; see `format-time-string' for the various forms of a time
     * value.
     *
     * The optional ZONE is omitted or nil for Emacs local time, t for
     * Universal Time, `wall' for system wall clock time, or a string as in
     * the TZ environment variable.  It can also be a list (as from
     * `current-time-zone') or an integer (as from `decode-time') applied
     * without consideration for daylight saving time.
     *
     * Some operating systems cannot provide all this information to Emacs;
     * in this case, `current-time-zone' returns a list containing nil for
     * the data it can't find.
     * </pre>
     */
    @ELispBuiltIn(name = "current-time-zone", minArgs = 0, maxArgs = 2)
    @GenerateNodeFactory
    public abstract static class FCurrentTimeZone extends ELispBuiltInBaseNode {
        @Specialization
        public static Void currentTimeZone(Object specifiedTime, Object zone) {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * <pre>
     * Set the Emacs local time zone using TZ, a string specifying a time zone rule.
     * If TZ is nil or `wall', use system wall clock time; this differs from
     * the usual Emacs convention where nil means current local time.  If TZ
     * is t, use Universal Time.  If TZ is a list (as from
     * `current-time-zone') or an integer (as from `decode-time'), use the
     * specified time zone without consideration for daylight saving time.
     *
     * Instead of calling this function, you typically want something else.
     * To temporarily use a different time zone rule for just one invocation
     * of `decode-time', `encode-time', or `format-time-string', pass the
     * function a ZONE argument.  To change local time consistently
     * throughout Emacs, call (setenv "TZ" TZ): this changes both the
     * environment of the Emacs process and the variable
     * `process-environment', whereas `set-time-zone-rule' affects only the
     * former.
     * </pre>
     */
    @ELispBuiltIn(name = "set-time-zone-rule", minArgs = 1, maxArgs = 1)
    @GenerateNodeFactory
    public abstract static class FSetTimeZoneRule extends ELispBuiltInBaseNode {
        @Specialization
        public static Void setTimeZoneRule(Object tz) {
            throw new UnsupportedOperationException();
        }
    }
}
