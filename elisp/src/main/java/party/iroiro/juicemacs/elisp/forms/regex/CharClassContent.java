package party.iroiro.juicemacs.elisp.forms.regex;

import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;

import java.util.ArrayList;
import java.util.Arrays;

sealed interface CharClassContent {
    @SuppressWarnings("PMD.FieldNamingConventions")
    enum Named implements CharClassContent {
        alnum(0),
        alpha(1),
        ascii(2),
        blank(3),
        cntrl(4),
        digit(5),
        graph(6),
        lower(7),
        multibyte(8),
        nonascii(9),
        print(10),
        punct(11),
        space(12),
        unibyte(13),
        upper(14),
        word(15),
        xdigit(16);

        static final int MAX_NAME_LENGTH = Arrays.stream(values()).mapToInt((e) -> e.name().length()).max().orElse(16);
        public final int mask;

        Named(int i) {
            this.mask = 1 << i;
        }

        public boolean match(int bits) {
            return (mask & bits) != 0;
        }
    }

    record Range(int min, int max) implements CharClassContent {
        Range(int single) {
            this(single, single);
        }
    }

    static boolean translateRanges(
            CharClassContent.Range[] charRanges,
            ArrayList<Range> newRanges,
            ELispCharTable canon
    ) {
        boolean charRangesFitInInt = true;
        int start = -1;
        int end = -1;
        for (CharClassContent.Range range : charRanges) {
            int min = range.min();
            int max = range.max();
            for (int i = min; i <= max; i++) {
                int translated = ELispRegExpNode.translate(i, canon);
                if (start == -1) {
                    start = end = translated;
                    continue;
                }
                if (translated < start - 1 || end + 1 < translated) {
                    charRangesFitInInt &= end <= 0xFFFF;
                    newRanges.add(new CharClassContent.Range(start, end));
                    start = end = translated;
                }
                if (translated == start - 1) {
                    start = translated;
                } else if (translated == end + 1) {
                    end = translated;
                }
            }
        }
        charRangesFitInInt &= end <= 0xFFFF;
        newRanges.add(new CharClassContent.Range(start, end));
        return charRangesFitInInt;
    }
}
