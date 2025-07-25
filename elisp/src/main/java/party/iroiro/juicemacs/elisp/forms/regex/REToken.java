package party.iroiro.juicemacs.elisp.forms.regex;

import java.util.ArrayList;

sealed interface REToken {
    /// The `.` notation, matching any char but `\n`
    record AnyChar() implements REToken {
    }

    /// Literally any char, emitted by the compiler/optimizer
    record ReallyAnyChar() implements REToken {
    }

    /// @param c a Unicode codepoint
    record Char(int c) implements REToken {
    }

    record CharClass(
            CharClassContent.Named[] namedClasses,
            CharClassContent.Range[] charRanges,
            boolean charRangesFitInInt,
            boolean invert
    ) implements REToken {
        static REToken preprocess(CharClassContent[] array, boolean invert) {
            ArrayList<CharClassContent.Named> namedClasses = new ArrayList<>();
            ArrayList<CharClassContent.Range> charRanges = new ArrayList<>();
            boolean charRangesFitInInt = true;
            for (CharClassContent content : array) {
                switch (content) {
                    case CharClassContent.Range range when range.min() <= range.max() -> {
                        charRangesFitInInt &= range.max() <= 0xFFFF;
                        charRanges.add(range);
                    }
                    case CharClassContent.Named named -> namedClasses.add(named);
                    default -> {
                    }
                }
            }
            if (namedClasses.isEmpty() && charRanges.size() == 1 && !invert) {
                CharClassContent.Range first = charRanges.getFirst();
                if (first.min() == first.max()) {
                    return new Char(first.min());
                }
            }
            return new CharClass(
                    namedClasses.toArray(CharClassContent.Named[]::new),
                    charRanges.toArray(CharClassContent.Range[]::new),
                    charRangesFitInInt,
                    invert
            );
        }
    }

    record SyntaxChar(byte kind, boolean invert) implements REToken {
    }

    record CategoryChar(byte kind, boolean invert) implements REToken {
    }

    /// @param index the explicit group index, -1 if non-capturing, 0 if auto-numbered
    record GroupStart(int index) implements REToken {
    }

    record GroupEnd() implements REToken {
    }

    /// @param min    the minimum number of repetitions
    /// @param max    the maximum number of repetitions
    /// @param greedy true if the quantifier is greedy
    record Quantifier(int min, int max, boolean greedy) implements REToken {
    }

    record Alternation() implements REToken {
    }

    record StartOfString() implements REToken {
    }

    record EndOfString() implements REToken {
    }

    record StartOfLine() implements REToken {
    }

    record EndOfLine() implements REToken {
    }

    record BufferPoint() implements REToken {
    }

    record StartOfWord() implements REToken {
    }

    record EndOfWord() implements REToken {
    }

    record WordBoundary(boolean invert) implements REToken {
    }

    record StartOfSymbol() implements REToken {
    }

    record EndOfSymbol() implements REToken {
    }

    record BackReference(int index) implements REToken {
    }
}
