package party.iroiro.juicemacs.elisp.forms.regex;

import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;

import java.util.ArrayList;
import java.util.Arrays;

sealed interface REAst {
    int LOOKAHEAD_MASK_CHAR_LIMIT = 512;
    int LOOKAHEAD_MASK_MAX_BITS = 21;

    default int lookbehind(@Nullable ELispCharTable canon) {
        return -1;
    }

    default int lookahead(@Nullable ELispCharTable canon) {
        return -1;
    }

    default int minLength() {
        return 0;
    }

    static int getCharMask(int c) {
        return (1 << (c & 0b111))
                | (1 << (((c >> 3) & 0b111) + 8))
                | (1 << (((c >> 5) & 0b111) + 16));
    }

    static boolean checkCharMask(int c, int mask) {
        return (((1 << (c & 0b111)) & mask) != 0)
                && (((1 << (((c >> 3) & 0b111) + 8)) & mask) != 0)
                && (((1 << (((c >> 5) & 0b111) + 16)) & mask) != 0);
    }

    record Atom(REToken token) implements REAst {
        @Override
        public int lookahead(@Nullable ELispCharTable canon) {
            return switch (token) {
                case REToken.Char(int c) -> canon == null ? c : ELispRegExpNode.translate(c, canon);
                case REToken.CharClass(
                        CharClassContent.Named[] classes,
                        CharClassContent.Range[] ranges,
                        _, boolean invert
                ) when !invert && classes.length == 0 -> {
                    CharClassContent.Range[] translated = ranges;
                    if (canon != null) {
                        ArrayList<CharClassContent.Range> newRanges = new ArrayList<>();
                        CharClassContent.translateRanges(translated, newRanges, canon);
                        translated = newRanges.toArray(new CharClassContent.Range[0]);
                    }
                    if (ranges.length == 1 && ranges[0].min() == ranges[0].max()) {
                        yield ranges[0].min();
                    }
                    int count = 0;
                    int mask = 0;
                    for (CharClassContent.Range range : translated) {
                        for (int c = range.min(); c <= range.max(); c++) {
                            if (count >= LOOKAHEAD_MASK_CHAR_LIMIT) {
                                yield -1;
                            }
                            mask |= getCharMask(c);
                            count++;
                        }
                    }
                    if (Integer.bitCount(mask) > LOOKAHEAD_MASK_MAX_BITS) {
                        yield -1;
                    }
                    yield mask | Integer.MIN_VALUE;
                }
                case REToken.EndOfLine _ -> '\n';
                default -> -1;
            };
        }

        @Override
        public int lookbehind(@Nullable ELispCharTable canon) {
            if (token instanceof REToken.StartOfLine) {
                return '\n';
            }
            return -1;
        }

        @Override
        public int minLength() {
            return switch (token) {
                case REToken.AnyChar _,
                     REToken.CharClass _,
                     REToken.CategoryChar _,
                     REToken.SyntaxChar _ -> 1;
                default -> 0;
            };
        }
    }

    /// A group with nested unions `(a|b)`, capturing or not
    ///
    /// @param index        -1 if non-capturing
    /// @param alternations the unions, note that it is in reverse order
    record Group(int index, REAst[][] alternations) implements REAst {
        @Override
        public int lookahead(@Nullable ELispCharTable canon) {
            int count = 0;
            int lookahead = 0;
            for (REAst[] alternation : alternations) {
                if (alternation.length == 0) {
                    return -1;
                } else {
                    REAst start = alternation[0];
                    int next = start.lookahead(canon);
                    if (count == 0) {
                        lookahead = next;
                        count = 1;
                    } else if (count == 1) {
                        if (lookahead != next) {
                            lookahead = lookahead < 0 ? lookahead : getCharMask(lookahead);
                            lookahead |= next < 0 ? next : getCharMask(next);
                            count++;
                        }
                    } else {
                        lookahead |= next < 0 ? next : getCharMask(next);
                    }
                }
            }
            if (Integer.bitCount(lookahead) > LOOKAHEAD_MASK_MAX_BITS) {
                return -1;
            }
            return lookahead;
        }

        @Override
        public int lookbehind(@Nullable ELispCharTable canon) {
            int lookbehind = -1;
            for (REAst[] alternation : alternations) {
                if (alternation.length == 0) {
                    return -1;
                }
                int next = alternation[0].lookbehind(canon);
                if (lookbehind == -1) {
                    lookbehind = next;
                } else if (lookbehind != next) {
                    return -1;
                }
            }
            return lookbehind;
        }

        @Override
        public int minLength() {
            int min = Integer.MAX_VALUE;
            for (REAst[] alternation : alternations) {
                int length = 0;
                for (REAst child : alternation) {
                    length += child.minLength();
                }
                min = Math.min(min, length);
            }
            return min;
        }

        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder("Group{index=");
            builder.append(index).append(", alternations=[");
            for (REAst[] alternation : alternations) {
                builder.append(Arrays.toString(alternation)).append(", ");
            }
            return builder.append("]}").toString();
        }
    }

    record Literal(int[] chars) implements REAst {
        @Override
        public int lookahead(@Nullable ELispCharTable canon) {
            int c = chars[0];
            return canon == null ? c : ELispRegExpNode.translate(c, canon);
        }

        @Override
        public int minLength() {
            return chars.length;
        }

        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder("Literal{chars=[");
            for (int c : chars) {
                builder.appendCodePoint(c);
            }
            return builder.append("]}").toString();
        }
    }

    record Quantified(REAst child, REToken.Quantifier quantifier) implements REAst {
        @Override
        public int lookahead(@Nullable ELispCharTable canon) {
            if (quantifier.min() == 0) {
                return -1;
            }
            return child.lookahead(canon);
        }

        @Override
        public int lookbehind(@Nullable ELispCharTable canon) {
            if (quantifier.min() == 0) {
                return -1;
            }
            return child.lookbehind(canon);
        }

        @Override
        public int minLength() {
            return child.minLength() * quantifier.min();
        }
    }
}
