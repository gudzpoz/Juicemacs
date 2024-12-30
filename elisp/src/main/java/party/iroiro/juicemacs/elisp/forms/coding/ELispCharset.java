package party.iroiro.juicemacs.elisp.forms.coding;

import org.eclipse.collections.impl.list.mutable.primitive.LongArrayList;
import org.eclipse.jdt.annotation.Nullable;
import party.iroiro.juicemacs.elisp.forms.BuiltInCharSet;
import party.iroiro.juicemacs.elisp.forms.BuiltInCharTab;
import party.iroiro.juicemacs.elisp.forms.BuiltInLRead;
import party.iroiro.juicemacs.elisp.runtime.ELispContext;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCharTable;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispCons;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispString;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispVector;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.function.Consumer;
import java.util.regex.Pattern;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.CHARSET_MAP_PATH;
import static party.iroiro.juicemacs.elisp.runtime.ELispGlobals.NIL;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.*;

/// ### Implementation Notes
///
/// - We use `long` for all character-related operations, because under
///   some encoding, a character actually needs an unsigned int to represent,
///   and we have to use a `long` because Java integers are signed.
public final class ELispCharset {
    public final int id;
    public final ELispVector attributes;
    public final int dimension;
    public final byte[] codeSpaceMask;
    public final int[] codeSpace;
    public final boolean codeLinearP;
    public final boolean isoChars96;
    public final boolean asciiCompatibleP;
    public final boolean supplementaryP;
    public final boolean compactCodesP;
    public final boolean unifiedP;
    public final int isoFinal;
    public final int isoRevision;
    public final int emacsMuleId;
    public final CharsetMethod method;
    public final long minCode;
    public final long maxCode;
    public final long charIndexOffset;
    public long minChar;
    public long maxChar;
    public final long invalidCode;
    public final byte[] fastMap;
    public final long codeOffset;

    public ELispCharset(
            int id,
            ELispVector attributes,
            int dimension,
            byte[] codeSpaceMask,
            int[] codeSpace,
            boolean codeLinearP,
            boolean isoChars96,
            boolean asciiCompatibleP,
            boolean supplementaryP,
            boolean compactCodesP,
            boolean unifiedP,
            int isoFinal,
            int isoRevision,
            int emacsMuleId,
            CharsetMethod method,
            long minCode,
            long maxCode,
            long charIndexOffset,
            long minChar,
            long maxChar,
            long invalidCode,
            byte[] fastMap,
            long codeOffset
    ) {
        this.id = id;
        this.attributes = attributes;
        this.dimension = dimension;
        this.codeSpaceMask = codeSpaceMask;
        this.codeSpace = codeSpace;
        this.codeLinearP = codeLinearP;
        this.isoChars96 = isoChars96;
        this.asciiCompatibleP = asciiCompatibleP;
        this.supplementaryP = supplementaryP;
        this.compactCodesP = compactCodesP;
        this.unifiedP = unifiedP;
        this.isoFinal = isoFinal;
        this.isoRevision = isoRevision;
        this.emacsMuleId = emacsMuleId;
        this.method = method;
        this.minCode = minCode;
        this.maxCode = maxCode;
        this.charIndexOffset = charIndexOffset;
        this.minChar = minChar;
        this.maxChar = maxChar;
        this.invalidCode = invalidCode;
        this.fastMap = fastMap;
        this.codeOffset = codeOffset;
    }

    private boolean notInFastMap(int c) {
        if (c < 0x10000) {
            return (fastMap[c >> 10] & (1 << ((c >> 7) & 7))) == 0;
        } else {
            return (fastMap[(c >> 15) + 62] & (1 << ((c >> 12) & 7))) == 0;
        }
    }

    public void fastMapSet(int c) {
        if (c < 0x10000) {
            fastMap[c >> 10] |= (byte) (1 << ((c >> 7) & 7));
        } else {
            fastMap[(c >> 15) + 62] |= (byte) (1 << ((c >> 12) & 7));
        }
    }

    public long decodeChar(long code) {
        if (code <= 0x7F && asciiCompatibleP) {
            return code;
        }
        if (code < minCode || maxCode < code) {
            return -1;
        }
        if (!unifiedP && codeLinearP) {
            if (method == CharsetMethod.OFFSET) {
                return code - minCode + codeOffset;
            }
            if (method == CharsetMethod.MAP && attributes.get(CHARSET_DECODER) instanceof ELispVector vector) {
                return asNatInt(vector.get(Math.toIntExact(code - minCode)));
            }
        }
        return method.decodeChar(code, this);
    }

    public long encodeChar(long c) {
        if (c <= 0x7F && asciiCompatibleP) {
            return c;
        }
        if (!unifiedP && method != CharsetMethod.SUBSET && method != CharsetMethod.SUPERSET) {
            if (c < minChar || maxChar < c) {
                return invalidCode;
            }
            if (method == CharsetMethod.OFFSET && codeLinearP) {
                return c - codeOffset + minCode;
            }
            if (method == CharsetMethod.MAP && compactCodesP && attributes.get(CHARSET_ENCODER) instanceof ELispCharTable table) {
                return (int) notNilOr(table.get(Math.toIntExact(c)), invalidCode);
            }
        }
        return encodeCharSlow(c);
    }

    private long encodeCharSlow(long code) {
        if (unifiedP) {
            ELispCharTable deunifier = asCharTable(attributes.get(CHARSET_DEUNIFIER));
            Object deunified = deunifier.get(Math.toIntExact(code));
            if (deunified instanceof Long l) {
                code = codeOffset + l;
            }
        }
        return method.encodeChar(code, this);
    }

    public void mapChars(Consumer<ELispCons> callback, int from, int to) {
        boolean partial = minCode < from || to < maxCode;

        switch (method) {
            case OFFSET -> {
                long fromIndex = codePointToIndex(from);
                long toIndex = codePointToIndex(to);
                long fromChar = fromIndex + codeOffset;
                long toChar = toIndex + codeOffset;
                if (unifiedP) {
                    // TODO: load_charset if we are to implement lazy loading
//                    if (isNil(attributes.get(CHARSET_DEUNIFIER))) {}
                    ELispCharTable deunifier = asCharTable(attributes.get(CHARSET_DEUNIFIER));
                    mapCharTable(deunifier, callback);
                }
                ELispCons range = new ELispCons(fromChar, toChar);
                callback.accept(range);
            }
            case MAP -> {
                // TODO: load_charset if we are to implement lazy loading
//                if (isNil(attributes.get(CHARSET_ENCODER))) {}
                ELispCharTable encoder = asCharTable(attributes.get(CHARSET_ENCODER));
                mapCharTable(encoder, callback);
            }
            case SUBSET -> {
                ELispVector subsetInfo = asVector(attributes.get(CHARSET_SUBSET));
                ELispCharset parent = BuiltInCharSet.getCharsetFromId(asNatInt(subsetInfo.getFirst()));
                int offset = asInt(subsetInfo.get(3));
                from -= offset;
                to -= offset;
                int parentMinCode = asNatInt(subsetInfo.get(1));
                int parentMaxCode = asNatInt(subsetInfo.get(2));
                from = Math.max(from, parentMinCode);
                to = Math.min(to, parentMaxCode);
                parent.mapChars(callback, from, to);
            }
            case SUPERSET -> {
                Object parents = attributes.get(CHARSET_SUPERSET);
                for (Object parentCons : asConsOrNil(parents)) {
                    ELispCons cons = asCons(parentCons);
                    int parentId = asInt(cons.car());
                    int offset = asInt(cons.cdr());
                    ELispCharset parent = BuiltInCharSet.getCharsetFromId(parentId);
                    long parentFrom = from > offset ? from - offset : 0;
                    long parentTo = to > offset ? to - offset : 0;
                    parentFrom = Math.max(parentFrom, parent.minCode);
                    parentTo = Math.min(parentTo, parent.maxCode);
                    parent.mapChars(callback, Math.toIntExact(parentFrom), Math.toIntExact(parentTo));
                }
            }
        }
    }

    private void mapCharTable(ELispCharTable table, Consumer<ELispCons> callback) {
        BuiltInCharTab.charTableMap(table, (range, value) -> {
            if (isNil(value)) {
                return;
            }
            if (range instanceof ELispCons cons) {
                callback.accept(cons);
            } else {
                long c = asLong(range);
                callback.accept(new ELispCons(c, c));
            }
        });
    }

    public long indexToCodePoint(long index) {
        if (codeLinearP) {
            return index + minCode;
        }
        index += charIndexOffset;
        return (codeSpace[0] + index % codeSpace[2])
                | ((codeSpace[4] + (index / codeSpace[3] % codeSpace[6])) << 8)
                | ((codeSpace[8] + (index / codeSpace[7] % codeSpace[10])) << 16)
                | ((codeSpace[12] + (index / codeSpace[11])) << 24);
    }

    @SuppressWarnings("PMD.TruffleNoDirectRecursion")
    public long codePointToIndex(long codepoint) {
        return codePointToIndex(codepoint, codeLinearP, minCode, codeSpaceMask, codeSpace, charIndexOffset);
    }

    public static long codePointToIndex(
            long codepoint,
            boolean codeLinearP, long minCode,
            byte[] codeSpaceMask, int[] codeSpace, long charIndexOffset
    ) {
        if (codeLinearP) {
            return codepoint - minCode;
        }
        if (
                (codeSpaceMask[Math.toIntExact(codepoint >> 24)] & 0x8) == 0
                        || (codeSpaceMask[Math.toIntExact(codepoint >> 16 & 0xFF)] & 0x4) == 0
                        || (codeSpaceMask[Math.toIntExact(codepoint >> 8 & 0xFF)] & 0x2) == 0
                        || (codeSpaceMask[Math.toIntExact(codepoint & 0xFF)] & 0x1) == 0
        ) {
            return -1;
        }
        return ((((codepoint >> 24) & 0xFF) - codeSpace[12]) * codeSpace[11])
                + (((codepoint >> 16) & 0xFF) - codeSpace[8]) * codeSpace[7]
                + (((codepoint >> 8) & 0xFF) - codeSpace[4]) * codeSpace[3]
                + ((codepoint & 0xFF) - codeSpace[0])
                - charIndexOffset;
    }

    public void load(BuiltInCharSet builtInCharSet) {
        Object map;
        if (method == CharsetMethod.MAP) {
            map = attributes.get(CHARSET_MAP);
        } else {
            if (!unifiedP) {
                throw ELispSignals.fatal();
            }
            map = attributes.get(CHARSET_UNIFY_MAP);
        }
        loadCharsetMap(
                map instanceof ELispString file
                        ? loadMapFromFile(file)
                        : loadMapFromVector(asVector(map)),
                builtInCharSet
        );
    }

    private CharsetMap loadMapFromVector(ELispVector vector) {
        if (vector.size() % 2 == 1) {
            throw ELispSignals.fatal();
        }
        int capacity = vector.size() / 2;
        LongArrayList fromEntries = new LongArrayList(capacity);
        LongArrayList toEntries = new LongArrayList(capacity);
        LongArrayList valueEntries = new LongArrayList(capacity);
        for (int i = 0; i < vector.size(); i += 2) {
            int from, to;
            Object key = vector.get(i);
            if (key instanceof ELispCons cons) {
                from = asNatInt(cons.car());
                to = asNatInt(cons.cdr());
            } else {
                from = to = asNatInt(key);
            }
            int value = asNatInt(vector.get(i + 1));
            if (from < minCode || maxCode < to || from > to || value > MAX_CHAR) {
                continue;
            }
            fromEntries.add(from);
            toEntries.add(to);
            valueEntries.add(value);
        }
        return new CharsetMap(fromEntries, toEntries, valueEntries);
    }

    private static long readHex(Scanner scanner) {
        if (scanner.hasNextInt()) {
            return scanner.nextInt();
        }
        String next = scanner.next();
        if (!next.startsWith("0x")) {
            throw ELispSignals.fatal();
        }
        return Long.parseLong(next, 2, next.length(), 16);
    }

    private CharsetMap loadMapFromFile(ELispString file) {
        ELispString path = BuiltInLRead.locateOpenP(
                CHARSET_MAP_PATH.getValue(),
                file,
                ELispCons.listOf(new ELispString(".map"), new ELispString(".txt")),
                false,
                false,
                false
        );
        if (path == null) {
            throw ELispSignals.fatal();
        }
        try (Scanner scanner = new Scanner(new FileInputStream(path.toString()))) {
            scanner.useDelimiter("\\s+|\\b");
            scanner.useRadix(16);
            LongArrayList fromEntries = new LongArrayList();
            LongArrayList toEntries = new LongArrayList();
            Pattern hexNumber = Pattern.compile("0x[0-9a-fA-F]+");
            LongArrayList valueEntries = new LongArrayList();
            while (scanner.hasNextLine()) {
                if (!scanner.hasNext(hexNumber)) {
                    String comment = scanner.nextLine().stripLeading();
                    if (!comment.isBlank() && !comment.startsWith("#")) {
                        throw ELispSignals.fatal();
                    }
                    continue;
                }
                long from = readHex(scanner);
                long to;
                if (scanner.hasNext("-")) {
                    scanner.next();
                    to = readHex(scanner);
                } else {
                    to = from;
                }
                long value = readHex(scanner);
                fromEntries.add(from);
                toEntries.add(to);
                valueEntries.add(value);
            }
            return new CharsetMap(fromEntries, toEntries, valueEntries);
        } catch (FileNotFoundException e) {
            throw ELispSignals.fileMissing(e, e.getMessage());
        }
    }

    private void loadCharsetMap(CharsetMap charsetMap, BuiltInCharSet builtInCharSet) {
        if (charsetMap.from.isEmpty()) {
            return;
        }
        // TODO: Support lazy loading of charset maps
        @Nullable ELispVector decodingMap = null;
        ELispCharTable unifyTable = asCharTable(builtInCharSet.charUnifyTable.getValue());
        ELispCharTable encodingTable;
        {
            // Decoding
            if (method == CharsetMethod.MAP) {
                int n = Math.toIntExact(codePointToIndex(maxCode));
                decodingMap = new ELispVector(n + 1, -1L);
                attributes.set(CHARSET_DECODER, decodingMap);
            } else {
                unifyTable.setRange(Math.toIntExact(minChar), Math.toIntExact(maxChar), false);
            }
            // Encoding
            encodingTable = ELispCharTable.create(false, NIL, 0);
            attributes.set(method == CharsetMethod.MAP ? CHARSET_ENCODER : CHARSET_DEUNIFIER, encodingTable);
        }
        long minChar = charsetMap.value.get(0);
        long maxChar = minChar;
        long nonAsciiMinChar = MAX_CHAR;
        for (int i = 0; i < charsetMap.from.size(); i++) {
            final long from = charsetMap.from.get(i);
            final long to = charsetMap.to.get(i);
            final long fromChar = charsetMap.value.get(i);
            final long toChar;
            final int fromIndex = Math.toIntExact(codePointToIndex(from));
            final int toIndex;
            if (from == to) {
                toIndex = fromIndex;
                toChar = fromChar;
            } else {
                toIndex = Math.toIntExact(codePointToIndex(to));
                toChar = fromChar + (toIndex - fromIndex);
            }
            if (fromIndex < 0 || toIndex < 0) {
                continue;
            }
            int limIndex = toIndex + 1;
            minChar = Math.min(minChar, fromChar);
            maxChar = Math.max(maxChar, toChar);
            // Decoding
            if (method == CharsetMethod.MAP) {
                for (long j = fromIndex, c = fromChar;j < limIndex; j++, c++) {
                    decodingMap.set(Math.toIntExact(j), c);
                }
            } else {
                for (long j = fromIndex, c = fromChar; j < limIndex; j++, c++) {
                    unifyTable.set(Math.toIntExact(codeOffset + j), c);
                }
            }
            // Encoding
            if (method == CharsetMethod.MAP && compactCodesP) {
                for (long j = fromIndex, c = fromChar; j < limIndex; j++, c++) {
                    long code = indexToCodePoint(j);
                    if (isNil(encodingTable.get(Math.toIntExact(c)))) {
                        encodingTable.set(Math.toIntExact(c), code);
                    }
                }
            } else {
                for (long j = 0, c = fromChar; j < limIndex; j++, c++) {
                    if (isNil(encodingTable.get(Math.toIntExact(c)))) {
                        encodingTable.set(Math.toIntExact(c), j);
                    }
                }
            }
            // Init
            if (asciiCompatibleP) {
                if (fromChar >= 0x80) {
                    nonAsciiMinChar = Math.min(nonAsciiMinChar, fromChar);
                }
            } else if (toChar >= 0x80) {
                nonAsciiMinChar = 0x80;
            }
            for (long j = fromChar; j < toChar; j++) {
                fastMapSet(Math.toIntExact(j));
            }
        }
        this.minChar = asciiCompatibleP ? nonAsciiMinChar : minChar;
        this.maxChar = maxChar;
    }

    public enum CharsetMethod {
        OFFSET {
            @Override
            public long decodeChar(long code, ELispCharset charset) {
                long index = charset.codePointToIndex(code);
                if (index < 0) {
                    return -1;
                }
                long c = index + charset.codeOffset;
                if (charset.unifiedP && Character.MAX_CODE_POINT < c && c <= MAX_CHAR) {
                    // TODO: Is ELispContext.get(null) efficient enough?
                    ValueStorage.Forwarded charUnifyTable = ELispContext.get(null).globals().builtInCharSet.charUnifyTable;
                    Object result = asCharTable(charUnifyTable.getValue()).getChar(Math.toIntExact(c));
                    return notNilOr(result, c);
                }
                return c;
            }

            @Override
            public long encodeChar(long ch, ELispCharset charset) {
                if (charset.notInFastMap(Math.toIntExact(ch)) || ch < charset.minChar || charset.maxChar < ch) {
                    return charset.invalidCode;
                }
                long index = ch - charset.codeOffset;
                return charset.indexToCodePoint(index);
            }
        },
        MAP {
            @Override
            public long decodeChar(long code, ELispCharset charset) {
                long index = charset.codePointToIndex(code);
                if (index < 0) {
                    return -1;
                }
                ELispVector decoder = asVector(charset.attributes.get(CHARSET_DECODER));
                return asLong(decoder.get(Math.toIntExact(index)));
            }

            @Override
            public long encodeChar(long ch, ELispCharset charset) {
                if (charset.notInFastMap(Math.toIntExact(ch)) || ch < charset.minChar || charset.maxChar < ch) {
                    return charset.invalidCode;
                }
                ELispCharTable encoder = asCharTable(charset.attributes.get(CHARSET_ENCODER));
                Object result = encoder.getChar(Math.toIntExact(ch));
                if (isNil(result)) {
                    return charset.invalidCode;
                }
                long code = asLong(result);
                if (!charset.compactCodesP) {
                    code = charset.indexToCodePoint(code);
                }
                return code;
            }
        },
        SUBSET {
            @Override
            public long decodeChar(long code, ELispCharset charset) {
                ELispVector subsetInfo = asVector(charset.attributes.get(CHARSET_SUBSET));
                ELispCharset parent = BuiltInCharSet.getCharsetFromId(asInt(subsetInfo.getFirst()));
                long offset = asLong(subsetInfo.get(3));
                code -= offset;
                long parentStart = asLong(subsetInfo.get(1));
                long parentEnd = asLong(subsetInfo.get(2));
                if (code < parentStart || parentEnd < code) {
                    return -1;
                } else {
                    return parent.decodeChar(code);
                }
            }

            @Override
            public long encodeChar(long ch, ELispCharset charset) {
                ELispVector subsetInfo = asVector(charset.attributes.get(CHARSET_SUBSET));
                ELispCharset parent = BuiltInCharSet.getCharsetFromId(asInt(subsetInfo.getFirst()));
                long parentStart = asLong(subsetInfo.get(1));
                long parentEnd = asLong(subsetInfo.get(2));
                long code = parent.encodeChar(ch);
                if (code == parent.invalidCode || code < parentStart || parentEnd < code) {
                    return charset.invalidCode;
                }
                long offset = asLong(subsetInfo.get(3));
                return code + offset;
            }
        },
        SUPERSET {
            @Override
            public long decodeChar(long code, ELispCharset charset) {
                Iterable<Object> parents = asConsOrNil(charset.attributes.get(CHARSET_SUPERSET));
                for (Object parent : parents) {
                    ELispCons cons = asCons(parent);
                    int id = asInt(cons.car());
                    long offset = asLong(cons.cdr());
                    ELispCharset parentCs = BuiltInCharSet.getCharsetFromId(id);
                    long c = parentCs.decodeChar(code - offset);
                    if (c >= 0) {
                        return c;
                    }
                }
                return -1;
            }

            @Override
            public long encodeChar(long ch, ELispCharset charset) {
                Iterable<Object> parents = asConsOrNil(charset.attributes.get(CHARSET_SUPERSET));
                for (Object parent : parents) {
                    ELispCons cons = asCons(parent);
                    int id = asInt(cons.car());
                    long offset = asLong(cons.cdr());
                    ELispCharset parentCs = BuiltInCharSet.getCharsetFromId(id);
                    long code = parentCs.encodeChar(ch);
                    if (code != parentCs.invalidCode) {
                        return code + offset;
                    }
                }
                return charset.invalidCode;
            }
        };

        public abstract long decodeChar(long code, ELispCharset charset);
        public abstract long encodeChar(long ch, ELispCharset charset);
    }

    private record CharsetMap(LongArrayList from, LongArrayList to, LongArrayList value) {
    }
}
