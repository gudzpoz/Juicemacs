package party.iroiro.juicemacs.elisp.forms.coding;

import org.jspecify.annotations.Nullable;
import org.mozilla.universalchardet.UniversalDetector;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispSymbol;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.Objects;

import static party.iroiro.juicemacs.elisp.forms.ELispBuiltInConstants.*;

final class ELispCodingDetector {
    @Nullable
    private final ELispSymbol[] prioritizedCodingPerCategory = new ELispSymbol[CODING_CATEGORY_MAX];
    private final int[] categoryPriority = new int[CODING_CATEGORY_MAX];

    public ELispCodingDetector() {
        for (int i = 0; i < categoryPriority.length; i++) {
            categoryPriority[i] = i;
        }
    }

    public ELispSymbol detect(ByteIterator iterator) throws IOException {
        int size = Math.clamp(iterator.end - iterator.start, 0, 16 * 1024);
        byte[] bytes = new byte[size];
        ByteBuffer buffer = ByteBuffer.wrap(bytes);
        iterator.input.position(iterator.start).read(buffer);
        UniversalDetector detector = new UniversalDetector();
        detector.handleData(bytes);
        detector.dataEnd();
        int category = universalCharDetCodingToCategory(detector.getDetectedCharset());
        ELispSymbol detected = prioritizedCodingPerCategory[category];
        if (detected == null) {
            return Objects.requireNonNull(prioritizedCodingPerCategory[CODING_CATEGORY_RAW_TEXT]);
        }
        return detected;
    }

    static int universalCharDetCodingToCategory(@Nullable String coding) {
        return switch (coding) {
            case "UTF-8", "US-ASCII" -> CODING_CATEGORY_UTF_8_AUTO;
            case "UTF-16BE" -> CODING_CATEGORY_UTF_16_BE;
            case "UTF-16LE" -> CODING_CATEGORY_UTF_16_LE;
            case "BIG5" -> CODING_CATEGORY_BIG5;
            case "SHIFT_JIS" -> CODING_CATEGORY_SJIS;
            // TODO: We should probably fallback to a customized implementation,
            //   since ISO-2022 categories in universalchardet do not map directly
            //   to Emacs ones.
            case null, default -> CODING_CATEGORY_UTF_8_NOSIG;
        };
    }

    public void tryInitPriority(int category, ELispSymbol coding) {
        if (prioritizedCodingPerCategory[category] == null) {
            prioritizedCodingPerCategory[category] = coding;
        }
    }
}
