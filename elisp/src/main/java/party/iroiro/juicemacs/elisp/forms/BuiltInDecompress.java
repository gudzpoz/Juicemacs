package party.iroiro.juicemacs.elisp.forms;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.GenerateNodeFactory;
import com.oracle.truffle.api.dsl.NodeFactory;
import com.oracle.truffle.api.dsl.Specialization;
import party.iroiro.juicemacs.elisp.runtime.ELispSignals;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.piecetree.PieceTreeBase.NodeIterator;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.zip.GZIPInputStream;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.isNil;

public class BuiltInDecompress extends ELispBuiltIns {
    @Override
    protected List<? extends NodeFactory<? extends ELispBuiltInBaseNode>> getNodeFactories() {
        return BuiltInDecompressFactory.getFactories();
    }

    /**
     * <pre>
     * Return t if zlib decompression is available in this instance of Emacs.
     * </pre>
     */
    @ELispBuiltIn(name = "zlib-available-p", minArgs = 0, maxArgs = 0)
    @GenerateNodeFactory
    public abstract static class FZlibAvailableP extends ELispBuiltInBaseNode {
        @Specialization
        public static boolean zlibAvailableP() {
            return true;
        }
    }

    /**
     * <pre>
     * Decompress a gzip- or zlib-compressed region.
     * Replace the text in the region by the decompressed data.
     *
     * If optional parameter ALLOW-PARTIAL is nil or omitted, then on
     * failure, return nil and leave the data in place.  Otherwise, return
     * the number of bytes that were not decompressed and replace the region
     * text by whatever data was successfully decompressed (similar to gzip).
     * If decompression is completely successful return t.
     *
     * This function can be called only in unibyte buffers.
     * </pre>
     */
    @ELispBuiltIn(name = "zlib-decompress-region", minArgs = 2, maxArgs = 3)
    @GenerateNodeFactory
    public abstract static class FZlibDecompressRegion extends ELispBuiltInBaseNode {
        @TruffleBoundary
        @Specialization
        public boolean zlibDecompressRegion(long start, long end, Object allowPartial) {
            ELispBuffer buffer = getContext().currentBuffer();
            if (!isNil(buffer.getEnableMultibyteCharacters())) {
                throw ELispSignals.error("This function can be called only in unibyte buffers");
            }
            ELispString s = buffer.subString(start, end);
            NodeIterator iterator = buffer.iterator(start, end);
            byte[] bytes;
            try (GZIPInputStream input = new GZIPInputStream(new InputStream() {
                @Override
                public int read() {
                    return iterator.nextInt();
                }
            })) {
                bytes = input.readAllBytes();
            } catch (IOException e) {
                throw ELispSignals.reportFileError(e, buffer);
            }
            ELispString insertion = ELispString.ofBytes(bytes);
            buffer.delete(start, end - start);
            buffer.insert(start, insertion);
            return true;
        }
    }
}
