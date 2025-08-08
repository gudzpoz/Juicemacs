package party.iroiro.juicemacs.elisp.forms;

import party.iroiro.juicemacs.elisp.ELispLanguage;
import party.iroiro.juicemacs.elisp.runtime.internal.ELispFrame;
import party.iroiro.juicemacs.elisp.runtime.objects.ELispBuffer;
import party.iroiro.juicemacs.elisp.runtime.string.ELispString;
import party.iroiro.juicemacs.elisp.runtime.scopes.ValueStorage;

import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asBuffer;
import static party.iroiro.juicemacs.elisp.runtime.ELispTypeSystem.asFrame;

public abstract class ELispBuiltInUtils {
    public static ELispBuffer currentBuffer() {
        return asBuffer(ELispLanguage.get(null).currentBuffer().getValue());
    }

    public static ELispFrame currentFrame() {
        return asFrame(ELispLanguage.get(null).currentFrame().getValue());
    }

    public static CurrentBufferScope withCurrentBuffer(ELispBuffer buffer) {
        ValueStorage.Forwarded storage = ELispLanguage.get(null).currentBuffer();
        Object prev = storage.getValue();
        storage.setValue(buffer);
        return new CurrentBufferScope(buffer, storage, prev);
    }

    public static CurrentBufferScope withInternalBufferReset(String string) {
        ValueStorage.Forwarded storage = ELispLanguage.get(null).currentBuffer();
        ELispBuffer prev = asBuffer(storage.getValue());

        ELispString name = new ELispString(string);
        ELispBuffer internal = BuiltInBuffer.FGetBufferCreate.getBufferCreate(name, true);
        internal.setDirectory(prev.getDirectory());
        internal.setReadOnly(false);
        internal.setFilename(false);
        internal.setUndoList(true);
        // TODO: Delete overlays
        // TODO: set_buffer_internal
        storage.setValue(internal);
        BuiltInBuffer.FEraseBuffer.eraseBuffer();
        return new CurrentBufferScope(internal, storage, prev);
    }

    public record CurrentBufferScope(ELispBuffer current, ValueStorage.Forwarded storage, Object prev)
            implements AutoCloseable {
        @Override
        public void close() {
            storage.setValue(prev);
        }
    }
}
