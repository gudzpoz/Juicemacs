package party.iroiro.juicemacs.mule;

import java.io.IOException;

public interface CodedStringWriter {
    void writeUtf8(byte[] bytes, int from, int to) throws IOException;
    void writeRawByte(byte b) throws IOException;
}
