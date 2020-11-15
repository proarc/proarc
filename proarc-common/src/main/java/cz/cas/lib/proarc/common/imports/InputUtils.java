/*
 * Copyright (C) 2014 Jan Pokorsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.imports;

import org.apache.commons.io.IOUtils;
import javax.xml.bind.DatatypeConverter;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Helper class to check imported data.
 *
 * @author Jan Pokorsky
 */
public class InputUtils {

    public static final byte[] JP2000_MAGIC_NUMBER =
            DatatypeConverter.parseHexBinary("0000000C6A5020200D0A870A");

    public static final byte[] JPEG_MAGIC_NUMBER =
            DatatypeConverter.parseHexBinary("FFD8FF");

    public static final byte[] PDF_MAGIC_NUMBER = "%PDF".getBytes();

    public static final byte[] TIFF_BE_MAGIC_NUMBER =
            DatatypeConverter.parseHexBinary("4D4D002A");

    public static final byte[] TIFF_LE_MAGIC_NUMBER =
            DatatypeConverter.parseHexBinary("49492A00");

    public static final byte[] WAVE_MAGIC_NUMBER =
            DatatypeConverter.parseHexBinary("52494646");

    public static final byte[] FLAC_MAGIC_NUMBER =
            DatatypeConverter.parseHexBinary("664C6143");

    public static final byte[] MP3_ID3V1_MAGIC_NUMBER =
            DatatypeConverter.parseHexBinary("FFFB");

    public static final byte[] MP3_ID3V2_MAGIC_NUMBER =
            DatatypeConverter.parseHexBinary("494433");

    public static final byte[] OGG_MAGIC_NUMBER =
            DatatypeConverter.parseHexBinary("4F676753");

    private static final Logger LOG = Logger.getLogger(InputUtils.class.getName());


    public static boolean isJp2000(File f) throws IOException {
        return hasMagicNumber(f, JP2000_MAGIC_NUMBER);
    }

    public static boolean isJpeg(File f) throws IOException {
        return hasMagicNumber(f, JPEG_MAGIC_NUMBER);
    }

    public static boolean isPdf(File f) throws IOException {
        return hasMagicNumber(f, PDF_MAGIC_NUMBER);
    }

    public static boolean isTiff(File f) throws IOException {
        return hasMagicNumber(f, TIFF_LE_MAGIC_NUMBER, TIFF_BE_MAGIC_NUMBER);
    }

    public static boolean isWave(File f) throws IOException {
        return hasMagicNumber(f, WAVE_MAGIC_NUMBER);
    }

    public static boolean isFlac(File f) throws IOException {
        return hasMagicNumber(f, FLAC_MAGIC_NUMBER);
    }

    public static boolean isMp3(File f) throws IOException {
        return hasMagicNumber(f, MP3_ID3V1_MAGIC_NUMBER, MP3_ID3V2_MAGIC_NUMBER);
    }

    public static boolean isOgg(File f) throws  IOException {
        return hasMagicNumber(f, OGG_MAGIC_NUMBER);
    }

    /**
     * Checks a file for magic numbers.
     * @param f file to check
     * @param magics list of magic numbers
     * @return {@code true} if the file starts with some of magic numbers.
     * @throws IOException file access failure
     */
    public static boolean hasMagicNumber(File f, byte[]... magics) throws IOException {
        int maxlength = 0;
        int minlength = 0;
        for (byte[] magic : magics) {
            maxlength = Math.max(maxlength, magic.length);
            minlength = Math.min(minlength, magic.length);
        }
        FileInputStream fis = new FileInputStream(f);
        try {
            byte[] buf = new byte[maxlength];
            int length = fis.read(buf);
            if (length < minlength) {
                return false;
            }
            for (byte[] magic : magics) {
                if (startWith(buf, length, magic)) {
                    return true;
                }
            }
            return false;
        } finally {
            IOUtils.closeQuietly(fis);
        }
    }

    private static boolean startWith(byte[] src, int srcLength, byte[] subarray) {
        if (subarray.length > srcLength ) {
            return false;
        }
        StringBuilder s1 = new StringBuilder();
        StringBuilder s2 = new StringBuilder();
        for (int i = 0; i < subarray.length; i++) {
            s1.append(src[i]);
            s2.append(subarray[i]);
            if (src[i] != subarray[i]) {
                if (LOG.isLoggable(Level.FINE)) {
                    LOG.log(Level.FINE, "\nsrc: {0}\nsub: {1}", new Object[]{
                        DatatypeConverter.printHexBinary(src),
                        DatatypeConverter.printHexBinary(subarray)});
                }
                return false;
            }
        }
        return true;
    }

}
