/*
 * Copyright (C) 2021 Lukas Sykora
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

package cz.cas.lib.proarc.z3950;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import org.marc4j.MarcException;
import org.marc4j.MarcReader;
import org.marc4j.marc.ControlField;
import org.marc4j.marc.DataField;
import org.marc4j.marc.Leader;
import org.marc4j.marc.MarcFactory;
import org.marc4j.marc.Record;
import org.marc4j.marc.Subfield;
import org.marc4j.marc.impl.Verifier;

public class MyMarcStreamReader implements MarcReader {
    private InputStream input;
    private Record record;
    private MarcFactory factory;
    private String encoding;
    private boolean override;
    private boolean hasNext;

    public MyMarcStreamReader(InputStream input) {
        this(input, (String) null);
    }

    public MyMarcStreamReader(InputStream input, String encoding) {
        this.input = null;
        this.encoding = "ISO8859_1";
        this.override = false;
        this.hasNext = true;
        this.input = input;
        this.factory = MarcFactory.newInstance();
        if (encoding != null) {
            this.encoding = encoding;
            this.override = true;
        }

    }

    public boolean hasNext() {
        try {
            return this.input.available() != 0;
        } catch (IOException var2) {
            throw new MarcException(var2.getMessage(), var2);
        }
    }

    public Record next() {
        this.record = this.factory.newRecord();

        try {
            byte[] byteArray = new byte[24];
            int bytesRead = this.input.read(byteArray);
            if (bytesRead == -1) {
                throw new MarcException("no data to read");
            }

            while (bytesRead != -1 && bytesRead != byteArray.length) {
                bytesRead += this.input.read(byteArray, bytesRead, byteArray.length - bytesRead);
            }

            Leader ldr;
            try {
                ldr = this.parseLeader(byteArray);
            } catch (IOException var15) {
                throw new MarcException("error parsing leader with data: " + new String(byteArray), var15);
            }

            switch (ldr.getCharCodingScheme()) {
                case ' ':
                    if (!this.override) {
                        this.encoding = "ISO8859_1";
                    }
                    break;
                case 'a':
                    if (!this.override) {
                        this.encoding = "UTF8";
                    }
            }

            this.record.setLeader(ldr);
            int directoryLength = ldr.getBaseAddressOfData() - 25;
            if (directoryLength % 12 != 0) {
                throw new MarcException("invalid directory");
            }

            int size = directoryLength / 12;
            String[] tags = new String[size];
            int[] lengths = new int[size];
            byte[] tag = new byte[3];
            byte[] length = new byte[4];
            byte[] start = new byte[5];

            int i;
            for (i = 0; i < size; ++i) {
                for (bytesRead = this.input.read(tag); bytesRead != -1 && bytesRead != tag.length; bytesRead += this.input.read(tag, bytesRead, tag.length - bytesRead)) {
                }

                String tmp = new String(tag);
                tags[i] = tmp;

                for (bytesRead = this.input.read(length); bytesRead != -1 && bytesRead != length.length; bytesRead += this.input.read(length, bytesRead, length.length - bytesRead)) {
                }

                tmp = new String(length);
                lengths[i] = Integer.parseInt(tmp);

                for (bytesRead = this.input.read(start); bytesRead != -1 && bytesRead != start.length; bytesRead += this.input.read(start, bytesRead, start.length - bytesRead)) {
                }
            }

            if (this.input.read() != 30) {
                throw new MarcException("expected field terminator at end of directory");
            }
            int r = 0;
            String message = "";

            for (i = 0; i < size; ++i) {
                if (!isNumeric(tags[i])) {
                    byteArray = new byte[lengths[i] - 1];

                    for (bytesRead = this.input.read(byteArray); bytesRead != -1 && bytesRead != byteArray.length; bytesRead += this.input.read(byteArray, bytesRead, byteArray.length - bytesRead)) {
                    }

                    if (this.input.read() != 30) {
                        throw new MarcException("expected field terminator at end of field");
                    }
                    continue;
                }
                try {
                    if (Verifier.isControlField(tags[i])) {
                        byteArray = new byte[lengths[i] - 1];

                        for (bytesRead = this.input.read(byteArray); bytesRead != -1 && bytesRead != byteArray.length; bytesRead += this.input.read(byteArray, bytesRead, byteArray.length - bytesRead)) {
                        }

                        if (this.input.read() != 30) {
                            throw new MarcException("expected field terminator at end of field");
                        }

                        ControlField field = this.factory.newControlField();
                        field.setTag(tags[i]);
                        field.setData(this.getDataAsString(byteArray));

                        if (isNumeric(field.getTag())) {
                            this.record.addVariableField(field);
                        }
                    } else {
                        byteArray = new byte[lengths[i]];

                        for (bytesRead = this.input.read(byteArray); bytesRead != -1 && bytesRead != byteArray.length; bytesRead += this.input.read(byteArray, bytesRead, byteArray.length - bytesRead)) {
                        }

                        try {
                            this.record.addVariableField(this.parseDataField(tags[i], byteArray));
                        } catch (IOException var14) {
                            throw new MarcException("error parsing data field for tag: " + tags[i] + " with data: " + new String(byteArray), var14);
                        }
                    }
                } catch (NumberFormatException ex) {
                    message += tags[i] + ", ";
                }
            }


            if (this.input.read() != 29 ) {
                throw new MarcException("expected record terminator");
            }

        } catch (IOException var16) {
            throw new MarcException("an error occured reading input", var16);
        }

        return this.record;
    }

    private boolean isNumeric(String value) {
        if (value == null) {
            return false;
        }
        try {
            double d = Double.parseDouble(value);
        } catch (NumberFormatException nfe) {
            return false;
        }
        return true;
    }

    private DataField parseDataField(String tag, byte[] field) throws IOException {
        ByteArrayInputStream bais = new ByteArrayInputStream(field);
        char ind1 = (char) bais.read();
        char ind2 = (char) bais.read();
        DataField dataField = this.factory.newDataField();
        dataField.setTag(tag);
        dataField.setIndicator1(ind1);
        dataField.setIndicator2(ind2);

        while (true) {
            int readByte = bais.read();
            if (readByte < 0) {
                return dataField;
            }

            switch (readByte) {
                case 30:
                default:
                    break;
                case 31:
                    int code = bais.read();
                    if (code < 0) {
                        throw new IOException("unexpected end of data field");
                    }

                    if (code != 30) {
                        int size = this.getSubfieldLength(bais);
                        byte[] data = new byte[size];
                        bais.read(data);
                        Subfield subfield = this.factory.newSubfield();
                        subfield.setCode((char) code);
                        subfield.setData(this.getDataAsString(data));
                        dataField.addSubfield(subfield);
                    }
            }
        }
    }

    private int getSubfieldLength(ByteArrayInputStream bais) throws IOException {
        bais.mark(9999);
        int bytesRead = 0;

        while (true) {
            switch (bais.read()) {
                case -1:
                    bais.reset();
                    throw new IOException("subfield not terminated");
                case 30:
                case 31:
                    bais.reset();
                    return bytesRead;
                default:
                    ++bytesRead;
            }
        }
    }

    private Leader parseLeader(byte[] leaderData) throws IOException {
        InputStreamReader isr = new InputStreamReader(new ByteArrayInputStream(leaderData));
        Leader ldr = this.factory.newLeader();
        char[] tmp = new char[5];
        isr.read(tmp);

        try {
            ldr.setRecordLength(Integer.parseInt(new String(tmp)));
        } catch (NumberFormatException var9) {
            throw new MarcException("unable to parse record length", var9);
        }

        ldr.setRecordStatus((char) isr.read());
        ldr.setTypeOfRecord((char) isr.read());
        tmp = new char[2];
        isr.read(tmp);
        ldr.setImplDefined1(tmp);
        ldr.setCharCodingScheme((char) isr.read());

        try {
            ldr.setIndicatorCount(Integer.parseInt(String.valueOf((char) isr.read())));
        } catch (NumberFormatException var8) {
            throw new MarcException("unable to parse indicator count", var8);
        }

        try {
            ldr.setSubfieldCodeLength(Integer.parseInt(String.valueOf((char) isr.read())));
        } catch (NumberFormatException var7) {
            throw new MarcException("unable to parse subfield code length", var7);
        }

        tmp = new char[5];
        isr.read(tmp);

        try {
            ldr.setBaseAddressOfData(Integer.parseInt(new String(tmp)));
        } catch (NumberFormatException var6) {
            throw new MarcException("unable to parse base address of data", var6);
        }

        tmp = new char[3];
        isr.read(tmp);
        ldr.setImplDefined2(tmp);
        tmp = new char[4];
        isr.read(tmp);
        ldr.setEntryMap(tmp);
        isr.close();
        return ldr;
    }

    private String getDataAsString(byte[] bytes) {
        String dataElement = null;
        if (this.encoding != null) {
            try {
                dataElement = new String(bytes, this.encoding);
            } catch (UnsupportedEncodingException var4) {
                throw new MarcException("unsupported encoding", var4);
            }
        } else {
            dataElement = new String(bytes);
        }

        return dataElement;
    }
}
