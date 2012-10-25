/*
 * Copyright (C) 2012 Jan Pokorsky
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.server.dublincore;

import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.ObjectFactory;
import cz.incad.pas.editor.server.fedora.FedoraObject;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalObject;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalXmlStreamEditor;
import cz.incad.pas.editor.server.fedora.RemoteStorage.RemoteObject;
import cz.incad.pas.editor.server.fedora.RemoteStorage.RemoteXmlStreamEditor;
import cz.incad.pas.editor.server.fedora.XmlStreamEditor;
import cz.incad.pas.editor.server.fedora.XmlStreamEditor.EditorResult;
import cz.incad.pas.editor.server.mods.ModsUtils;
import cz.incad.pas.oaidublincore.OaiDcType;
import java.io.IOException;
import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.util.JAXBSource;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;

/**
 * Dublin Core editor implements I/O over Fedora data stream.
 *
 * @author Jan Pokorsky
 */
public final class DcStreamEditor {

    public static final String DATASTREAM_ID = "DC";
    public static final String DATASTREAM_FORMAT_URI = DcUtils.OAI_DC_NAMESPACE;
    public static final String DATASTREAM_LABEL = "Dublin Core Record for this object";

    private final XmlStreamEditor editor;
    private final FedoraObject object;

    private static XmlStreamEditor createEditor(FedoraObject object) {
        XmlStreamEditor editor;
        if (object instanceof LocalObject) {
            editor = new LocalXmlStreamEditor((LocalObject) object, DATASTREAM_ID, DATASTREAM_FORMAT_URI, DATASTREAM_LABEL);
        } else if (object instanceof RemoteObject) {
            editor = new RemoteXmlStreamEditor((RemoteObject) object, DATASTREAM_ID);
        } else {
            throw new IllegalArgumentException("Unsupported fedora object: " + object.getClass());
        }
        return editor;
    }
    public DcStreamEditor(FedoraObject object) {
        this(createEditor(object), object);
    }
//
//    public DcStreamEditor(RemoteObject object) {
//        this(new RemoteXmlStreamEditor(object, DATASTREAM_ID), object);
//    }
//
//    public DcStreamEditor(LocalObject object) {
//        this(new LocalXmlStreamEditor(object, DATASTREAM_ID, DATASTREAM_FORMAT_URI, DATASTREAM_LABEL),
//                object);
//    }

    DcStreamEditor(XmlStreamEditor editor, FedoraObject object) {
        this.editor = editor;
        this.object = object;
    }

    public long getLastModified() {
        return editor.getLastModified();
    }

    public DublinCoreRecord read() {
        Source src = editor.read();
        if (src == null) {
            return null;
        }
        return new DublinCoreRecord(DcUtils.unmarshal(src, OaiDcType.class),
                editor.getLastModified(), object.getPid());
    }

    public void write(DublinCoreRecord record) {
        EditorResult result = editor.createResult();
        // DO NOT include schemaLocation. Fedora validator does not accept it.
        DcUtils.marshal(result, record.getDc(), false);
        editor.write(result, record.getTimestamp());
    }

    public void write(ModsType mods, String model, long timestamp) throws IOException {
        try {
            JAXBSource jaxbSource = new JAXBSource(ModsUtils.defaultMarshaller(false),
                    new ObjectFactory().createMods(mods));
            // DO NOT include schemaLocation. Fedora validator does not accept it.
            Transformer t = DcUtils.modsTransformer(model);
            EditorResult result = editor.createResult();
            t.transform(jaxbSource, result);
            editor.write(result, timestamp);
        } catch (TransformerException ex) {
            throw new IOException(ex);
        } catch (JAXBException ex) {
            throw new IOException(ex);
        }
    }

    @XmlRootElement(name="dcRecord", namespace="http://www.incad.cz/pas/editor/dor/")
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(namespace="http://www.incad.cz/pas/editor/dor/")
    public static class DublinCoreRecord {

        @XmlElement(name="pid", namespace="http://www.incad.cz/pas/editor/dor/")
        private String pid;

        @XmlElement(name="batchId", namespace="http://www.incad.cz/pas/editor/dor/", nillable=true)
        private Integer batchId;

        /** last modification of the DC content*/
        @XmlElement(name="timestamp", namespace="http://www.incad.cz/pas/editor/dor/")
        private long timestamp;

        @XmlElement(namespace = "http://www.openarchives.org/OAI/2.0/oai_dc/", name = "dc", required = true)
        private OaiDcType dc;

        public DublinCoreRecord() {
        }

        public DublinCoreRecord(OaiDcType dc, long timestamp, String pid) {
            this.dc = dc;
            this.timestamp = timestamp;
            this.pid = pid;
        }

        public Integer getBatchId() {
            return batchId;
        }

        public void setBatchId(Integer batchId) {
            this.batchId = batchId;
        }

        public OaiDcType getDc() {
            return dc;
        }

        public void setDc(OaiDcType dc) {
            this.dc = dc;
        }

        public String getPid() {
            return pid;
        }

        public void setPid(String pid) {
            this.pid = pid;
        }

        public long getTimestamp() {
            return timestamp;
        }

        public void setTimestamp(long timestamp) {
            this.timestamp = timestamp;
        }

    }

}
