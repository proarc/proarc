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
package cz.cas.lib.proarc.common.dublincore;

import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor.EditorResult;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.oaidublincore.DcConstants;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.ObjectFactory;
import javax.xml.bind.JAXBException;
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
    public static final String DATASTREAM_FORMAT_URI = DcConstants.NS_OAIDC;
    public static final String DATASTREAM_LABEL = "Dublin Core Record for this object";

    private final XmlStreamEditor editor;
    private final FedoraObject object;

    public static DatastreamProfile dcProfile() {
        return FoxmlUtils.inlineProfile(DATASTREAM_ID, DATASTREAM_FORMAT_URI, DATASTREAM_LABEL);
    }

    public DcStreamEditor(FedoraObject object) {
        this(object.getEditor(dcProfile()), object);
    }

    DcStreamEditor(XmlStreamEditor editor, FedoraObject object) {
        this.editor = editor;
        this.object = object;
    }

    public long getLastModified() throws DigitalObjectException {
        return editor.getLastModified();
    }

    public DublinCoreRecord read() throws DigitalObjectException {
        Source src = editor.read();
        OaiDcType dc;
        if (src != null) {
            dc = DcUtils.unmarshal(src, OaiDcType.class);
        } else {
            dc = new OaiDcType();
        }
        return new DublinCoreRecord(dc, editor.getLastModified(), object.getPid());
    }

    public void write(DublinCoreRecord record, String message) throws DigitalObjectException {
        EditorResult result = editor.createResult();
        // DO NOT include schemaLocation. Fedora validator does not accept it.
        DcUtils.marshal(result, record.getDc(), false);
        editor.write(result, record.getTimestamp(), message);
    }

    public void write(ModsType mods, String model, long timestamp, String message) throws DigitalObjectException {
        try {
            JAXBSource jaxbSource = new JAXBSource(ModsUtils.defaultMarshaller(false),
                    new ObjectFactory().createMods(mods));
            // DO NOT include schemaLocation. Fedora validator does not accept it.
            Transformer t = DcUtils.modsTransformer(model);
            EditorResult result = editor.createResult();
            t.transform(jaxbSource, result);
            editor.write(result, timestamp, message);
        } catch (TransformerException ex) {
            throw new DigitalObjectException(object.getPid(), ex);
        } catch (JAXBException ex) {
            throw new DigitalObjectException(object.getPid(), ex);
        }
    }

    public static class DublinCoreRecord {

        private String pid;
        private Integer batchId;
        /** last modification of the DC content*/
        private long timestamp;
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
