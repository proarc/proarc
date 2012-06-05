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
package cz.incad.pas.editor.server.fedora;

import com.yourmediashelf.fedora.generated.foxml.ContentLocationType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.PropertyType;
import com.yourmediashelf.fedora.generated.foxml.StateType;
import com.yourmediashelf.fedora.generated.foxml.XmlContentType;
import cz.incad.pas.editor.server.fedora.FoxmlUtils.ControlGroup;
import cz.incad.pas.editor.server.fedora.XmlStreamEditor.EditorResult;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.net.URI;
import java.util.ConcurrentModificationException;
import java.util.EnumSet;
import java.util.Set;
import java.util.logging.Logger;
import javax.ws.rs.core.MediaType;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * Local storage supporting FOXML 1.1.
 * It contains newly imported but not yet ingested files.
 *
 * @author Jan Pokorsky
 */
public final class LocalStorage {

    private static final Logger LOG = Logger.getLogger(LocalStorage.class.getName());

    public LocalObject load(String pid, File foxml) {
        DigitalObject dobj = FoxmlUtils.unmarshal(new StreamSource(foxml), DigitalObject.class);
        LocalObject result = new LocalObject(pid, foxml, dobj);
        return result;
    }

    public LocalObject create() {
        return create(null);
    }

    public LocalObject create(File foxml) {
        String pid = FoxmlUtils.createPid();
        DigitalObject dobj = FoxmlUtils.createFoxml(pid);
        return new LocalObject(pid, foxml, dobj);
    }

    public static final class LocalObject extends AbstractFedoraObject {

        private DigitalObject dobj;
        /** {@code null} for in memory object. */
        private File foxml;

        LocalObject(String pid, File foxml, DigitalObject dobj) {
            super(pid);
            this.foxml = foxml;
            if (dobj == null) {
                throw new NullPointerException("dobj");
            }
            this.dobj = dobj;
        }

        public String getOwner() {
            PropertyType p = FoxmlUtils.findProperty(dobj, FoxmlUtils.PROPERTY_OWNER);
            return p == null ? null : p.getVALUE();
        }

        @Override
        public void setLabel(String label) {
            FoxmlUtils.setProperty(dobj, FoxmlUtils.PROPERTY_LABEL, label);
        }

        public void setOwner(String owner) {
            FoxmlUtils.setProperty(dobj, FoxmlUtils.PROPERTY_OWNER, owner);
        }

        @Override
        public void flush() {
            super.flush();
            if (foxml != null) {
                FoxmlUtils.marshal(new StreamResult(foxml), dobj, true);
            }
        }

        public File getFoxml() {
            return foxml;
        }

        public DigitalObject getDigitalObject() {
            return dobj;
        }

    }

    public static final class LocalXmlStreamEditor implements XmlStreamEditor {

        private static final Set<ControlGroup> SUPPORTED_CONTROL_GROUPS =
                EnumSet.of(ControlGroup.INLINE, ControlGroup.MANAGED);

        private final LocalObject object;
        private final String dsId;
        private final String formatUri;
        private final String defaultLabel;
        private final boolean isXml;
        private final MediaType defaultMimetype;
        private String realMimetype;
        private final ControlGroup control;
        private boolean storeExternally;

        /** Use for binary content */
        public LocalXmlStreamEditor(LocalObject object, String dsId,
                MediaType mimetype, String label, ControlGroup control) {

            this(object, dsId, null, label, mimetype, control, false);
        }
        /** Use for binary content referenced externally for ingest purposes */
        public LocalXmlStreamEditor(LocalObject object, String dsId,
                MediaType mimetype, String label, ControlGroup control, boolean storeExternally) {

            this(object, dsId, null, label, mimetype, control, storeExternally);
        }
        
        /** Use for XML content */
        public LocalXmlStreamEditor(LocalObject object, String dsId, String formatUri, String label) {
            this(object, dsId, formatUri, label, MediaType.TEXT_XML_TYPE, ControlGroup.INLINE, false);
        }

        private LocalXmlStreamEditor(
                LocalObject object, String dsId, String formatUri, String label,
                MediaType mimetype, ControlGroup control, boolean storeExternally) {

            this.object = object;
            this.dsId = dsId;
            this.formatUri = formatUri;
            this.defaultLabel = label;
            this.isXml = mimetype == MediaType.TEXT_XML_TYPE;
            this.defaultMimetype = mimetype;
            this.control = control;
            if (!SUPPORTED_CONTROL_GROUPS.contains(control)) {
                throw new IllegalArgumentException("Unsupported control group: " + control);
            }
            this.storeExternally = storeExternally;
        }

        @Override
        public Source read() {
            // find version
            DatastreamVersionType version = FoxmlUtils.findDataStreamVersion(object.getDigitalObject(), dsId);
            if (version != null) {
                realMimetype = version.getMIMETYPE();
            }
            return createSource(version);
        }

        private Source createSource(DatastreamVersionType version) {
            if (version == null) {
                return null;
            }
            if (isXml) {
                XmlContentType xmlContent = version.getXmlContent();
                if (xmlContent != null) {
                    Element elm = xmlContent.getAny().get(0);
                    return new DOMSource(elm);
                }
            } else {
                byte[] binaryContent = version.getBinaryContent();
                ContentLocationType contentLocation = version.getContentLocation();
                if (binaryContent != null) {
                    return new StreamSource(new ByteArrayInputStream(binaryContent));
                } else if (contentLocation != null) {
                    String ref = contentLocation.getREF();
                    if (ref != null) {
                        URI refUri = URI.create(ref);
                        return new StreamSource(new File(refUri));
                    }
                }
            }
            return null;
        }

        @Override
        public long getLastModified() {
            DatastreamVersionType version = FoxmlUtils.findDataStreamVersion(object.getDigitalObject(), dsId);
            long lastModified = getLastModified(version);
            return lastModified;
        }

        private long getLastModified(DatastreamVersionType version) {
            long last = Long.MIN_VALUE;
            if (version != null) {
                last = version.getCREATED().toGregorianCalendar().getTimeInMillis();
            }
            return last;
        }

        @Override
        public String getMimetype() {
            if (realMimetype == null) {
                DatastreamVersionType version = FoxmlUtils.findDataStreamVersion(object.getDigitalObject(), dsId);
                realMimetype = version.getMIMETYPE();
            }
            if (realMimetype == null) {
                realMimetype = defaultMimetype.toString();
            }
            return realMimetype;
        }

        @Override
        public void write(EditorResult data, long timestamp) {
            DatastreamVersionType version = FoxmlUtils.findDataStreamVersion(object.getDigitalObject(), dsId);
            if (version == null) {
                version = FoxmlUtils.createDataStreamVersion(
                        object.getDigitalObject(), dsId, control, false, StateType.A);
                version.setMIMETYPE(getMimetype());
                version.setLABEL(defaultLabel);
            } else if (timestamp != getLastModified(version)) {
                throw new ConcurrentModificationException(dsId);
            }

            if (data instanceof EditorBinaryResult) {
                writeBinaryData(version, (EditorBinaryResult) data);
            } else if (data instanceof EditorDomResult) {
                writeXmlContent(version, (EditorDomResult) data);
            } else {
                throw new IllegalArgumentException("Unsupported data: " + data);
            }

            version.setCREATED(FoxmlUtils.createXmlDate());
            object.register(this);
        }
        
        @Override
        public EditorResult createResult() {
            return isXml ? new EditorDomResult() : new EditorBinaryResult();
        }

        @Override
        public void flush() {
            // no op
        }

        private void writeBinaryData(DatastreamVersionType version, EditorBinaryResult data) {
            storeExternally |= version.getContentLocation() != null;
            if (storeExternally) {
                String systemId = data.getSystemId();
                if (systemId == null) {
                    throw new IllegalStateException("Missing systemId of external resource. " + toString());
                }
                ContentLocationType contentLocation = new ContentLocationType();
                contentLocation.setREF(systemId);
                contentLocation.setTYPE("URL");
                version.setContentLocation(contentLocation);
            } else {
                version.setBinaryContent(data.asBytes());
            }
        }

        private void writeXmlContent(DatastreamVersionType version, EditorDomResult data) {
            XmlContentType xmlContent = new XmlContentType();
            Node root = data.getNode();
            Document doc = root.getOwnerDocument() == null ? (Document) root : root.getOwnerDocument();
            xmlContent.getAny().add(doc.getDocumentElement());
            version.setXmlContent(xmlContent);
            version.setFORMATURI(formatUri);
        }

        @Override
        public String toString() {
            return String.format("%s{pid=%s, dsId=%s, mimetype=%s, controlGroup=%s,\nfoxml=%s}",
                    getClass().getSimpleName(), object.getPid(), dsId, getMimetype(), control, object.getFoxml());
        }
    }

    private static final class EditorDomResult extends DOMResult implements EditorResult {
    }

    private static final class EditorBinaryResult extends StreamResult implements EditorResult {

        public EditorBinaryResult() {
            super(new ByteArrayOutputStream());
        }

        public byte[] asBytes() {
            return ((ByteArrayOutputStream) getOutputStream()).toByteArray();
        }

    }

}
