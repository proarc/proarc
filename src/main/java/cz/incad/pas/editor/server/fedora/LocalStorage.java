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

import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.StateType;
import com.yourmediashelf.fedora.generated.foxml.XmlContentType;
import cz.incad.pas.editor.server.fedora.FoxmlUtils.ControlGroup;
import cz.incad.pas.editor.server.fedora.XmlStreamEditor.EditorResult;
import java.io.File;
import java.util.ConcurrentModificationException;
import java.util.List;
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

        private final LocalObject object;
        private final String dsId;
        private final String formatUri;
        private final String defaultLabel;
        private long lastModified;

        public LocalXmlStreamEditor(LocalObject object, String dsId, String formatUri, String label) {
            this.object = object;
            this.dsId = dsId;
            this.formatUri = formatUri;
            this.defaultLabel = label;
        }

        @Override
        public Source read() {
            // find version
            DatastreamVersionType version = FoxmlUtils.findDataStreamVersion(object.getDigitalObject(), dsId);
            lastModified = getLastModified(version);
            if (version != null) {
                XmlContentType xmlContent = version.getXmlContent();
                if (xmlContent != null) {
                    Element elm = xmlContent.getAny().get(0);
                    return new DOMSource(elm);
                }
            }
            return null;
        }

        @Override
        public long getLastModified() {
            DatastreamVersionType version = FoxmlUtils.findDataStreamVersion(object.getDigitalObject(), dsId);
            lastModified = getLastModified(version);
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
        public void write(EditorResult data, long timestamp) {
            if (!(data instanceof DOMResult)) {
                throw new IllegalArgumentException("Unsupported data: " + data);
            }
            DOMResult result = (DOMResult) data;
            Node root = result.getNode();
            Document doc = root.getOwnerDocument() == null ? (Document) root : root.getOwnerDocument();
            writeXmlContent(doc.getDocumentElement(), timestamp);
            object.register(this);
        }
        
        @Override
        public EditorResult createResult() {
            return new EditorDomResult();
        }

        @Override
        public void flush() {
            // no op
        }

        private void writeXmlContent(Element mods, long timestamp) {
            DatastreamVersionType version = FoxmlUtils.findDataStreamVersion(object.getDigitalObject(), dsId);
            if (version == null /* || versionable */) {
                version = FoxmlUtils.createDataStreamVersion(
                        object.getDigitalObject(), dsId, ControlGroup.INLINE, false, StateType.A);
                XmlContentType xmlContent = new XmlContentType();
                version.setXmlContent(xmlContent);
                version.setMIMETYPE(MediaType.TEXT_XML);
                version.setFORMATURI(formatUri);
                version.setLABEL(defaultLabel);
            } else if (timestamp != lastModified) {
                throw new ConcurrentModificationException(dsId);
            }

            List<Element> roots = version.getXmlContent().getAny();
            version.setCREATED(FoxmlUtils.createXmlDate());
            this.lastModified = getLastModified(version);
            roots.clear();
            roots.add(mods);
        }
    }

    private static final class EditorDomResult extends DOMResult implements EditorResult {
    }

}
