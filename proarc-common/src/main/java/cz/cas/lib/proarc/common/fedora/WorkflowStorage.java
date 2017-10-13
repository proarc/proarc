/*
 * Copyright (C) 2017 Martin Rumanek
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

package cz.cas.lib.proarc.common.fedora;

import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.workflow.WorkflowManager;
import cz.cas.lib.proarc.common.workflow.model.MaterialFilter;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.MaterialView;

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import java.io.InputStream;
import java.io.StringReader;
import java.math.BigDecimal;
import java.net.URI;
import java.util.List;
import java.util.Locale;

/**
 * Workflow storage
 *
 * It contains objects with metadata BIBILIO MODS.
 * These objects are not stored in the Fedora Repository and are only a small subset of FOXML objects.
 */
public class WorkflowStorage {

    public FedoraObject load(BigDecimal workflowJobId, String modelId, Locale locale) {
        return new WorkflowObject(workflowJobId, locale, modelId);
    }

    public static final class WorkflowObject implements FedoraObject {

        private final String modelId;
        private final BigDecimal workflowJobId;
        private final Locale locale;

        WorkflowObject(BigDecimal workflowJobId, Locale locale, String modelId) {
            this.modelId = modelId;
            this.workflowJobId = workflowJobId;
            this.locale = locale;
        }

        @Override
        public String getPid() {
            return null;
        }

        public BigDecimal getWorkflowJobId() {
            return workflowJobId;
        }

        @Override
        public List<DatastreamProfile> getStreamProfile(String dsId) {
            throw new UnsupportedOperationException(dsId);
        }

        @Override
        public XmlStreamEditor getEditor(DatastreamProfile datastream) {
            return new WorkflowXmlStreamEditor(this, datastream);
        }

        @Override
        public void register(XmlStreamEditor editor) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void setLabel(String label) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void flush() {
            throw new UnsupportedOperationException();
        }

        @Override
        public String asText() {
            return null;
        }

        public String getModel() {
            return modelId;
        }

        public Locale getLocale() {
            return locale;
        }

        @Override
        public void purgeDatastream(String datastream, String logMessage) {
            throw new UnsupportedOperationException(datastream);
        }
    }

    public static final class WorkflowXmlStreamEditor implements XmlStreamEditor {

        private final WorkflowObject workflowObject;

        public WorkflowXmlStreamEditor(WorkflowObject workflowObject, DatastreamProfile datastream) {
            if (ModsStreamEditor.DATASTREAM_ID.equals(datastream.getDsID())) {
               this.workflowObject = workflowObject;
            } else {
                throw new UnsupportedOperationException(datastream.getDsID());
            }
        }

        @Override
        public EditorResult createResult() {
            throw new UnsupportedOperationException();
        }

        @Override
        public long getLastModified() {
            return 0;
        }

        @Override
        public DatastreamProfile getProfile() {
            throw new UnsupportedOperationException();
        }

        @Override
        public void setProfile(DatastreamProfile profile) {
            throw new UnsupportedOperationException();
        }

        @Override
        public Source read() throws DigitalObjectException {
            MaterialFilter filter = new MaterialFilter();
            filter.setLocale(workflowObject.getLocale());
            filter.setType(MaterialType.PHYSICAL_DOCUMENT);
            filter.setJobId(workflowObject.getWorkflowJobId());
            WorkflowManager workflowManager = WorkflowManager.getInstance();
            List<MaterialView> mvs = workflowManager.findMaterial(filter);

            if (mvs == null || mvs.isEmpty()) {
                throw new DigitalObjectNotFoundException(workflowObject.getWorkflowJobId().toString());
            } else {
               return new StreamSource(new StringReader(mvs.get(0).getMetadata()));
            }
        }

        @Override
        public InputStream readStream() {
            throw new UnsupportedOperationException();
        }

        @Override
        public void write(EditorResult data, long timestamp, String message) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void write(byte[] data, long timestamp, String message) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void write(URI data, long timestamp, String message) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void write(InputStream data, long timestamp, String message) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void flush() {
            throw new UnsupportedOperationException();
        }
    }
}
