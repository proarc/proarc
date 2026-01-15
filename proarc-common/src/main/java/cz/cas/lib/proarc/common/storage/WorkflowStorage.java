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

package cz.cas.lib.proarc.common.storage;

import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.workflow.WorkflowException;
import cz.cas.lib.proarc.common.workflow.WorkflowManager;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.JobFilter;
import cz.cas.lib.proarc.common.workflow.model.JobView;
import cz.cas.lib.proarc.common.workflow.model.MaterialFilter;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.MaterialView;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.StringReader;
import java.math.BigDecimal;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Set;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

/**
 * Workflow storage
 * <p>
 * It contains objects with metadata BIBILIO MODS.
 * These objects are not stored in the Fedora Repository and are only a small subset of FOXML objects.
 *
 * @author Martin Rumanek
 */
public class WorkflowStorage {

    public ProArcObject load(BigDecimal workflowJobId, String modelId, Locale locale) {
        return new WorkflowObject(workflowJobId, locale, modelId);
    }

    public static final class WorkflowObject implements ProArcObject {

        private String modelId;
        private final BigDecimal workflowJobId;
        private final Locale locale;
        private String label;
        private String owner;
        private boolean indexHierarchical;

        private final Set<XmlStreamEditor> editors = new LinkedHashSet<>();

        WorkflowObject(BigDecimal workflowJobId, Locale locale, String modelId) {
            this.modelId = modelId;
            this.workflowJobId = workflowJobId;
            this.locale = locale;
            this.indexHierarchical = true;
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
            editors.add(editor);
        }

        @Override
        public void setLabel(String label) {
            this.label = label;
        }

        @Override
        public void setModel(String modelId) {
            this.modelId = modelId;
        }

        @Override
        public void setOwner(String owner) {
            this.owner = owner;
        }

        @Override
        public void flush() throws DigitalObjectException {
            for (XmlStreamEditor editor : editors) {
                editor.flush();
            }
        }

        @Override
        public void indexHierarchical(boolean indexHierarchical) {
            this.indexHierarchical = indexHierarchical;
        }

        @Override
        public String asText() {
            return null;
        }

        public String getModel() {
            return modelId;
        }

        public String getLabel() {
            return label;
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
        private final String dsId;
        private byte[] data;

        public WorkflowXmlStreamEditor(WorkflowObject workflowObject, DatastreamProfile datastream) {
            dsId = datastream.getDsID();

            switch (dsId) {
                case ModsStreamEditor.DATASTREAM_ID:
                case DcStreamEditor.DATASTREAM_ID:
                    this.workflowObject = workflowObject;
                    break;
                case RelationEditor.DATASTREAM_ID:
                    this.workflowObject = null;
                    break;
                default:
                    throw new UnsupportedOperationException(dsId);
            }
        }

        @Override
        public EditorResult createResult() {
            return new EditorStreamResult();
        }

        @Override
        public long getLastModified() {
            Objects.requireNonNull(workflowObject);

            JobFilter jobFilter = new JobFilter();
            jobFilter.setLocale(workflowObject.getLocale());
            jobFilter.setId(workflowObject.getWorkflowJobId());
            return WorkflowManager.getInstance().findJob(jobFilter)
                    .stream().map(JobView::getTimestampAsLong).findFirst().orElse(Long.MIN_VALUE);
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
            if (workflowObject == null) {
                return null;
            }
            MaterialFilter filter = new MaterialFilter();
            filter.setLocale(workflowObject.getLocale());
            filter.setType(MaterialType.PHYSICAL_DOCUMENT);
            filter.setJobId(workflowObject.getWorkflowJobId());
            WorkflowManager workflowManager = WorkflowManager.getInstance();
            List<MaterialView> mvs = workflowManager.findMaterial(filter);
            if (mvs == null || mvs.isEmpty()) {
                throw new DigitalObjectNotFoundException(workflowObject.getWorkflowJobId().toString());
            } else {
                String xmlMetadata;
                if (mvs.get(0).getMetadata() == null) {
                    ModsDefinition mods = new ModsDefinition();
                    mods.setVersion(ModsUtils.VERSION);
                    xmlMetadata = ModsUtils.toXml(mods, true);
                } else {
                    xmlMetadata = mvs.get(0).getMetadata();
                }

                return new StreamSource(new StringReader(xmlMetadata));
            }
        }

        @Override
        public InputStream readStream() {
            throw new UnsupportedOperationException();
        }

        @Override
        public void write(EditorResult data, long timestamp, String message) {
            if (!(data instanceof EditorStreamResult)) {
                throw new IllegalArgumentException("Unsupported data: " + data);
            }
            EditorStreamResult result = (EditorStreamResult) data;
            write(result.asBytes(), timestamp, message);
            workflowObject.register(this);
        }

        @Override
        public void write(byte[] data, long timestamp, String message) {
            Objects.requireNonNull(workflowObject);
            this.data = data;
        }

        @Override
        public void write(URI data, long timestamp, String message) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void write(InputStream data, long timestamp, String message) {
            throw new UnsupportedOperationException();
        }

        /**
         * Save BIBLIO_MODS to Physical material and label to JOB
         * <p>
         * Only one editor is used (BIBLIO_MODS), but a label is saved later. DC from plugins and RELS-EXT are ignored.
         *
         * @throws DigitalObjectException
         */
        @Override
        public void flush() throws DigitalObjectException {
            Objects.requireNonNull(workflowObject);

            if (!dsId.equals(ModsStreamEditor.DATASTREAM_ID)) {
                return;
            }

            try {
                MaterialFilter filter = new MaterialFilter();
                filter.setLocale(workflowObject.getLocale());
                filter.setJobId(workflowObject.getWorkflowJobId());
                filter.setType(MaterialType.PHYSICAL_DOCUMENT);
                WorkflowManager manager = WorkflowManager.getInstance();
                List<MaterialView> materials = manager.findMaterial(filter);
                if (materials != null && materials.size() == 1) {
                    MaterialView material = materials.get(0);
                    material.setMetadata(new String(data, StandardCharsets.UTF_8));
                    manager.updateMaterial(material);
                    JobFilter jobFilter = new JobFilter();
                    jobFilter.setId(workflowObject.getWorkflowJobId());
                    jobFilter.setLocale(workflowObject.getLocale());
                    Job job = manager.findJob(jobFilter).get(0);
                    job.setLabel(workflowObject.getLabel());
                }
            } catch (WorkflowException e) {
                throw new DigitalObjectException(e.getMessage());
            }
        }
    }

    private static final class EditorStreamResult extends StreamResult implements XmlStreamEditor.EditorResult {

        public EditorStreamResult() {
            super(new ByteArrayOutputStream());
        }

        public byte[] asBytes() {
            return ((ByteArrayOutputStream) getOutputStream()).toByteArray();
        }

    }
}
