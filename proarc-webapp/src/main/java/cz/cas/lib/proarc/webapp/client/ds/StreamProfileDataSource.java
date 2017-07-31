/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.client.ds;

import com.google.gwt.core.client.GWT;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.util.Offline;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.widget.MediaEditor;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Handles data stream profiles.
 *
 * @author Jan Pokorsky
 */
public class StreamProfileDataSource extends ProarcDataSource {

    public static final String ID = "DatastreamDataSource";
    public static final String FIELD_ID = DigitalObjectResourceApi.STREAMPROFILE_ID;
    public static final String FIELD_MIME = DigitalObjectResourceApi.STREAMPROFILE_MIME;
    public static final String FIELD_LABEL = "_PROARC_LABEL";
    public static final String FIELD_ORDER = "_PROARC_ORDER";
    private static final String FIELD_INSTANCE = "_PROARC_INSTANCE";
    public static final String PARAM_PID = DigitalObjectResourceApi.DIGITALOBJECT_PID;

    private static StreamProfileDataSource INSTANCE;

    public static StreamProfileDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new StreamProfileDataSource();
        }
        return INSTANCE;
    }

    public StreamProfileDataSource() {
        setID(ID);
        setDataURL(RestConfig.URL_DIGOBJECT_STREAMPROFILE);

        DataSourceTextField fieldId = new DataSourceTextField(FIELD_ID);
        fieldId.setPrimaryKey(Boolean.TRUE);

        DataSourceTextField fieldMime = new DataSourceTextField(FIELD_MIME);
        DataSourceTextField fieldLabel = new DataSourceTextField(FIELD_LABEL);
        DataSourceIntegerField fieldOrder = new DataSourceIntegerField(FIELD_ORDER);
        fieldOrder.setHidden(true);
        setFields(fieldId, fieldMime, fieldLabel, fieldOrder);
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    @Override
    protected void transformResponse(DSResponse dsResponse, DSRequest dsRequest, Object data) {
        if (dsRequest.getOperationType() == DSOperationType.FETCH) {
            Record[] records = dsResponse.getData();

            String source = dsRequest.getAttribute("source");

            if (records != null) {
                int unsortedIdx = 1000;
                for (Record record : records) {
                    StreamProfile stream = StreamProfile.create(record, unsortedIdx, source);
                    if (stream.getOrder() == unsortedIdx) {
                        ++unsortedIdx;
                    }
                }
            }
        }
        super.transformResponse(dsResponse, dsRequest, data);
    }

    public static final class StreamProfile {

        private static List<StreamProfile> TEMPLATES;
        private final Record record;

        /** Gets stream instance or {@code null}. */
        public static StreamProfile get(Record r) {
            if (r == null) {
                return null;
            }
            StreamProfile sv = (StreamProfile) r.getAttributeAsObject(FIELD_INSTANCE);
            if (sv == null) {
                sv = new StreamProfile(r);
                if (sv.getId() == null) {
                    return null;
                }
                r.setAttribute(FIELD_INSTANCE, sv);
            }
            return sv;
        }

        private static StreamProfile create(Record r, int defaultOrder, String source) {
            StreamProfile sv = get(r);
            StreamProfile template = getTemplate(sv.getId(), source);
            r.setAttribute(FIELD_ORDER, template == null ? defaultOrder : template.getOrder());
            r.setAttribute(FIELD_LABEL, template == null ? r.getAttribute(FIELD_ID) : template.getLabel());
            return sv;
        }

        private StreamProfile(Record record) {
            this.record = record;
        }

        public String getId() {
            return record.getAttribute(FIELD_ID);
        }

        public String getMime() {
            return record.getAttribute(FIELD_MIME);
        }

        public int getOrder() {
            return record.getAttributeAsInt(FIELD_ORDER);
        }

        public String getLabel() {
            return record.getAttribute(FIELD_LABEL);
        }

        /**
         * Gets default stream profile labels and display order.
         */
        public static List<StreamProfile> getTemplates(String source) {
            if (TEMPLATES != null) {
                reorderTemplates(source);
                return TEMPLATES;
            }

            ClientMessages i18n = GWT.create(ClientMessages.class);
            TEMPLATES = new ArrayList<StreamProfile>();
            template("PREVIEW", i18n.DigitalObjectEditor_MediaEditor_DSPreview_Title());
            template("FULL", i18n.DigitalObjectEditor_MediaEditor_DSFull_Title());
            template("RAW", i18n.DigitalObjectEditor_MediaEditor_DSRaw_Title());
            template("THUMBNAIL", i18n.DigitalObjectEditor_MediaEditor_DSThumbnail_Title());
            template("NDK_ARCHIVAL", i18n.DigitalObjectEditor_MediaEditor_DSNdkArchival_Title());
            template("NDK_USER", i18n.DigitalObjectEditor_MediaEditor_DSNdkUser_Title());

            reorderTemplates(source);
            return TEMPLATES;
        }

        /**
         * Gets default label and display order for a given stream or {@code null}
         * for unknown stream ID.
         */
        public static StreamProfile getTemplate(String dsId, String source) {
            List<StreamProfile> streams = getTemplates(source);
            for (StreamProfile stream : streams) {
                if (stream.getId().equals(dsId)) {
                    return stream;
                }
            }
            return null;
        }

        private static StreamProfile template(String id, String title) {
            StreamProfile sv = new StreamProfile(new Record());
            sv.record.setAttribute(FIELD_ID, id);
            sv.record.setAttribute(FIELD_LABEL, title);
            sv.record.setAttribute(FIELD_ORDER, TEMPLATES.size());
            TEMPLATES.add(sv);
            return sv;
        }

        private static void reorderTemplates(String source) {
            if (source == null) {
                return;
            }

            Object selectedId = Offline.get(MediaEditor.MEDIA_EDITOR_LAST_SELECTION + "_" + source);

            if (selectedId != null) {
                for (int i = 0; i < TEMPLATES.size(); i++) {
                    if (TEMPLATES.get(i).getId().equals(selectedId)) {
                        Collections.swap(TEMPLATES, 0, i);

                        TEMPLATES.get(0).record.setAttribute(FIELD_ORDER, 0);
                        TEMPLATES.get(i).record.setAttribute(FIELD_ORDER, i);

                        return;
                    }
                }
            }
        }

    }

}
