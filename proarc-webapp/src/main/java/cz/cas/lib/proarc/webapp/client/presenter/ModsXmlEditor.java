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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.client.presenter;

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.widgets.Canvas;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.TextDataSource;
import cz.cas.lib.proarc.webapp.client.widget.CodeMirror;
import cz.cas.lib.proarc.webapp.client.widget.DatastreamEditor;
import java.util.logging.Logger;

/**
 * Edits MODS data in XML format.
 *
 * @author Jan Pokorsky
 */
final class ModsXmlEditor implements DatastreamEditor, Refreshable {

    private static final Logger LOG = Logger.getLogger(ModsXmlEditor.class.getName());
    private final CodeMirror sourceForm;
    private DigitalObject digitalObject;

    public ModsXmlEditor() {
        sourceForm = new CodeMirror();
    }

    @Override
    public void edit(DigitalObject digitalObject) {
        this.digitalObject = digitalObject;
        refresh();
    }

    @Override
    public void focus() {
        sourceForm.getUI().focus();
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getCapability(Class<T> clazz) {
        T c = null;
        if (Refreshable.class.equals(clazz)) {
            c = (T) this;
        }
        return c;
    }

    @Override
    public Canvas[] getToolbarItems() {
        return new Canvas[0];
    }

    @Override
    public Canvas getUI() {
        return sourceForm.getUI();
    }

    @Override
    public void refresh() {
        if (digitalObject != null) {
            Criteria pidCriteria = new Criteria(ModsCustomDataSource.FIELD_PID, digitalObject.getPid());
            if (digitalObject.getBatchId() != null) {
                pidCriteria.addCriteria(ModsCustomDataSource.FIELD_BATCHID, digitalObject.getBatchId());
            }
            TextDataSource.getMods().fetchData(pidCriteria, new DSCallback() {

                @Override
                public void execute(DSResponse response, Object rawData, DSRequest request) {
                    handleFetchResponse(response);
                }
            });
        }
    }

    private void handleFetchResponse(DSResponse response) {
        String xml = "";
        if (RestConfig.isStatusOk(response)) {
            Record[] data = response.getData();
            if (data != null && data.length == 1) {
                xml = data[0].getAttribute(TextDataSource.FIELD_CONTENT);
            }
        }
        sourceForm.setContent(xml);
        sourceForm.clearHistory();
    }

}
