/*
 * Copyright (C) 2011 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.client.ds;

import com.google.gwt.core.client.Callback;
import com.google.gwt.event.shared.HandlerRegistration;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.data.events.DataArrivedEvent;
import com.smartgwt.client.data.events.DataArrivedHandler;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FetchMode;
import com.smartgwt.client.types.FieldType;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;
import java.util.logging.Logger;

/**
 * Lists Fedora models.
 *
 * @author Jan Pokorsky
 */
public class MetaModelDataSource extends RestDataSource {

    private static final Logger LOG = Logger.getLogger(MetaModelDataSource.class.getName());

    public static final String ID = "MetaModelDataSource";
    public static final String FIELD_PID = DigitalObjectResourceApi.METAMODEL_PID_PARAM;
    public static final String FIELD_DISPLAY_NAME = DigitalObjectResourceApi.METAMODEL_DISPLAYNAME_PARAM;
    public static final String FIELD_IS_ROOT = DigitalObjectResourceApi.METAMODEL_ROOT_PARAM;
    public static final String FIELD_IS_LEAF = DigitalObjectResourceApi.METAMODEL_LEAF_PARAM;
    public static final String FIELD_EDITOR = DigitalObjectResourceApi.METAMODEL_MODSCUSTOMEDITORID_PARAM;
    public static final String FIELD_DATASTREAM_EDITORS = DigitalObjectResourceApi.METAMODEL_DATASTREAMEDITOR_PARAM;
    /**
     * Synthetic field holding {@link MetaModelRecord} instance.
     */
    public static final String FIELD_MODELOBJECT = "MetaModelRecord";

    public static final String EDITOR_PAGE = "cz.cas.lib.proarc.webapp.client.widget.mods.PageForm";
    public static final String EDITOR_PERIODICAL = "cz.cas.lib.proarc.webapp.client.widget.mods.PeriodicalForm";
    public static final String EDITOR_PERIODICAL_VOLUME = "cz.cas.lib.proarc.webapp.client.widget.mods.PeriodicalVolumeForm";
    public static final String EDITOR_PERIODICAL_ISSUE = "cz.cas.lib.proarc.webapp.client.widget.mods.PeriodicalIssueForm";
    public static final String EDITOR_MONOGRAPH = "cz.cas.lib.proarc.webapp.client.widget.mods.MonographForm";
    public static final String EDITOR_MONOGRAPH_UNIT = "cz.cas.lib.proarc.webapp.client.widget.mods.MonographUnitForm";
    private static ResultSet resultSet;

    public MetaModelDataSource() {
        setID(ID);

        setDataFormat(DSDataFormat.JSON);
        
        setDataURL(RestConfig.URL_DIGOBJECT_METAMODEL);

        DataSourceTextField pid = new DataSourceTextField(FIELD_PID);
        pid.setPrimaryKey(true);

        DataSourceTextField displayName = new DataSourceTextField(FIELD_DISPLAY_NAME);

        DataSourceBooleanField isRoot = new DataSourceBooleanField(FIELD_IS_ROOT);

        DataSourceBooleanField isLeaf = new DataSourceBooleanField(FIELD_IS_LEAF);

        DataSourceTextField editor = new DataSourceTextField(FIELD_EDITOR);

        DataSourceField dataStreamEditors = new DataSourceField(FIELD_DATASTREAM_EDITORS, FieldType.ANY);
        dataStreamEditors.setCanEdit(Boolean.FALSE);
        dataStreamEditors.setHidden(Boolean.TRUE);

        setFields(pid, displayName, isRoot, isLeaf, editor, dataStreamEditors);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public static MetaModelDataSource getInstance() {
        MetaModelDataSource ds = (MetaModelDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new MetaModelDataSource();
        return ds;
    }

    public static ResultSet getModels() {
        return getModels(false);
    }

    public static ResultSet getModels(boolean reload) {
        if (resultSet == null) {
            resultSet = new ResultSet(getInstance());
            resultSet.setFetchMode(FetchMode.LOCAL);
            resultSet.get(0);
        } else if (reload) {
            resultSet.invalidateCache();
            resultSet.get(0);
        }
        return resultSet;
    }

    public static void getModels(boolean reload, final Callback<ResultSet, Void> callback) {
        final ResultSet models = getModels(reload);
        if (models.lengthIsKnown()) {
            callback.onSuccess(models);
        } else {
            final HandlerRegistration[] handler = new HandlerRegistration[1];
            handler[0] = models.addDataArrivedHandler(new DataArrivedHandler() {

                @Override
                public void onDataArrived(DataArrivedEvent event) {
                    handler[0].removeHandler();
                    callback.onSuccess(models);
                }
            });
        }
    }

    /**
     * Gets model of a digital object. It searches for synthetic attribute
     * {@link #FIELD_MODELOBJECT}. If not found it searches for {@link SearchDataSource#FIELD_MODEL}
     * and sets FIELD_MODELOBJECT attribute.
     *
     * @param digitalObject record fetched by {@link SearchDataSource} or {@link RelationDataSource}
     * @return model object or {@code null}
     */
    public static MetaModelRecord getModel(Record digitalObject) {
        MetaModelRecord mmr = (MetaModelRecord) digitalObject.getAttributeAsObject(FIELD_MODELOBJECT);
        if (mmr == null) {
            String model = digitalObject.getAttribute(SearchDataSource.FIELD_MODEL);
            if (model != null) {
                if (resultSet == null) {
                    return null;
                }
                Record mr = resultSet.findByKey(model);
                if (mr != null) {
                    mmr = MetaModelRecord.get(mr);
                    digitalObject.setAttribute(FIELD_MODELOBJECT, mmr);
                } else {
                    ClientUtils.severe(LOG, "unkown model ID '%s' of record %s", model, digitalObject.getAttribute(SearchDataSource.FIELD_PID));
                }
            }
        }
        return mmr;
    }

    public static Menu createMenu(RecordList modelRecords, boolean includeRoots) {
        return createMenu(modelRecords.toArray(), includeRoots);
    }

    /**
     * Helper to create menu listing object models.
     * <p>{@link Menu#setDataSource} is broken (SmartGWT 3.0, 4.0).
     * @param modelRecords fetched model records
     * @param includeRoots include root models in menu?
     * @return the menu
     */
    public static Menu createMenu(Record[] modelRecords, boolean includeRoots) {
        Menu menu = new Menu();
        for (int i = 0; i < modelRecords.length; i++) {
            MetaModelRecord mmr = MetaModelRecord.get(modelRecords[i]);
            if (includeRoots || !mmr.isRoot()) {
                MenuItem menuItem = new MenuItem(mmr.getDisplayName());
                menuItem.setAttribute(FIELD_PID, mmr.getId());
                menu.addItem(menuItem);
            }
        }
        return menu;
    }

    public static final class MetaModelRecord {
        
        private final Record record;

        public static MetaModelRecord get(Record r) {
            return r == null ? null : new MetaModelRecord(r);
        }
        
        public MetaModelRecord(Record r) {
            this.record = r;
        }

        public String getId() {
            return record.getAttribute(FIELD_PID);
        }

        public boolean isRoot() {
            return record.getAttributeAsBoolean(FIELD_IS_ROOT);
        }

        public String getEditorId() {
            return record.getAttribute(FIELD_EDITOR);
        }

        public String getDisplayName() {
            return record.getAttribute(FIELD_DISPLAY_NAME);
        }

        public boolean isSupportedDatastream(String dsEditor) {
            String[] supportedEditors = record.getAttributeAsStringArray(FIELD_DATASTREAM_EDITORS);
            for (String supportedEditor : supportedEditors) {
                if (dsEditor.equals(supportedEditor)) {
                    return true;
                }
            }
            return false;
        }
    }

}
