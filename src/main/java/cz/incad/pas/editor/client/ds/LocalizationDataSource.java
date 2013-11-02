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
package cz.incad.pas.editor.client.ds;

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.data.events.DataArrivedEvent;
import com.smartgwt.client.data.events.DataArrivedHandler;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.form.fields.FormItem;
import cz.incad.pas.editor.shared.rest.LocalizationResourceApi;
import cz.incad.pas.editor.shared.rest.LocalizationResourceApi.BundleName;
import java.util.LinkedHashMap;

/**
 * Loads localized messages configurable by server configuration files.
 * Values are cached.
 *
 * @author Jan Pokorsky
 */
public final class LocalizationDataSource extends RestDataSource {

    public static final String ID = "LocalizationDataSource";
    private static LocalizationDataSource INSTANCE;
    private ResultSet cache;

    public LocalizationDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_LOCALIZATION);

        DataSourceTextField bundle = new DataSourceTextField(LocalizationResourceApi.ITEM_BUNDLENAME);
        bundle.setHidden(true);
        DataSourceTextField key = new DataSourceTextField(LocalizationResourceApi.ITEM_KEY);
        key.setHidden(true);
        // primary key is composite of bundleName and key; unsupported by SmartGWT 3.0
//        key.setPrimaryKey(true);

        DataSourceTextField value = new DataSourceTextField(LocalizationResourceApi.ITEM_VALUE);

        setFields(bundle, key, value);
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
        // cache data source for session
        setCacheAllData(Boolean.TRUE);
    }

    public static LocalizationDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new LocalizationDataSource();
        }
        return  INSTANCE;
    }

    public void initOnStart(final BooleanCallback callback) {
        if (cache == null) {
            cache = new ResultSet(this);
            cache.addDataArrivedHandler(new DataArrivedHandler() {

                @Override
                public void onDataArrived(DataArrivedEvent event) {
                    if (cache.allRowsCached()) {
                        callback.execute(Boolean.TRUE);
                    }
                }
            });
            cache.get(0);
        } else {
            cache.invalidateCache();
            cache.get(0);
        }
    }

    public static Criteria asCriteria(BundleName bundleName) {
        Criteria criteria = new Criteria(LocalizationResourceApi.ITEM_BUNDLENAME,
                bundleName.toString());
        return criteria;
    }

    public LinkedHashMap<String, String> asValueMap(BundleName bundleName) {
        LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
        if (cache != null) {
            Record[] findAll = cache.findAll(LocalizationResourceApi.ITEM_BUNDLENAME, bundleName.toString());
            for (Record record : findAll) {
                String key = record.getAttribute(LocalizationResourceApi.ITEM_KEY);
                String value = record.getAttribute(LocalizationResourceApi.ITEM_VALUE);
                map.put(key, value);
            }
        }
        return map;
    }

    public static void setOptionDataSource(FormItem field, BundleName bundleName) {
        field.setOptionDataSource(getInstance());
        field.setOptionCriteria(asCriteria(bundleName));
        field.setValueField(LocalizationResourceApi.ITEM_KEY);
        field.setDisplayField(LocalizationResourceApi.ITEM_VALUE);
    }

}
