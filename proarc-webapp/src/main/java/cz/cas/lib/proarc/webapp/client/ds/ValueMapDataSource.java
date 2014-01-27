/*
 * Copyright (C) 2014 Jan Pokorsky
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

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.data.events.DataArrivedEvent;
import com.smartgwt.client.data.events.DataArrivedHandler;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;
import com.smartgwt.client.util.BooleanCallback;
import cz.cas.lib.proarc.webapp.shared.rest.ValueMapResourceApi;
import java.util.HashMap;

/**
 * Provides value maps fetched from {@link cz.cas.lib.proarc.webapp.server.rest.ValueMapResource}.
 * It is expected to be fetched on client start.
 *
 * @author Jan Pokorsky
 */
public class ValueMapDataSource extends RestDataSource {

    private static ValueMapDataSource INSTANCE;

    private ResultSet cache;
    private HashMap<String, DataSource> optionDataSources = new HashMap<String, DataSource>();

    public static ValueMapDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new ValueMapDataSource();
        }
        return INSTANCE;
    }

    public ValueMapDataSource() {
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_VALUEMAP);

        DataSourceTextField mapId = new DataSourceTextField(ValueMapResourceApi.RESULT_MAPID);
        mapId.setPrimaryKey(true);
        DataSourceField values = new DataSourceField(ValueMapResourceApi.RESULT_VALUES, FieldType.ANY);
        setFields(mapId, values);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
        // cache data source for session
        setCacheAllData(Boolean.TRUE);
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

    /**
     * Gets value map with mapId as local data source.
     */
    public DataSource getOptionDataSource(String mapId) {
        DataSource dataSource = optionDataSources.get(mapId);
        if (cache != null) {
            dataSource = new DataSource();
            dataSource.setClientOnly(true);
            Record mapRecord = cache.findByKey(mapId);
            if (mapRecord != null) {
                Record[] attributeAsRecordArray = mapRecord.getAttributeAsRecordArray(
                        ValueMapResourceApi.RESULT_VALUES);
                dataSource.setTestData(attributeAsRecordArray);
            }
            optionDataSources.put(mapId, dataSource);
        }
        return dataSource;
    }

}
