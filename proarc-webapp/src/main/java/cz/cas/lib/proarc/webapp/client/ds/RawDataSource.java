package cz.cas.lib.proarc.webapp.client.ds;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.RestDataSource;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;

/**
 * Serves as access point to RAW datastream for its removal.
 *
 * Created by Jakub Kremlacek on 24.3.17.
 */
public class RawDataSource extends RestDataSource {

    public static final String FIELD_PID = DigitalObjectResourceApi.DIGITALOBJECT_PID;
    public static final String ID_RAW = "RawDataSource";

    public RawDataSource(String dsId, String dsUrl) {
        setID(dsId);

        setRecordXPath('/' + DigitalObjectResourceApi.STRINGRECORD_ELEMENT);
        setDataFormat(DSDataFormat.JSON);

        setDataURL(dsUrl);

        DataSourceField fieldPid = new DataSourceField(FIELD_PID, FieldType.TEXT);
        fieldPid.setPrimaryKey(true);
        fieldPid.setRequired(true);

        setFields(fieldPid);

        setOperationBindings(RestConfig.createDeleteOperation());

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public static RawDataSource getRaw() {
        return getDS(ID_RAW, RestConfig.URL_DIGOBJECT_RAW);
    }

    private static RawDataSource getDS(String dsId, String dsUrl) {
        RawDataSource ds = (RawDataSource) DataSource.get(dsId);

        ds = (ds != null) ? ds : new RawDataSource(dsId, dsUrl);
        return ds;
    }
}
