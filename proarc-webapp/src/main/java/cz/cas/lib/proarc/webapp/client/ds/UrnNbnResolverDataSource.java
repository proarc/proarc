package cz.cas.lib.proarc.webapp.client.ds;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.types.FieldType;
import cz.cas.lib.proarc.webapp.shared.rest.UrnNbnResourceApi;

import java.util.logging.Logger;

public class UrnNbnResolverDataSource extends ProarcDataSource {

    private static final Logger LOG = Logger.getLogger(UrnNbnResolverDataSource.class.getName());

    public static final String ID = "UrnNbnResolverDataSource";

    public UrnNbnResolverDataSource() {
        setID(ID);

        setDataURL(RestConfig.URL_URNNBN_RESOLVER);

        DataSourceField fieldId = new DataSourceField(UrnNbnResourceApi.RESOLVER_ID, FieldType.TEXT);
        fieldId.setPrimaryKey(true);

        DataSourceField fieldTitle = new DataSourceField(UrnNbnResourceApi.RESOLVER_NAME, FieldType.TEXT);

        setFields(fieldId, fieldTitle);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));

    }

    public static UrnNbnResolverDataSource getInstance() {
        UrnNbnResolverDataSource ds = (UrnNbnResolverDataSource) DataSource.get(ID);
        ds = (ds != null) ? ds : new UrnNbnResolverDataSource();
        return ds;
    }

}
