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
package cz.incad.pas.editor.client.ds;

import com.google.gwt.core.client.JavaScriptObject;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.XMLTools;
import com.smartgwt.client.data.XmlNamespaces;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.FieldType;
import com.smartgwt.client.util.JSOHelper;
import cz.incad.pas.editor.client.ClientUtils;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public class DcRecordDataSource extends DataSource {
    private static final String FIX_SINGLE_STRING_VALUE = "fixSingleStringValue";

    private static final Logger LOG = Logger.getLogger(DcRecordDataSource.class.getName());

    public static final String ID = "DcRecordDataSource";

    private static final HashMap<String, String> XML_NAMESPACE_MAP = new HashMap<String, String>();
    private static final XmlNamespaces XML_NAMESPACES = new XmlNamespaces();

    static {
        XML_NAMESPACE_MAP.put("oai_dc", "http://www.openarchives.org/OAI/2.0/oai_dc/");
        XML_NAMESPACE_MAP.put("dc", "http://purl.org/dc/elements/1.1/");
        XML_NAMESPACE_MAP.put("dcr", "http://www.incad.cz/pas/editor/dor/");

        for (Map.Entry<String, String> entry : XML_NAMESPACE_MAP.entrySet()) {
            XML_NAMESPACES.addNamespace(entry.getKey(), entry.getValue());
        }
    }

    public static final String PREFIX_DCR = "dcr";
    public static final String PREFIX_DC = "dc";
    public static final QName FIELD_PID = new QName(XML_NAMESPACE_MAP, "pid", PREFIX_DCR);
    public static final QName FIELD_TIMESTAMP = new QName(XML_NAMESPACE_MAP, "timestamp", PREFIX_DCR);
    public static final QName FIELD_DC = new QName(XML_NAMESPACE_MAP, "dc", "oai_dc");
    
    public static final QName FIELD_CREATOR = new QName(XML_NAMESPACE_MAP, "creator", PREFIX_DC);
    public static final QName FIELD_CONTRIBUTOR = new QName(XML_NAMESPACE_MAP, "contributor", PREFIX_DC);
    public static final QName FIELD_COVERAGE = new QName(XML_NAMESPACE_MAP, "coverage", PREFIX_DC);
    public static final QName FIELD_DATE = new QName(XML_NAMESPACE_MAP, "date", PREFIX_DC);
    public static final QName FIELD_DESCRIPTION = new QName(XML_NAMESPACE_MAP, "description", PREFIX_DC);
    public static final QName FIELD_FORMAT = new QName(XML_NAMESPACE_MAP, "format", PREFIX_DC);
    public static final QName FIELD_IDENTIFIER = new QName(XML_NAMESPACE_MAP, "identifier", PREFIX_DC);
    public static final QName FIELD_LANGUAGE = new QName(XML_NAMESPACE_MAP, "language", PREFIX_DC);
    public static final QName FIELD_PUBLISHER = new QName(XML_NAMESPACE_MAP, "publisher", PREFIX_DC);
    public static final QName FIELD_RELATION = new QName(XML_NAMESPACE_MAP, "relation", PREFIX_DC);
    public static final QName FIELD_RIGHTS = new QName(XML_NAMESPACE_MAP, "rights", PREFIX_DC);
    public static final QName FIELD_SOURCE = new QName(XML_NAMESPACE_MAP, "source", PREFIX_DC);
    public static final QName FIELD_SUBJECT = new QName(XML_NAMESPACE_MAP, "subject", PREFIX_DC);
    public static final QName FIELD_TITLE = new QName(XML_NAMESPACE_MAP, "title", PREFIX_DC);
    public static final QName FIELD_TYPE = new QName(XML_NAMESPACE_MAP, "type", PREFIX_DC);

    public static final String FIELD_XML_LANG = "xml:lang";
    /** SmartGWT field name for text inside ComplexType element with attributes */
    public static final String FIELD_XML_TEXT_CONTENT = "xmlTextContent";

    public static DcRecordDataSource getInstance() {
        DcRecordDataSource ds = (DcRecordDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new DcRecordDataSource();
        return ds;
    }

    public DcRecordDataSource() {
        setID(ID);

        setDataFormat(DSDataFormat.XML);
//        setDataURL("ds/dsRecord.xml");
        setDataURL(RestConfig.URL_DIGOBJECT_DC);
        setXmlNamespaces(XML_NAMESPACES);
        setGlobalNamespaces(XML_NAMESPACE_MAP);
        setRecordXPath("/dcr:dcRecord");
        setTagName("dcr:dcRecord");
//        setSendExtraFields(false);
//        setDropExtraFields(true);

        DataSourceField fieldPid = new DataSourceField(FIELD_PID.getQualifiedName(), FieldType.TEXT);
        fieldPid.setPrimaryKey(true);
        fieldPid.setRequired(true);
        fieldPid.setValueXPath(FIELD_PID.getQualifiedName());

        DataSourceField fieldTimestamp = new DataSourceField(FIELD_TIMESTAMP.getQualifiedName(), FieldType.TEXT);
        fieldTimestamp.setRequired(true);
        fieldTimestamp.setHidden(true);
        fieldTimestamp.setValueXPath(FIELD_TIMESTAMP.getQualifiedName());

        DataSourceField fieldDc = new DataSourceField(FIELD_DC.getQualifiedName(), FieldType.TEXT);
        fieldDc.setRequired(true);
        fieldDc.setTypeAsDataSource(getDcDataSource());
        fieldDc.setValueXPath(FIELD_DC.getQualifiedName());

        setFields(fieldPid, fieldTimestamp, fieldDc);

        setOperationBindings(RestConfig.createUpdateOperation());
        
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    @Override
    protected void transformResponse(DSResponse response, DSRequest request, Object data) {
        // transform oai_dc:dc element to properly resolve namespaces
        Object dcNode = XMLTools.selectNodes(data, FIELD_DC.getQualifiedName(), XML_NAMESPACE_MAP);

        DataSource ds = getDcDataSource();
        Record[] recordsFromXML = new Record[0]; // XXX test
//        Record[] recordsFromXML = ds.recordsFromXML(dcNode);
        Record[] responseData = response.getData();
        if (responseData.length > 0) {
            fixDcRecord(responseData[0].getAttributeAsRecord(FIELD_DC.getQualifiedName()));
//            LOG.fine(ClientUtils.dump(responseData[0], "transformResponse.responseData:"));
        }

        LOG.fine(ClientUtils.format("transformResponse: responseData.length: %s, recordsFromXML.length: %s",
                responseData.length, recordsFromXML.length));
        for (int i = 0; i < recordsFromXML.length; i++) {
//            LOG.fine(ClientUtils.dump(recordsFromXML[i], ""));
            fixDcRecord(recordsFromXML[i]);
            responseData[i].setAttribute(FIELD_DC.getQualifiedName(), recordsFromXML[i]);
        }

        super.transformResponse(response, request, data);
    }

    @Override
    protected Object transformRequest(DSRequest dsRequest) {
        if (dsRequest.getOperationType() == DSOperationType.UPDATE) {
            // Uff, it seems the only way how to dispose of garbage from resulting XML
            // is to copy relevant attributes to a new record recursively.
            Record r = Record.getOrCreateRef(dsRequest.getData());
            Record update = new Record();
            update.setAttribute(FIELD_PID.getQualifiedName(), r.getAttribute(FIELD_PID.getQualifiedName()));
            update.setAttribute(FIELD_TIMESTAMP.getQualifiedName(), r.getAttribute(FIELD_TIMESTAMP.getQualifiedName()));
            Record oaiDc = r.getAttributeAsRecord(FIELD_DC.getQualifiedName());
            Record updateOaiDc = new Record();
            for (String fieldName : getDcDataSource().getFieldNames()) {
                updateOaiDc.setAttribute(fieldName, oaiDc.getAttributeAsObject(fieldName));
            }
            update.setAttribute(FIELD_DC.getQualifiedName(), updateOaiDc);
            LOG.fine(ClientUtils.dump(update, "transformRequest"));

            dsRequest.setData(update);
        }
        return super.transformRequest(dsRequest);
    }

    public static DataSource getDcDataSource() {
        DataSource result = DataSource.get("DcDataSource");
        if (result != null) {
            return result;
        } else {
            return createDcDataSource();
        }
    }

    private static DataSource createDcDataSource() {
        final DataSource dsDC = new DataSource() {

            @Override
            protected void transformResponse(DSResponse response, DSRequest request, Object data) {
                // data is a xml Document
                // WARNING: it is com.google.gwt.dom.client.Document not com.google.gwt.xml.client.Document!!!
                // following fix transforms parsed result that contains various object types to array of Records for each Record's attribute
                // Maybe we should create own data result from data parameter using
                // XMLTools or XMLParser instead of fixing default result.
                // In order to get data as string use DataSource.setDataFormat(DSDataFormat.CUSTOM)
                // or setDataProtocol(DSProtocol.CLIENTCUSTOM).
                // Do not use XMLTools.loadXMLSchema as it does not work well with tricky ComplexTypes.

                LOG.fine(ClientUtils.format("transformResponse.data: %s, class: %s", data, data.getClass()));

                Record[] rs = response.getData();
                if (rs != null) {
                    for (Record r : rs) {
                        fixDcRecord(r);
                    }
                }

                super.transformResponse(response, request, data);
            }
        };

        dsDC.setID("DcDataSource");
//        dsDC.setDataURL("ds/dc.xml");
//        dsDC.setDataURL("ds/dc_drobnustky.xml");
        dsDC.setXmlNamespaces(XML_NAMESPACES);
        dsDC.setGlobalNamespaces(XML_NAMESPACE_MAP);
//        dsDC.setRecordXPath("oai_dc:dc");
//        dsDC.setRecordXPath("/dc");
        dsDC.setTagName(FIELD_DC.getQualifiedName());
        dsDC.setSendExtraFields(false);
        dsDC.setDropExtraFields(true);

        dsDC.setFields(
                createDcDataSourceField(FIELD_TITLE),
                createDcDataSourceField(FIELD_CREATOR),
                createDcDataSourceField(FIELD_SUBJECT),
                createDcDataSourceField(FIELD_DESCRIPTION),
                createDcDataSourceField(FIELD_PUBLISHER),
                createDcDataSourceField(FIELD_CONTRIBUTOR),
                createDcDataSourceField(FIELD_DATE),
                createDcDataSourceField(FIELD_TYPE),
                createDcDataSourceField(FIELD_FORMAT),
                createDcDataSourceField(FIELD_IDENTIFIER),
                createDcDataSourceField(FIELD_SOURCE),
                createDcDataSourceField(FIELD_LANGUAGE),
                createDcDataSourceField(FIELD_RELATION),
                createDcDataSourceField(FIELD_COVERAGE),
                createDcDataSourceField(FIELD_RIGHTS)
        );

        return dsDC;
    }

    private static DataSourceField createDcDataSourceField(QName name) {
        DataSourceField field = new DataSourceField(name.getQualifiedName(), FieldType.TEXT);
        field.setTypeAsDataSource(createDcElementDataSource(name.getQualifiedName()));
        field.setRequired(true);
        field.setAttribute(FIX_SINGLE_STRING_VALUE, true);
        field.setValueXPath(name.getQualifiedName());
        return field;
    }

    /**
     * Holds element text content and its attributes
     */
    private static DataSource createDcElementDataSource(String name) {
        DataSource ds = new DataSource();
//        ds.setID("DcElementDataSource");
        ds.setTagName(name);
        DataSourceField lang = new DataSourceField(FIELD_XML_LANG, FieldType.TEXT);
        lang.setXmlAttribute(true);

        ds.setFields(lang);

        ds.setXmlNamespaces(XML_NAMESPACES);
        ds.setGlobalNamespaces(XML_NAMESPACE_MAP);

        return ds;
    }

    /**
     * Ensures that subelements of {@code oai_dc:dc} are represented as list of JavaScriptObjects
     * in order to allow to easy handling by ListGrid.
     *
     * @param r {@code oai_dc:dc} record
     */
    public static void fixDcRecord(Record r) {
        if (r == null) {
            return ;
        }
        DataSourceField[] fields = getDcDataSource().getFields();
        for (DataSourceField field : fields) {
            Boolean fix = field.getAttributeAsBoolean(FIX_SINGLE_STRING_VALUE);
            if (fix != null && fix) {
                fixField(r, field);
            }
        }
    }

    private static void fixField(Record r, DataSourceField field) {
        String fieldName = field.getName();
        Object fieldValue = r.getAttributeAsObject(fieldName);
        LOG.finest(ClientUtils.format("fixField: %s, fieldValue: %s, class: %s", fieldName, fieldValue, fieldValue.getClass()));
        if (JSOHelper.isJSO(fieldValue)) {
            JavaScriptObject jso = r.getAttributeAsJavaScriptObject(fieldName);
            if (JSOHelper.isArray(jso)) {
                int arrayLength = JSOHelper.arrayLength(jso);
                for (int i = 0; i < arrayLength; i++) {
                    Object arrayItem = JSOHelper.arrayGetObject(jso, i);
                    // transforms String object to Record
                    fix(arrayItem, jso, i, field);
                }
            } else {
                // transforms single JSO to array: JSO -> [JSO]
                JavaScriptObject array = JSOHelper.arrayConvert(new JavaScriptObject[] {jso});
                r.setAttribute(fieldName, array);
                LOG.fine(ClientUtils.format("fix.field: %s, JavaScriptObject -> [JavaScriptObject] fix", fieldName));
            }
        } else if (JSOHelper.isJavaString(fieldValue)) {
            JavaScriptObject fix = JSOHelper.createObject();
            JSOHelper.setAttribute(fix, FIELD_XML_TEXT_CONTENT, fieldValue);
            JavaScriptObject array = JSOHelper.arrayConvert(new JavaScriptObject[] {fix});
            r.setAttribute(fieldName, array);
            LOG.fine(ClientUtils.format("fix.field: %s, String -> [JavaScriptObject] fix", fieldName));
        }
    }

    private static void fix(Object arrayItem, JavaScriptObject jso, int i, DataSourceField field) {
        LOG.finest(ClientUtils.format("fix.field: %s, index: %s, arrayItem: %s, class: %s",
                field.getName(), i, arrayItem, arrayItem.getClass()));
        if (arrayItem instanceof String) {
            JavaScriptObject fix = JSOHelper.createObject();
            JSOHelper.setAttribute(fix, FIELD_XML_TEXT_CONTENT, arrayItem);
            JSOHelper.setArrayValue(jso, i, fix);
            LOG.fine(ClientUtils.format("fix.field: %s, index: %s, String -> JavaScriptObject fix", field.getName(), i));
        }
    }

    public static final class QName {

        private final String namespace;
        private final String localPart;
        private final String prefix;

        public QName(String localPart) {
            this((String) null, localPart, null);
        }

        public QName(Map<String, String> nsMap, String localPart, String prefix) {
            this(nsMap.get(prefix), localPart, prefix);
        }

        public QName(String namespace, String localPart, String prefix) {
            if (localPart == null) {
                throw new NullPointerException("localPart");
            }
            if (namespace != null && namespace.trim().isEmpty()) {
                namespace = null;
            }
            if (prefix != null && prefix.trim().isEmpty()) {
                prefix = null;
            }
            if (prefix != null && namespace == null) {
                throw new IllegalArgumentException("Missing namespace for prefix: " + prefix);
            }
            this.namespace = namespace;
            this.localPart = localPart;
            this.prefix = prefix;
        }

        public String getQualifiedName() {
            return (prefix == null) ? localPart : prefix + ':' + localPart;
        }
        
        public String getLocalPart() {
            return localPart;
        }

        public String getNamespace() {
            return namespace;
        }

        public String getPrefix() {
            return prefix;
        }
    }

}
