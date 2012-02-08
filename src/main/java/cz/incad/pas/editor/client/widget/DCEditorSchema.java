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
package cz.incad.pas.editor.client.widget;

import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.core.client.JsArray;
import com.google.gwt.dom.client.Node;
import com.google.gwt.dom.client.NodeList;
import com.google.gwt.dom.client.Element;
import com.google.gwt.xml.client.Document;
import com.google.gwt.xml.client.XMLParser;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.SchemaSet;
import com.smartgwt.client.data.SerializationContext;
import com.smartgwt.client.data.XMLTools;
import com.smartgwt.client.data.XSDLoadCallback;
import com.smartgwt.client.data.XmlNamespaces;
import com.smartgwt.client.rpc.RPCRequest;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DSProtocol;
import com.smartgwt.client.types.FieldType;
import com.smartgwt.client.util.JSOHelper;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.ds.DcRecordDataSource;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public class DCEditorSchema extends DynamicForm {
    
    private static final Logger LOG = Logger.getLogger(DCEditorSchema.class.getName());

    public DataSource schema;

//    public static final String PREFIX_DC = "dc:";
//    public static final String FIELD_CREATOR = PREFIX_DC + "creator";
//    public static final String FIELD_CONTRIBUTOR = PREFIX_DC + "contributor";
//    public static final String FIELD_COVERAGE = PREFIX_DC + "coverage";
//    public static final String FIELD_DATE = PREFIX_DC + "date";
//    public static final String FIELD_DESCRIPTION = PREFIX_DC + "description";
//    public static final String FIELD_FORMAT = PREFIX_DC + "format";
//    public static final String FIELD_IDENTIFIER = PREFIX_DC + "identifier";
//    public static final String FIELD_LANGUAGE = PREFIX_DC + "language";
//    public static final String FIELD_PUBLISHER = PREFIX_DC + "publisher";
//    public static final String FIELD_RELATION = PREFIX_DC + "relation";
//    public static final String FIELD_RIGHTS = PREFIX_DC + "rights";
//    public static final String FIELD_SOURCE = PREFIX_DC + "source";
//    public static final String FIELD_SUBJECT = PREFIX_DC + "subject";
//    public static final String FIELD_TITLE = PREFIX_DC + "title";
//    public static final String FIELD_TYPE = PREFIX_DC + "type";
//    public static final String FIELD_XML_LANG = "xml:lang";
//    /** SmartGWT field name for text inside ComplexType element with attributes */
//    public static final String FIELD_XML_TEXT_CONTENT = "xmlTextContent";
//
//    private static final HashMap<String, String> XML_NAMESPACE_MAP = new HashMap<String, String>();
//    private static final XmlNamespaces XML_NAMESPACES = new XmlNamespaces();
//
//    static {
//        XML_NAMESPACE_MAP.put("oai_dc", "http://www.openarchives.org/OAI/2.0/oai_dc/");
////        XML_NAMESPACE_MAP.put("default", "http://www.openarchives.org/OAI/2.0/oai_dc/");
//        XML_NAMESPACE_MAP.put("dc", "http://purl.org/dc/elements/1.1/");
//
//        for (Map.Entry<String, String> entry : XML_NAMESPACE_MAP.entrySet()) {
//            XML_NAMESPACES.addNamespace(entry.getKey(), entry.getValue());
//        }
//    }
//
//
//    private static DataSource createDataSource(String name) {
//        DataSource ds = new DataSource() {
//
////            @Override
////            protected void transformResponse(DSResponse response, DSRequest request, Object data) {
////                Record[] rs = response.getData();
////                if (rs != null) {
////                    for (Record r : rs) {
//////                        fix(r);
////                    }
////                }
////                super.transformResponse(response, request, data);
////            }
////
////            private void fix(Record r) {
////                JavaScriptObject jso = r.getAttributeAsJavaScriptObject(FIELD_CREATOR);
////                if (JSOHelper.isArray(jso)) {
////                    int arrayLength = JSOHelper.arrayLength(jso);
////                    for (int i = 0; i < arrayLength; i++) {
////                        Object arrayGetObject = JSOHelper.arrayGetObject(jso, i);
////                        System.out.println("...transformResponse: " + arrayGetObject + ", class: " +arrayGetObject.getClass());
////                        if (arrayGetObject instanceof String) {
////                            LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
////                            map.put(FIELD_XML_TEXT_CONTENT, (String) arrayGetObject);
////                        }
////                    }
////                }
////
////            }
//        };
//        ds.setTagName(name);
//        ds.setClientOnly(true);
//        DataSourceField lang = new DataSourceField(FIELD_XML_LANG, FieldType.TEXT);
//        lang.setXmlAttribute(true);
//
//        ds.setFields(lang);
//
//        ds.setXmlNamespaces(XML_NAMESPACES);
//        ds.setGlobalNamespaces(XML_NAMESPACE_MAP);
//
//        return ds;
//    }

    public DCEditorSchema() {
//        final DataSource dsDC = new DataSource() {
//
//            @Override
//            protected void transformResponse(DSResponse response, DSRequest request, Object data) {
//                // data is a xml Document
//                // WARNING: it is com.google.gwt.dom.client.Document not com.google.gwt.xml.client.Document!!!
//                // following fix transforms parsed result that contains various object types to array of Records for each Record's attribute
//                // Maybe we should create own data result from data parameter using
//                // XMLTools or XMLParser instead of fixing default result.
//                // In order to get data as string use DataSource.setDataFormat(DSDataFormat.CUSTOM)
//                // or setDataProtocol(DSProtocol.CLIENTCUSTOM).
//                // Do not use XMLTools.loadXMLSchema as it does not work well with tricky ComplexTypes.
//
//                LOG.warning("transformResponse.data: " + data + ", class: " + data.getClass());
//
//                DataSourceField[] fields = getFields();
//                Record[] rs = response.getData();
//                if (rs != null) {
//                    for (Record r : rs) {
//                        checkRecord(r, fields);
//                    }
//                }
//
//                super.transformResponse(response, request, data);
//            }
//
//            private void checkRecord(Record r, DataSourceField[] fields) {
//                for (DataSourceField field : fields) {
//                    Boolean fix = field.getAttributeAsBoolean("fixSingleStringValue");
//                    if (fix != null && fix) {
//                        checkField(r, field);
//                    }
//                }
//            }
//            private void checkField(Record r, DataSourceField field) {
//                String fieldName = field.getName();
//                Object creatorObject = r.getAttributeAsObject(fieldName);
////                System.out.println("...transformResponse.creatorObject: " + creatorObject + ", class: " + creatorObject.getClass() + ", field: " + fieldName);
//                if (JSOHelper.isJSO(creatorObject)) {
//                    JavaScriptObject jso = r.getAttributeAsJavaScriptObject(fieldName);
//                    if (JSOHelper.isArray(jso)) {
//                        int arrayLength = JSOHelper.arrayLength(jso);
//                        for (int i = 0; i < arrayLength; i++) {
//                            Object arrayGetObject = JSOHelper.arrayGetObject(jso, i);
//                            // transforms String object to Record
//                            fix(arrayGetObject, jso, i, field);
//                        }
//                    } else {
//                        // transforms single JSO to array: JSO -> [JSO]
//                        JavaScriptObject array = JSOHelper.arrayConvert(new JavaScriptObject[] {jso});
//                        r.setAttribute(fieldName, array);
//                        LOG.fine(ClientUtils.format("fix.field: %s, JavaScriptObject -> [JavaScriptObject] fix", fieldName));
//                    }
//                } else if (JSOHelper.isJavaString(creatorObject)) {
//                    JavaScriptObject fix = JSOHelper.createObject();
//                    JSOHelper.setAttribute(fix, FIELD_XML_TEXT_CONTENT, creatorObject);
//                    JavaScriptObject array = JSOHelper.arrayConvert(new JavaScriptObject[] {fix});
//                    r.setAttribute(fieldName, array);
//                    LOG.fine(ClientUtils.format("fix.field: %s, String -> [JavaScriptObject] fix", fieldName));
//                }
//            }
//
//            private void fix(Object arrayGetObject, JavaScriptObject jso, int i, DataSourceField field) {
////                System.out.println("...transformResponse: " + arrayGetObject + ", class: " +arrayGetObject.getClass());
//                if (arrayGetObject instanceof String) {
//                    JavaScriptObject fix = JSOHelper.createObject();
//                    JSOHelper.setAttribute(fix, FIELD_XML_TEXT_CONTENT, arrayGetObject);
//                    JSOHelper.setArrayValue(jso, i, fix);
//                    LOG.fine(ClientUtils.format("fix.field: %s, index: %s, String -> JavaScriptObject fix", field.getName(), i));
//                }
//            }
//        };
////        dsDC.setDataURL("ds/dc.xml");
//        dsDC.setDataURL("ds/dc_drobnustky.xml");
//        dsDC.setXmlNamespaces(XML_NAMESPACES);
//        dsDC.setGlobalNamespaces(XML_NAMESPACE_MAP);
//        dsDC.setRecordXPath("/oai_dc:dc");
////        dsDC.setRecordXPath("/dc");
//        dsDC.setTagName("oai_dc:dc");
//        dsDC.setSendExtraFields(false);
//        dsDC.setDropExtraFields(true);
//
//        dsDC.setFields(
//                createDCDataSourceField(FIELD_TITLE),
//                createDCDataSourceField(FIELD_CREATOR),
//                createDCDataSourceField(FIELD_SUBJECT),
//                createDCDataSourceField(FIELD_DESCRIPTION),
//                createDCDataSourceField(FIELD_PUBLISHER),
//                createDCDataSourceField(FIELD_CONTRIBUTOR),
//                createDCDataSourceField(FIELD_DATE),
//                createDCDataSourceField(FIELD_TYPE),
//                createDCDataSourceField(FIELD_FORMAT),
//                createDCDataSourceField(FIELD_IDENTIFIER),
//                createDCDataSourceField(FIELD_SOURCE),
//                createDCDataSourceField(FIELD_LANGUAGE),
//                createDCDataSourceField(FIELD_RELATION),
//                createDCDataSourceField(FIELD_COVERAGE),
//                createDCDataSourceField(FIELD_RIGHTS)
//        );
//
//        dsDC.fetchData(null, new DSCallback() {
//
//            @Override
//            public void execute(DSResponse response, Object rawData, DSRequest request) {
//                Record r = response.getData()[0];
//                LOG.info(ClientUtils.format("fetched dcRecord: \n%s", ClientUtils.dump(r, "")));
//
//                LOG.info(dsDC.xmlSerialize(r, new SerializationContext()));
//            }
//        });

//        setFields(
//                createDCListFormItem(DcRecordDataSource.FIELD_TITLE, "Titles"),
//                createDCListFormItem(DcRecordDataSource.FIELD_CREATOR, "Creators"),
//                createDCListFormItem(DcRecordDataSource.FIELD_SUBJECT, "Subjects"),
//                createDCListFormItem(DcRecordDataSource.FIELD_DESCRIPTION, "Descriptions"),
//                createDCListFormItem(DcRecordDataSource.FIELD_PUBLISHER, "Publishers"),
//                createDCListFormItem(DcRecordDataSource.FIELD_CONTRIBUTOR, "Contributors"),
//                createDCListFormItem(DcRecordDataSource.FIELD_DATE, "Dates", false),
//                createDCListFormItem(DcRecordDataSource.FIELD_TYPE, "Types"),
//                createDCListFormItem(DcRecordDataSource.FIELD_FORMAT, "Formats"),
//                createDCListFormItem(DcRecordDataSource.FIELD_IDENTIFIER, "Identifiers", false),
//                createDCListFormItem(DcRecordDataSource.FIELD_SOURCE, "Sources"),
//                createDCListFormItem(DcRecordDataSource.FIELD_LANGUAGE, "Languages", false),
//                createDCListFormItem(DcRecordDataSource.FIELD_RELATION, "Relations"),
//                createDCListFormItem(DcRecordDataSource.FIELD_COVERAGE, "Coverage"),
//                createDCListFormItem(DcRecordDataSource.FIELD_RIGHTS, "Rights")
//        );

//        setDataSource(dsDC);
//        fetchData(null, new DSCallback() {
//
//            @Override
//            public void execute(DSResponse response, Object rawData, DSRequest request) {
//                Record r = response.getData()[0];
//                LOG.fine(dumpRecord(r, "fetched DC"));
//
//                editRecord(r);
////                selectRecord(r);
//            }
//        });


        setShowComplexFields(true);
        setShowComplexFieldsRecursively(true);

        // XXX consider usage
//        setLongTextEditorType(id);
        setLongTextEditorThreshold(10);
//        setSynchronousValidation(true)

        RPCRequest request = new RPCRequest();
        request.setBypassCache(true);
////        XMLTools.loadXMLSchema("ds/dc_smartgwt.xsd", new XSDLoadCallback() {
////        XMLTools.loadXMLSchema("ds/dc_oai.xsd", new XSDLoadCallback() {
        XMLTools.loadXMLSchema("ds/sc_oai_dublincore.xsd", new XSDLoadCallback() {

            @Override
            public void execute(SchemaSet schemaSet) {
                schema = schemaSet.getSchema("dc");
                setAttribute("origDataSource", schema.getID(), true);
                final DataSource dcDS = new DataSource();
                final DataSource tempDS = schema;
//                dcDS.setTagName("dc");
                dcDS.setDataURL("ds/sc_oai_dublincore.xml");
                dcDS.setInheritsFrom(schema);
                dcDS.setUseParentFieldOrder(true);

                DataSourceField fieldTitles = new DataSourceField();
                fieldTitles.setName("title");
                fieldTitles.setMultiple(true);

                DataSourceField fieldCreators = new DataSourceField();
                fieldCreators.setName("creator");
                fieldCreators.setMultiple(true);

//                dcDS.setFields(fieldTitles, fieldTitles);
//                dcDS.setXmlNamespaces(XML_NS);
//                dcDS.setRecordXPath("/oai_dc:dc");
                setDataSource(dcDS);
//                setUseAllDataSourceFields(true);
                setFields(
                        createDCListFormItem("title", "Titles", true),
                        createDCListFormItem("identifier", "Identifier", false)
                        );

                fetchData(null, new DSCallback() {

                    @Override
                    public void execute(DSResponse response, Object rawData, DSRequest request) {
                        String[] fieldNames = tempDS.getFieldNames();
                        String[] fieldNames1 = dcDS.getFieldNames();
                        Map globalNamespaces = tempDS.getGlobalNamespaces();
                        String schemaNamespace = tempDS.getSchemaNamespace();
                        String serviceNamespace = tempDS.getServiceNamespace();
//                        tempDS.setXmlNamespaces(null);
                        LOG.fine("XMLSchema.fieldNames: " + Arrays.toString(fieldNames));
                        LOG.fine("XMLSchema.nsmap: " + globalNamespaces);
                        LOG.fine("XMLSchema.schemaNamespace: " + schemaNamespace);
                        LOG.fine("XMLSchema.serviceNamespace: " + serviceNamespace);
                        Record r = response.getData()[0];
                        LOG.info(ClientUtils.dump(r, "After fetch"));
                        LOG.fine("XMLSchema.serialize: " + tempDS.xmlSerialize(r, new SerializationContext()));
//                        LOG.fine(dumpRecord(r, "XMLSchema fetched"));

                        editRecord(r);
                        focus();

//                        selectRecord(r);
//                        getValuesAsRecord();
                    }
                });
            }
        }, request, true);

    }

//    private static DataSourceField createDCDataSourceField(String name) {
//        DataSourceField field = new DataSourceField(name, FieldType.TEXT);
//        field.setTypeAsDataSource(createDataSource(name));
//        field.setRequired(true);
//        field.setAttribute("fixSingleStringValue", true);
//        field.setValueXPath(name);
////        field.setChildTagName("dc:" + name);
////        field.setChildrenProperty(true);
////        field.setMultiple(false);
//        return field;
//    }

    private static ListFormItem createDCListFormItem(String name, String title) {
        return createDCListFormItem(name, title, true);
    }

    private static ListFormItem createDCListFormItem(String name, String title, boolean addLang) {
        ArrayList<ListGridField> fields = new ArrayList<ListGridField>(2);
        // text content field
        ListGridField fieldTextContent = new ListGridField("content", title);
//        fieldTextContent.setRequired(true);
        fields.add(fieldTextContent);

        if (addLang) {
            ComboBoxItem langComboBoxItem = new ComboBoxItem();
            langComboBoxItem.setAddUnknownValues(true);
            langComboBoxItem.setCompleteOnTab(true);
            langComboBoxItem.setBrowserSpellCheck(false);
    //        langComboBoxItem.setDefaultValue("cs");
            langComboBoxItem.setShowAllOptions(true);
            ListGridField fieldLang = new ListGridField("language", "Language");
    //        Map<String, String> langMap = new HashMap<String, String>();
    //        langMap.put("cs", "Czech");
    //        langMap.put("de", "German");
    //        langMap.put("en", "English");
    //        langFormItem.setValueMap(langMap);
            fieldLang.setEditorType(langComboBoxItem);
            fieldLang.setValueMap("cs", "de", "en");
            fieldLang.setWidth(100);
            fieldLang.setDefaultValue("cs");
            fieldLang.setEmptyCellValue("cs - empty");
            fields.add(fieldLang);
        }

        ListFormItem editorTitle = new ListFormItem(name, title);
        editorTitle.setFields(fields.toArray(new ListGridField[fields.size()]));
        editorTitle.setShowTitle(false);
//        editorTitle.setDataPath(name);
        return editorTitle;
    }
//
//    public String dumpRecord(Record r, String msg) {
//        StringBuilder sb = new StringBuilder();
//        sb.append(ClientUtils.format("%s, getAttributes:\n", msg));
////        System.out.println("--dump---- " + msg);
//        for (String attr : r.getAttributes()) {
//            sb.append(ClientUtils.format(" attr: %s\n", attr));
////            System.out.println(">>> attr: " + attr);
//        }
//
//        sb.append("-- toMap:\n");
//        Map m = r.toMap();
//        for (Object entry : m.entrySet()) {
//            Map.Entry e = (Map.Entry) entry;
//            Object value = e.getValue();
//            sb.append(ClientUtils.format(" map.key: %s, value: %s, value class %s\n", e.getKey(), value, value.getClass()));
////            System.out.println(">>> map.key: " + e.getKey() + ", value: " + value + ", value class: " + value.getClass());
//            if (value instanceof List) {
//                List l = (List) value;
//                for (Object valItem : l) {
//                    sb.append(ClientUtils.format("    item.value: %s, value class %s\n", valItem, valItem.getClass()));
////                    System.out.println("  >>> item.value: " + valItem + ", value class: " + valItem.getClass());
//                }
//            }
//        }
//
//        DataSource dcDS = getDataSource();
//        String xml = dcDS.xmlSerialize(r, new SerializationContext());
//        sb.append(" XML:\n").append(xml).append("\n-------\n");
////        System.out.println(">>> xml: " + xml);
////        System.out.println("--------------");
//        return sb.toString();
//    }

}
