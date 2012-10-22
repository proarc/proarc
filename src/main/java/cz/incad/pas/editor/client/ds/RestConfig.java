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

import com.google.gwt.core.client.GWT;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.OperationBinding;
import com.smartgwt.client.rpc.RPCResponse;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.DSProtocol;
import cz.incad.pas.editor.shared.rest.BibliographicCatalogResourceApi;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;
import cz.incad.pas.editor.shared.rest.ExportResourceApi;
import cz.incad.pas.editor.shared.rest.LocalizationResourceApi;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Jan Pokorsky
 */
public final class RestConfig {

//    public static final String URL_ROOT =  "/rest";
    public static final String URL_ROOT =  GWT.getHostPageBaseURL() + "rest";
    public static final String URL_SCAN_IMPORT =  URL_ROOT + "/import";
    public static final String URL_IMPORT_BATCH =  URL_SCAN_IMPORT + "/batch";
    public static final String URL_IMPORT_BATCH_ITEM =  URL_IMPORT_BATCH + "/item";
    public static final String URL_DIGOBJECT =  URL_ROOT + "/object";
    public static final String URL_DIGOBJECT_CHILDREN =  URL_DIGOBJECT + "/members";
    public static final String URL_DIGOBJECT_DC =  URL_DIGOBJECT + "/dc";
    public static final String URL_DIGOBJECT_DISSEMINATION =  URL_DIGOBJECT + "/dissemination";
    public static final String URL_DIGOBJECT_FULL =  URL_DIGOBJECT + "/full";
    public static final String URL_DIGOBJECT_MODS =  URL_DIGOBJECT + "/mods";
    public static final String URL_DIGOBJECT_MODS_CUSTOM =  URL_DIGOBJECT_MODS + "/custom";
    public static final String URL_DIGOBJECT_MODS_PLAIN =  URL_DIGOBJECT_MODS + "/plain";
    public static final String URL_DIGOBJECT_OCR =  URL_DIGOBJECT + "/ocr";
    public static final String URL_DIGOBJECT_PREVIEW =  URL_DIGOBJECT + "/preview";
    public static final String URL_DIGOBJECT_PRIVATE_NOTE =  URL_DIGOBJECT + "/privatenote";
    public static final String URL_DIGOBJECT_RAW =  URL_DIGOBJECT + "/raw";
    public static final String URL_DIGOBJECT_SEARCH =  path(URL_DIGOBJECT, DigitalObjectResourceApi.SEARCH_PATH);
    public static final String URL_DIGOBJECT_THUMBNAIL =  URL_DIGOBJECT + "/thumb";
    public static final String URL_DIGOBJECT_METAMODEL =  path(
            URL_DIGOBJECT, DigitalObjectResourceApi.METAMODEL_PATH);
    public static final String URL_LOCALIZATION =  path(URL_ROOT, LocalizationResourceApi.PATH);
    public static final String URL_BIBLIOCATALOG =  path(URL_ROOT, BibliographicCatalogResourceApi.PATH);
    public static final String URL_BIBLIOCATALOG_QUERY =  path(
            URL_BIBLIOCATALOG, BibliographicCatalogResourceApi.FIND_PATH);
    public static final String URL_USER =  URL_ROOT + "/user";
    public static final String URL_USER_PERMISSIONS =  URL_USER + "/permissions";
    public static final String URL_EXPORT = path(URL_ROOT, ExportResourceApi.PATH);
    public static final String URL_EXPORT_DATASTREAM = path(
            URL_EXPORT, ExportResourceApi.DATASTREAM_PATH);
    public static final String URL_EXPORT_KRAMERIUS4 = path(
            URL_EXPORT, ExportResourceApi.KRAMERIUS4_PATH);

    public static DSRequest createRestRequest(DSDataFormat format) {
        DSRequest dsr = new DSRequest();
        dsr.setUseSimpleHttp(true);
        Map<String, String> defaultHeaders = new HashMap<String, String>();
        switch (format) {
            case XML:
                defaultHeaders.put("Accept", "application/xml");
                break;
            case JSON:
                defaultHeaders.put("Accept", "application/json");
                break;
        }
        defaultHeaders.put("Accept-Language", LanguagesDataSource.activeLocale());
        dsr.setHttpHeaders(defaultHeaders);
        return dsr;
    }

    /**
     * Helper for RESTful POST method.
     *
     * @return add operation
     */
    public static OperationBinding createAddOperation() {
        OperationBinding op = new OperationBinding();
        op.setOperationType(DSOperationType.ADD);
        op.setDataProtocol(DSProtocol.POSTPARAMS);
        DSRequest dsRequest = new DSRequest();
        dsRequest.setHttpMethod("POST");
        op.setRequestProperties(dsRequest);
        return op;
    }

    /**
     * Helper for RESTful PUT method.
     *
     * @return update operation
     */
    public static OperationBinding createUpdateOperation() {
        OperationBinding op = new OperationBinding();
        op.setOperationType(DSOperationType.UPDATE);
        op.setDataProtocol(DSProtocol.POSTPARAMS);
        DSRequest dsRequest = new DSRequest();
        dsRequest.setHttpMethod("PUT");
        op.setRequestProperties(dsRequest);
        return op;
    }

    /**
     * Helper for RESTful DELETE method.
     *
     * @return remove operation
     */
    public static OperationBinding createDeleteOperation() {
        OperationBinding op = new OperationBinding();
        op.setOperationType(DSOperationType.REMOVE);
        op.setDataProtocol(DSProtocol.GETPARAMS);
        DSRequest dsRequest = new DSRequest();
        dsRequest.setHttpMethod("DELETE");
        op.setRequestProperties(dsRequest);
        return op;
    }

    /**
     * Helper to get REST response status.
     */
    public static boolean isStatusOk(DSResponse response) {
        int status = response.getStatus();
        Integer httpStatus = response.getHttpResponseCode();
        return status == RPCResponse.STATUS_SUCCESS && httpStatus == 200;
    }

    private static String path(String parent, String path) {
        return parent + '/' + path;
    }

}
