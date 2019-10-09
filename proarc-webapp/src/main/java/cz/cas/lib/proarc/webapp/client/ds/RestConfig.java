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

import com.google.gwt.core.client.GWT;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.OperationBinding;
import com.smartgwt.client.rpc.RPCResponse;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.DSProtocol;
import cz.cas.lib.proarc.webapp.shared.rest.BibliographicCatalogResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ConfigurationProfileResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.DeviceResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.LocalizationResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.UserResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ValueMapResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.WorkflowResourceApi;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Jan Pokorsky
 */
public final class RestConfig {

    public static final String TYPE_APPLICATION_JSON = "application/json";
    public static final String TYPE_APPLICATION_XML = "application/xml";

//    public static final String URL_ROOT =  "/rest";
    /** Login servlet dialog */
    public static final String URL_LOGIN_SERVLET =  GWT.getHostPageBaseURL() + "proarclogin";

    public static final String URL_ROOT =  GWT.getHostPageBaseURL() + "rest/v1";
    public static final String URL_IMPORT = path(URL_ROOT, ImportResourceApi.PATH);
    public static final String URL_IMPORT_FOLDER = path(URL_IMPORT, ImportResourceApi.FOLDER_PATH);
    public static final String URL_IMPORT_BATCH = path(URL_IMPORT, ImportResourceApi.BATCH_PATH);
    public static final String URL_IMPORT_BATCH_ITEM = path(URL_IMPORT_BATCH, ImportResourceApi.BATCHITEM_PATH);
    public static final String URL_DIGOBJECT =  path(URL_ROOT, DigitalObjectResourceApi.PATH);
    public static final String URL_DIGOBJECT_ATM =  path(URL_DIGOBJECT, DigitalObjectResourceApi.ATM_PATH);
    public static final String URL_DIGOBJECT_CHILDREN =  path(URL_DIGOBJECT, DigitalObjectResourceApi.MEMBERS_PATH);
    public static final String URL_DIGOBJECT_CHILDREN_MOVE =  path(URL_DIGOBJECT_CHILDREN, DigitalObjectResourceApi.MEMBERS_MOVE_PATH);
    public static final String URL_DIGOBJECT_DC = path(URL_DIGOBJECT, DigitalObjectResourceApi.DC_PATH);
    public static final String URL_DIGOBJECT_DISSEMINATION = path(
            URL_DIGOBJECT, DigitalObjectResourceApi.DISSEMINATION_PATH);
    public static final String URL_DIGOBJECT_FULL = path(URL_DIGOBJECT, DigitalObjectResourceApi.FULL_PATH);
    public static final String URL_DIGOBJECT_METAMODEL =  path(
            URL_DIGOBJECT, DigitalObjectResourceApi.METAMODEL_PATH);
    public static final String URL_DIGOBJECT_MODS = path(URL_DIGOBJECT, DigitalObjectResourceApi.MODS_PATH);
    public static final String URL_DIGOBJECT_MODS_CUSTOM = path(
            URL_DIGOBJECT_MODS, DigitalObjectResourceApi.MODS_CUSTOM_PATH);
    public static final String URL_DIGOBJECT_MODS_PLAIN = path(
            URL_DIGOBJECT_MODS, DigitalObjectResourceApi.MODS_PLAIN_PATH);
    public static final String URL_DIGOBJECT_OCR = path(URL_DIGOBJECT, DigitalObjectResourceApi.OCR_PATH);
    public static final String URL_DIGOBJECT_PREVIEW = path(URL_DIGOBJECT, DigitalObjectResourceApi.PREVIEW_PATH);
    public static final String URL_DIGOBJECT_PRIVATE_NOTE = path(
            URL_DIGOBJECT, DigitalObjectResourceApi.PRIVATENOTE_PATH);
    public static final String URL_DIGOBJECT_RAW = path(URL_DIGOBJECT, DigitalObjectResourceApi.RAW_PATH);
    public static final String URL_DIGOBJECT_SEARCH =  path(URL_DIGOBJECT, DigitalObjectResourceApi.SEARCH_PATH);
    public static final String URL_DIGOBJECT_STREAMPROFILE = path(URL_DIGOBJECT, DigitalObjectResourceApi.STREAMPROFILE_PATH);
    public static final String URL_DIGOBJECT_THUMBNAIL = path(URL_DIGOBJECT, DigitalObjectResourceApi.THUMB_PATH);
    public static final String URL_DIGOBJECT_URNNBN =  path(
            URL_DIGOBJECT, DigitalObjectResourceApi.URNNBN_PATH);
    public static final String URL_DIGOBJECT_COPYOBJECT = path(
            URL_DIGOBJECT, DigitalObjectResourceApi.COPYOBJECT_PATH);
    public static final String URL_LOCALIZATION =  path(URL_ROOT, LocalizationResourceApi.PATH);
    public static final String URL_BIBLIOCATALOG =  path(URL_ROOT, BibliographicCatalogResourceApi.PATH);
    public static final String URL_BIBLIOCATALOG_QUERY =  path(
            URL_BIBLIOCATALOG, BibliographicCatalogResourceApi.FIND_PATH);
    public static final String URL_USER =  path(URL_ROOT, UserResourceApi.PATH);
    public static final String URL_USER_PERMISSIONS =  URL_USER + "/permissions";
    public static final String URL_EXPORT = path(URL_ROOT, ExportResourceApi.PATH);
    public static final String URL_EXPORT_ARCHIVE = path(
            URL_EXPORT, ExportResourceApi.ARCHIVE_PATH);
    public static final String URL_EXPORT_CEJSH = path(
            URL_EXPORT, ExportResourceApi.CEJSH_PATH);
    public static final String URL_EXPORT_CROSSREF = path(
            URL_EXPORT, ExportResourceApi.CROSSREF_PATH);
    public static final String URL_EXPORT_DATASTREAM = path(
            URL_EXPORT, ExportResourceApi.DATASTREAM_PATH);
    public static final String URL_EXPORT_DESA = path(
            URL_EXPORT, ExportResourceApi.DESA_PATH);
    public static final String URL_EXPORT_KRAMERIUS4 = path(
            URL_EXPORT, ExportResourceApi.KRAMERIUS4_PATH);
    public static final String URL_EXPORT_NDK = path(
            URL_EXPORT, ExportResourceApi.NDK_PATH);
    public static final String URL_DEVICE = path(URL_ROOT, DeviceResourceApi.PATH);
    public static final String URL_VALUEMAP = path(URL_ROOT, ValueMapResourceApi.PATH);
    public static final String URL_CONFIGPROFILE = path(URL_ROOT, ConfigurationProfileResourceApi.PATH);
    public static final String URL_WORKFLOW = path(URL_ROOT, WorkflowResourceApi.PATH);
    public static final String URL_WORKFLOW_MATERIAL = path(URL_WORKFLOW, WorkflowResourceApi.MATERIAL_PATH);
    public static final String URL_WORKFLOW_PARAMETER = path(URL_WORKFLOW, WorkflowResourceApi.PARAMETER_PATH);
    public static final String URL_WORKFLOW_PROFILE = path(URL_WORKFLOW, WorkflowResourceApi.PROFILE_PATH);
    public static final String URL_WORKFLOW_TASK = path(URL_WORKFLOW, WorkflowResourceApi.TASK_PATH);
    public static final String URL_WORKFLOW_MODS = path(URL_WORKFLOW, WorkflowResourceApi.MODS_PATH);

    public static DSRequest createRestRequest(DSDataFormat format) {
        DSRequest dsr = new DSRequest();
        dsr.setUseSimpleHttp(true);
        Map<String, String> defaultHeaders = new HashMap<String, String>();
        switch (format) {
            case XML:
                defaultHeaders.put("Accept", TYPE_APPLICATION_XML);
                break;
            case JSON:
                defaultHeaders.put("Accept", TYPE_APPLICATION_JSON);
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
     * Helper for RESTful PUT method with parameters sent as JSON in the request body.
     * <p>Requires implementation of {@link com.smartgwt.client.data.DataSource#transformRequest}.
     *
     * @return update operation
     */
    public static OperationBinding createUpdatePostOperation() {
        OperationBinding op = new OperationBinding();
        op.setOperationType(DSOperationType.UPDATE);
        op.setDataProtocol(DSProtocol.POSTMESSAGE);
        DSRequest dsRequest = new DSRequest();
        dsRequest.setHttpMethod("PUT");
        dsRequest.setContentType(TYPE_APPLICATION_JSON);
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
        // httpStatus == null returns locally cached data sources
        return status == RPCResponse.STATUS_SUCCESS && (httpStatus == null || httpStatus == 200);
    }

    public static boolean isConcurrentModification(DSResponse response) {
        return response.getHttpResponseCode() == 409;
    }

    private static String path(String parent, String path) {
        return parent + '/' + path;
    }

}
