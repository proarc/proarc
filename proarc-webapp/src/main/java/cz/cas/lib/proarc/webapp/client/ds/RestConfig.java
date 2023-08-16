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
import cz.cas.lib.proarc.webapp.shared.rest.ApplicationResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.AuthorityCatalogResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.BibliographicCatalogResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ConfigurationProfileResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.DeviceResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.IndexerResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.LocalizationResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.UrnNbnResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.UserResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ValueMapResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.WorkflowResourceApi;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Jan Pokorsky
 */
public final class RestConfig {

    public static final String TYPE_APPLICATION_JSON = "application/json";
    public static final String TYPE_APPLICATION_XML = "application/xml";

//    public static final String URL_ROOT =  "/rest";
    /**
     * Login servlet dialog
     */
    public static final String URL_LOGIN_SERVLET =  GWT.getHostPageBaseURL() + "proarclogin";

    public static final String URL_API_VERSION_1 = "v1";
    public static final String URL_API_VERSION_2 = "v2";

    public static final String URL_ROOT = GWT.getHostPageBaseURL() + "rest/" + URL_API_VERSION_1;
    public static final String URL_IMPORT = path(URL_ROOT, ImportResourceApi.PATH);
    public static final String URL_IMPORT_FOLDER = path(URL_IMPORT, ImportResourceApi.FOLDER_PATH);
    public static final String URL_IMPORT_BATCH = path(URL_IMPORT, ImportResourceApi.BATCH_PATH);
    public static final String URL_IMPORT_BATCHES = path(URL_IMPORT, ImportResourceApi.BATCHES_PATH);
    public static final String URL_IMPORT_BATCH_ITEM = path(URL_IMPORT_BATCH, ImportResourceApi.BATCHITEM_PATH);
    public static final String URL_DIGOBJECT = path(URL_ROOT, DigitalObjectResourceApi.PATH);
    public static final String URL_INDEXER = path(URL_ROOT, IndexerResourceApi.PATH);
    public static final String URL_DIGOBJECT_ATM = path(URL_DIGOBJECT, DigitalObjectResourceApi.ATM_PATH);
    public static final String URL_DIGOBJECT_CHILDREN = path(URL_DIGOBJECT, DigitalObjectResourceApi.MEMBERS_PATH);
    public static final String URL_DIGOBJECT_CHILDREN_MOVE = path(URL_DIGOBJECT_CHILDREN, DigitalObjectResourceApi.MEMBERS_MOVE_PATH);
    public static final String URL_DIGOBJECT_DC = path(URL_DIGOBJECT, DigitalObjectResourceApi.DC_PATH);
    public static final String URL_DIGOBJECT_DISSEMINATION = path(
            URL_DIGOBJECT, DigitalObjectResourceApi.DISSEMINATION_PATH);
    public static final String URL_DIGOBJECT_FULL = path(URL_DIGOBJECT, DigitalObjectResourceApi.FULL_PATH);
    public static final String URL_DIGOBJECT_METAMODEL = path(
            URL_DIGOBJECT, DigitalObjectResourceApi.METAMODEL_PATH);
    public static final String URL_DIGOBJECT_MODS = path(URL_DIGOBJECT, DigitalObjectResourceApi.MODS_PATH);
    public static final String URL_DIGOBJECT_MODS_CUSTOM = path(
            URL_DIGOBJECT_MODS, DigitalObjectResourceApi.MODS_CUSTOM_PATH);
    public static final String URL_DIGOBJECT_MODS_ADD_AUTHORITY = path(URL_DIGOBJECT_MODS,
            DigitalObjectResourceApi.MODS_ADD_AUTHORITY);

    public static final String URL_DIGOBJECT_MODS_PLAIN = path(
            URL_DIGOBJECT_MODS, DigitalObjectResourceApi.MODS_PLAIN_PATH);
    public static final String URL_DIGOBJECT_OCR = path(URL_DIGOBJECT, DigitalObjectResourceApi.OCR_PATH);
    public static final String URL_DIGOBJECT_PREVIEW = path(URL_DIGOBJECT, DigitalObjectResourceApi.PREVIEW_PATH);
    public static final String URL_DIGOBJECT_PRIVATE_NOTE = path(
            URL_DIGOBJECT, DigitalObjectResourceApi.PRIVATENOTE_PATH);
    public static final String URL_DIGOBJECT_TECHNICAL_METADATA_AES = path(URL_DIGOBJECT, DigitalObjectResourceApi.TECHNICALMETADATA_AES_PATH);
    public static final String URL_DIGOBJECT_TECHNICAL_METADATA_XML_AES = path(URL_DIGOBJECT, DigitalObjectResourceApi.TECHNICALMETADATA_XML_AES_PATH);
    public static final String URL_DIGOBJECT_TECHNICAL_METADATA_CODING_HISTORY = path(URL_DIGOBJECT, DigitalObjectResourceApi.TECHNICALMETADATA_CODING_HISTORY_PATH);
    public static final String URL_DIGOBJECT_TECHNICAL_METADATA_XML_CODING_HISTORY = path(URL_DIGOBJECT, DigitalObjectResourceApi.TECHNICALMETADATA_XML_CODING_HISTORY_PATH);
    public static final String URL_DIGOBJECT_TECHNICAL_METADATA_PREMIS = path(URL_DIGOBJECT, DigitalObjectResourceApi.TECHNICALMETADATA_PREMIS_PATH);
    public static final String URL_DIGOBJECT_TECHNICAL_METADATA_XML_PREMIS = path(URL_DIGOBJECT, DigitalObjectResourceApi.TECHNICALMETADATA_XML_PREMIS_PATH);
    public static final String URL_DIGOBJECT_RAW = path(URL_DIGOBJECT, DigitalObjectResourceApi.RAW_PATH);
    public static final String URL_DIGOBJECT_SEARCH = path(URL_DIGOBJECT, DigitalObjectResourceApi.SEARCH_PATH);
    public static final String URL_DIGOBJECT_STREAMPROFILE = path(URL_DIGOBJECT, DigitalObjectResourceApi.STREAMPROFILE_PATH);
    public static final String URL_DIGOBJECT_THUMBNAIL = path(URL_DIGOBJECT, DigitalObjectResourceApi.THUMB_PATH);
    public static final String URL_DIGOBJECT_URNNBN = path(
            URL_DIGOBJECT, DigitalObjectResourceApi.URNNBN_PATH);
    public static final String URL_URNNBN_RESOLVER = path(URL_ROOT, UrnNbnResourceApi.PATH);
    public static final String URL_DIGOBJECT_COPYOBJECT = path(
            URL_DIGOBJECT, DigitalObjectResourceApi.COPYOBJECT_PATH);
    public static final String URL_DIGOBJECT_LOCK_OBJECT = path(URL_DIGOBJECT, DigitalObjectResourceApi.LOCK_OBJECT_PATH);
    public static final String URL_DIGOBJECT_UNLOCK_OBJECT = path(URL_DIGOBJECT, DigitalObjectResourceApi.UNLOCK_OBJECT_PATH);
    public static final String URL_DIGOBJEKT_REINDEX_OBJECTS = path(
            URL_DIGOBJECT, DigitalObjectResourceApi.REINDEX_PATH);
    public static final String URL_DIGOBJEKT_UPDATE_ALL_OBJECTS = path(
            URL_DIGOBJECT, DigitalObjectResourceApi.UPDATE_ALL_OBJECTS_PATH);
    public static final String URL_GENERATE_JP2 = path(URL_DIGOBJECT, DigitalObjectResourceApi.GENERATE_JP2_PATH);
    public static final String URL_CHANGE_PAGE_TO_NDK_PAGE = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_PAGE_TO_NDK_PAGE);
    public static final String URL_CHANGE_STT_PAGE_TO_NDK_PAGE = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_STT_PAGE_TO_NDK_PAGE);
    public static final String URL_CHANGE_NDK_PAGE_TO_STT_PAGE = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_NDK_PAGE_TO_STT_PAGE);
    public static final String URL_CHANGE_NDK_PAGE_TO_PAGE = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_NDK_PAGE_TO_PAGE);
    public static final String URL_CHANGE_PAGE_TO_STT_PAGE = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_PAGE_TO_STT_PAGE);
    public static final String URL_CHANGE_STT_PAGE_TO_PAGE = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_STT_PAGE_TO_PAGE);
    public static final String URL_CHANGE_CLIPPINGS_VOLUME_TO_NDK_MONOGRAPH_VOLUME = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_CLIPPINGS_VOLUME_TO_NDK_MONOGRAPH_VOLUME);
    public static final String URL_CHANGE_NDK_MONOGRAPH_VOLUME_TO_CLIPPINGS_VOLUME = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_VOLUME_TO_CLIPPINGS_VOLUME);
    public static final String URL_CHANGE_NDK_MONOGRAPH_VOLUME_TO_NDK_MONOGRAPH_TITLE = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_VOLUME_TO_NDK_MONOGRAPH_TITLE);
    public static final String URL_CHANGE_CLIPPINGS_TITLE_TO_NDK_MONOGRAPH_TITLE = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_CLIPPINGS_TITLE_TO_NDK_MONOGRAPH_TITLE);
    public static final String URL_CHANGE_NDK_MONOGRAPH_TITLE_TO_CLIPPINGS_TITLE = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_TITLE_TO_CLIPPINGS_TITLE);
    public static final String URL_CHANGE_NDK_MONOGRAPH_TITLE_TO_NDK_MONOGRAPH_VOLUME = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_TITLE_TO_NDK_MONOGRAPH_VOLUME);
    public static final String URL_CHANGE_K4_PERIODICAL_TO_NDK_PERIODICAL = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_K4_PERIODICAL_TO_NDK_PERIODICAL);
    public static final String URL_CHANGE_K4_PERIODICAL_VOLUME_TO_NDK_PERIODICAL_VOLUME = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_K4_PERIODICAL_VOLUME_TO_NDK_PERIODICAL_VOLUME);
    public static final String URL_CHANGE_K4_PERIODICAL_ISSUE_TO_NDK_PERIODICAL_ISSUE = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_K4_PERIODICAL_ISSUE_TO_NDK_PERIODICAL_ISSUE);
    public static final String URL_CHANGE_K4_MONOGRAPH_TO_NDK_MONOGRAPH_VOLUME = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_K4_MONOGRAPH_TO_NDK_MONOGRAPH_VOLUME);
    public static final String URL_CHANGE_K4_MONOGRAPH_UNIT_TO_NDK_MONOGRAPH_VOLUME = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_K4_MONOGRAPH_UNIT_TO_NDK_MONOGRAPH_VOLUME);
    public static final String URl_CHANGE_NDK_MUSICSHEET_TO_STT_MUSICSHEET = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_NDK_MUSICSHEET_TO_STT_MUSICSHEET);
    public static final String URl_CHANGE_STT_MUSICSHEET_TO_NDK_MUSICSHEET = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_STT_MUSICSHEET_TO_NDK_MUSICSHEET);
    public static final String URl_CHANGE_STT_CHAPTER_TO_NDK_CHAPTER = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_STT_CHAPTER_TO_NDK_CHAPTER);
    public static final String URl_CHANGE_NDK_CHAPTER_TO_STT_CHAPTER = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_NDK_CHAPTER_TO_STT_CHAPTER);
    public static final String URl_CHANGE_NDK_PICTURE_TO_STT_GRAPHIC = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_NDK_PICTURE_TO_STT_GRAPHIC);
    public static final String URl_CHANGE_STT_GRAPHIC_TO_NDK_PICTURE = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_STT_GRAPHIC_TO_NDK_PICTURE);
    public static final String URl_CHANGE_NDK_CARTOGRAPHIC_TO_STT_CARTOGRAPHIC = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_NDK_CARTOGRAPHIC_TO_STT_CARTOGRAPHIC);
    public static final String URl_CHANGE_STT_CARTOGRAPHIC_TO_NDK_CARTOGRAPHIC = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_STT_CARTOGRAPHIC_TO_NDK_CARTOGRAPHIC);
    public static final String URl_CHANGE_NDK_MONOGRAPH_TO_STT_MONOGRAPH = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_TO_STT_MONOGRAPH);
    public static final String URl_CHANGE_STT_MONOGRAPH_TO_NDK_MONOGRAPH = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_STT_MONOGRAPH_TO_NDK_MONOGRAPH);
    public static final String URl_CHANGE_NDK_SUPPLEMENT_TO_STT_SUPPLEMENT = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_NDK_SUPPLEMENT_TO_STT_SUPPLEMENT);
    public static final String URl_CHANGE_STT_SUPPLEMENT_TO_NDK_SUPPLEMENT = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_STT_SUPPLEMENT_TO_NDK_SUPPLEMENT);
    public static final String URl_CHANGE_STT_GRAPHIC_TO_STT_MONOGRAPH_VOLUME = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_STT_GRAPHIC_TO_STT_MONOGRAPH_VOLUME);
    public static final String URL_CHANGE_STT_MONOGRAPH_TO_STT_MUSICSHEET = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_STT_MONOGRAPH_TO_STT_MUSICSHEET);
    public static final String URL_CHANGE_STT_MONOGRAPH_TO_STT_GRAPHIC = path(URL_DIGOBJECT, DigitalObjectResourceApi.CHANGE_STT_MONOGRAPH_TO_STT_GRAPHIC);
    public static final String URL_UPDATE_NDK_ARTICLE = path(URL_DIGOBJECT, DigitalObjectResourceApi.UPDATE_NDK_ARTICLE);
    public static final String URL_UPDATE_NDK_PAGE = path(URL_DIGOBJECT, DigitalObjectResourceApi.UPDATE_NDK_PAGE);
    public static final String URL_LOCALIZATION = path(URL_ROOT, LocalizationResourceApi.PATH);
    public static final String URL_BIBLIOCATALOG = path(URL_ROOT, BibliographicCatalogResourceApi.PATH);
    public static final String URL_BIBLIOCATALOG_QUERY = path(
            URL_BIBLIOCATALOG, BibliographicCatalogResourceApi.FIND_PATH);
    public static final String URL_AUTHORITY_CATALOG = path(URL_ROOT, AuthorityCatalogResourceApi.PATH);
    public static final String URL_AUTHORITYCATALOG_QUERY =  path(
            URL_AUTHORITY_CATALOG, AuthorityCatalogResourceApi.FIND_PATH);
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
    public static final String URL_EXPORT_KWIS = path(
            URL_EXPORT, ExportResourceApi.KWIS_PATH);
    public static final String URL_DEVICE = path(URL_ROOT, DeviceResourceApi.PATH);
    public static final String URL_APPLICATION_INFO = path(URL_ROOT, ApplicationResourceApi.PATH);
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
        dsRequest.setContentType(RestConfig.TYPE_APPLICATION_JSON);
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
