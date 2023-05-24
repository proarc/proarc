/*
 * Copyright (C) 2023 Lukas Sykora
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
package cz.cas.lib.proarc.webapp.server.rest.v2;

import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.fedora.AtmEditor.AtmItem;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.SearchViewItem;
import cz.cas.lib.proarc.common.fedora.StringEditor.StringRecord;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.server.rest.AnnotatedMetaModel;
import cz.cas.lib.proarc.webapp.server.rest.LocalDateParam;
import cz.cas.lib.proarc.webapp.server.rest.ProArcRequest;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.DigitalObjectResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi.SearchSort;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi.SearchType;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import java.io.InputStream;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;
import org.glassfish.jersey.media.multipart.FormDataBodyPart;
import org.glassfish.jersey.media.multipart.FormDataContentDisposition;
import org.glassfish.jersey.media.multipart.FormDataParam;

/**
 * Resource to manage digital objects.
 * <p>
 * /object/{pid}/ GET - read DigObjDesc:{pid, displayname, date, owner};
 * /object/ GET - lists all DigObjDesc
 * /object/{pid}/foxml
 * /object/{pid}/scan
 * /object/{pid}/preview
 * /object/{pid}/thumb
 * /object/{pid}/ocr
 * /object/{pid}/metadata
 * /object/{pid}/relations
 * /object/metamodel/ GET - lists model:{pid, displayname, type:(TOP|LEAF)}
 *
 * @author Lukas Sykora
 */

@Path(RestConfig.URL_API_VERSION_2 + "/" + DigitalObjectResourceApi.PATH)
public class DigitalObjectResource extends DigitalObjectResourceV1 {

    private static final Logger LOG = Logger.getLogger(DigitalObjectResource.class.getName());

    public DigitalObjectResource(
            @Context Request request,
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest
    ) throws AppConfigurationException {
        super(request, securityCtx, httpHeaders, uriInfo, httpRequest);
    }

    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> newObject(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parentPid,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_DATE_FROM_PARAM) LocalDateParam seriesDateFrom,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_DATE_TO_PARAM) LocalDateParam seriesDateTo,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_DAYS_INCLUDED_PARAM) List<Integer> seriesDaysIncluded,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_PARTNUMBER_FROM_PARAM) Integer seriesPartNumberFrom,
            @FormParam(DigitalObjectResourceApi.NEWOBJECT_XML_PARAM) String xmlMetadata,
            @FormParam(WorkflowModelConsts.PARAMETER_JOBID) BigDecimal workflowJobId,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CATALOGID) String catalogId,
            @DefaultValue("true") @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CREATE_OBJECT) boolean createObject,
            @DefaultValue("true") @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_VALIDATE_OBJECT) boolean validation
    ) {
        try {
            return super.newObject(modelId, pid, parentPid, seriesDateFrom, seriesDateTo, seriesDaysIncluded,
                    seriesPartNumberFrom, xmlMetadata, workflowJobId, catalogId, createObject, validation);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @DELETE
    @Consumes({MediaType.APPLICATION_FORM_URLENCODED})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DigitalObject> deleteObject(
            @QueryParam(DigitalObjectResourceApi.DELETE_PID_PARAM) List<String> pids,
            @QueryParam(DigitalObjectResourceApi.DELETE_HIERARCHY_PARAM)
            @DefaultValue("true") boolean hierarchy,
            @QueryParam(DigitalObjectResourceApi.DELETE_PURGE_PARAM)
            @DefaultValue("false") boolean purge,
            @QueryParam(DigitalObjectResourceApi.DELETE_RESTORE_PARAM)
            @DefaultValue("false") boolean restore
    ) {
        try {
            return super.deleteObject(pids, hierarchy, purge, restore);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @DELETE
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DigitalObject> deleteObject(
            ProArcRequest.DeleteObjectRequest deleteObjectRequest,
            @QueryParam(DigitalObjectResourceApi.DELETE_PID_PARAM) List<String> pids,
            @QueryParam(DigitalObjectResourceApi.DELETE_HIERARCHY_PARAM)
            @DefaultValue("true") boolean hierarchy,
            @QueryParam(DigitalObjectResourceApi.DELETE_PURGE_PARAM)
            @DefaultValue("false") boolean purge,
            @QueryParam(DigitalObjectResourceApi.DELETE_RESTORE_PARAM)
            @DefaultValue("false") boolean restore) {
        try {
            return super.deleteObject(deleteObjectRequest, pids, hierarchy, purge, restore);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.SEARCH_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> search(
            @QueryParam(DigitalObjectResourceApi.SEARCH_OWNER_PARAM) String owner,
            @DefaultValue(SearchType.DEFAULT)
            @QueryParam(DigitalObjectResourceApi.SEARCH_TYPE_PARAM) SearchType type,
            @QueryParam(DigitalObjectResourceApi.SEARCH_PID_PARAM) List<String> pids,
            @QueryParam(DigitalObjectResourceApi.SEARCH_BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.SEARCH_PHRASE_PARAM) String phrase,
            @QueryParam(DigitalObjectResourceApi.SEARCH_QUERY_CREATOR_PARAM) String queryCreator,
            @QueryParam(DigitalObjectResourceApi.SEARCH_QUERY_IDENTIFIER_PARAM) String queryIdentifier,
            @QueryParam(DigitalObjectResourceApi.SEARCH_QUERY_LABEL_PARAM) String queryLabel,
            @QueryParam(DigitalObjectResourceApi.SEARCH_QUERY_MODEL_PARAM) String queryModel,
            @QueryParam(DigitalObjectResourceApi.SEARCH_QUERY_TITLE_PARAM) String queryTitle,
            @QueryParam(DigitalObjectResourceApi.SEARCH_STATUS_PARAM) String queryStatus,
            @QueryParam(DigitalObjectResourceApi.SEACH_ORGANIZATION_PARAM) String queryOrganization,
            @QueryParam(DigitalObjectResourceApi.SEARCH_PROCESSOR_PARAM) String queryProcessor,
            @QueryParam(DigitalObjectResourceApi.SEARCH_START_ROW_PARAM) int startRow,
            @DefaultValue(SearchSort.DEFAULT_DESC)
            @QueryParam(DigitalObjectResourceApi.SEARCH_SORT_PARAM) SearchSort sort,
            @QueryParam(DigitalObjectResourceApi.SEARCH_SORT_FIELD_PARAM) String sortField
    ) {
        try {
            return super.search(owner, type, pids, batchId, phrase, queryCreator, queryIdentifier, queryLabel, queryModel,
                    queryTitle, queryStatus, queryOrganization, queryProcessor, startRow, sort, sortField);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.MEMBERS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> findMembers(
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parent,
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ROOT_PARAM) String root
    ) {
        try {
            return super.findMembers(parent, root);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.MEMBERS_PATH)
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> setMembers(
            ProArcRequest.SetMemberRequest request
    ) {
        try {
            return super.setMembers(request);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.MEMBERS_PATH)
    @Consumes({MediaType.APPLICATION_FORM_URLENCODED})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> setMembers(
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parentPid,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PID) List<String> toSetPids
    ) {
        try {
            return super.setMembers(parentPid, batchId, toSetPids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }

    }

    @POST
    @Path(DigitalObjectResourceApi.MEMBERS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> addMembers(
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parentPid,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PID) List<String> toAddPids,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId
    ) {
        try {
            return super.addMembers(parentPid, toAddPids, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @DELETE
    @Path(DigitalObjectResourceApi.MEMBERS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> deleteMembers(
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parentPid,
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ITEM_PID) List<String> toRemovePids,
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId
    ) {
        try {
            return super.deleteMembers(parentPid, toRemovePids, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.MEMBERS_PATH + '/' + DigitalObjectResourceApi.MEMBERS_MOVE_PATH)
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> moveMembers(
            ProArcRequest.MoveMembersRequest request
    ) {
        try {
            return super.moveMembers(request);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.MEMBERS_PATH + '/' + DigitalObjectResourceApi.MEMBERS_MOVE_PATH)
    @Consumes({MediaType.APPLICATION_FORM_URLENCODED})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> moveMembers(
            @FormParam(DigitalObjectResourceApi.MEMBERS_MOVE_SRCPID) String srcParentPid,
            @FormParam(DigitalObjectResourceApi.MEMBERS_MOVE_DSTPID) String dstParentPid,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PID) List<String> movePids
    ) {
        try {
            return super.moveMembers(srcParentPid, dstParentPid, batchId, movePids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.DC_PATH)
    @Produces(MediaType.APPLICATION_XML)
    public DublinCoreRecord getDublinCore(
            @QueryParam(DigitalObjectResourceApi.DUBLINCORERECORD_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.DUBLINCORERECORD_BATCHID) Integer batchId
    ) {
        try {
            return super.getDublinCore(pid, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return new DublinCoreRecord();
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.CREATE_PID_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord createPid(
    ) {
        try {
            return super.createPid();
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return new StringRecord(t);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.DC_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public DublinCoreRecord getDublinCoreJson(
            @QueryParam(DigitalObjectResourceApi.DUBLINCORERECORD_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.DUBLINCORERECORD_BATCHID) Integer batchId
    ) {
        try {
            return super.getDublinCoreJson(pid, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return new DublinCoreRecord();
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.DC_PATH)
    @Consumes({MediaType.TEXT_XML, MediaType.APPLICATION_XML})
    @Produces(MediaType.APPLICATION_XML)
    public DublinCoreRecord updateDublinCore(DublinCoreRecord update
    ) {
        try {
            return super.updateDublinCore(update);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return new DublinCoreRecord();
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<DescriptionMetadata<Object>> getDescriptionMetadata(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId
    ) {
        try {
            return super.getDescriptionMetadata(pid, batchId, editorId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DescriptionMetadata<Object>> updateDescriptionMetadata(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMJSONDATA) String jsonData,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMXMLDATA) String xmlData,
            @FormParam(WorkflowModelConsts.PARAMETER_JOBID) BigDecimal jobId,
            @FormParam(MetaModelDataSource.FIELD_MODELOBJECT) String model,
            @DefaultValue("false")
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_IGNOREVALIDATION) boolean ignoreValidation,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_STANDARD) String standard
    ) {
        try {
            return super.updateDescriptionMetadata(pid, batchId, editorId, timestamp, jsonData, xmlData, jobId, model, ignoreValidation, standard);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_VALIDATE_OBJECT_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DescriptionMetadata<Object>> validateObject(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) {
        try {
            return super.validateObject(pid, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_ADD_AUTHORITY)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DescriptionMetadata<Object>> addAuthority(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMJSONDATA) String jsonData,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId
    ) {
        try {
            return super.addAuthority(pid, batchId, timestamp, jsonData, editorId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_EDITOR_PAGES)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DescriptionMetadata<Object>> updateDescriptionMetadataPages(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PIDS) String pidsArray,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_APPLY_TO) String applyTo,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_APPLY_TO_FIRST_PAGE) String applyToFirstPage,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_NUMBER_PREFIX) String prefix,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_NUMBER_SUFFIX) String suffix,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_USE_BRACKETS) @DefaultValue("false") String useBrackets,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_NUMBER_SEQUENCE_TYPE) String sequenceType,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_NUMBER_START_NUMBER) String startNumber,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_NUMBER_INCREMENT_NUMBER) String incrementNumber,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_INDEX_START_NUMBER) String startIndex,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_TYPE_PAGE) String pageType,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_DOUBLE_COLUMNS) String doubleColumns,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_PAGE_POSITION) String pagePosition,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId
    ) {
        try {
            return super.updateDescriptionMetadataPages(pidsArray, applyTo, applyToFirstPage, prefix, suffix, useBrackets,
                    sequenceType, startNumber, incrementNumber, startIndex, pageType, doubleColumns, pagePosition, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_EDITOR_PAGES_COPY_METADATA)
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DescriptionMetadata<Object>> copyDescriptionMetadataToPages(
            ProArcRequest.CopyPagesMetadataRequest request
    ) {
        try {
            return super.copyDescriptionMetadataToPages(request);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_EDITOR_PAGES_COPY_METADATA)
    @Consumes({MediaType.APPLICATION_FORM_URLENCODED})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DescriptionMetadata<Object>> copyDescriptionMetadataToPages(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SOURCE_PIDS) List<String> sourcePids,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_DESTINATION_PIDS) List<String> destinationPids,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_NUMBER) Boolean copyPageNumber,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_TYPE) Boolean copyPageType,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_INDEX) Boolean copyPageIndex,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_POSITION) Boolean copyPagePosition,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId
    ) {
        try {
            return super.copyDescriptionMetadataToPages(sourcePids, destinationPids, copyPageNumber, copyPageType, copyPageIndex, copyPagePosition, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_FUNCTION_ADD_BRACKETS)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> updatePagesAddBrackets(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PIDS) String pidsArray,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId
    ) {
        try {
            return super.updatePagesAddBrackets(pidsArray, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_FUNCTION_REMOVE_BRACKETS)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> updatePagesRemoveBrackets(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PIDS) String pidsArray,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId
    ) {
        try {
            return super.updatePagesRemoveBrackets(pidsArray, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.METAMODEL_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<AnnotatedMetaModel> listModels(
    ) {
        try {
            return super.listModels();
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.STREAMPROFILE_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DatastreamResult> getStreamProfile(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.STREAMPROFILE_ID) String dsId
    ) {
        try {
            return super.getStreamProfile(pid, batchId, dsId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.PREVIEW_PATH)
    @Produces("*/*")
    public Response getPreview(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) {
        try {
            return super.getPreview(pid, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return Response.serverError().entity(t).build();
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.FULL_PATH)
    @Produces("*/*")
    public Response getFull(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) {
        try {
            return super.getFull(pid, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return Response.serverError().entity(t).build();
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.RAW_PATH)
    @Produces("*/*")
    public Response getRaw(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) {
        try {
            return super.getRaw(pid, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return Response.serverError().entity(t).build();
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.DISSEMINATION_PATH)
    @Produces("*/*")
    public Response getDissemination(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.DISSEMINATION_DATASTREAM) String dsId
    ) {
        try {
            return super.getDissemination(pid, batchId, dsId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return Response.serverError().entity(t).build();
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.DISSEMINATION_PATH)
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Map<String, Object>> updateDissemination(
            @FormDataParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormDataParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_DATASTREAM) String dsId,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_FILE) InputStream file,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_FILE) FormDataContentDisposition fileInfo,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_FILE) FormDataBodyPart fileBodyPart,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_MIME) String mimeType,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_ERROR) @DefaultValue("false") boolean jsonErrors
    ) {
        try {
            return super.updateDissemination(pid, batchId, dsId, file, fileInfo, fileBodyPart, mimeType, jsonErrors);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @DELETE
    @Path(DigitalObjectResourceApi.DISSEMINATION_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse deleteDissemination(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.DISSEMINATION_DATASTREAM) String dsId
    ) {
        try {
            return super.deleteDissemination(pid, batchId, dsId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.THUMB_PATH)
    @Produces("image/*")
    public Response getThumbnail(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) {
        try {
            return super.getThumbnail(pid, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return Response.serverError().entity(t).build();
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_PLAIN_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getDescriptionMetadataTxt(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) {
        try {
            return super.getDescriptionMetadataTxt(pid, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return new StringRecord(t);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.OCR_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getOcr(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) {
        try {
            return super.getOcr(pid, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return new StringRecord(t);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.OCR_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord updateOcr(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.STRINGRECORD_CONTENT) String content
    ) {
        try {
            return super.updateOcr(pid, batchId, timestamp, content);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return new StringRecord(t);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_XML_AES_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getTechnicalMetadataAesTxt(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) {
        try {
            return super.getTechnicalMetadataAesTxt(pid, modelId, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return new StringRecord(t);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_AES_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<DescriptionMetadata<Object>> updateTechnicalMetadataAes(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.TECHNICAL_CUSTOM_XMLDATA) String xmlData,
            @FormParam(DigitalObjectResourceApi.TECHNICAL_CUSTOM_JSONDATA) String jsonData
    ) {
        try {
            return super.updateTechnicalMetadataAes(pid, batchId, timestamp, xmlData, jsonData);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_AES_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<DescriptionMetadata<Object>> getTechnicalMetadataAes(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId
    ) {
        try {
            return super.getTechnicalMetadataAes(pid, batchId, editorId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_XML_PREMIS_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getTechnicalMetadataPremisTxt(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) {
        try {
            return super.getTechnicalMetadataPremisTxt(pid, modelId, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return new StringRecord(t);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_PREMIS_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<DescriptionMetadata<Object>> updateTechnicalMetadataPremis(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.TECHNICAL_CUSTOM_XMLDATA) String xmlData,
            @FormParam(DigitalObjectResourceApi.TECHNICAL_CUSTOM_JSONDATA) String jsonData
    ) {
        try {
            return super.updateTechnicalMetadataPremis(pid, batchId, timestamp, xmlData, jsonData);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }


    @GET
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_XML_CODING_HISTORY_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getCodingHistoryTxt(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) {
        try {
            return super.getCodingHistoryTxt(pid, modelId, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return new StringRecord(t);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_CODING_HISTORY_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<DescriptionMetadata<Object>> updateCodingHisotry(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.TECHNICAL_CUSTOM_XMLDATA) String xmlData,
            @FormParam(DigitalObjectResourceApi.TECHNICAL_CUSTOM_JSONDATA) String jsonData
    ) {
        try {
            return super.updateCodingHisotry(pid, batchId, timestamp, xmlData, jsonData);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_CODING_HISTORY_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<DescriptionMetadata<Object>> getCodingHistoryMetadata(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId
    ) {
        try {
            return super.getCodingHistoryMetadata(pid, batchId, editorId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.PRIVATENOTE_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getPrivateNote(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) {
        try {
            return super.getPrivateNote(pid, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return new StringRecord(t);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.PRIVATENOTE_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord updatePrivateNote(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.STRINGRECORD_CONTENT) String content
    ) {
        try {
            return super.updatePrivateNote(pid, batchId, timestamp, content);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return new StringRecord(t);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.ATM_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<AtmItem> getAtm(
            @QueryParam(DigitalObjectResourceApi.ATM_ITEM_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.ATM_ITEM_BATCHID) Integer batchId
    ) {
        try {
            return super.getAtm(pid, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.ATM_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<AtmItem> updateAtm(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) Set<String> pids,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_OWNER) String owner,
            @FormParam(DigitalObjectResourceApi.ATM_ITEM_DEVICE) String deviceId,
            @FormParam(DigitalObjectResourceApi.ATM_ITEM_ORGANIZATION) String organization,
            @FormParam(DigitalObjectResourceApi.ATM_ITEM_STATUS) String status,
            @FormParam(DigitalObjectResourceApi.ATM_ITEM_USER) String userName,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String model,
            @FormParam(DigitalObjectResourceApi.ATM_ITEM_DONATOR) String donator
    ) {
        try {
            return super.updateAtm(pids, batchId, owner, deviceId, organization, status, userName, model, donator);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.URNNBN_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<UrnNbnResult> registerUrnNbn(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids,
            @FormParam(DigitalObjectResourceApi.URNNBN_RESOLVER) String resolverId,
            @FormParam(DigitalObjectResourceApi.URNNBN_HIERARCHY) @DefaultValue("true") boolean hierarchy
    ) {
        try {
            return super.registerUrnNbn(pids, resolverId, hierarchy);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.LOCK_OBJECT_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> lockObject(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.lockObject(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.UNLOCK_OBJECT_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> unlockObject(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.unlockObject(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.COPYOBJECT_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> copyObject(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pidOld,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId
    ) {
        try {
            return super.copyObject(pidOld, batchId, modelId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.GENERATE_JP2_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> generateJp2(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.GENERATE_TYPE) String type,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId
    ) throws DigitalObjectException {
        try {
            return super.generateJp2(pid, type, modelId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_PAGE_TO_NDK_PAGE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changePageToNdkPage(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changePageToNdkPage(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }


    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_PAGE_TO_PAGE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkPageToPage(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeNdkPageToPage(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_PAGE_TO_NDK_PAGE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeSttPageToNdkPage(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeSttPageToNdkPage(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_PAGE_TO_STT_PAGE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkPageToSttPage(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeNdkPageToSttPage(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_CLIPPINGS_VOLUME_TO_NDK_MONOGRAPH_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeClippingsVolumeToNdkMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeClippingsVolumeToNdkMonographVolume(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_VOLUME_TO_CLIPPINGS_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographVolumeToClippingsVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeNdkMonographVolumeToClippingsVolume(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_TITLE_TO_CLIPPINGS_TITLE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographTitleToClippingsTitle(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeNdkMonographTitleToClippingsTitle(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }


    @POST
    @Path(DigitalObjectResourceApi.CHANGE_CLIPPINGS_TITLE_TO_NDK_MONOGRAPH_TITLE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeClippingsTitleToNdkMonographTitle(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeClippingsTitleToNdkMonographTitle(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_K4_PERIODICAL_TO_NDK_PERIODICAL)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeK4PeriodicalToNdkPeriodical(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeK4PeriodicalToNdkPeriodical(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_K4_PERIODICAL_VOLUME_TO_NDK_PERIODICAL_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeK4PeriodicalVolumeToNdkPeriodicalVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeK4PeriodicalVolumeToNdkPeriodicalVolume(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_K4_PERIODICAL_ISSUE_TO_NDK_PERIODICAL_ISSUE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeK4PeriodicalIssueToNdkPeriodicalIssue(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeK4PeriodicalIssueToNdkPeriodicalIssue(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_K4_MONOGRAPH_TO_NDK_MONOGRAPHT_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeK4MonographToNdkMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeK4MonographToNdkMonographVolume(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_K4_MONOGRAPH_UNIT_TO_NDK_MONOGRAPHT_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeK4MonographUnitToNdkMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeK4MonographUnitToNdkMonographVolume(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_TITLE_TO_NDK_MONOGRAPH_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographTitleToNdkMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeNdkMonographTitleToNdkMonographVolume(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_VOLUME_TO_NDK_MONOGRAPH_TITLE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographVolumeToNdkMonographTitle(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeNdkMonographVolumeToNdkMonographTitle(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MUSICSHEET_TO_STT_MUSICSHEET)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMusicsheetToOldprintMusicsheet(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeNdkMusicsheetToOldprintMusicsheet(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_MUSICSHEET_TO_NDK_MUSICSHEET)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeSttMusicsheetToNdkMusicsheet(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeSttMusicsheetToNdkMusicsheet(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }


    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_CHAPTER_TO_STT_CHAPTER)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkChapterToOldprintChapter(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeNdkChapterToOldprintChapter(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_CHAPTER_TO_NDK_CHAPTER)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldprintChapterToNdkChapter(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeOldprintChapterToNdkChapter(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_PICTURE_TO_STT_GRAPHIC)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkPictureToOldprintGraphic(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeNdkPictureToOldprintGraphic(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_GRAPHIC_TO_NDK_PICTURE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldprintGraphicToNdkPicture(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeOldprintGraphicToNdkPicture(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_CARTOGRAPHIC_TO_STT_CARTOGRAPHIC)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkCartographicToOldprintCartographic(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeNdkCartographicToOldprintCartographic(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_CARTOGRAPHIC_TO_NDK_CARTOGRAPHIC)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldprintCartographicToNdkCartographic(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeOldprintCartographicToNdkCartographic(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_TO_STT_MONOGRAPH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographVolumeToOldprintMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeNdkMonographVolumeToOldprintMonographVolume(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_MONOGRAPH_TO_NDK_MONOGRAPH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldPrintMonographVolumeToNdkMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeOldPrintMonographVolumeToNdkMonographVolume(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_SUPPLEMENT_TO_STT_SUPPLEMENT)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographSupplementToOldPrintSupplement(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeNdkMonographSupplementToOldPrintSupplement(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_SUPPLEMENT_TO_NDK_SUPPLEMENT)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldPrintSupplementToNdkMonographSupplement(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeOldPrintSupplementToNdkMonographSupplement(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_MONOGRAPH_TO_STT_GRAPHIC)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldPrintMonographVolumeToOldprintGraphic(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeOldPrintMonographVolumeToOldprintGraphic(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_MONOGRAPH_TO_STT_MUSICSHEET)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldPrintMonographVolumeToOldprintMusicSheet(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        try {
            return super.changeOldPrintMonographVolumeToOldprintMusicSheet(pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }


    @PUT
    @Path(DigitalObjectResourceApi.REINDEX_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> reindex(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PARENT_PID) String parentPid,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId,
            @FormParam(ImportResourceApi.BATCHITEM_BATCHID) Integer batchId
    ) {
        try {
            return super.reindex(pid, parentPid, modelId, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.UPDATE_ALL_OBJECTS_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> updateAllObjects(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId
    ) {
        try {
            return super.updateAllObjects(pid, modelId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.UPDATE_NDK_ARTICLE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> updateNdkArticeObjects(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId
    ) {
        try {
            return super.updateNdkArticeObjects(pid, modelId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.UPDATE_NDK_PAGE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> updateNdkPageObjects(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId
    ) {
        try {
            return super.updateNdkPageObjects(pid, modelId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }
}
