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
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.SearchViewItem;
import cz.cas.lib.proarc.common.fedora.StringEditor.StringRecord;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.user.Permissions;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
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

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_DUPLICATES;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_IS_LOCKED;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_MISSING_PARAMETER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_MISSING_PARAMETERS;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_NO_PERMISSION;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_SAME_PID_AND_PARENT;

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
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_DAYS_IN_RANGE_PARAM) List<Integer> seriesDaysInRange,
            @DefaultValue("false") @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_MISSING_DAYS_INCLUDED_PARAM) boolean seriesMissingDaysIncluded,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_PARTNUMBER_FROM_PARAM) Integer seriesPartNumberFrom,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_SIGNATURA) String seriesSignatura,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_FREQUENCY) String seriesFrequency,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_DATE_FORMAT) String seriesDateFormat,
            @FormParam(DigitalObjectResourceApi.NEWOBJECT_XML_PARAM) String xmlMetadata,
            @FormParam(WorkflowModelConsts.PARAMETER_JOBID) BigDecimal workflowJobId,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CATALOGID) String catalogId,
            @DefaultValue("true") @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CREATE_OBJECT) boolean createObject,
            @DefaultValue("true") @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_VALIDATE_OBJECT) boolean validation
    ) {

        Set<String> models = MetaModelRepository.getInstance().find()
                .stream().map(metaModel -> metaModel.getPid()).collect(Collectors.toSet());

        if (isLocked(parentPid)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }

        if (modelId == null || !models.contains(modelId)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_MODEL));
        }
        try {
            if (!seriesMissingDaysIncluded) {
                if (seriesDaysIncluded == null || seriesDaysIncluded.isEmpty()) {
                    seriesMissingDaysIncluded = true;
                }
            }
            return super.newObject(modelId, pid, parentPid, seriesDateFrom, seriesDateTo, seriesDaysIncluded, seriesDaysInRange, seriesMissingDaysIncluded,
                    seriesPartNumberFrom, seriesSignatura, seriesFrequency, seriesDateFormat, xmlMetadata, workflowJobId, catalogId, createObject, validation);
        } catch (DigitalObjectException ex) {
                LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
                return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
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
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (batchId == null && parentPid == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.MEMBERS_ITEM_PARENT));
        }
        if (isLocked(parentPid)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        boolean batchImportMembers = batchId != null;
        if (toSetPids == null || toSetPids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.MEMBERS_ITEM_PID));
        }
        if (!batchImportMembers && toSetPids.contains(parentPid)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_SAME_PID_AND_PARENT, DigitalObjectResourceApi.MEMBERS_ITEM_PID, DigitalObjectResourceApi.MEMBERS_ITEM_PARENT));
        }

        HashSet<String> toSetPidSet = new HashSet<String>(toSetPids);
        if (toSetPidSet.size() != toSetPids.size()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_DUPLICATES, DigitalObjectResourceApi.MEMBERS_ITEM_PID));
        }
        try {
            return super.setMembers(parentPid, batchId, toSetPids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (parentPid == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.MEMBERS_ITEM_PARENT));
        }
        if (toAddPids == null || toAddPids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.MEMBERS_ITEM_PID));
        }
        if (toAddPids.contains(parentPid)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_SAME_PID_AND_PARENT, DigitalObjectResourceApi.MEMBERS_ITEM_PID, DigitalObjectResourceApi.MEMBERS_ITEM_PARENT));
        }
        if (isLocked(parentPid) || isLocked(toAddPids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        HashSet<String> addPidSet = new HashSet<String>(toAddPids);
        if (addPidSet.size() != toAddPids.size()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_DUPLICATES, DigitalObjectResourceApi.MEMBERS_ITEM_PID));
        }
        try {
            return super.addMembers(parentPid, toAddPids, batchId);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (parentPid == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.MEMBERS_ITEM_PARENT));
        }
        if (toRemovePids == null || toRemovePids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.MEMBERS_ITEM_PID));
        }
        if (toRemovePids.contains(parentPid)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_SAME_PID_AND_PARENT, DigitalObjectResourceApi.MEMBERS_ITEM_PID, DigitalObjectResourceApi.MEMBERS_ITEM_PARENT));
        }
        if (isLocked(parentPid) || isLocked(toRemovePids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.deleteMembers(parentPid, toRemovePids, batchId);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (srcParentPid == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.MEMBERS_MOVE_SRCPID));
        }
        if (dstParentPid == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.MEMBERS_MOVE_DSTPID));
        }
        if (srcParentPid.equals(dstParentPid)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_SAME_PID_AND_PARENT, DigitalObjectResourceApi.MEMBERS_MOVE_SRCPID, DigitalObjectResourceApi.MEMBERS_MOVE_DSTPID));
        }
        if (movePids == null || movePids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.MEMBERS_ITEM_PID));
        }

        if (isLocked(srcParentPid) || isLocked(dstParentPid) || isLocked(movePids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }

        HashSet<String> movePidSet = new HashSet<String>(movePids);
        if (movePidSet.isEmpty()) {
            return new SmartGwtResponse<SearchViewItem>(Collections.<SearchViewItem>emptyList());
        } else if (movePidSet.size() != movePids.size()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_DUPLICATES, DigitalObjectResourceApi.MEMBERS_ITEM_PID));
        }
        if (movePidSet.contains(dstParentPid)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_SAME_PID_AND_PARENT, DigitalObjectResourceApi.MEMBERS_ITEM_PID, DigitalObjectResourceApi.MEMBERS_MOVE_DSTPID));
        }

        try {
            return super.moveMembers(srcParentPid, dstParentPid, batchId, movePids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (pid == null || pid.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        try {
            return super.getDescriptionMetadata(pid, batchId, editorId);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if ((pid == null || pid.isEmpty()) && (jobId == null)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (timestamp == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.TIMESTAMP_PARAM));
        }
        if (isLocked(pid)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.updateDescriptionMetadata(pid, batchId, editorId, timestamp, jsonData, xmlData, jobId, model, ignoreValidation, standard);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (pid == null || pid.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (timestamp == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.TIMESTAMP_PARAM));
        }
        if (isLocked(pid)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.addAuthority(pid, batchId, timestamp, jsonData, editorId);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (pidsArray == null || pidsArray.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PIDS));
        }
        try {
            return super.updateDescriptionMetadataPages(pidsArray, applyTo, applyToFirstPage, prefix, suffix, useBrackets,
                    sequenceType, startNumber, incrementNumber, startIndex, pageType, doubleColumns, pagePosition, batchId);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (pidsArray == null || pidsArray.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PIDS));
        }
        try {
            return super.updatePagesAddBrackets(pidsArray, batchId);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (pidsArray == null || pidsArray.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PIDS));
        }
        try {
            return super.updatePagesRemoveBrackets(pidsArray, batchId);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (pid == null || pid.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        try {
            return super.getStreamProfile(pid, batchId, dsId);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (pid == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (file == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DISSEMINATION_FILE));
        }
        if (dsId != null && !dsId.equals(BinaryEditor.RAW_ID)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DISSEMINATION_DATASTREAM));
        }
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
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (timestamp == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.TIMESTAMP_PARAM));
        }
        if (pid == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if ((xmlData == null || xmlData.length() == 0) && (jsonData == null || jsonData.length() == 0)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETERS, DigitalObjectResourceApi.TECHNICAL_CUSTOM_XMLDATA, DigitalObjectResourceApi.TECHNICAL_CUSTOM_JSONDATA));
        }
        if (isLocked(pid)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.updateTechnicalMetadataAes(pid, batchId, timestamp, xmlData, jsonData);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (pid == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        try {
            return super.getTechnicalMetadataAes(pid, batchId, editorId);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (timestamp == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.TIMESTAMP_PARAM));
        }
        if (pid == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if ((xmlData == null || xmlData.length() == 0) && (jsonData == null || jsonData.length() == 0)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETERS, DigitalObjectResourceApi.TECHNICAL_CUSTOM_XMLDATA, DigitalObjectResourceApi.TECHNICAL_CUSTOM_JSONDATA));
        }
        if (isLocked(pid)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.updateTechnicalMetadataPremis(pid, batchId, timestamp, xmlData, jsonData);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
    public SmartGwtResponse<DescriptionMetadata<Object>> updateCodingHistory(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.TECHNICAL_CUSTOM_XMLDATA) String xmlData,
            @FormParam(DigitalObjectResourceApi.TECHNICAL_CUSTOM_JSONDATA) String jsonData
    ) {
        if (timestamp == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.TIMESTAMP_PARAM));
        }
        if (pid == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if ((xmlData == null || xmlData.length() == 0) && (jsonData == null || jsonData.length() == 0)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETERS, DigitalObjectResourceApi.TECHNICAL_CUSTOM_XMLDATA, DigitalObjectResourceApi.TECHNICAL_CUSTOM_JSONDATA));
        }
        if (isLocked(pid)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.updateCodingHistory(pid, batchId, timestamp, xmlData, jsonData);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (pid == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        try {
            return super.getCodingHistoryMetadata(pid, batchId, editorId);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
            @FormParam(DigitalObjectResourceApi.ATM_ITEM_DONATOR) String donator,
            @FormParam(DigitalObjectResourceApi.ATM_ITEM_ARCHIVAL_COPIES) String archivalCopiesPath
    ) {
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        try {
            return super.updateAtm(pids, batchId, owner, deviceId, organization, status, userName, model, donator, archivalCopiesPath);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.registerUrnNbn(pids, resolverId, hierarchy);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.URNNBN_PATH + "/" + DigitalObjectResourceApi.URNNBN_INVALIDATE_LOCAL_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<UrnNbnResult> invalidateLocalUrnNbn(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids,
            @FormParam(DigitalObjectResourceApi.URNNBN_HIERARCHY) @DefaultValue("true") boolean hierarchy
    ) {
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_CZIDLO_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }

        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.invalidateLocalUrnNbn(pids, hierarchy);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.URNNBN_PATH + "/" + DigitalObjectResourceApi.URNNBN_CREATE_SUCCESSOR_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<UrnNbnResult> createSuccessorUrnNbn(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids,
            @FormParam(DigitalObjectResourceApi.URNNBN_RESOLVER) String resolverId,
            @FormParam(DigitalObjectResourceApi.URNNBN_HIERARCHY) @DefaultValue("true") boolean hierarchy
    ) {
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_CZIDLO_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }

        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.createSuccessorUrnNbn(pids, resolverId, hierarchy);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.URNNBN_PATH + "/" + DigitalObjectResourceApi.URNNBN_INVALIDATE_REMOTE_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<UrnNbnResult> invalidateRemoteUrnNbn(
            @FormParam(DigitalObjectResourceApi.URNNBN_VALUE_TO_DEACTIVATE) String urnNbnValue,
            @FormParam(DigitalObjectResourceApi.URNNBN_RESOLVER) String resolverId,
            @FormParam(DigitalObjectResourceApi.URNNBN_HIERARCHY) @DefaultValue("true") boolean hierarchy
    ) {
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_CZIDLO_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        try {
            return super.invalidateRemoteUrnNbn(urnNbnValue, resolverId, hierarchy);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.URNNBN_PATH + "/" + DigitalObjectResourceApi.URNNBN_REGISTER_AGAIN_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<UrnNbnResult> registerAgainUrnNbn(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids,
            @FormParam(DigitalObjectResourceApi.URNNBN_RESOLVER) String resolverId,
            @FormParam(DigitalObjectResourceApi.URNNBN_HIERARCHY) @DefaultValue("true") boolean hierarchy
    ) {
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.registerAgainUrnNbn(pids, resolverId, hierarchy);
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_LOCK_OBJECT_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        try {
            return super.lockObject(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_UNLOCK_OBJECT_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        try {
            return super.unlockObject(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (pidOld == null || pidOld.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pidOld)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.copyObject(pidOld, batchId, modelId);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changePageToNdkPage(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeNdkPageToPage(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_PAGE_TO_STT_PAGE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changePageToSttPage(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changePageToSttPage(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_PAGE_TO_PAGE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeSttPageToPage(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeSttPageToPage(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeSttPageToNdkPage(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeNdkPageToSttPage(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeClippingsVolumeToNdkMonographVolume(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeNdkMonographVolumeToClippingsVolume(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeNdkMonographTitleToClippingsTitle(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeClippingsTitleToNdkMonographTitle(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeK4PeriodicalToNdkPeriodical(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeK4PeriodicalVolumeToNdkPeriodicalVolume(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeK4PeriodicalIssueToNdkPeriodicalIssue(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_K4_MONOGRAPH_TO_NDK_MONOGRAPH_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeK4MonographToNdkMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeK4MonographToNdkMonographVolume(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_K4_MONOGRAPH_UNIT_TO_NDK_MONOGRAPH_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeK4MonographUnitToNdkMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeK4MonographUnitToNdkMonographVolume(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_K4_MONOGRAPH_UNIT_TO_NDK_MONOGRAPH_UNIT)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeK4MonographUnitToNdkMonographUnit(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeK4MonographUnitToNdkMonographUnit(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeNdkMonographTitleToNdkMonographVolume(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_UNIT_TO_NDK_MONOGRAPH_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographUnitToNdkMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeNdkMonographUnitToNdkMonographVolume(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeNdkMonographVolumeToNdkMonographTitle(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_VOLUME_TO_NDK_MONOGRAPH_UNIT)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographVolumeToNdkMonographUnit(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeNdkMonographVolumeToNdkMonographUnit(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeNdkMusicsheetToOldprintMusicsheet(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeSttMusicsheetToNdkMusicsheet(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeNdkChapterToOldprintChapter(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeOldprintChapterToNdkChapter(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeNdkPictureToOldprintGraphic(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeOldprintGraphicToNdkPicture(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeNdkCartographicToOldprintCartographic(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeOldprintCartographicToNdkCartographic(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeNdkMonographVolumeToOldprintMonographVolume(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeOldPrintMonographVolumeToNdkMonographVolume(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeNdkMonographSupplementToOldPrintSupplement(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeOldPrintSupplementToNdkMonographSupplement(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeOldPrintMonographVolumeToOldprintGraphic(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_GRAPHIC_TO_STT_MONOGRAPH_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldPrintGraphicToOldPrintMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) {
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeOldPrintGraphicToOldPrintMonographVolume(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pids == null || pids.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        try {
            return super.changeOldPrintMonographVolumeToOldprintMusicSheet(pids);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_UPDATE_ALL_OBJECTS_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        try {
            return super.updateAllObjects(pid, modelId);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_UPDATE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pid == null || pid.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        try {
            return super.updateNdkArticeObjects(pid, modelId);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
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
        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_UPDATE_MODEL_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        if (pid == null || pid.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        try {
            return super.updateNdkPageObjects(pid, modelId);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.UPDATE_CATALOG_RECORD)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> updateCatalogRecord(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CATALOGID) String catalogId
    ) {

        if (!hasPermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }

        if (pid == null || pid.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID));
        }
        if (catalogId == null || catalogId.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.MODS_CUSTOM_CATALOGID));
        }


        try {
            return super.updateCatalogRecord(pid, catalogId);
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, ex.getMyMessage(), ex);
            return SmartGwtResponse.asError(ex.getMyMessage());
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }
}
