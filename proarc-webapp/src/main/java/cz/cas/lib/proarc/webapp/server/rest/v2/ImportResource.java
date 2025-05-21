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
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchView;
import cz.cas.lib.proarc.common.storage.PageView.Item;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.server.rest.DateTimeParam;
import cz.cas.lib.proarc.webapp.server.rest.ImportFolder;
import cz.cas.lib.proarc.webapp.server.rest.ProArcRequest;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.DigitalObjectResourceV1;
import cz.cas.lib.proarc.webapp.server.rest.v1.ImportResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
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
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_MISSING_PARAMETER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_MISSING_PARAMETERS;

/**
 * Resource to handle imports.
 *
 *      /import/folder/ GET - lists subfolders; POST - import folder; DELETE - delete folder
 *      /import/batch/ GET - lists imported folders; POST - import folder
 *      /import/item/ GET - lists imported objects; POST - import folder
 * 
 * @author Lukas Sykora
 * @see <a href="http://127.0.0.1:8888/Editor/rest/import">test in dev mode</a>
 * @see <a href="http://127.0.0.1:8888/Editor/rest/application.wadl">WADL in dev mode</a>
 * @see <a href="http://127.0.0.1:8888/Editor/rest/application.wadl/xsd0.xsd">XML Scema in dev mode</a>
 */
@Path(RestConfig.URL_API_VERSION_2 + "/" + ImportResourceApi.PATH)
public class ImportResource extends ImportResourceV1 {

    private static final Logger LOG = Logger.getLogger(ImportResource.class.getName());

    public ImportResource(
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest
            /*UserManager userManager*/
            ) throws AppConfigurationException {
        super(securityCtx, httpHeaders, uriInfo, httpRequest);
    }

    @Path(ImportResourceApi.FOLDER_PATH)
    @GET
    @Produces({MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ImportFolder> listFolder(
            @QueryParam(ImportResourceApi.IMPORT_FOLDER_PARENT_PARAM) @DefaultValue("") String parent,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_PROFILE) String profileId,
            @QueryParam(ImportResourceApi.IMPORT_START_ROW_PARAM) @DefaultValue("-1") int startRow
    ) {
        try {
            return super.listFolder(parent, profileId, startRow);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(ImportResourceApi.BATCH_PATH)
    @Produces({MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON})
    public SmartGwtResponse<BatchView> newBatch(
            @FormParam(ImportResourceApi.IMPORT_BATCH_FOLDER) @DefaultValue("") String path,
            @FormParam(ImportResourceApi.NEWBATCH_DEVICE_PARAM) String device,
            @FormParam(ImportResourceApi.NEWBATCH_SOFTWARE_PARAM) String software,
            @FormParam(ImportResourceApi.NEWBATCH_INDICES_PARAM) @DefaultValue("true") boolean indices,
            @FormParam(ImportResourceApi.IMPORT_BATCH_PROFILE) String profileId,
            @FormParam(ImportResourceApi.IMPORT_BATCH_PRIORITY) @DefaultValue(Batch.PRIORITY_MEDIUM) String priority,
            @FormParam(ImportResourceApi.IMPORT_BATCH_USE_NEW_METADATA) @DefaultValue("false") boolean useNewMetadata,
            @FormParam(ImportResourceApi.IMPORT_BATCH_USE_ORIGINAL_METADATA) @DefaultValue("false") boolean useOriginalMetadata
    ) {
        try {
            return super.newBatch(path, device, software, indices, profileId, priority, useNewMetadata, useOriginalMetadata);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(ImportResourceApi.BATCH_GENERATE_PATH)
    @Produces({MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON})
    public SmartGwtResponse<BatchView> newBatch(
            @FormParam(ImportResourceApi.IMPORT_BATCH_FOLDER) @DefaultValue("") String path
    ) {
        try {
            return super.newBatch(path);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }


        @POST
    @Path(ImportResourceApi.BATCHES_PATH)
    @Produces({MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON})
    public SmartGwtResponse<BatchView> newBatches(
            @FormParam(ImportResourceApi.IMPORT_BATCH_FOLDER) @DefaultValue("") String pathes,
            @FormParam(ImportResourceApi.NEWBATCH_DEVICE_PARAM) String device,
            @FormParam(ImportResourceApi.NEWBATCH_SOFTWARE_PARAM) String software,
            @FormParam(ImportResourceApi.NEWBATCH_INDICES_PARAM) @DefaultValue("true") boolean indices,
            @FormParam(ImportResourceApi.IMPORT_BATCH_PROFILE) String profileId,
            @FormParam(ImportResourceApi.IMPORT_BATCH_PRIORITY) @DefaultValue(Batch.PRIORITY_MEDIUM) String priority,
            @FormParam(ImportResourceApi.IMPORT_BATCH_USE_NEW_METADATA) @DefaultValue("false") boolean useNewMetadata,
            @FormParam(ImportResourceApi.IMPORT_BATCH_USE_ORIGINAL_METADATA) @DefaultValue("false") boolean useOriginalMetadata
    ) {
        try {
            return super.newBatches(pathes, device, software, indices, profileId, priority, useNewMetadata, useOriginalMetadata);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @DELETE
    @Path(ImportResourceApi.BATCH_PATH)
    @Produces({MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON})
    public SmartGwtResponse<BatchView> deleteBatch(
            @QueryParam(ImportResourceApi.IMPORT_BATCH_ID) Integer batchId,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_STATE) Set<Batch.State> batchState,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_PROFILE) String profileId,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_USERID) Integer creatorId
    ) {
        try {
            return super.deleteBatch(batchId, batchState, profileId, creatorId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(ImportResourceApi.BATCH_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<BatchView> listBatches(
            @QueryParam(ImportResourceApi.IMPORT_BATCH_ID) Integer batchId,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_STATE) Set<Batch.State> batchState,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_CREATE_FROM) DateTimeParam createFrom,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_CREATE_TO) DateTimeParam createTo,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_MODIFIED_FROM) DateTimeParam modifiedFrom,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_MODIFIED_TO) DateTimeParam modifiedTo,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_DESCRIPTION) String filePattern,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_PROFILE) String profile,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_USERID) Integer creatorId,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_PRIORITY) String priority,
            @QueryParam("_startRow") int startRow,
            @QueryParam("_size") int size,
            @QueryParam("_sortBy") String sortBy
    ) {
        try {
            return super.listBatches(batchId, batchState, createFrom, createTo, modifiedFrom, modifiedTo, filePattern,
                    profile, creatorId, priority, startRow, size, sortBy);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }


    @GET
    @Path(ImportResourceApi.BATCHES_IN_PROCESS_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<BatchView> listProcessingBatches(
            @QueryParam(ImportResourceApi.IMPORT_BATCH_STATE) Set<Batch.State> batchState
    ) {
        try {
            return super.listProcessingBatches(batchState);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(ImportResourceApi.BATCH_STOP_PATH)
    @Produces({MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON})
    public SmartGwtResponse<BatchView> stopBatch(
            @FormParam(ImportResourceApi.IMPORT_BATCH_ID) Integer batchId
    ) {
        if (batchId == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, ImportResourceApi.IMPORT_BATCH_ID));
        }
        try {
            return super.stopBatch(batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @PUT
    @Path(ImportResourceApi.BATCH_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<BatchView> updateBatch(
            @FormParam(ImportResourceApi.IMPORT_BATCH_ID) Integer batchId,
            @FormParam(ImportResourceApi.IMPORT_BATCH_PARENTPID) String parentPid,
            @FormParam(ImportResourceApi.IMPORT_BATCH_STATE) Batch.State state,
            @FormParam(ImportResourceApi.IMPORT_BATCH_PROFILE) String profileId,
            @FormParam(ImportResourceApi.IMPORT_BATCH_USE_NEW_METADATA) @DefaultValue("false") boolean useNewMetadata,
            @FormParam(ImportResourceApi.IMPORT_BATCH_USE_ORIGINAL_METADATA) @DefaultValue("false") boolean useOriginalMetadata
    ) {
        if (batchId == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, ImportResourceApi.IMPORT_BATCH_ID));
        }
        try {
            return super.updateBatch(batchId, parentPid, state, profileId, useNewMetadata, useOriginalMetadata);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(ImportResourceApi.BATCH_PATH + '/' + ImportResourceApi.BATCHITEM_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<Item> listBatchItems(
            @QueryParam(ImportResourceApi.BATCHITEM_BATCHID) Integer batchId,
            @QueryParam(ImportResourceApi.BATCHITEM_PID) String pid,
            @QueryParam("_startRow") int startRow
    ) {
        try {
            return super.listBatchItems(batchId, pid, startRow);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @DELETE
    @Path(ImportResourceApi.BATCH_PATH + '/' + ImportResourceApi.BATCHITEM_PATH)
    @Consumes({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Item> deleteBatchItem(
            ProArcRequest.DeleteObjectRequest deleteObjectRequest) {
        return deleteBatchItem(deleteObjectRequest.batchId, deleteObjectRequest.getPidsAsSet());
    }

    @DELETE
    @Path(ImportResourceApi.BATCH_PATH + '/' + ImportResourceApi.BATCHITEM_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<Item> deleteBatchItem(
            @QueryParam(ImportResourceApi.BATCHITEM_BATCHID) Integer batchId,
            @QueryParam(ImportResourceApi.BATCHITEM_PID) Set<String> pids
    ) {
        try {
            return super.deleteBatchItem(batchId, pids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(ImportResourceApi.BATCH_PATH + '/' + ImportResourceApi.IMPORT_FUNCTION_UNLOCK_FOLDER)
    @Produces({MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON})
    public SmartGwtResponse<BatchView> unlockFolder(
            @FormParam(ImportResourceApi.IMPORT_BATCH_FOLDER) @DefaultValue("") String path,
            @FormParam(ImportResourceApi.IMPORT_BATCH_ID) Integer batchId
    ) {
        if ((path == null || path.isEmpty()) && batchId == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETERS, ImportResourceApi.IMPORT_BATCH_FOLDER, ImportResourceApi.IMPORT_BATCH_ID));
        }
        try {
            return super.unlockFolder(path, batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(ImportResourceApi.GENERATE_ALTO_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DigitalObjectResourceV1.InternalExternalProcessResult> generateAlto(
            @FormParam(ImportResourceApi.IMPORT_BATCH_FOLDER) @DefaultValue("") String path
    ) {
        try {
            return super.generateAlto(path);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }
}
