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
package cz.incad.pas.editor.server.rest;

import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchView;
import cz.incad.pas.editor.server.config.AppConfiguration;
import cz.incad.pas.editor.server.config.AppConfigurationException;
import cz.incad.pas.editor.server.config.AppConfigurationFactory;
import cz.incad.pas.editor.server.fedora.DigitalObjectException;
import cz.incad.pas.editor.server.fedora.PageView;
import cz.incad.pas.editor.server.fedora.PageView.Item;
import cz.incad.pas.editor.server.fedora.RemoteStorage;
import cz.incad.pas.editor.server.imports.FedoraImport;
import cz.incad.pas.editor.server.imports.ImportBatchManager;
import cz.incad.pas.editor.server.imports.ImportBatchManager.BatchItemObject;
import cz.incad.pas.editor.server.imports.ImportDispatcher;
import cz.incad.pas.editor.server.imports.ImportFileScanner;
import cz.incad.pas.editor.server.imports.ImportFileScanner.Folder;
import cz.incad.pas.editor.server.imports.ImportProcess;
import cz.incad.pas.editor.server.user.UserManager;
import cz.incad.pas.editor.server.user.UserProfile;
import cz.incad.pas.editor.server.user.UserUtil;
import cz.incad.pas.editor.shared.rest.ImportResourceApi;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.security.Principal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import javax.servlet.http.HttpServletRequest;
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
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;

/**
 * Resource to handle imports.
 *
 *      /import/folder/ GET - lists subfolders; POST - import folder; DELETE - delete folder
 *      /import/batch/ GET - lists imported folders; POST - import folder
 *      /import/item/ GET - lists imported objects; POST - import folder
 * 
 * @author Jan Pokorsky
 * @see <a href="http://127.0.0.1:8888/Editor/rest/import">test in dev mode</a>
 * @see <a href="http://127.0.0.1:8888/Editor/rest/application.wadl">WADL in dev mode</a>
 * @see <a href="http://127.0.0.1:8888/Editor/rest/application.wadl/xsd0.xsd">XML Scema in dev mode</a>
 */
@Path(ImportResourceApi.PATH)
public class ImportResource {

    private static final Logger LOG = Logger.getLogger(ImportResource.class.getName());
    private static final Pattern INVALID_PATH_CONTENT = Pattern.compile("\\.\\.|//");

    // XXX inject with guice
    private final UserManager userManager;
    private final ImportBatchManager importManager;
    private final AppConfiguration appConfig;

    private final SecurityContext securityCtx;
    private final UserProfile user;
    private final HttpHeaders httpHeaders;
    private final UriInfo uriInfo;
    private final SessionContext session;

    public ImportResource(
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest
            /*UserManager userManager*/
            ) throws AppConfigurationException {

        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        this.importManager = ImportBatchManager.getInstance(appConfig);
        this.securityCtx = securityCtx;
        this.userManager = UserUtil.getDefaultManger(); // XXX replace with injection
        Principal userPrincipal = securityCtx.getUserPrincipal();
        String userName;
        if (userPrincipal != null) {
            userName = userPrincipal.getName();
        } else {
            userName = UserManager.GUEST_ID;
        }
        user = userManager.find(userName);

        this.httpHeaders = httpHeaders;
        this.uriInfo = uriInfo;
        session = SessionContext.from(httpRequest);
    }

    /**
     * Lists subfolders and their import states.
     *
     * @param parent folder path relative to user's import folder
     * @return folder contents (path without initial slash and always terminated with slash: A/, A/B/)
     * @throws FileNotFoundException
     * @throws URISyntaxException
     */
    @Path(ImportResourceApi.FOLDER_PATH)
    @GET
    @Produces({MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ImportFolder> listFolder(
            @QueryParam(ImportResourceApi.IMPORT_FOLDER_PARENT_PARAM) @DefaultValue("") String parent
            ) throws FileNotFoundException, URISyntaxException {

        String parentPath = validateParentPath(parent);

        URI userRoot = user.getImportFolder();
        URI path = (parentPath != null)
                // URI multi param constructor escapes input unlike single param constructor or URI.create!
                ? userRoot.resolve(new URI(null, null, parentPath, null))
                : userRoot;
        LOG.log(Level.FINE, "parent: {0} used as {1} resolved to {2}", new Object[] {parent, parentPath, path});

        ImportFileScanner scanner = new ImportFileScanner();
        List<Folder> subfolders = scanner.findSubfolders(new File(path));
        List<ImportFolder> result = new ArrayList<ImportFolder>(subfolders.size());
        for (Folder subfolder : subfolders) {
            String subfolderName = subfolder.getHandle().getName();
            String subfolderStatus = subfolder.getStatus().name();
            String subfolderPath = userRoot.relativize(subfolder.getHandle().toURI()).getPath();
            result.add(new ImportFolder(subfolderName, subfolderStatus, parentPath, subfolderPath));
        }

        return new SmartGwtResponse<ImportFolder>(result);
    }

    private ImportFolder create(File folder, ImportFileScanner.State state, URI userRoot) {
        String name = folder.getName();
        String path = userRoot.relativize(folder.toURI()).getPath();
        String parentPath = userRoot.relativize(folder.getParentFile().toURI()).getPath();
        return new ImportFolder(name, state.name(), parentPath, path);
    }

    @POST
    @Path(ImportResourceApi.BATCH_PATH)
    @Produces({MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON})
    public SmartGwtResponse<BatchView> newBatch(
            @FormParam(ImportResourceApi.IMPORT_BATCH_FOLDER) @DefaultValue("") String path,
            @FormParam(ImportResourceApi.NEWBATCH_MODEL_PARAM) @DefaultValue("model:page") String model,
            @FormParam(ImportResourceApi.NEWBATCH_DEVICE_PARAM) String device,
            @FormParam(ImportResourceApi.NEWBATCH_INDICES_PARAM) @DefaultValue("true") boolean indices
            ) throws URISyntaxException, IOException {
        
        LOG.log(Level.FINE, "import path: {0} as model: {1}, indices: {2}, device: {3}",
                new Object[] {path, model, indices, device});
        String folderPath = validateParentPath(path);
        URI userRoot = user.getImportFolder();
        URI folderUri = (folderPath != null)
                // URI multi param constructor escapes input unlike single param constructor or URI.create!
                ? userRoot.resolve(new URI(null, null, folderPath, null))
                : userRoot;
        File folder = new File(folderUri);
        ImportProcess process = ImportProcess.prepare(folder, folderPath, user,
                importManager, model, device, indices);
        ImportDispatcher.getDefault().addImport(process);
        Batch batch = process.getBatch();
        return new SmartGwtResponse<BatchView>(importManager.viewBatch(batch.getId()));
    }

    /**
     * Gets list of import batches.
     * 
     * @param batchId optional batch ID to find
     * @param batchState optional states to find
     * @param createFrom optional create date as lower bound of query
     * @param createTo  optional create date as upper bound of query
     * @param startRow optional offset of the result
     * @param sortBy optional {@link BatchView} property name to sort the result. Value syntax: {@code [-]propertyName} where
     *              {@code '-'} stands for descending sort. Default is {@code sortBy=-create}.
     * @return the sorted list of batches.
     */
    @GET
    @Path(ImportResourceApi.BATCH_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<BatchView> listBatches(
            @QueryParam(ImportResourceApi.IMPORT_BATCH_ID) Integer batchId,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_STATE) Set<Batch.State> batchState,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_CREATE_FROM) DateTimeParam createFrom,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_CREATE_TO) DateTimeParam createTo,
            @QueryParam("_startRow") int startRow,
            @QueryParam("_sortBy") String sortBy
            ) {

        int pageSize = 100;
        Timestamp from = createFrom == null ? null : createFrom.toTimestamp();
        Timestamp to = createTo == null ? null : createTo.toTimestamp();
        // admin may see all users; XXX use permissions for this!
        Integer userFilter = user.getId() == 1 ? null : user.getId();
        List<BatchView> batches = importManager.viewBatch(userFilter, batchId, batchState,
                from, to, startRow, pageSize, sortBy);
        int batchSize = batches.size();
        int endRow = startRow + batchSize;
        int total = (batchSize != pageSize) ? endRow: endRow + 1;
        return new SmartGwtResponse<BatchView>(SmartGwtResponse.STATUS_SUCCESS, startRow, endRow, total, batches);
    }

    @PUT
    @Path(ImportResourceApi.BATCH_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<BatchView> updateBatch(
            @FormParam(ImportResourceApi.IMPORT_BATCH_ID) Integer batchId,
            // empty string stands for remove
            @FormParam(ImportResourceApi.IMPORT_BATCH_PARENTPID) String parentPid,
            @FormParam(ImportResourceApi.IMPORT_BATCH_STATE) Batch.State state
            ) throws IOException, FedoraClientException, DigitalObjectException {

        Batch batch = importManager.get(batchId);
        if (batch == null) {
            throw RestException.plainNotFound(
                    ImportResourceApi.IMPORT_BATCH_ID, String.valueOf(batchId));
        }
        if (parentPid != null) {
            checkBatchState(batch);
            // XXX check PID is valid and exists
            parentPid = parentPid.isEmpty() ? null : parentPid;
            batch.setParentPid(parentPid);
            batch = importManager.update(batch);
        }
        if (state == Batch.State.INGESTING) {
            // ingest
            batch = new FedoraImport(RemoteStorage.getInstance(appConfig), importManager)
                    .importBatch(batch, user.getUserName(), session.asFedoraLog());
        }
        BatchView batchView = importManager.viewBatch(batch.getId());
        return new SmartGwtResponse<BatchView>(batchView);
    }

    @GET
    @Path(ImportResourceApi.BATCH_PATH + '/' + ImportResourceApi.BATCHITEM_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<PageView.Item> listBatchItems(
            @QueryParam(ImportResourceApi.BATCHITEM_BATCHID) Integer batchId,
            @QueryParam(ImportResourceApi.BATCHITEM_PID) String pid,
            @QueryParam("_startRow") int startRow
            ) throws DigitalObjectException {

        startRow = Math.max(0, startRow);
        List<BatchItemObject> imports = null;

        Batch batch = null;
        if (batchId != null) {
            batch = importManager.get(batchId);
            if (batch.getState() == Batch.State.LOADING_FAILED) {
                throw RestException.plainText(Status.FORBIDDEN, "Batch not loaded.");
            }
            imports = pid != null && !pid.isEmpty()
                    ? importManager.findBatchObjects(batchId, pid)
                    : importManager.findLoadedObjects(batch);
        }
        if (imports == null) {
            throw RestException.plainText(Status.NOT_FOUND, String.format("Not found! batchId: %s, pid: %s", batchId, pid));
        }

        int totalImports = imports.size();
        int totalRows = (batch.getState() == Batch.State.LOADING) ? batch.getEstimateItemNumber(): totalImports;
        if (totalImports == 0) {
            return new SmartGwtResponse<Item>(SmartGwtResponse.STATUS_SUCCESS, 0, 0, totalRows, null);
        }

        if (startRow >= totalImports) {
            return new SmartGwtResponse<Item>(SmartGwtResponse.STATUS_SUCCESS, startRow, startRow, totalRows, null);
        }

        int endRow = totalImports;

        if (startRow > 0) {
            imports = imports.subList(startRow, totalImports);
        }
        List<Item> records = new PageView().list(batchId, imports);
        return new SmartGwtResponse<Item>(SmartGwtResponse.STATUS_SUCCESS, startRow, endRow, totalRows, records);
    }

    @PUT
    @Path(ImportResourceApi.BATCH_PATH + '/' + ImportResourceApi.BATCHITEM_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<PageView.Item> updateBatchItem(
            @FormParam(ImportResourceApi.BATCHITEM_BATCHID) Integer batchId,
            @FormParam(ImportResourceApi.BATCHITEM_PID) String pid,
            @FormParam(ImportResourceApi.BATCHITEM_TIMESTAMP) long timestamp,
            @FormParam(ImportResourceApi.BATCHITEM_PAGEINDEX) String pageIndex,
            @FormParam(ImportResourceApi.BATCHITEM_PAGENUMBER) String pageNumber,
            @FormParam(ImportResourceApi.BATCHITEM_PAGETYPE) String pageType,
            @FormParam(ImportResourceApi.BATCHITEM_FILENAME) String filename
            ) throws IOException, DigitalObjectException {

        BatchItemObject item = null;
        if (batchId != null && pid != null && !pid.isEmpty()) {
            item = importManager.findBatchObject(batchId, pid);
        }
        if (item == null) {
            throw RestException.plainText(Status.NOT_FOUND, "Item not found!");
        }

        Batch batch = importManager.get(batchId);
        checkBatchState(batch);
        Item updatedItem = new PageView().updateItem(
                batchId, item, timestamp, session.asFedoraLog(), pageIndex, pageNumber, pageType);
        return new SmartGwtResponse<Item>(updatedItem);
    }

    @DELETE
    @Path(ImportResourceApi.BATCH_PATH + '/' + ImportResourceApi.BATCHITEM_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<PageView.Item> deleteBatchItem(
            @QueryParam(ImportResourceApi.BATCHITEM_BATCHID) Integer batchId,
            @QueryParam(ImportResourceApi.BATCHITEM_PID) String pid
            ) {

        boolean changed = false;
        if (batchId != null && pid != null && !pid.isEmpty()) {
            Batch batch = importManager.get(batchId);
            if (batch != null) {
                checkBatchState(batch);
                changed = importManager.excludeBatchObject(batch, pid);
            }
        }
        if (changed) {
            Item deletedItem = new PageView.Item(batchId, null, pid, null, null, null, null, 0, null);
            return new SmartGwtResponse<Item>(deletedItem);
        } else {
            throw RestException.plainText(Status.NOT_FOUND, "Batch item not found!");
        }
    }

    private static String validateParentPath(String parent) {
        if (parent == null || parent.length() == 0) {
            return null;
        }

        if ("null".equals(parent)) { // XXX fix parent param on client side
            return null;
        }

        // stop on dangerous chars; it could introduce vulnerability
        if (INVALID_PATH_CONTENT.matcher(parent).find()) {
            throw new IllegalArgumentException("Invalid 'parent' param! " + parent);
        }

        // parent must not be absolute path
        if (parent.charAt(0) == '/') {
            parent = (parent.length() == 1) ? null : parent.substring(1);
        }
        return parent;

    }

    public static void checkBatchState(Batch batch) throws RestException {
        if (batch.getState() != Batch.State.LOADED) {
            throw RestException.plainText(Status.FORBIDDEN, String.format(
                    "Batch %s is not editable! Unexpected state: %s", batch.getId(), batch.getState()));
        }
    }

}
