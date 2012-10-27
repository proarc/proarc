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
import cz.incad.pas.editor.server.config.PasConfiguration;
import cz.incad.pas.editor.server.config.PasConfigurationException;
import cz.incad.pas.editor.server.config.PasConfigurationFactory;
import cz.incad.pas.editor.server.fedora.DigitalObjectException;
import cz.incad.pas.editor.server.fedora.PageView;
import cz.incad.pas.editor.server.fedora.PageView.Item;
import cz.incad.pas.editor.server.fedora.RemoteStorage;
import cz.incad.pas.editor.server.imports.FedoraImport;
import cz.incad.pas.editor.server.imports.ImportBatchManager;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportBatch;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportBatch.State;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportItem;
import cz.incad.pas.editor.server.imports.ImportDispatcher;
import cz.incad.pas.editor.server.imports.ImportFileScanner;
import cz.incad.pas.editor.server.imports.ImportFileScanner.Folder;
import cz.incad.pas.editor.server.imports.ImportProcess;
import cz.incad.pas.editor.server.user.UserManager;
import cz.incad.pas.editor.server.user.UserProfile;
import cz.incad.pas.editor.server.user.UserUtil;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
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
 * REST resource to retrieve import folders.
 *
 * TODO /import/ GET - lists subfolders; POST - import folder; DELETE - delete folder
 *      /import/batch/ GET - lists imported folders; POST - import folder
 *      /import/object/ GET - lists imported objects; POST - import folder
 * TODO /object/{pid}/ GET - read DigObjDesc:{pid, displayname, date, owner};
 *      /object/ GET - lists all DigObjDesc
 *      /object/{pid}/foxml
 *      /object/{pid}/scan
 *      /object/{pid}/preview
 *      /object/{pid}/thumb
 *      /object/{pid}/ocr
 *      /object/{pid}/metadata
 *      /object/{pid}/relations
 *      /object/metamodel/ GET - lists model:{pid, displayname, type:(TOP|LEAF)}
 *      /user/ GET list User:{id, uname, displayName}, POST - new user, DELETE - delete user, PUT - update user
 *      /user/{id}/profile GET profile:{importFolder, roles:[IMPORTER|ADMIN|...]}, PUT - update
 * 
 * @author Jan Pokorsky
 * @see <a href="http://127.0.0.1:8888/Editor/rest/import">test in dev mode</a>
 * @see <a href="http://127.0.0.1:8888/Editor/rest/application.wadl">WADL in dev mode</a>
 * @see <a href="http://127.0.0.1:8888/Editor/rest/application.wadl/xsd0.xsd">XML Scema in dev mode</a>
 */
@Path("/import")
public class ImportResource {

    private static final Logger LOG = Logger.getLogger(ImportResource.class.getName());
    private static final Pattern INVALID_PATH_CONTENT = Pattern.compile("\\.\\.|//");

    // XXX inject with guice
    private final UserManager userManager;
    private final ImportBatchManager importManager;
    private final PasConfiguration pasConfig;

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
            ) throws PasConfigurationException {

        this.pasConfig = PasConfigurationFactory.getInstance().defaultInstance();
        this.importManager = ImportBatchManager.getInstance(pasConfig);
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
    @GET
    @Produces({MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ImportFolder> list(
            @QueryParam("parent") @DefaultValue("") String parent
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
    @Path("batch")
    @Produces({MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ImportBatch> importFolder(
            @FormParam("folderPath") @DefaultValue("") String path,
            @FormParam("model") @DefaultValue("model:page") String model,
            @FormParam("device") String device,
            @FormParam("indices") @DefaultValue("true") boolean indices
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
        ImportBatch batch = process.getBatch();
        if (batch == null) {
            return new SmartGwtResponse<ImportBatch>(Collections.<ImportBatch>emptyList());
        }
        ImportDispatcher.getDefault().addImport(process);
        return new SmartGwtResponse<ImportBatch>(batch);
    }

    @GET
    @Path("batch")
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<ImportBatch> listBatches(
            @QueryParam("id") Integer batchId,
            @QueryParam("_startRow") int startRow
            ) {

        List<ImportBatch> batches;
        if (batchId != null) {
            batches = importManager.find(user, batchId, null);
        } else {
            batches = importManager.findAll(user);
        }
        return new SmartGwtResponse<ImportBatch>(batches);
    }

    @PUT
    @Path("batch")
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<ImportBatch> updateBatch(
            @FormParam("id") Integer batchId,
            // empty string stands for remove
            @FormParam("parentPid") String parentPid,
            @FormParam("state") ImportBatch.State state
            ) throws IOException, FedoraClientException, DigitalObjectException {

        ImportBatch batch = importManager.get(batchId);
        if (batch == null) {
            throw RestException.plainNotFound("id", String.valueOf(batchId));
        }
        if (parentPid != null) {
            checkBatchState(batch);
            // XXX check PID is valid and exists
            parentPid = parentPid.isEmpty() ? null : parentPid;
            batch.setParentPid(parentPid);
            batch = importManager.update(batch);
        }
        if (state == ImportBatch.State.INGESTING) {
            // ingest
            batch = new FedoraImport(RemoteStorage.getInstance(pasConfig), importManager)
                    .importBatch(batch, user.getUserName(), session.asFedoraLog());
        }
        return new SmartGwtResponse<ImportBatch>(batch);
    }

    @GET
    @Path("batch/item")
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<PageView.Item> listBatchItems(
            @QueryParam("batchId") Integer batchId,
            @QueryParam("pid") String pid,
            @QueryParam("_startRow") int startRow
            ) throws DigitalObjectException {

        startRow = Math.max(0, startRow);
        List<ImportItem> imports = null;

        if (batchId != null) {
            imports = importManager.findItems(batchId, pid);
        }
        if (imports == null) {
            throw RestException.plainText(Status.NOT_FOUND, String.format("Not found! batchId: %s, pid: %s", batchId, pid));
        }

        ImportBatch batch = importManager.get(batchId);
        if (batch.getState() == State.LOADING_FAILED) {
            throw RestException.plainText(Status.FORBIDDEN, "Batch not loaded.");
        }
        int totalImports = imports.size();
        int totalRows = (batch.getState() == State.LOADING) ? batch.getEstimateFileCount() : totalImports;
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
    @Path("batch/item")
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<PageView.Item> updateBatchItem(
            @FormParam("batchId") Integer batchId,
            @FormParam("pid") String pid,
            @FormParam("timestamp") long timestamp,
            @FormParam("pageIndex") String pageIndex,
            @FormParam("pageNumber") String pageNumber,
            @FormParam("pageType") String pageType,
            @FormParam("filename") String filename
            ) throws IOException, DigitalObjectException {

        ImportItem item = null;
        if (batchId != null && pid != null && !pid.isEmpty()) {
            item = importManager.findItem(pid);
        }
        if (item == null) {
            throw RestException.plainText(Status.NOT_FOUND, "Item not found!");
        }

        List<ImportBatch> batches = importManager.find(null, batchId, null);
        checkBatchState(batches.get(0));
        Item updatedItem = new PageView().updateItem(
                batchId, item, timestamp, session.asFedoraLog(), pageIndex, pageNumber, pageType);
        return new SmartGwtResponse<Item>(updatedItem);
    }

    @DELETE
    @Path("batch/item")
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<PageView.Item> deleteBatchItem(
            @QueryParam("batchId") Integer batchId,
            @QueryParam("pid") String pid
            ) {

        ImportItem item = null;
        if (batchId != null && pid != null && !pid.isEmpty()) {
            item = importManager.findItem(pid);
        }
        if (item == null) {
            throw RestException.plainText(Status.NOT_FOUND, "Batch item not found!");
        }
        List<ImportBatch> batches = importManager.find(null, batchId, null);
        checkBatchState(batches.get(0));
        importManager.removeItem(batchId, pid);
        Item deletedItem = new PageView.Item(batchId, null, pid, null, null, null, null, 0, null);
        return new SmartGwtResponse<Item>(deletedItem);
    }

    private static String normalizeParam(String p) {
        if (p != null) {
            p = p.trim();
            if (p.isEmpty() || "null".equals(p)) {
                p = null;
            }
        }
        return p;
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

    public static void checkBatchState(ImportBatch batch) throws RestException {
        if (batch.getState() != State.LOADED) {
            throw RestException.plainText(Status.FORBIDDEN, String.format(
                    "Batch %s is not editable! Unexpected state: %s", batch.getId(), batch.getState()));
        }
    }

}
