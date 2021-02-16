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
package cz.cas.lib.proarc.webapp.server.rest;

import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchView;
import cz.cas.lib.proarc.common.dao.BatchViewFilter;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.PageView;
import cz.cas.lib.proarc.common.fedora.PageView.Item;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.imports.FedoraImport;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.imports.ImportDispatcher;
import cz.cas.lib.proarc.common.imports.ImportFileScanner;
import cz.cas.lib.proarc.common.imports.ImportFileScanner.Folder;
import cz.cas.lib.proarc.common.imports.ImportProcess;
import cz.cas.lib.proarc.common.imports.ImportProfile;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
import cz.cas.lib.proarc.webapp.server.ServerMessages;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import org.apache.commons.io.IOUtils;
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
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

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

    private final HttpHeaders httpHeaders;
    // XXX inject with guice
    private final ImportBatchManager importManager;
    private final AppConfiguration appConfig;

    private final UserProfile user;
    private final SessionContext session;

    public ImportResource(
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest
            /*UserManager userManager*/
            ) throws AppConfigurationException {

        this.httpHeaders = httpHeaders;
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        this.importManager = ImportBatchManager.getInstance();
        session = SessionContext.from(httpRequest);
        user = session.getUser();
    }

    /**
     * Lists subfolders and their import states.
     *
     * @param parent folder path relative to user's import folder
     * @param profileId profile ID
     * @return folder contents (path without initial slash and always terminated with slash: A/, A/B/)
     * @throws FileNotFoundException
     * @throws URISyntaxException
     */
    @Path(ImportResourceApi.FOLDER_PATH)
    @GET
    @Produces({MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ImportFolder> listFolder(
            @QueryParam(ImportResourceApi.IMPORT_FOLDER_PARENT_PARAM) @DefaultValue("") String parent,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_PROFILE) String profileId,
            @QueryParam(ImportResourceApi.IMPORT_START_ROW_PARAM) @DefaultValue("-1") int startRow

            ) throws IOException, URISyntaxException {

        int endRow = 0;
        int total = 0;
        String parentPath = validateParentPath(parent);
        ImportProfile importProfile;
        if (profileId == null || profileId.isEmpty()) {
            importProfile = appConfig.getImportConfiguration();
        } else {
            ConfigurationProfile profile = findImportProfile(null, profileId);
            importProfile = appConfig.getImportConfiguration(profile);
        }

        URI userRoot = user.getImportFolder();
        URI path = (parentPath != null)
                // URI multi param constructor escapes input unlike single param constructor or URI.create!
                ? userRoot.resolve(new URI(null, null, parentPath, null))
                : userRoot;
        LOG.log(Level.FINE, "parent: {0} used as {1} resolved to {2}", new Object[] {parent, parentPath, path});

        ImportFileScanner scanner = new ImportFileScanner();
        List<Folder> subfolders = scanner.findSubfolders(new File(path), importProfile.createImporter());
        List<ImportFolder> result = new ArrayList<ImportFolder>();
        if (startRow < 0) {
            result = setResult(result, subfolders, userRoot, parentPath);
            startRow = 0;
            total = subfolders.size();
            endRow =  startRow + total -1;
        } else {
            int size = 100;
            List<Folder> selectedSubfolders = new ArrayList<>();
            for (int i = startRow; i < startRow + size; i++) {
                if (subfolders.size() - 1 < i) {
                    break;
                }
                selectedSubfolders.add(subfolders.get(i));
            }
            result = setResult(result, selectedSubfolders, userRoot, parentPath);
            total = subfolders.size();
            endRow = startRow + total - 1;
        }
        return new SmartGwtResponse<ImportFolder>(SmartGwtResponse.STATUS_SUCCESS, startRow, endRow, total, result);
    }

    private List<ImportFolder> setResult(List<ImportFolder> result, List<Folder> subfolders, URI userRoot, String parentPath) {
        for (Folder subfolder : subfolders) {
            String subfolderName = subfolder.getHandle().getName();
            String subfolderStatus = subfolder.getStatus().name();
            String subfolderPath = userRoot.relativize(subfolder.getHandle().toURI()).getPath();
            result.add(new ImportFolder(subfolderName, subfolderStatus, parentPath, subfolderPath));
        }
        return result;
    }

    @POST
    @Path(ImportResourceApi.BATCH_PATH)
    @Produces({MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON})
    public SmartGwtResponse<BatchView> newBatch(
            @FormParam(ImportResourceApi.IMPORT_BATCH_FOLDER) @DefaultValue("") String path,
            @FormParam(ImportResourceApi.NEWBATCH_DEVICE_PARAM) String device,
            @FormParam(ImportResourceApi.NEWBATCH_INDICES_PARAM) @DefaultValue("true") boolean indices,
            @FormParam(ImportResourceApi.IMPORT_BATCH_PROFILE) String profileId
            ) throws URISyntaxException, IOException {
        
        LOG.log(Level.FINE, "import path: {0}, indices: {1}, device: {2}",
                new Object[] {path, indices, device});
        String folderPath = validateParentPath(path);
        URI userRoot = user.getImportFolder();
        URI folderUri = (folderPath != null)
                // URI multi param constructor escapes input unlike single param constructor or URI.create!
                ? userRoot.resolve(new URI(null, null, folderPath, null))
                : userRoot;
        File folder = new File(folderUri);
        ConfigurationProfile profile = findImportProfile(null, profileId);
        ImportProcess process = ImportProcess.prepare(folder, folderPath, user,
                importManager, device, indices, appConfig.getImportConfiguration(profile));
        ImportDispatcher.getDefault().addImport(process);
        Batch batch = process.getBatch();
        return new SmartGwtResponse<BatchView>(importManager.viewBatch(batch.getId()));
    }

    @POST
    @Path(ImportResourceApi.BATCHES_PATH)
    @Produces({MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON})
    public SmartGwtResponse<BatchView> newBatches(
            @FormParam(ImportResourceApi.IMPORT_BATCH_FOLDER) @DefaultValue("") String pathes,
            @FormParam(ImportResourceApi.NEWBATCH_DEVICE_PARAM) String device,
            @FormParam(ImportResourceApi.NEWBATCH_INDICES_PARAM) @DefaultValue("true") boolean indices,
            @FormParam(ImportResourceApi.IMPORT_BATCH_PROFILE) String profileId
    ) throws URISyntaxException, IOException {

        LOG.log(Level.FINE, "import path: {0}, indices: {1}, device: {2}",
                new Object[] {pathes, indices, device});

        List<String> listFolders = createListOfPath(pathes);
        URI userRoot = user.getImportFolder();
        List<Batch> listBatches = new ArrayList<>();
        List <IOException> listExceptions = new ArrayList<>();

        for (String folderPath : listFolders) {
            try {
                URI folderUri = (folderPath != null)
                        // URI multi param constructor escapes input unlike single param constructor or URI.create!
                        ? userRoot.resolve(new URI(null, null, folderPath, null))
                        : userRoot;
                File folder = new File(folderUri);
                ConfigurationProfile profile = findImportProfile(null, profileId);
                ImportProcess process = ImportProcess.prepare(folder, folderPath, user,
                        importManager, device, indices, appConfig.getImportConfiguration(profile));
                ImportDispatcher.getDefault().addImport(process);
                listBatches.add(process.getBatch());
            } catch (IOException ex) {
                listExceptions.add(ex);
            }
        }
        if (listExceptions.size() > 0) {
            throw listExceptions.get(0);
        }
        if (listBatches.size() > 0) {
            return new SmartGwtResponse<BatchView>(importManager.viewBatch(listBatches.get(0).getId()));
        } else {
            return new SmartGwtResponse<BatchView>();
        }
    }

    private List<String> createListOfPath(String path) {
        List<String> pathes = new ArrayList<>();
        path = trim(path, "{", "}");
        path = trim(path, "[", "]");

        String[] pathesArray = path.split(",");

        if (pathesArray.length > 0) {
            for (int i = 0; i < pathesArray.length; i++) {

                pathes.add(validateParentPath(pathesArray[i].trim()));
            }
        }
        return pathes;
    }

    private String trim(String value, String start, String end) {
        if (value != null && !value.isEmpty()) {
            if (value.startsWith(start)) {
                value = value.substring(start.length());
            }
            if (value.endsWith(end)) {
                value = value.substring(0, value.length()-end.length());
            }
        }
        return value;
    }

    /**
     * Gets list of batch imports. DateTime format is ISO 8601.
     * 
     * @param batchId optional batch ID to find
     * @param batchState optional states to find
     * @param createFrom optional create dateTime as lower bound of query
     * @param createTo  optional create dateTime as upper bound of query
     * @param modifiedFrom optional modified dateTime as lower bound of query
     * @param modifiedTo optional modified dateTime as upper bound of query
     * @param filePattern optional file pattern to match folder or batch item file
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
            @QueryParam(ImportResourceApi.IMPORT_BATCH_MODIFIED_FROM) DateTimeParam modifiedFrom,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_MODIFIED_TO) DateTimeParam modifiedTo,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_DESCRIPTION) String filePattern,
            @QueryParam(ImportResourceApi.IMPORT_BATCH_PROFILE) String profile,
            @QueryParam("_startRow") int startRow,
            @QueryParam("_sortBy") String sortBy
            ) throws IOException {

        RemoteStorage remote = RemoteStorage.getInstance(appConfig);
        int pageSize = 100;
        BatchViewFilter filterAll = new BatchViewFilter()
                    .setBatchId(batchId)
                    .setUserId(user.getId() == 1 ? null : (UserRole.ROLE_SUPERADMIN.equals(user.getRole()) ? null : user.getId()))
                    .setState(batchState)
                    .setCreatedFrom(createFrom == null ? null : createFrom.toTimestamp())
                    .setCreatedTo(createTo == null ? null : createTo.toTimestamp())
                    .setModifiedFrom(modifiedFrom == null ? null : modifiedFrom.toTimestamp())
                    .setModifiedTo(modifiedTo == null ? null : modifiedTo.toTimestamp())
                    .setFilePattern(filePattern)
                    .setProfile(profile)
                    .setMaxCount(100000)
                    .setSortBy(sortBy);
        List<BatchView> allBatches = importManager.viewBatch(filterAll);


        BatchViewFilter filter = new BatchViewFilter()
                .setBatchId(batchId)
                // admin may see all users; XXX use permissions for this!
                .setUserId(user.getId() == 1 ? null : (UserRole.ROLE_SUPERADMIN.equals(user.getRole()) ? null : user.getId()))
                .setState(batchState)
                .setCreatedFrom(createFrom == null ? null : createFrom.toTimestamp())
                .setCreatedTo(createTo == null ? null : createTo.toTimestamp())
                .setModifiedFrom(modifiedFrom == null ? null : modifiedFrom.toTimestamp())
                .setModifiedTo(modifiedTo == null ? null : modifiedTo.toTimestamp())
                .setFilePattern(filePattern)
                .setProfile(profile)
                .setOffset(startRow).setMaxCount(pageSize)
                .setSortBy(sortBy)
                ;
        List<BatchView> batches = importManager.viewBatch(filter);
        int batchSize = batches.size();
        int endRow = startRow + batchSize - 1;
        int total = allBatches.size();
        return new SmartGwtResponse<BatchView>(SmartGwtResponse.STATUS_SUCCESS, startRow, endRow, total, batches);
    }

    @GET
    @Path(ImportResourceApi.BATCHES_IN_PROCESS_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<BatchView> listProcessingBatches(
            @QueryParam(ImportResourceApi.IMPORT_BATCH_STATE) Set<Batch.State> batchState

    ) throws IOException {
        if (batchState.isEmpty()) {
            batchState.add(Batch.State.LOADING);
        }
        BatchViewFilter filterAll = new BatchViewFilter()
                .setState(batchState)
                .setMaxCount(1000)
                .setSortBy("id");

        List<BatchView> loadingBatches = importManager.viewProcessingBatches(filterAll, user, UserRole.ROLE_USER);

        int endRow = 0 + loadingBatches.size() - 1;
        int total = loadingBatches.size();
        return new SmartGwtResponse<BatchView>(SmartGwtResponse.STATUS_SUCCESS, 0, endRow, total, loadingBatches);
    }

    @PUT
    @Path(ImportResourceApi.BATCH_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<BatchView> updateBatch(
            @FormParam(ImportResourceApi.IMPORT_BATCH_ID) Integer batchId,
            // empty string stands for remove
            @FormParam(ImportResourceApi.IMPORT_BATCH_PARENTPID) String parentPid,
            @FormParam(ImportResourceApi.IMPORT_BATCH_STATE) Batch.State state,
            @FormParam(ImportResourceApi.IMPORT_BATCH_PROFILE) String profileId
            ) throws IOException, FedoraClientException, DigitalObjectException {

        Batch batch = importManager.get(batchId);
        if (batch == null) {
            throw RestException.plainNotFound(
                    ImportResourceApi.IMPORT_BATCH_ID, String.valueOf(batchId));
        }
        if (state == Batch.State.INGESTING) {
            // ingest or reingest for INGESTING_FAILED
            batch.getFolder();

            if (user.getId() != batch.getUserId()) {
                // batch was imported by different user (store metadata editor userid instead)
                File batchDir = new File(appConfig.getDefaultUsersHome(), batch.getFolder() + "/" + ImportProcess.TMP_DIR_NAME);
                File [] batchFiles = batchDir.listFiles((dir, name) -> name.endsWith(".foxml") && !name.startsWith(".proarc"));

                if (batchFiles == null) {
                    LOG.log(Level.INFO, "BatchFiles is null, trying to get batchFiles again. BatchId: "
                            + batchId + ", parentPid: " + parentPid + ", profileId: " + profileId + ", state: "
                            + state.toString() + " batchDir: " + batchDir + ".");

                    FilenameFilter filter = new FilenameFilter() {
                        @Override
                        public boolean accept(File dir, String name) {
                            return  (name.endsWith(".foxml") && !name.startsWith(".proarc"));
                        }
                    };

                    batchFiles = batchDir.listFiles(filter);
                }

                for (File batchFile : batchFiles) {
                    String fileContents = IOUtils.toString(new FileInputStream(batchFile), Charset.defaultCharset());
                    fileContents = fileContents.replaceAll(
                            "<property NAME=\"info:fedora/fedora-system:def/model#ownerId\" VALUE=\"" + "[^/]*" + "\"/>",
                            "<property NAME=\"info:fedora/fedora-system:def/model#ownerId\" VALUE=\"" + user.getUserName() + "\"/>"
                    );
                    IOUtils.write(fileContents, new FileOutputStream(batchFile), Charset.defaultCharset());
                }
            }

            batch = new FedoraImport(appConfig, RemoteStorage.getInstance(appConfig), importManager, user)
                    .importBatch(batch, user.getUserName(), session.asFedoraLog());
        } else if (state == Batch.State.LOADING_FAILED) {
            Batch.State realState = batch.getState();
            // try to reset import
            if (realState != Batch.State.LOADING_FAILED && realState != Batch.State.LOADED) {
                throw new UnsupportedOperationException("Cannot reset: " + batch);
            }
            ConfigurationProfile profile = findImportProfile(batchId, profileId);
            ImportProcess resume = ImportProcess.resume(batch, importManager,
                    appConfig.getImportConfiguration(profile));
            ImportDispatcher.getDefault().addImport(resume);
        } else if (parentPid != null) {
            checkBatchState(batch);
            // XXX check PID is valid and exists
            parentPid = parentPid.isEmpty() ? null : parentPid;
            batch.setParentPid(parentPid);
            batch = importManager.update(batch);
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
        final boolean listLoadedItems = pid == null || pid.isEmpty();

        Batch batch = null;
        if (batchId != null) {
            batch = importManager.get(batchId);
            if (batch.getState() == Batch.State.LOADING_FAILED) {
                Locale locale = session.getLocale(httpHeaders);
                throw RestException.plainText(Status.FORBIDDEN,
                        ServerMessages.get(locale).ImportResource_BatchLoadingFailed_Msg());
            }
            imports = listLoadedItems
                    ? importManager.findLoadedObjects(batch)
                    : importManager.findBatchObjects(batchId, pid);
        }
        if (imports == null) {
            throw RestException.plainText(Status.NOT_FOUND, String.format("Not found! batchId: %s, pid: %s", batchId, pid));
        }

        int totalImports = imports.size();
        if (listLoadedItems && batch.getState() == Batch.State.LOADING
                && totalImports > 0 && totalImports >= batch.getEstimateItemNumber()) {

            // #fix a situation when all items are already loaded but the batch has not been closed yet.
            --totalImports;
            imports.subList(0, totalImports);
        }
        int totalRows = (batch.getState() == Batch.State.LOADING) ? batch.getEstimateItemNumber(): totalImports;

        if (totalImports == 0 || startRow >= totalImports) {
            return new SmartGwtResponse<Item>(SmartGwtResponse.STATUS_SUCCESS, startRow, startRow, totalRows, null);
        }

        int endRow = totalImports;

        if (startRow > 0) {
            imports = imports.subList(startRow, totalImports);
        }
        List<Item> records = new PageView().list(batchId, imports, session.getLocale(httpHeaders));
        return new SmartGwtResponse<Item>(SmartGwtResponse.STATUS_SUCCESS, startRow, endRow, totalRows, records);
    }

    @DELETE
    @Path(ImportResourceApi.BATCH_PATH + '/' + ImportResourceApi.BATCHITEM_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<PageView.Item> deleteBatchItem(
            @QueryParam(ImportResourceApi.BATCHITEM_BATCHID) Integer batchId,
            @QueryParam(ImportResourceApi.BATCHITEM_PID) Set<String> pids
            ) {

        boolean changed = false;
        if (batchId != null && pids != null && !pids.isEmpty()) {
            Batch batch = importManager.get(batchId);
            if (batch != null) {
                checkBatchState(batch);
                changed = importManager.excludeBatchObject(batch, pids);
            }
        }
        if (changed) {
            ArrayList<Item> deletedItems = new ArrayList<PageView.Item>(pids.size());
            for (String pid : pids) {
                deletedItems.add(new PageView.Item(batchId, null, pid, null, null, null, null, 0, null, null));
            }
            return new SmartGwtResponse<Item>(deletedItems);
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

    private ConfigurationProfile findImportProfile(Integer batchId, String profileId) {
        ConfigurationProfile profile = appConfig.getProfiles().getProfile(ImportProfile.PROFILES, profileId);
        if (profile == null) {
            LOG.log(Level.SEVERE,"Batch {3}: Unknown profile: {0}! Check {1} in proarc.cfg",
                    new Object[]{ImportProfile.PROFILES, profileId, batchId});
            throw RestException.plainText(Status.BAD_REQUEST, "Unknown profile: " + profileId);
        }
        return profile;
    }

}
