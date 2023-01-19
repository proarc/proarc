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
package cz.cas.lib.proarc.common.imports;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.Batch.State;
import cz.cas.lib.proarc.common.dao.BatchDao;
import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dao.BatchItem.FileState;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.dao.BatchItemDao;
import cz.cas.lib.proarc.common.dao.BatchView;
import cz.cas.lib.proarc.common.dao.BatchViewFilter;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.dao.Transaction;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.FileSet.FileEntry;
import cz.cas.lib.proarc.common.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URI;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.JAXB;

/**
 *
 * @author Jan Pokorsky
 */
public class ImportBatchManager {

    private static final Logger LOG = Logger.getLogger(ImportBatchManager.class.getName());
    private static ImportBatchManager INSTANCE;
    public static final String ROOT_ITEM_PID = "proarc:root_item";
    public static final String ROOT_ITEM_FILENAME = ".proarc_root.foxml";

    private AppConfiguration appConfig;
    private final DaoFactory daos;


    public AppConfiguration getAppConfig(){
        return appConfig;
    }


    /** XXX replace with guice */
    public static void setInstance(AppConfiguration config, DaoFactory daos) {
        INSTANCE = new ImportBatchManager(config, daos);
    }

    public static ImportBatchManager getInstance() {
        return getInstance(null);
    }

    @Deprecated
    public static ImportBatchManager getInstance(AppConfiguration config) {
        if (INSTANCE == null) {
            throw new IllegalStateException("set instance first!");
        }
        return INSTANCE;
    }

    /** package private for unit tests */
    public ImportBatchManager(AppConfiguration appConfig, DaoFactory daos) {
        if (appConfig == null) {
            throw new NullPointerException("appConfig");
        }
        if (daos == null) {
            throw new NullPointerException("daos");
        }
        this.appConfig = appConfig;
        this.daos = daos;
    }

    /**
     * Finds given object.
     * @param batchId batch to find
     * @param pid object ID to find
     * @return the object or {@code null}.
     */
    public BatchItemObject findBatchObject(int batchId, String pid) {
        if (pid == null || pid.isEmpty()) {
            throw new IllegalArgumentException("pid: " + pid);
        }
        List<BatchItemObject> result = findBatchObjects(batchId, pid);
        return result.isEmpty() ? null : result.get(0);
    }

    public List<BatchItemObject> findBatchObjects(int batchId, String pid) {
        return findBatchObjects(batchId, pid, null);
    }

    /**
     * Finds all objects of given batch.
     * @param batchId batch to find
     * @param pid object ID to find; can be {@code null}
     * @return list of objects in unspecified order.
     */
    public List<BatchItemObject> findBatchObjects(int batchId, String pid, BatchItem.ObjectState state) {
        BatchItemDao itemDao = daos.createBatchItem();
        Transaction tx = daos.createTransaction();
        itemDao.setTransaction(tx);
        pid = (pid == null || pid.isEmpty()) ? null : pid;
        String stateParam = state == null ? null : state.name();
        try {
            List<BatchItem> result = itemDao.find(batchId, pid, null, stateParam, BatchItem.Type.OBJECT.name());
            tx.commit();
            return toBatchObjects(result);
        } catch (Throwable t) {
            tx.rollback();
            throw new IllegalStateException(String.format("batchId: %s, pid: %s", batchId, pid), t);
        } finally {
            tx.close();
        }
    }

    /**
     * Finds objects of given batch prepared for ingest.
     * @param batch batch to find
     * @return objects sorted according to RELS-EXT.
     */
    public List<BatchItemObject> findLoadedObjects(Batch batch) {
        List<BatchItemObject> items = findBatchObjects(batch.getId(), null, ObjectState.LOADED);
        if (batch.getState() == State.LOADING) {
            // issue 245: scheduled or loading batches do not have created or complete
            //      the root object! Timestamp order should be sufficient here.
            return items;
        }
        LocalObject root = getRootObject(batch);
        RelationEditor relationEditor = new RelationEditor(root);
        List<String> members;
        try {
            members = relationEditor.getMembers();
        } catch (DigitalObjectException ex) {
            throw new IllegalStateException(batch.toString(), ex);
        }
        ArrayList<BatchItemObject> result = new ArrayList<BatchItemObject>(items.size());
        for (String member : members) {
            if (items.isEmpty()) {
                throw new IllegalStateException(String.format("Unknown %s in %s", member, batch));
            }
            for (int i = 0; i < items.size(); i++) {
                BatchItemObject item = items.get(i);
                if (member.equals(item.getPid())) {
                    result.add(item);
                    items.remove(i);
                    break;
                }
            }
        }
        return result;
    }

    private List<BatchItemObject> toBatchObjects(List<BatchItem> items) {
        ArrayList<BatchItemObject> result = new ArrayList<BatchItemObject>(items.size());
        URI batchRoot = getBatchRoot();
        for (BatchItem item : items) {
            result.add(new BatchItemObject(item, batchRoot));
        }
        return result;
    }

    public BatchView viewBatch(int batchId) {
        List<BatchView> view = viewBatch(new BatchViewFilter().setBatchId(batchId).setOffset(0).setMaxCount(1));
        return view.get(0);
    }

    public List<BatchView> viewBatch(BatchViewFilter filter) {
        BatchDao dao = daos.createBatch();
        BatchItemDao itemDao = daos.createBatchItem();
        Transaction tx = daos.createTransaction();
        dao.setTransaction(tx);
        itemDao.setTransaction(tx);
        try {
            List<BatchView> result = dao.view(filter);
            Set<Batch.State> state = filter.getState();
            if (state != null && !state.contains(Batch.State.INGESTING_FAILED)) {
                return result;
            }
            // issue 265: ingest failures are stored within object items
            StringBuilder sb = new StringBuilder(1024);
            int threshold = 3;
            for (BatchView bv : result) {
                if (Batch.State.INGESTING_FAILED.name().equals(bv.getState())) {
                    List<BatchItem> failures = itemDao.find(bv.getId(), null, null,
                            BatchItem.ObjectState.INGESTING_FAILED.name(), BatchItem.Type.OBJECT.name());
                    sb.setLength(0);
                    if (bv.getLog() != null) {
                        sb.append(bv.getLog()).append("\n\n");
                    }
                    int itemCounter = 0;
                    for (BatchItem failure : failures) {
                        if (itemCounter >= threshold) {
                            sb.append("...\n\nTotal errors: ").append(failures.size()).append('\n');
                            break;
                        }
                        String log = failure.getLog();
                        if (log != null) {
                            ++itemCounter;
                            sb.append(failure.getFile()).append('\n');
                            sb.append(log).append("\n\n");
                        }
                    }
                    if (sb.length() > 0) {
                        bv.setLog(sb.toString());
                    }
                }
            }
            return result;
        } finally {
            tx.close();
        }
    }

    public List<BatchView> viewProcessingBatches(BatchViewFilter filter, UserProfile user, String roleUser) {
        BatchDao dao = daos.createBatch();
        BatchItemDao itemDao = daos.createBatchItem();
        Transaction tx = daos.createTransaction();
        dao.setTransaction(tx);
        itemDao.setTransaction(tx);
        try {
            List<BatchView> result = dao.view(filter);
            for (BatchView batchView : result) {
                batchView.setPageCount(batchView.getEstimateItemNumber() != null ? batchView.getEstimateItemNumber() : 0);
                batchView.setParentPid("SECRET");
                batchView.setProfileId("SECRET");
                batchView.setLog("SECRET");
            }
            if (user.getRole() == null || user.getRole().length() == 0 || user.getRole().equals(roleUser)) {
                for (BatchView batchView : result) {
                    batchView.setTitle("SECRET");
                    batchView.setUserId(0);
                    batchView.setUserName("SECRET");
                }
            }
            return result;
        } finally {
            tx.close();
        }
    }

    public Batch add(File folder, String title, UserProfile user, int itemNumber, ImportOptions options) {
        Batch batch = new Batch();
        batch.setCreate(new Timestamp(System.currentTimeMillis()));
        batch.setDevice(options.getDevice());
        batch.setEstimateItemNumber(itemNumber);
        String folderPath = relativizeBatchFile(folder);
        batch.setFolder(folderPath);
        batch.setGenerateIndices(options.isGenerateIndices());
        batch.setGeneratePageNumber(options.isGeneratePageNumber());
        batch.setPriority(options.getPriority());
        batch.setState(Batch.State.LOADING);
        batch.setTitle(title);
        batch.setUserId(user.getId());
        batch.setProfileId(options.getConfig().getProfileId());
        Batch updated = update(batch);
        updateFolderStatus(updated);
        return updated;
    }
    public Batch add(String pid, UserProfile user, String profile, State state) {
        Batch batch = new Batch();
        batch.setCreate(new Timestamp(System.currentTimeMillis()));
        batch.setDevice(null);
        batch.setEstimateItemNumber(null);
        batch.setFolder(pid);
        batch.setPriority(Batch.PRIORITY_MEDIUM);
        batch.setState(state);
        batch.setTitle(pid);
        batch.setUserId(user.getId());
        batch.setProfileId(profile);
        Batch updated = update(batch);
        return updated;
    }

    public Batch update(Batch update) {
        if (update == null) {
            throw new NullPointerException();
        }
        BatchDao batchDao = daos.createBatch();
        Transaction tx = daos.createTransaction();
        batchDao.setTransaction(tx);
        try {
            batchDao.update(update);
            tx.commit();
            return update;
        } catch (Throwable t) {
            tx.rollback();
            throw new IllegalStateException(String.valueOf(update), t);
        } finally {
            tx.close();
        }
    }

    public void updateFolderStatus(Batch batch) {
        File importFolder = resolveBatchFile(batch.getFolder());
        ImportFolderStatus ifs = new ImportFolderStatus(batch);
        JAXB.marshal(ifs, new File(importFolder, ImportFileScanner.IMPORT_STATE_FILENAME));
    }

    /**
     * Gets batch import folder status.
     * @param batch
     * @return status or {@code null} in case of empty file (backward compatibility)
     * @throws FileNotFoundException missing batch import folder
     */
    public ImportFolderStatus getFolderStatus(Batch batch) throws FileNotFoundException {
        File importFolder = resolveBatchFile(batch.getFolder());
        ImportFileScanner.validateImportFolder(importFolder);
        File f = new File(importFolder, ImportFileScanner.IMPORT_STATE_FILENAME);
        ImportFolderStatus ifs = null;
        if (f.exists() && f.length() > 0) {
            ifs = JAXB.unmarshal(f, ImportFolderStatus.class);
        }
        return ifs;
    }

    public List<Batch> findLoadingBatches() {
        BatchDao batchDao = daos.createBatch();
        Transaction tx = daos.createTransaction();
        batchDao.setTransaction(tx);
        try {
            return batchDao.findLoadingBatches();
        } finally {
            tx.close();
        }
    }

    public List<Batch> findExportingBatches() {
        BatchDao batchDao = daos.createBatch();
        Transaction tx = daos.createTransaction();
        batchDao.setTransaction(tx);
        try {
            return batchDao.findExportingBatches();
        } finally {
            tx.close();
        }
    }

    public List<Batch> findBatch(String pid, String processProfile, State state) {
        BatchDao batchDao = daos.createBatch();
        Transaction tx = daos.createTransaction();
        batchDao.setTransaction(tx);
        try {
            return batchDao.findBatch(pid, processProfile, state);
        } finally {
            tx.close();
        }
    }

    public String relativizeBatchFile(File file) {
        return file == null ? null : getBatchRoot().relativize(file.toURI()).toASCIIString();
    }

    public File resolveBatchFile(String file) {
        URI uri = getBatchRoot().resolve(file);
        return new File(uri);
    }

    URI getBatchRoot() {
        try {
            return appConfig.getDefaultUsersHome().toURI();
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

    public Batch get(int id) {
        BatchDao dao = daos.createBatch();
        Transaction tx = daos.createTransaction();
        dao.setTransaction(tx);
        try {
            return dao.find(id);
        } catch (Throwable t) {
            tx.rollback();
            throw new IllegalStateException(String.valueOf(id), t);
        } finally {
            tx.close();
        }
    }

    public BatchItemObject addLocalObject(Batch batchDb, LocalObject local) {
        File foxml = local.getFoxml();
        BatchItemDao bitemDao = daos.createBatchItem();
        Transaction tx = daos.createTransaction();
        bitemDao.setTransaction(tx);
        BatchItem batchItem = bitemDao.create();
        try {
            batchItem.setBatchId(batchDb.getId());
            batchItem.setFile(relativizeBatchFile(foxml));
            batchItem.setPid(local.getPid());
            batchItem.setState(ObjectState.LOADING.name());
            batchItem.setType(BatchItem.Type.OBJECT);
            bitemDao.update(batchItem);
            tx.commit();
            return new BatchItemObject(batchItem, getBatchRoot());
        } catch (Throwable ex) {
            tx.rollback();
            throw new IllegalStateException(String.valueOf(batchItem), ex);
        } finally {
            tx.close();
        }

    }

    public LocalObject getRootObject(Batch batch) {
        File folder = resolveBatchFile(batch.getFolder());
        LocalStorage storage = new LocalStorage();
        File targetBatchFolder = ImportProcess.getTargetFolder(folder, appConfig.getImportConfiguration(findImportProfile(batch.getId(), batch.getProfileId())));
        if (!targetBatchFolder.exists()) {
            throw new IllegalStateException(
                    String.format("Cannot resolve folder path: %s for %s!", targetBatchFolder, batch));
        }
        File root = new File(targetBatchFolder, ROOT_ITEM_FILENAME);
        LocalObject loRoot;
        if (root.exists()) {
            loRoot = storage.load(ROOT_ITEM_PID, root);
        } else {
            loRoot = storage.create(ROOT_ITEM_PID, root);
        }
        return loRoot;
    }

    private ConfigurationProfile findImportProfile(Integer batchId, String profileId) {
        ConfigurationProfile profile = appConfig.getProfiles().getProfile(ImportProfile.PROFILES, profileId);
        if (profile == null) {
            LOG.log(Level.SEVERE,"Batch {3}: Unknown profile: {0}! Check {1} in proarc.cfg",
                    new Object[]{ImportProfile.PROFILES, profileId, batchId});
            return null;
        }
        return profile;
    }

    public void addFileItem(int batchId, String pid, FileState state, List<FileEntry> files) {
        BatchItemDao bitemDao = daos.createBatchItem();
        Transaction tx = daos.createTransaction();
        bitemDao.setTransaction(tx);
        String filename = null;
        try {
            for (FileEntry file : files) {
                filename = file.getFile().getName();
                addFileItem(batchId, pid, state.name(), filename, bitemDao);
            }
            tx.commit();
        } catch (Throwable ex) {
            tx.rollback();
            throw new IllegalStateException(
                    String.format("batch: %s, pid: %s, state: %s, file: %s", batchId, pid, state, filename),
                    ex);
        } finally {
            tx.close();
        }
    }

    private BatchItem addFileItem(int batchId, String pid, String state, String file, BatchItemDao bitemDao) {
        BatchItem bitem = bitemDao.create();
        bitem.setBatchId(batchId);
        bitem.setFile(file);
        bitem.setPid(pid);
        bitem.setState(state);
        bitem.setType(BatchItem.Type.FILE);
        bitemDao.update(bitem);
        return bitem;
    }

    public void update(AbstractBatchItem item) {
        update(item.getItem());
    }
    
    public void update(BatchItem item) {
        BatchItemDao bitemDao = daos.createBatchItem();
        Transaction tx = daos.createTransaction();
        bitemDao.setTransaction(tx);
        try {
            bitemDao.update(item);
            tx.commit();
        } catch (Throwable ex) {
            tx.rollback();
            throw new IllegalStateException(String.valueOf(item), ex);
        } finally {
            tx.close();
        }
    }

    public boolean excludeBatchObject(Batch batch, String pid) {
        return excludeBatchObject(batch, pid == null ? null : Collections.singleton(pid));
    }

    public boolean excludeBatchObject(Batch batch, Collection<String> pids) {
        if (batch == null) {
            throw new NullPointerException("batch");
        }
        if (pids == null || pids.isEmpty()) {
            throw new IllegalArgumentException("pid");
        }
        BatchItemDao bitemDao = daos.createBatchItem();
        Transaction tx = daos.createTransaction();
        bitemDao.setTransaction(tx);
        try {
            List<BatchItem> items;
            if (pids.isEmpty()) {
                items = bitemDao.find(batch.getId(), null, null, null, BatchItem.Type.OBJECT.name());
                items = filterBatchItems(items, pids);
            } else {
                items = bitemDao.find(batch.getId(), pids.iterator().next(), null, null, BatchItem.Type.OBJECT.name());
            }
            if (items.isEmpty()) {
                return false;
            }
            List<BatchItemObject> objs = toBatchObjects(items);
            for (BatchItemObject obj : objs) {
                obj.setState(ObjectState.EXCLUDED);
                bitemDao.update(obj.getItem());
            }
            removeChildRelation(batch, null, pids);
            tx.commit();
            return true;
        } catch (Throwable ex) {
            tx.rollback();
            throw new IllegalStateException(String.format("pid: %s, %s", pids, batch), ex);
        } finally {
            tx.close();
        }
    }

    private List<BatchItem> filterBatchItems(List<BatchItem> items, Collection<String> pids) {
        if (items.isEmpty()) {
            return items;
        } else if (pids == null || pids.isEmpty()) {
            return Collections.emptyList();
        }
        ArrayList<BatchItem> result = new ArrayList<BatchItem>(pids.size());
        for (BatchItem item : items) {
            if (pids.contains(item.getPid())) {
                result.add(item);
            }
        }
        return result;
    }

    public boolean addChildRelation(Batch batch, String parentPid, String childPid) throws DigitalObjectException {
        if (batch == null) {
            throw new NullPointerException("batch");
        }
        LocalObject rootObject;
        if (parentPid == null) {
            rootObject = getRootObject(batch);
        } else {
            throw new UnsupportedOperationException();
        }
        RelationEditor relationEditor = new RelationEditor(rootObject);
        List<String> members = relationEditor.getMembers();
        if (members.contains(childPid)) {
            return false;
        }
        members.add(childPid);
        relationEditor.setMembers(members);
        relationEditor.write(relationEditor.getLastModified(), null);
        rootObject.flush();
        return true;
    }

    boolean removeChildRelation(Batch batch, String parentPid, Collection<String> childPid) throws DigitalObjectException {
        if (batch == null) {
            throw new NullPointerException("batch");
        }
        LocalObject rootObject;
        if (parentPid == null) {
            rootObject = getRootObject(batch);
        } else {
            throw new UnsupportedOperationException();
        }
        RelationEditor relationEditor = new RelationEditor(rootObject);
        List<String> members = relationEditor.getMembers();
        boolean changed = members.removeAll(childPid);
        if (changed) {
            relationEditor.setMembers(members);
            relationEditor.write(relationEditor.getLastModified(), null);
            rootObject.flush();
        }
        return changed;
    }

    /**
     * Clears all batch items and RELS-EXTs
     *
     * @param batch batch to reset
     */
    public void resetBatch(Batch batch) {
        if (batch == null) {
            throw new NullPointerException("batch");
        }
        batch.setState(State.LOADING);
        batch.setLog(null);
        BatchDao dao = daos.createBatch();
        BatchItemDao itemDao = daos.createBatchItem();
        Transaction tx = daos.createTransaction();
        dao.setTransaction(tx);
        itemDao.setTransaction(tx);
        try {
            dao.update(batch);
            itemDao.removeItems(batch.getId());
            tx.commit();
        } catch (Throwable t) {
            tx.rollback();
            throw new IllegalStateException(String.format("batch: %s", batch), t);
        } finally {
            tx.close();
        }
    }

    public static String toString(Throwable ex) {
        StringWriter sw = new StringWriter();
        ex.printStackTrace(new PrintWriter(sw, true));
        return sw.toString();
    }

    public static abstract class AbstractBatchItem {

        protected final BatchItem item;

        public AbstractBatchItem(BatchItem item) {
            this.item = item;
        }

        public BatchItem getItem() {
            return item;
        }

        public Integer getId() {
            return item.getId();
        }

        public Integer getBatchId() {
            return item.getBatchId();
        }

        public String getPid() {
            return item.getPid();
        }

        public String getLog() {
            return item.getLog();
        }

        public void setLog(String log) {
            item.setLog(log);
        }

        @Override
        public String toString() {
            return getClass().getSimpleName() + " as " + item.toString();
        }
    }

    public static class BatchItemObject extends AbstractBatchItem {

        private final URI root;

        BatchItemObject(BatchItem item, URI root) {
            super(item);
            this.root = root;
        }

        public File getFile() {
            URI uri = root.resolve(item.getFile());
            return new File(uri);
        }

        public ObjectState getState() {
            String state = item.getState();
            return state == null ? null : ObjectState.valueOf(state);
        }

        public void setState(ObjectState state) {
            item.setState(state == null ? null : state.name());
        }

    }

}
