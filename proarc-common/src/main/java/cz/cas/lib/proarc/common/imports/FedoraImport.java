/*
 * Copyright (C) 2012 Jan Pokorsky
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

import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * XXX write result to proarc_import_status.log
 *
 * @author Jan Pokorsky
 */
public final class FedoraImport {

    private static final Logger LOG = Logger.getLogger(FedoraImport.class.getName());
    private final RemoteStorage fedora;
    private final ImportBatchManager ibm;
    private final SearchView search;

    public FedoraImport(RemoteStorage fedora, ImportBatchManager ibm) {
        this.fedora = fedora;
        this.search = fedora.getSearch();
        this.ibm = ibm;
    }

    public Batch importBatch(Batch batch, String importer, String message) throws DigitalObjectException {
        checkParent(batch.getParentPid());
        boolean repair = false;
        if (batch.getState() == Batch.State.INGESTING_FAILED) {
            repair = true;
            batch.setLog(null);
        } else if (batch.getState() != Batch.State.LOADED) {
            throw new IllegalStateException("Invalid batch state: " + batch);
        }
        batch.setState(Batch.State.INGESTING);
        batch = ibm.update(batch);
        String parentPid = batch.getParentPid();
        long startTime = System.currentTimeMillis();
        ArrayList<String> ingestedPids = new ArrayList<String>();
        try {
            boolean itemFailed = importItems(batch, importer, ingestedPids, repair);
            addParentMembers(parentPid, ingestedPids, message);
            batch.setState(itemFailed ? Batch.State.INGESTING_FAILED : Batch.State.INGESTED);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, String.valueOf(batch), t);
            batch.setState(Batch.State.INGESTING_FAILED);
            batch.setLog(ImportBatchManager.toString(t));
        } finally {
            batch = ibm.update(batch);
            LOG.log(Level.FINE, "Total ingest time {0} ms. Ingested items: {1}.\n{2}",
                    new Object[]{System.currentTimeMillis() - startTime, ingestedPids.size(), batch});
        }
        return batch;
    }

    private boolean importItems(Batch batch, String importer, List<String> ingests, boolean repair)
            throws DigitalObjectException {

        LocalObject root = ibm.getRootObject(batch);
        RelationEditor rootRels = new RelationEditor(root);
        List<String> batchMemberPids = rootRels.getMembers();
        for (String batchMemberPid : batchMemberPids) {
            BatchItemObject item = ibm.findBatchObject(batch.getId(), batchMemberPid);
            item = importItem(item, importer, repair);
            if (item != null) {
                if (ObjectState.INGESTING_FAILED == item.getState()) {
                    return true;
                } else {
                    ingests.add(item.getPid());
                }
            }
        }
        return false;
    }

    /**
     * Fedora ingest of an import item.
     * @param item item to import
     * @param importer who imports
     * @param repair {@code true} if the item is from the failed ingest.
     * @return the import item with proper state or {@code null} if the item
     *      was skipped
     */
    public BatchItemObject importItem(BatchItemObject item, String importer, boolean repair) {
        try {
            if (repair) {
                item = repairItemImpl(item, importer);
            } else {
                item = importItemImpl(item, importer);
            }
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, String.valueOf(item), t);
            item.setState(ObjectState.INGESTING_FAILED);
            item.setLog(ImportBatchManager.toString(t));
        }
        if (item != null) {
            ibm.update(item);
        }
        return item;
    }

    /**
     * Fedora ingest of an import item coming from a failed ingest.
     * @param item item to analyze and import
     * @param importer who imports
     * @return the import item with proper state or {@code null} if the item
     *      was skipped
     * @throws DigitalObjectException failure
     */
    private BatchItemObject repairItemImpl(BatchItemObject item, String importer) throws DigitalObjectException, IOException, FedoraClientException {
        ObjectState state = item.getState();
        if (state == ObjectState.LOADED) {
            // ingest
            return importItemImpl(item, importer);
        } else if (state == ObjectState.INGESTED) {
            // check parent
            String itemPid = item.getPid();
            List<Item> parents = search.findReferrers(itemPid);
            if (parents.isEmpty()) {
                boolean existRemotely = fedora.exist(itemPid);
                if (existRemotely) {
                    // ingested but not linked
                    return item;
                } else {
                    // broken ingest > ingest again + link
                    item.setState(ObjectState.LOADED);
                    item.setLog(null);
                    return importItemImpl(item, importer);
                }
            } else {
                // ingested and linked > skip
                return null;
            }
        } else if (state == ObjectState.INGESTING_FAILED) {
            // ingest again
            item.setState(ObjectState.LOADED);
            item.setLog(null);
            return importItemImpl(item, importer);
        } else {
            return null;
        }
    }

    /**
     * Fedora ingest of an import item.
     * @param item item to import
     * @param importer who imports
     * @return the import item with proper state or {@code null} if the item
     *      was skipped
     * @throws DigitalObjectException failure
     */
    private BatchItemObject importItemImpl(BatchItemObject item, String importer) throws DigitalObjectException {
        ObjectState state = item.getState();
        if (state != ObjectState.LOADED) {
            return null;
        }
        File foxml = item.getFile();
        if (foxml == null || !foxml.exists() || !foxml.canRead()) {
            throw new IllegalStateException("Cannot read foxml: " + foxml);
        }
        fedora.ingest(foxml, item.getPid(), importer,
                "Ingested with ProArc by " + importer
                + " from local file " + foxml);
        item.setState(ObjectState.INGESTED);
        return item;
    }

    private void addParentMembers(String parent, List<String> pids, String message) throws DigitalObjectException {
        if (parent == null || pids.isEmpty()) {
            return ;
        }
        RemoteObject remote = fedora.find(parent);
        RelationEditor editor = new RelationEditor(remote);
        List<String> members = editor.getMembers();
        members.addAll(pids);
        editor.setMembers(members);
        editor.write(editor.getLastModified(), message);
        remote.flush();
    }

    private void checkParent(String parent) throws DigitalObjectException {
        if (parent == null) {
            return ;
        }
        RemoteObject remote = fedora.find(parent);
        RelationEditor editor = new RelationEditor(remote);
        editor.getModel();
    }

}
