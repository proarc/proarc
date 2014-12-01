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

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * XXX write result to proarc_import_status.log
 * XXX needs tests
 *
 * @author Jan Pokorsky
 */
public final class FedoraImport {

    private static final Logger LOG = Logger.getLogger(FedoraImport.class.getName());
    private final RemoteStorage fedora;
    private final ImportBatchManager ibm;

    public FedoraImport(RemoteStorage fedora, ImportBatchManager ibm) {
        this.fedora = fedora;
        this.ibm = ibm;
    }

    public Batch importBatch(Batch batch, String importer, String message) throws DigitalObjectException {
        checkParent(batch.getParentPid());
        if (batch.getState() == Batch.State.INGESTING_FAILED) {
            // reset
            List<BatchItemObject> allItems = ibm.findBatchObjects(batch.getId(), null);
            for (BatchItemObject item : allItems) {
                if (item.getState() == ObjectState.INGESTING_FAILED) {
                    item.setState(ObjectState.LOADED);
                    item.setLog(null);
                    ibm.update(item);
                }
            }
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
            boolean itemFailed = importItems(batch, importer, ingestedPids);
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

    private boolean importItems(Batch batch, String importer, List<String> ingests) {
        for (BatchItemObject item : ibm.findLoadedObjects(batch)) {
            item = importItem(item, importer);
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

    private BatchItemObject importItem(BatchItemObject item, String importer) {
        try {
            return importItemImpl(item, importer);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, item + "\n" + ImportBatchManager.toString(t), t);
            item.setState(ObjectState.INGESTING_FAILED);
            item.setLog(ImportBatchManager.toString(t));
            ibm.update(item);
            return item;
        }
    }

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
        ibm.update(item);
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
