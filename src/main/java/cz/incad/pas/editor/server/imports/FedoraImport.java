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
package cz.incad.pas.editor.server.imports;

import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.incad.pas.editor.server.fedora.RemoteStorage;
import cz.incad.pas.editor.server.fedora.RemoteStorage.RemoteObject;
import cz.incad.pas.editor.server.fedora.relation.RelationEditor;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportBatch;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportItem;
import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * XXX write result to proarch_import_status.log
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

    public ImportBatch importBatch(int batchId, String importer) throws FedoraClientException {
        ImportBatch batch = ibm.update(batchId, ImportBatch.State.INGESTING);
        if (batch == null) {
            throw new IllegalArgumentException("batch not found: " + batchId);
        }
        String parentPid = batch.getParentPid();
        ArrayList<String> ingests = new ArrayList<String>();
        ArrayList<String> failures = new ArrayList<String>();
        boolean done = false;
        try {
            for (ImportItem item : batch.getItems()) {
                try {
                    importItem(item, importer);
                    ingests.add(item.getPid());
                } catch (Throwable t) {
                    item.setFailure("Fedora ingest failed. " + t.getMessage());
                    StringWriter dump = new StringWriter();
                    t.printStackTrace(new PrintWriter(dump));
                    item.setFailureDescription(dump.toString());
                    failures.add(item.getPid());
                    LOG.log(Level.SEVERE, "batch: {0}, PID: {1}, file: {2}",
                            new Object[]{batch.getId(), item.getPid(), item.getFoxml()});
                    ibm.update(item);
                }
            }

            addParentMembers(parentPid, ingests);
            done = true;
        } finally {
            if (done && failures.isEmpty()) {
                batch = ibm.update(batchId, ImportBatch.State.INGESTED);
            } else {
                batch = ibm.update(batchId, ImportBatch.State.INGESTING_FAILED);
            }
        }
        return batch;
    }

    private void importItem(ImportItem item, String importer) throws FedoraClientException {
        File foxml = item.getFoxmlAsFile();
        if (foxml == null || !foxml.exists() || !foxml.canRead()) {
            throw new IllegalStateException("Cannot read foxml: " + foxml);
        }
//        LocalObject local = istorage.load(item.getPid(), foxml);
        fedora.ingest(foxml, item.getPid(), importer, "Ingested with Proarch from local file " + foxml);
    }

    private void addParentMembers(String parent, List<String> pids) {
        RemoteObject remote = fedora.find(parent);
        RelationEditor editor = new RelationEditor(remote);
        List<String> members = editor.getMembers();
        members.addAll(pids);
        editor.setMembers(members);
        editor.write(editor.getLastModified());
        remote.flush();
    }

}
