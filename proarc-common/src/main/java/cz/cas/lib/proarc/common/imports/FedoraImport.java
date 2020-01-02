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
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.device.DeviceRepository;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.user.UserProfile;
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
    private final LocalStorage localStorage;
    private final ImportBatchManager ibm;
    private final SearchView search;
    private final UserProfile user;
    private final AppConfiguration config;

    public FedoraImport(AppConfiguration config, RemoteStorage fedora, ImportBatchManager ibm, UserProfile user) {
        this.config = config;
        this.fedora = fedora;
        this.search = fedora.getSearch();
        this.ibm = ibm;
        this.user = user;
        this.localStorage = new LocalStorage();
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
            addParentMembers(batch, parentPid, ingestedPids, message);
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
        List<BatchItemObject> batchItems = ibm.findBatchObjects(batch.getId(), null);
        if (batch.getParentPid() != null) {
            // in case of including items in a parent object it is neccessary to sort the ingests
            batchItems = sortItems(batch, batchItems);
        }
        for (BatchItemObject item : batchItems) {
            item = importItem(item, importer, repair);
            if (item != null) {
                if (ObjectState.INGESTING_FAILED == item.getState()) {
                    batch.setLog(item.getLog());
                    return true;
                } else {
                    ingests.add(item.getPid());
                }
            }
        }
        return false;
    }

    /**
     * Sorts the batch items according to the RELS-EXT members of the parent object.
     */
    private ArrayList<BatchItemObject> sortItems(Batch batch, List<BatchItemObject> batchItems) throws DigitalObjectException {
        LocalObject root = ibm.getRootObject(batch);
        RelationEditor rootRels = new RelationEditor(root);
        List<String> batchMemberPids = rootRels.getMembers();
        ArrayList<BatchItemObject> result = new ArrayList<BatchItemObject>(batchItems.size());
        for (String member : batchMemberPids) {
            if (batchItems.isEmpty()) {
                throw new DigitalObjectException(member, batch.getId(), null,
                        String.format("Unknown %s in %s", member, root.getPid()), null);
            }
            for (int i = 0; i < batchItems.size(); i++) {
                BatchItemObject item = batchItems.get(i);
                if (member.equals(item.getPid())) {
                    result.add(item);
                    batchItems.remove(i);
                    break;
                }
            }
        }
        return result;
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
            if (item.getState() == ObjectState.EXCLUDED) {
                return null;
            }
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
        LocalObject lobj = localStorage.load(item.getPid(), foxml);
        if (lobj.isRemoteCopy()) {
            RemoteObject rObj = fedora.find(item.getPid());
            RelationEditor localRelEditor = new RelationEditor(lobj);
            if (!DeviceRepository.METAMODEL_AUDIODEVICE_ID.equals(localRelEditor.getModel()) || !DeviceRepository.METAMODEL_ID.equals(localRelEditor.getModel())) {
                List<String> members = localRelEditor.getMembers();
                RelationEditor remoteRelEditor = new RelationEditor(rObj);
                List<String> oldMembers = remoteRelEditor.getMembers();
                if (!oldMembers.equals(members)) {
                    if (!members.containsAll(oldMembers)) {
                        throw new DigitalObjectException(lobj.getPid(),
                                item.getBatchId(), RelationEditor.DATASTREAM_ID,
                                "The members of the remote object have changed!", null);
                    }
                    remoteRelEditor.setMembers(members);
                    remoteRelEditor.write(remoteRelEditor.getLastModified(), "The ingest from " + foxml);
                    rObj.flush();
                }
            }
        } else {
            fedora.ingest(foxml, item.getPid(), importer,
                    "Ingested with ProArc by " + importer
                    + " from local file " + foxml);
        }
        item.setState(ObjectState.INGESTED);
        return item;
    }

    private void addParentMembers(Batch batch, String parent, List<String> pids, String message) throws DigitalObjectException {
        if (parent == null || pids.isEmpty()) {
            return ;
        }
        ConfigurationProfile profile = findImportProfile(batch.getId(), batch.getProfileId());
        ImportProfile importProfile = profile != null ? config.getImportConfiguration(profile) : config.getImportConfiguration();
        if (ConfigurationProfile.DEFAULT_SOUNDRECORDING_IMPORT.equals(batch.getProfileId()) &&
                importProfile.getCreateModelsHierarchy()) {
            createHierarchy(batch, pids, parent, message);
        }
        if (pids.size()!= 0){
            setParent(parent, pids, message);
        }
    }

    private ConfigurationProfile findImportProfile(Integer batchId, String profileId) {
        ConfigurationProfile profile = config.getProfiles().getProfile(ImportProfile.PROFILES, profileId);
        if (profile == null) {
            LOG.log(Level.SEVERE,"Batch {3}: Unknown profile: {0}! Check {1} in proarc.cfg",
                    new Object[]{ImportProfile.PROFILES, profileId, batchId});
            return null;
        }
        return profile;
    }

    private void createHierarchy(Batch batch, List<String> pids, String documentPid, String message) {
        List<BatchItemObject> batchItems = ibm.findBatchObjects(batch.getId(), null);
        ArrayList<Hierarchy> songsPid = new ArrayList<>();
        ArrayList<ArrayList<Hierarchy>> tracksPid = new ArrayList<>();

        boolean hierarchyCreated = createPidHierarchy(batchItems, documentPid, songsPid, tracksPid, pids);

        if (!hierarchyCreated) {
            return;
        }
        try {
            if (tracksPid.size() == 0) {
                createModels(documentPid, songsPid, message);
            } else if (tracksPid.size() != 0) {
                createModels(documentPid, songsPid, tracksPid, message);
            }
        } catch (DigitalObjectException ex) {
            LOG.log(Level.WARNING, "Nepodarilo se automaticky vytvorit hierarchii objektu, protoze se nepodarilo vytvorit objekt " + ex.getPid());
            return;
        }
        return;
    }

    private void createModels(String documentPid, ArrayList<Hierarchy> songsPid, String message)  throws  DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        String pid = "";
        for (Hierarchy song : songsPid) {
            if (pid != song.getParent()) {
                pid = song.getParent();
                DigitalObjectManager.CreateHandler songHandler = dom.create(NdkAudioPlugin.MODEL_SONG, pid, documentPid, user, null, "create new object with pid: " + songsPid.get(0));
                songHandler.create();
            }
            List<String> songList = new ArrayList<>();
            songList.add(song.getChild());
            setParent(song.getParent(), songList, message);
        }
    }

    private void createModels(String documentPid, ArrayList<Hierarchy> songsPid, ArrayList<ArrayList<Hierarchy>> tracksPid, String message) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        for (int tmp = 0; tmp < songsPid.size(); tmp++) {
            String songPid = songsPid.get(tmp).getParent();
            DigitalObjectManager.CreateHandler songHandler = dom.create(NdkAudioPlugin.MODEL_SONG, songPid, documentPid, user, null,  "create new object with pid: " + songsPid.get(0));
            songHandler.create();

            for (Hierarchy track : tracksPid.get(tmp)) {
                DigitalObjectManager.CreateHandler trackHandler = dom.create(NdkAudioPlugin.MODEL_TRACK, track.getParent(), songPid, user, null,  "create new object with pid: " + songsPid.get(0));
                trackHandler.create();

                List<String> trackList = new ArrayList<>();
                trackList.add(track.getChild());
                setParent(track.getParent(), trackList, message);
            }
        }

    }

    private void setParent(String parent, List<String> pids, String message) throws DigitalObjectException {
        RemoteObject remote = fedora.find(parent);
        RelationEditor editor = new RelationEditor(remote);
        List<String> members = editor.getMembers();
        members.addAll(pids);
        editor.setMembers(members);
        editor.write(editor.getLastModified(), message);
        remote.flush();
    }

    private boolean createPidHierarchy(List<BatchItemObject> batchItems, String documentPid, ArrayList<Hierarchy> songsPid, ArrayList<ArrayList<Hierarchy>> tracksPid, List<String> pids) {
        pids.clear();
        String pid = "";
        int pidObjektu = 0;
        for (BatchItemObject batchItem : batchItems) {
            String name = nameWithoutExtention(batchItem.getFile().getName(), ".foxml");
            String[] splitName = name.split("-");

            try {
                int disc = Integer.valueOf(splitName[splitName.length-3]);
                int song = Integer.valueOf(splitName[splitName.length-2]);
                int track = Integer.valueOf(splitName[splitName.length-1]);

                if (splitName[splitName.length-3].length() == 2) {
                    if (disc < 1 || song < 1 || track < 1) {
                        LOG.log(Level.WARNING, "Spatna hodnota v nazvu souboru. Nepodarilo se automaticky vytvorit hierarchii objektu: " + splitName + ".");
                        return false;
                    }
                    if (songsPid.size() < song) {
                        pid = FoxmlUtils.createPid();
                        Hierarchy songHierarchy = new Hierarchy(pid, null);
                        songsPid.add(song - 1, songHierarchy);
                        tracksPid.add(song - 1, new ArrayList<>());
                    }
                    if (tracksPid.get(song - 1).size() < track) {
                        pid = FoxmlUtils.createPid();
                        Hierarchy trackHierarchy = new Hierarchy(pid, batchItem.getPid());
                        tracksPid.get(song - 1).add(track - 1, trackHierarchy);
                    }
                } else {
                    int id = Integer.valueOf(splitName[splitName.length-3]);
                    int deskaPid = Integer.valueOf(splitName[splitName.length-2]);
                    int songPid = Integer.valueOf(splitName[splitName.length-1]);

                    if (id < 1 || deskaPid < 1 || songPid < 1) {
                        LOG.log(Level.WARNING, "Spatna hodnota v nazvu souboru. Nepodarilo se automaticky vytvorit hierarchii objektu: " + splitName + ".");
                        return false;
                    }

                    if (pidObjektu != songPid) {
                        pid = FoxmlUtils.createPid();
                        pidObjektu = songPid;
                    }
                    Hierarchy songHierarchy = new Hierarchy(pid, batchItem.getPid());
                    songsPid.add(songHierarchy);
                }

            } catch (NumberFormatException ex) {
                pids.add(batchItem.getPid());
                continue;
            }

        }
        return true;
    }

    private String nameWithoutExtention(String name, String extension) {
        return name.substring(0, name.length() - extension.length());
    }

    private void checkParent(String parent) throws DigitalObjectException {
        if (parent == null) {
            return ;
        }
        RemoteObject remote = fedora.find(parent);
        RelationEditor editor = new RelationEditor(remote);
        editor.getModel();
    }

    class Hierarchy {
        String parent;
        String child;

        public Hierarchy(String parent, String child) {
            this.parent = parent;
            this.child = child;
        }

        public String getParent() {
            return parent;
        }

        public void setParent(String parent) {
            this.parent = parent;
        }

        public String getChild() {
            return child;
        }

        public void setChild(String child) {
            this.child = child;
        }
    }


}
