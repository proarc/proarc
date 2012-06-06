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
package cz.incad.pas.editor.server.imports;

import cz.incad.pas.editor.server.config.PasConfiguration;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportBatch.State;
import cz.incad.pas.editor.server.imports.ImportProcess.ImportOptions;
import cz.incad.pas.editor.server.user.UserManager;
import cz.incad.pas.editor.server.user.UserProfile;
import cz.incad.pas.editor.server.user.UserUtil;
import java.io.File;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;

/**
 *
 * @author Jan Pokorsky
 */
public final class ImportBatchManager {

    private static final Logger LOG = Logger.getLogger(ImportBatchManager.class.getName());
    private static ImportBatchManager INSTANCE;

    /** memory storage for now */
    private final Map<Integer, ImportBatch> map = new HashMap<Integer, ImportBatch>();
    private PasConfiguration pasConfig;
    private int temp_itemSequence;

    /** XXX replace with guice */
    public static ImportBatchManager getInstance(PasConfiguration config) {
        synchronized (ImportBatchManager.class) {
            if (INSTANCE == null) {
                INSTANCE = new ImportBatchManager(config);
                load(config.getConfigHome(), INSTANCE, UserUtil.getDefaultManger());
            }
        }
        return INSTANCE;
    }

    static void load(File targetFolder, ImportBatchManager ibm, UserManager users) {
        File ibmXml = new File(targetFolder, "ImportBatchManager.xml");
        if (!ibmXml.exists()) {
            return ;
        }
        ImportBatchList list = JAXB.unmarshal(ibmXml, ImportBatchList.class);
        int itemCount = 0;
        for (ImportBatch batch : list.getBatches()) {
            UserProfile user = users.find(batch.getUserId());
            batch.userProfile = user;
            ibm.map.put(batch.getId(), batch);
            itemCount += batch.getItems().size();
        }
        ibm.temp_itemSequence = itemCount;
    }

    static void save(File targetFolder, ImportBatchManager ibm) {
        File ibmXml = new File(targetFolder, "ImportBatchManager.xml");
        synchronized (ibm.map) {
            ImportBatchList list = new ImportBatchList(ibm.map.values());
            JAXB.marshal(list, ibmXml);
        }
    }

    /** package private for unit tests */
    ImportBatchManager(PasConfiguration pasConfig) {
        this.pasConfig = pasConfig;
    }

//    public Collection<ImportBatch> findAll() {
//        return findAll(false);
//    }
    public List<ImportItem> findItems(int batchId, String pid) {
        synchronized (map) {
            ImportBatch batch = map.get(batchId);
            if (batch == null) {
                return null;
            }
            List<ImportItem> items = batch.getItems();
            if (pid != null && !pid.isEmpty()) {
                for (ImportItem item : items) {
                    if (pid.equals(item.getPid())) {
                        items = Collections.singletonList(item);
                        break;
                    }
                }
            }
            return items;
        }
    }

    public ImportItem findItem(String pid) {
        synchronized (map) {
            for (ImportBatch batch : map.values()) {
                for (ImportItem item : batch.getItems()) {
                    if (item.getPid().equals(pid)) {
                        return item;
                    }
                }
            }
        }
        return null;
    }

    public List<ImportBatch> findAll(UserProfile user) {
        return find(user, null, null);
    }
    
    public List<ImportBatch> find(UserProfile user, Integer batchId, State state) {
        synchronized(map) {
            List<ImportBatch> result = new ArrayList<ImportBatch>(map.size());
            for (ImportBatch b : map.values()) {
                boolean include = true;
                include &= user == null || b.getUserId() == user.getId();
                include &= state == null || state == b.getState();
                include &= batchId == null || batchId == b.getId();
                if (include) {
                    result.add(copyBatch(b, false));
                }
            }
            return result;
        }
    }

    public ImportBatch update(ImportBatch update) {
        if (update == null) {
            throw new NullPointerException();
        }

        synchronized (map) {
            ImportBatch batch = map.get(update.getId());
            if (batch == null) {
                return null;
            }

            updateBatch(update, batch);
            save(pasConfig.getConfigHome(), this);
            return copyBatch(batch, false);
        }
    }

    public ImportItem update(ImportItem item) {
        if (item == null) {
            throw new IllegalArgumentException("item");
        }
        synchronized (map) {
            ImportItem foundItem = findItem(item.getPid());
            if (item != foundItem) {
                throw new UnsupportedOperationException();
            }
            save(pasConfig.getConfigHome(), this);
            return item;
        }
    }

    public ImportBatch add(ImportBatch newBatch) {
        synchronized(map) {
            int id = map.isEmpty() ? 1 : Collections.max(map.keySet()) + 1;
            ImportBatch batch = updateBatch(newBatch, new ImportBatch(id));
            batch.timeStamp = new Date();
            map.put(id, batch);
            save(pasConfig.getConfigHome(), this);
            return copyBatch(batch, false);
        }
    }

    public ImportBatch get(int id) {
        synchronized (map) {
            ImportBatch batch = map.get(id);
            if (batch != null) {
                batch = copyBatch(batch, false);
            }
            return batch;
        }
    }

    public ImportItem addItem(int batchId, ImportItem item) {
        synchronized(map) {
            ImportBatch batch = map.get(batchId);
            if (batch == null) {
                throw new IllegalStateException("Unknown batch: " + batchId);
            }
            item.id = ++temp_itemSequence;
            item.batchId = batchId;
            batch.addItem(item);
            save(pasConfig.getConfigHome(), this);
            return item;
        }
    }

    public boolean removeItem(int batchId, String pid) {
        synchronized(map) {
            ImportBatch batch = map.get(batchId);
            if (batch == null) {
                return false;
            }
            for (Iterator<ImportItem> it = batch.getItems().iterator(); it.hasNext();) {
                ImportItem item = it.next();
                if (item.getPid().equals(pid)) {
                    it.remove();
                    save(pasConfig.getConfigHome(), this);
                    return true;
                }
            }
            return false;
        }
    }

    public boolean removeItems(int batchId) {
        synchronized(map) {
            ImportBatch batch = map.get(batchId);
            if (batch == null) {
                return false;
            }
            batch.getItems().clear();
            save(pasConfig.getConfigHome(), this);
            return true;
        }
    }

    private ImportBatch updateBatch(ImportBatch from, ImportBatch to) {
        to.description = from.description;
        to.estimateFileCount = from.estimateFileCount;
        to.failure = from.failure;
        to.folderPath = from.folderPath;
        to.options = from.options;
        to.parentPid = from.parentPid;
        to.state = from.state;
        to.user = from.user;
        to.userId = from.userId;
        to.timeStamp = from.timeStamp;
        return to;
    }
    
    private ImportBatch copyBatch(ImportBatch batch, boolean withItems) {
        ImportBatch copy = updateBatch(batch, new ImportBatch(batch.getId()));
        List<ImportItem> items = batch.getItems();
        if (withItems && items != null) {
            ArrayList<ImportItem> itemsCopy = new ArrayList<ImportItem>(items.size());
            for (ImportItem item : items) {
                itemsCopy.add(item.copy());
            }
        }
        return copy;
    }

    /** do not use outside unit tests */
    Map<Integer, ImportBatch> getMap() {
        return map;
    }

    @javax.xml.bind.annotation.XmlRootElement(name="batch")
    @javax.xml.bind.annotation.XmlAccessorType(XmlAccessType.FIELD)
    public static class ImportBatch {

        public enum State {
            EMPTY, LOADING, LOADING_FAILED, LOADED, INGESTING, INGESTING_FAILED, INGESTED
        }

        @XmlElement(required=true)
        private int id;
        private String folderPath;
        private String description;
        private String parentPid;
        @XmlSchemaType(name="dateTime")
        private Date timeStamp;
        private int userId;
        private String user;
        private State state;
        /** list of batch option fromString context asString be persisted*/
        private String options;
        private int estimateFileCount;
        private String failure;
        private List<ImportItem> items;
        private transient UserProfile userProfile;

        public ImportBatch() {
        }

        public ImportBatch(Integer id) {
            this.id = id;
        }

        public ImportBatch(Integer id, File folderPath, String description, Date timeStamp, UserProfile user, State state) {
            this(id, folderPath.toURI().toASCIIString(), description, timeStamp, user, state, null);
        }

        ImportBatch(Integer id, String folderPath, String description, Date timeStamp, UserProfile user, State state, String failure) {
            this.id = id;
            this.folderPath = folderPath;
            this.description = description;
            this.timeStamp = timeStamp;
            this.userId = user.getId();
            this.user = user.getUserName();
            this.userProfile = user;
            this.state = state;
            this.failure = failure;
        }

        private void addItem(ImportItem item) {
            List<ImportItem> l = getItems();
            l.add(item);
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(String description) {
            this.description = description;
        }

        private List<ImportItem> getItems() {
            if (items == null) {
                items = new ArrayList<ImportItem>();
            }
            return items;
        }

        public String getFolderPath() {
            return folderPath;
        }

        public void setFolderPath(File folder) {
            setFolderPath(folder.toURI().toASCIIString());
        }

        public void setFolderPath(String folderPath) {
            this.folderPath = folderPath;
        }

        public int getId() {
            return id;
        }

        public String getParentPid() {
            return parentPid;
        }

        public void setParentPid(String parentPid) {
            this.parentPid = parentPid;
        }

        public Date getTimeStamp() {
            return timeStamp;
        }

        public int getUserId() {
            return userId;
        }

        public void setUserId(int userId) {
            this.userId = userId;
        }

        public String getUser() {
            return user;
        }

        public void setUser(String user) {
            this.user = user;
        }

        public State getState() {
            return state;
        }

        public void setState(State state) {
            this.state = state;
        }

        public String getFailure() {
            return failure;
        }

        public void setFailure(String failure) {
            this.failure = failure;
        }

        public ImportOptions getOptions() {
            File importFolder = new File(URI.create(getFolderPath()));
            ImportOptions config = ImportOptions.fromString(importFolder, getUser(), options);
            return config;
        }

        public void setOptions(ImportOptions context) {
            this.options = context.asString();
        }

        public int getEstimateFileCount() {
            return estimateFileCount;
        }

        public void setEstimateFileCount(int estimateFileCount) {
            this.estimateFileCount = estimateFileCount;
        }

    }

    /**
     * Describes imported digital object.
     * In case the special RELS-EXT relation (importBatch->batchId) will exist
     * it could be read from triple store index. Advantage of this solution
     * would be always live content. Otherwise we should update imports table
     * on Digital Object modifications.
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class ImportItem {
        @XmlElement(required=true)
        private Integer id;
        private Integer batchId;
        private String foxml;
        private String filename;
        private String pid;
        private String failure;
        private String failureDescription;

        private ImportItem() {
        }

        public ImportItem(File foxml, String originalFilename, String pid) {
            this(foxml.toURI().toASCIIString(), originalFilename, pid);
        }
        
        public ImportItem(String foxml, String originalFilename, String pid) {
            this.foxml = foxml;
            this.filename = originalFilename;
            this.pid = pid;
        }

        public String getFilename() {
            return filename;
        }

        public String getPid() {
            return pid;
        }

        public String getFoxml() {
            return foxml;
        }

        public File getFoxmlAsFile() {
            URI uri = URI.create(foxml);
            return new File(uri);
        }

        public String getFailure() {
            return failure;
        }

        public void setFailure(String failure) {
            this.failure = failure;
        }

        public String getFailureDescription() {
            return failureDescription;
        }

        public void setFailureDescription(String failureDescription) {
            this.failureDescription = failureDescription;
        }

        ImportItem copy() {
            ImportItem copy = new ImportItem(foxml, filename, pid);
            copy.id = id;
            copy.batchId = batchId;
            return copy;
        }

    }

    @XmlRootElement(name="batches")
    @XmlAccessorType(XmlAccessType.FIELD)
    static class ImportBatchList {

        @XmlElement(name="batch")
        private Collection<ImportBatch> batches;

        public ImportBatchList() {}

        public ImportBatchList(Collection<ImportBatch> batches) {
            this.batches = batches;
        }

        public Collection<ImportBatch> getBatches() {
            return batches;
        }

    }

}
