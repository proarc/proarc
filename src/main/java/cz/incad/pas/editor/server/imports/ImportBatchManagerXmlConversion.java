/*
 * Copyright (C) 2013 Jan Pokorsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.server.imports;

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchDao;
import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dao.BatchItem.FileState;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.dao.BatchItem.StreamState;
import cz.cas.lib.proarc.common.dao.BatchItemDao;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.dao.Transaction;
import cz.incad.pas.editor.server.config.AppConfiguration;
import cz.incad.pas.editor.server.fedora.DigitalObjectException;
import cz.incad.pas.editor.server.fedora.LocalStorage;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalObject;
import cz.incad.pas.editor.server.fedora.relation.RelationEditor;
import cz.incad.pas.editor.server.imports.ImportProcess.ImportOptions;
import cz.incad.pas.editor.server.user.UserManager;
import cz.incad.pas.editor.server.user.UserProfile;
import cz.incad.pas.editor.shared.rest.ImportResourceApi;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.net.URI;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;
import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;

/**
 * Converts XML batch repository to EmpireDB
 *
 * @author Jan Pokorsky
 */
public class ImportBatchManagerXmlConversion {

    private static final Logger LOG = Logger.getLogger(ImportBatchManagerXmlConversion.class.getName());
    private static final String XML_FILENAME = "ImportBatchManager.xml";

    private DaoFactory daoFactory;
    private BatchDao batchDao;
    private AppConfiguration config;
    private UserManager userManagerSql;
    private BatchItemDao bitemDao;

    public ImportBatchManagerXmlConversion(DaoFactory daoFactory, AppConfiguration config, UserManager userManagerSql) {
        this.daoFactory = daoFactory;
        this.config = config;
        this.userManagerSql = userManagerSql;
        batchDao = daoFactory.createBatch();
        bitemDao = daoFactory.createBatchItem();
    }

    public void convertXml2Db() {
        File ibmXml = new File(config.getConfigHome(), XML_FILENAME);
        if (!ibmXml.exists()) {
            return ;
        }
        LOG.info("Import Batch XML repository found: " + ibmXml + "Starting conversion to RDBMS...");
        Transaction tx = daoFactory.createTransaction();
        batchDao.setTransaction(tx);
        bitemDao.setTransaction(tx);
        try {
            load(ibmXml, null, userManagerSql, config.getDefaultUsersHome().toURI());
            tx.commit();
        } catch (Exception e) {
            tx.rollback();
            throw new IllegalStateException(e);
        } finally {
            tx.close();
        }
        ibmXml.renameTo(new File(ibmXml.getParentFile(), ibmXml.getName() + ".backup"));
        LOG.info("...Conversion done.");
    }

    void load(File ibmXml, ImportBatchManager ibm, UserManager users,
            URI usersHome
            ) throws DigitalObjectException {

        ImportBatchList list = JAXB.unmarshal(ibmXml, ImportBatchList.class);
        LocalStorage storage = new LocalStorage();
        for (ImportBatch batch : list.getBatches()) {
            UserProfile user = users.find(batch.getUserId());
            batch.userProfile = user;

            Batch batchDb = batchDao.create();
            batchDb.setCreate(new Timestamp(batch.getTimeStamp().getTime()));
            ImportOptions options = batch.getOptions();
            options.setTargetFolder(ImportProcess.getTargetFolder(new File(URI.create(batch.getFolderPath()))));
            batchDb.setDevice(options.getDevice());
            batchDb.setEstimateItemNumber(batch.getEstimateFileCount());
            batchDb.setFolder(usersHome.relativize(URI.create(batch.getFolderPath())).toASCIIString());
            batchDb.setGenerateIndices(options.isGenerateIndices());
            batchDb.setLog(batch.getFailure());
            batchDb.setParentPid(batch.getParentPid());
            batchDb.setStateAsString(batch.getState().name()); // XXX check values compatibility
            batchDb.setTitle(batch.getDescription());
            batchDb.setUserId(batch.getUserId());
            batch.getDescription();
            batch.getFailure();
            LOG.info(batchDb.toString());
            batchDao.update(batchDb);

            List<String> relsExt = new ArrayList<String>();

            List<ImportItem> items = batch.getItems();
            for (ImportItem item : items) {
                addLocalObject(batchDb, item, usersHome, batch, relsExt);
            }

            File targetFolder = options.getTargetFolder();

            if (targetFolder.exists()) {
                File rootFoxml = new File(targetFolder, ImportBatchManager.ROOT_ITEM_FILENAME);
                LocalObject loRoot = storage.create(ImportBatchManager.ROOT_ITEM_PID, rootFoxml);
                RelationEditor relationEditor = new RelationEditor(loRoot);
                relationEditor.setMembers(relsExt);
                relationEditor.write(relationEditor.getLastModified(), null);
                loRoot.flush();
            }
        }
    }

    private void addLocalObject(Batch batchDb, ImportItem item, URI usersHome, ImportBatch batch, List<String> relsExt) {
        BatchItem batchItem = bitemDao.create();
        batchItem.setBatchId(batchDb.getId());
        batchItem.setFile(usersHome.relativize(URI.create(item.getFoxml())).toASCIIString());
        batchItem.setLog(item.getFailureDescription());
        batchItem.setPid(item.getPid());
        ObjectState istate = resolveObjectState(item, batch);
        batchItem.setState(istate.name());
        batchItem.setType(BatchItem.Type.OBJECT);
        bitemDao.update(batchItem);
        if (istate == ObjectState.LOADED || istate == ObjectState.INGESTED || istate == ObjectState.INGESTING_FAILED) {
            relsExt.add(batchItem.getPid());
        }

        addFileLog(batchDb, item);

//        addDatastreamLog(ModsStreamEditor.DATASTREAM_ID, batchDb, item);
//        addDatastreamLog(DcStreamEditor.DATASTREAM_ID, batchDb, item);
//        addDatastreamLog(StringEditor.OCR_ID, batchDb, item);
//        addDatastreamLog(BinaryEditor.FULL_ID, batchDb, item);
//        addDatastreamLog(BinaryEditor.PREVIEW_ID, batchDb, item);
//        addDatastreamLog(BinaryEditor.THUMB_ID, batchDb, item);
//        addDatastreamLog(BinaryEditor.RAW_ID, batchDb, item);
    }

    private void addDatastreamLog(String dsId, Batch batchDb, ImportItem item) {
        BatchItem bitem = bitemDao.create();
        bitem.setBatchId(batchDb.getId());
        bitem.setPid(item.getPid());
        bitem.setDsId(dsId);
        bitem.setState(StreamState.UNKNOWN.name());
        bitem.setType(BatchItem.Type.DATASTREAM);
        bitemDao.update(bitem);
    }

    private void addFileLog(Batch batchDb, ImportItem item) {
        BatchItem bitem = bitemDao.create();
        bitem.setBatchId(batchDb.getId());
        bitem.setFile(item.getFilename() + ".tiff");
        bitem.setPid(item.getPid());
        bitem.setState(FileState.OK.name());
        bitem.setType(BatchItem.Type.FILE);
        bitemDao.update(bitem);
    }

    private static ObjectState resolveObjectState(ImportItem item, ImportBatch batch) {
        ObjectState istate = null;
        boolean ingestFailed = item.getFailure() != null;
        switch (batch.getState()) {
            case LOADED:
                istate = ObjectState.LOADED;
                break;
            case LOADING_FAILED:
                istate = ObjectState.LOADED;
                break;
            case INGESTED:
            case INGESTING_FAILED:
                istate = ingestFailed ? ObjectState.INGESTING_FAILED : ObjectState.INGESTED;
                break;
        }
        return istate;
    }

    @javax.xml.bind.annotation.XmlRootElement(name = ImportResourceApi.IMPORT_BATCH_ELEMENT)
    @javax.xml.bind.annotation.XmlAccessorType(XmlAccessType.FIELD)
    public static class ImportBatch {

        public enum State {
            EMPTY, LOADING, LOADING_FAILED, LOADED, INGESTING, INGESTING_FAILED, INGESTED
        }

        @XmlElement(required = true, name = ImportResourceApi.IMPORT_BATCH_ID)
        private int id;
        @XmlElement(name = ImportResourceApi.IMPORT_BATCH_FOLDER)
        private String folderPath;
        @XmlElement(name = ImportResourceApi.IMPORT_BATCH_DESCRIPTION)
        private String description;
        @XmlElement(name = ImportResourceApi.IMPORT_BATCH_PARENTPID)
        private String parentPid;
        @XmlSchemaType(name="dateTime")
        @XmlElement(name = ImportResourceApi.IMPORT_BATCH_TIMESTAMP)
        private Date timeStamp;
        @XmlElement(name = ImportResourceApi.IMPORT_BATCH_USERID)
        private int userId;
        @XmlElement(name = ImportResourceApi.IMPORT_BATCH_USER)
        private String user;
        @XmlElement(name = ImportResourceApi.IMPORT_BATCH_STATE)
        private State state;
        /** list of batch option fromString context asString be persisted*/
        @XmlElement(name = ImportResourceApi.IMPORT_BATCH_OPTIONS)
        private String options;
        @XmlElement(name = ImportResourceApi.IMPORT_BATCH_ESTIMATEFILECOUNT)
        private int estimateFileCount;
        @XmlElement(name = ImportResourceApi.IMPORT_BATCH_FAILURE)
        private String failure;
        @XmlElement(name = ImportResourceApi.IMPORT_BATCH_ITEMS)
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
            ImportOptions config = fromString(importFolder, getUser(), options);
            return config;
        }

        static ImportOptions fromString(File importFolder, String username, String options) {
            ImportOptions config;
            if (options != null && !options.isEmpty()) {
                Properties p = new Properties();
                try {
                    p.load(new StringReader(options));
                    String indicies = p.getProperty("indicies", "true");
                    String device = p.getProperty("device");
                    String model = p.getProperty("model");
                    config = new ImportOptions(importFolder, model, device, Boolean.parseBoolean(indicies), username);
                } catch (IOException ex) {
                    throw new IllegalStateException(ex);
                }
            } else {
                config = new ImportOptions(importFolder, null, null, true, username);
            }
            return config;
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
