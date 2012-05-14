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

import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportBatch;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportItem;
import cz.incad.pas.editor.server.imports.ImportFileScanner.State;
import cz.incad.pas.editor.server.user.UserProfile;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.FileNameMap;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

/**
 *
 * @author Jan Pokorsky
 */
public class ImportProcess {

    private static final Logger LOG = Logger.getLogger(ImportProcess.class.getName());
    static final String TMP_DIR_NAME = "proarch_import";
    private File importFolder;
    private ImportBatchManager batchManager;
    private List<TiffImporter> consumerRegistery = Collections.singletonList(new TiffImporter());
    private List<ImportItemFailure> failures = new ArrayList<ImportItemFailure>();
    private final String importFolderRelativePath;
    private final UserProfile user;
    private final boolean generateIndices;

    public ImportProcess(File importFolder, String importFolderRelativePath,
            UserProfile user, ImportBatchManager batchManager,
            boolean generateIndices) {
        this.importFolder = importFolder;
        this.importFolderRelativePath = importFolderRelativePath;
        this.user = user;
        this.batchManager = batchManager;
        this.generateIndices = generateIndices;
    }

    public ImportBatch start() throws IOException, DatatypeConfigurationException {
        // validate import folder
        ImportFileScanner.validateImportFolder(importFolder);

        // check import state
        setRunningState(importFolder);
        boolean transactionFailed = true;
        // check target folder
        ImportBatch batch = null;
        try {
            File targetFolder = createTargetFolder();
            ImportFileScanner scanner = new ImportFileScanner();
            List<File> files = scanner.findDigitalContent(importFolder);
            batch = batchManager.add(importFolderRelativePath, user);
            batch = batchManager.update(batch.getId(), ImportBatch.State.LOADING);
            consumeFiles(batch.getId(), files, new ImportContext(targetFolder, generateIndices, user.getUserName()));
            batch = batchManager.update(batch.getId(), ImportBatch.State.LOADED);
            transactionFailed = false;
            return batch;
        } finally {
            // XXX rollback running state or set failed state
            if (transactionFailed) {
                if (batch != null) {
                    batchManager.update(batch.getId(), ImportBatch.State.LOADING_FAILED);
                }
                File tmpFolder = new File(importFolder, TMP_DIR_NAME);
                deleteFolder(tmpFolder);
                ImportFileScanner.rollback(importFolder);
            }
        }

    }

    private static void deleteFolder(File folder) {
        if (folder.exists()) {
            for (File f : folder.listFiles()) {
                if (f.isDirectory()) {
                    deleteFolder(f);
                } else {
                    f.delete();
                }
            }
            folder.delete();
        }
    }

    public List<ImportItemFailure> getFailures() {
        return this.failures;
    }
    
    private void consumeFiles(int batchId, List<File> files, ImportContext ctx) {
        long start = System.currentTimeMillis();
        for (File file : files) {
            try {
                ImportItem item = consumeFile(file, ctx);
                if (item != null) {
                    batchManager.addItem(batchId, item);
                } else {
                    // XXX implement failed items in ImportBatchManager
                    // ImportItem importItem = new ImportItem((String) null, file.toString(), null);
                    this.failures.add(new ImportItemFailure(file.getName(), "unsupported file"));
                }
            } catch (IOException ex) {
                StringWriter sw = new StringWriter();
                ex.printStackTrace(new PrintWriter(sw));
                this.failures.add(new ImportItemFailure(file.getName(), sw.toString()));
                LOG.log(Level.SEVERE, file.toString(), ex);
            }
        }
        LOG.log(Level.INFO, "Total time: {0} ms", System.currentTimeMillis() - start);
    }

    private ImportItem consumeFile(File f, ImportContext ctx) throws IOException {
        long start = System.currentTimeMillis();
        String mimeType = findMimeType(f);
        List<TiffImporter> consumers = getConsumers();
        for (TiffImporter consumer : consumers) {
            ImportItem item = consumer.consume(f, mimeType, ctx);
            if (item != null) {
                LOG.log(Level.INFO, "time: {0} ms, {1}", new Object[] {System.currentTimeMillis() - start, f});
                ++ctx.consumedFileCounter;
                return item;
            }
        }

        return null;
    }

    private List<TiffImporter> getConsumers() {
        return consumerRegistery;
    }

    private File createTargetFolder() throws IOException {
        File folder = new File(importFolder, TMP_DIR_NAME);
        if (!folder.mkdir()) {
            throw new IOException("Import folder already exists: " + folder);
        }
        return folder;
    }

    private void setRunningState(File folder) throws IOException {
        State folderImportState = ImportFileScanner.folderImportState(folder);
        if (folderImportState != State.NEW) {
            throw new IOException("Folder imported: " + folder + ", state: " + folderImportState);
        }
        File statusFile = new File(folder, ImportFileScanner.IMPORT_STATE_FILENAME);
        if (statusFile.createNewFile()) {
            // lets import
        } else {
            folderImportState = ImportFileScanner.folderImportState(folder);
            if (folderImportState != State.NEW) {
                throw new IOException("Folder imported: " + folder + ", state: " + folderImportState);
            }
        }
    }

    /**
     * Simplified version uses filename extension. For niftier alternatives see
     * http://www.rgagnon.com/javadetails/java-0487.html
     */
    public static String findMimeType(File f) {
        FileNameMap fileNameMap = URLConnection.getFileNameMap();
        return fileNameMap.getContentTypeFor(f.getName());
    }

    public static final class ImportContext {
        private File targetFolder;
        private final XMLGregorianCalendar xmlNow;
        private final boolean generateIndices;
        private int consumedFileCounter;
        private final String username;

        ImportContext(File targetFolder, boolean generateIndices, String username) throws DatatypeConfigurationException {
            this.targetFolder = targetFolder;
            DatatypeFactory xmlDataFactory = DatatypeFactory.newInstance();
            GregorianCalendar gcNow = new GregorianCalendar();
            xmlNow = xmlDataFactory.newXMLGregorianCalendar(gcNow);
            this.generateIndices = generateIndices;
            this.username = username;
        }

        public File getTargetFolder() {
            return targetFolder;
        }

        public XMLGregorianCalendar getXmlNow() {
            return xmlNow;
        }

        public boolean isGenerateIndices() {
            return generateIndices;
        }

        public int getConsumedFileCounter() {
            return consumedFileCounter;
        }

        public String getUsername() {
            return username;
        }
        
    }
    
    public static class ImportItemFailure {
        private String filename;
        private String reason;

        private ImportItemFailure() {
        }

        public ImportItemFailure(String filename, String reason) {
            this.filename = filename;
            this.reason = reason;
        }

        public String getFilename() {
            return filename;
        }

        public String getReason() {
            return reason;
        }
        
    }

}
