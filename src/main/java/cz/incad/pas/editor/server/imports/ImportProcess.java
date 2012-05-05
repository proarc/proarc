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
    static final String TMP_DIR_NAME = "fedora_import";
    private File importFolder;
    private ImportManager imanager;
    private ImportBatchManager batchManager;
    private List<TiffImporter> consumerRegistery = Collections.singletonList(new TiffImporter());
    private List<ImportItem> imports;
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
        try {
            File targetFolder = createTargetFolder();
            ImportFileScanner scanner = new ImportFileScanner();
            List<File> files = scanner.findDigitalContent(importFolder);
            this.imports = new ArrayList<ImportItem>(files.size());
            List<ImportItem> consumedFiles = consumeFiles(files, new ImportContext(targetFolder, generateIndices));

            // import to Fedora
            ImportBatch batch = fedoraImport(consumedFiles);
            transactionFailed = false;
            return batch;
        } finally {
            // XXX rollback running state or set failed state
            if (transactionFailed) {
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

    public List<ImportItem> getImportedItems() {
        return this.imports;
    }

    public List<ImportItemFailure> getFailures() {
        return this.failures;
    }
    
    private List<ImportItem> consumeFiles(List<File> files, ImportContext ctx) {
        long start = System.currentTimeMillis();
        List<ImportItem> fedorarItems = new ArrayList<ImportItem>(files.size());
        for (File file : files) {
            try {
                ImportItem item = consumeFile(file, ctx);
                if (item != null) {
                    fedorarItems.add(item);
                } else {
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

        return fedorarItems;
    }

    private ImportItem consumeFile(File f, ImportContext ctx) throws IOException {
        long start = System.currentTimeMillis();
        String mimeType = findMimeType(f);
        List<TiffImporter> consumers = getConsumers();
        for (TiffImporter consumer : consumers) {
            ImportItem item = consumer.consume(f, mimeType, ctx);
            if (item != null) {
                LOG.log(Level.INFO, "time: {0} ms, {1}", new Object[] {System.currentTimeMillis() - start, f});
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

    private ImportBatch fedoraImport(List<ImportItem> fedoraItems) {
        ImportBatch batch = batchManager.add(importFolderRelativePath, user);
        for (ImportItem importItem : fedoraItems) {
            try {
                batchManager.addItem(batch.getId(), importItem);
                this.imports.add(importItem);
            } finally {
                // XXX rollback already imported objects?
            }
        }
        return batch;
    }

    public static final class ImportContext {
        private File targetFolder;
        private final XMLGregorianCalendar xmlNow;
        private final boolean generateIndices;

        ImportContext(File targetFolder, boolean generateIndices) throws DatatypeConfigurationException {
            this.targetFolder = targetFolder;
            DatatypeFactory xmlDataFactory = DatatypeFactory.newInstance();
            GregorianCalendar gcNow = new GregorianCalendar();
            xmlNow = xmlDataFactory.newXMLGregorianCalendar(gcNow);
            this.generateIndices = generateIndices;
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
