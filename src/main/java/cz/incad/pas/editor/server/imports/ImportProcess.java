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
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.FileNameMap;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public class ImportProcess {

    private static final Logger LOG = Logger.getLogger(ImportProcess.class.getName());
    private static final String TMP_DIR_NAME = "fedora_import";
    private File importFolder;
    private ImportManager imanager;
    private ImportBatchManager batchManager;
    private List<TiffImporter> consumerRegistery = Collections.singletonList(new TiffImporter());
    private List<ImportItem> imports;
    private List<ImportItemFailure> failures = new ArrayList<ImportItemFailure>();
    private final String importFolderRelativePath;
    private final int userId;

    public ImportProcess(File importFolder, String importFolderRelativePath, int userId, ImportBatchManager batchManager) {
        this.importFolder = importFolder;
        this.importFolderRelativePath = importFolderRelativePath;
        this.userId = userId;
        this.batchManager = batchManager;
    }

    public ImportBatch start() throws IOException {
        // validate import folder
        ImportFileScanner.validateImportFolder(importFolder);

        // check import state
        setRunningState(importFolder);
        // check target folder
        try {
            File targetFolder = createTargetFolder();
            ImportFileScanner scanner = new ImportFileScanner();
            List<File> files = scanner.findDigitalContent(importFolder);
            this.imports = new ArrayList<ImportItem>(files.size());
            List<FedoraImportItem> consumedFiles = consumeFiles(files, new ImportContext(targetFolder));

            // import to Fedora
            return fedoraImport(consumedFiles);
        } finally {
            // XXX rollback running state or set failed state
        }

    }

    public List<ImportItem> getImportedItems() {
        return this.imports;
    }

    public List<ImportItemFailure> getFailures() {
        return this.failures;
    }
    
    private List<FedoraImportItem> consumeFiles(List<File> files, ImportContext ctx) {
        List<FedoraImportItem> fedorarItems = new ArrayList<FedoraImportItem>(files.size());
        for (File file : files) {
            try {
                FedoraImportItem item = consumeFile(file, ctx);
                if (item != null) {
                    item.importFile = file;
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

        return fedorarItems;
    }

    private FedoraImportItem consumeFile(File f, ImportContext ctx) throws IOException {
        String mimeType = findMimeType(f);
        List<TiffImporter> consumers = getConsumers();
        for (TiffImporter consumer : consumers) {
            FedoraImportItem item = consumer.consume(f, mimeType, ctx);
            if (item != null) {
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
        if (folder.mkdir()) {
            throw new IOException("Import folder already exists: " + folder);
        }
        return folder;
    }

    private void setRunningState(File folder) throws IOException {
        State folderImportState = ImportFileScanner.folderImportState(importFolder);
        if (folderImportState != State.NEW) {
            throw new IOException("Folder imported: " + importFolder + ", state: " + folderImportState);
        }
        File statusFile = new File(importFolder, ImportFileScanner.IMPORT_STATE_FILENAME);
        if (statusFile.createNewFile()) {
            // lets import
        } else {
            folderImportState = ImportFileScanner.folderImportState(importFolder);
            if (folderImportState != State.NEW) {
                throw new IOException("Folder imported: " + importFolder + ", state: " + folderImportState);
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

    private ImportBatch fedoraImport(List<FedoraImportItem> fedoraItems) {
        ImportBatch batch = batchManager.add(importFolderRelativePath, userId);
        for (FedoraImportItem fedoraItem : fedoraItems) {
            try {
                // XXX import to fedora
                ImportItem importItem = new ImportItem(fedoraItem.getImportFile().getName(), fedoraItem.getPid());
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

        ImportContext(File targetFolder) {
            this.targetFolder = targetFolder;
        }

        public File getTargetFolder() {
            return targetFolder;
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

    /**
     * see https://wiki.duraspace.org/display/FEDORA35/Using+File+URIs to reference external files for ingest
     */
    public static class FedoraImportItem {
        private File foxml;
        private String pid;
        private File importFile;

        public FedoraImportItem(File foxml, String pid) {
            this.foxml = foxml;
            this.pid = pid;
        }

        public File getFoxml() {
            return foxml;
        }

        public String getPid() {
            return pid;
        }

        public File getImportFile() {
            return importFile;
        }
        
    }

}
