/*
 * Copyright (C) 2016 Jan Pokorsky
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
package cz.cas.lib.proarc.common.imports;

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem.FileState;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.imports.ImportProcess.ImportOptions;
import static cz.cas.lib.proarc.common.imports.ImportProcess.getConsumers;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Imports files grouped to {@link FileSet file sets}.
 *
 * @author Jan Pokorsky
 */
public class FileSetImport implements ImportHandler {

    private static final Logger LOG = Logger.getLogger(FileSetImport.class.getName());

    @Override
    public boolean isImportable(File folder) {
        String[] fileNames = folder.list();
        for (String fileName : fileNames) {
            if (ImportFileScanner.IMPORT_STATE_FILENAME.equals(fileName)) {
                continue;
            }
            File file = new File(folder, fileName);
            if (file.isFile() && file.canRead()) {
                List<FileSet> fileSets = ImportFileScanner.getFileSets(Arrays.asList(file));
                if (canImport(fileSets.get(0))) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public int estimateItemNumber(ImportOptions importConfig) throws IOException {
        File importFolder = importConfig.getImportFolder();
        ImportFileScanner scanner = new ImportFileScanner();
        List<File> files = scanner.findDigitalContent(importFolder);
        List<FileSet> fileSets = ImportFileScanner.getFileSets(files);
        if (!canImport(fileSets)) {
            // nothing to import
            return 0;
        } else {
            return fileSets.size();
        }
    }

    @Override
    public void start(ImportOptions importConfig) throws Exception {
        File importFolder = importConfig.getImportFolder();
        Batch batch = importConfig.getBatch();
        ImportFileScanner scanner = new ImportFileScanner();
        List<File> files = scanner.findDigitalContent(importFolder);
        List<FileSet> fileSets = ImportFileScanner.getFileSets(files);
        importConfig.setJhoveContext(JhoveUtility.createContext());
        try {
            consumeFileSets(batch, fileSets, importConfig);
        } finally {
            importConfig.getJhoveContext().destroy();
        }
    }

    private void consumeFileSets(Batch batch, List<FileSet> fileSets, ImportOptions ctx) throws InterruptedException {
        ImportBatchManager batchManager = ImportBatchManager.getInstance();
        long start = System.currentTimeMillis();
        for (FileSet fileSet : fileSets) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            BatchItemObject item = consumeFileSet(fileSet, ctx);
            String pid = item == null ? null : item.getPid();
            FileState state = item == null ? FileState.SKIPPED : FileState.OK;
            batchManager.addFileItem(batch.getId(), pid, state, fileSet.getFiles());
            if (item != null) {
                if (ObjectState.LOADING_FAILED == item.getState()) {
                    batch.setState(Batch.State.LOADING_FAILED);
                    batch.setLog(item.getFile() + "\n" + item.getLog());
                    return ;
                }
            }
        }
        LOG.log(Level.FINE, "Total time: {0} ms", System.currentTimeMillis() - start);
    }

    private BatchItemObject consumeFileSet(FileSet fileSet, ImportOptions ctx) {
        long start = System.currentTimeMillis();
        List<ImageImporter> consumers = getConsumers();
        for (ImageImporter consumer : consumers) {
            BatchItemObject item = consumer.consume(fileSet, ctx);
            if (item != null) {
                LOG.log(Level.FINE, "time: {0} ms, {1}", new Object[] {System.currentTimeMillis() - start, fileSet});
                ctx.setConsumedFileCounter(ctx.getConsumedFileCounter() + 1);
                return item;
            }
        }

        return null;
    }

    static boolean canImport(FileSet fileSet) {
        for (ImageImporter consumer : ImportProcess.getConsumers()) {
            if (consumer.accept(fileSet)) {
                return true;
            }
        }
        return false;
    }

    static boolean canImport(List<FileSet> fileSets) {
        for (FileSet fileSet : fileSets) {
            if (canImport(fileSet)) {
                return true;
            }
        }
        return false;
    }

}
