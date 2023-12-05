/*
 * Copyright (C) 2017 Lukas Sykora
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
package cz.cas.lib.proarc.common.process.imports.audio;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem;

import cz.cas.lib.proarc.common.process.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.process.imports.FileSet;
import cz.cas.lib.proarc.common.process.imports.FileSetImport;
import cz.cas.lib.proarc.common.process.imports.ImageImporter;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.imports.ImportFileScanner;
import cz.cas.lib.proarc.common.process.imports.ImportProcess;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * Imports soundrecording files
 * @author Lukas Sykora
 */
public class SoundRecordingImport extends FileSetImport {

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
    public int estimateItemNumber(ImportProcess.ImportOptions importConfig) throws IOException {
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
    public void start(ImportProcess.ImportOptions importConfig, BatchManager batchManager, AppConfiguration configuration) throws Exception {
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

    @Override
    protected void consumeFileSets(Batch batch, List<FileSet> fileSets, ImportProcess.ImportOptions ctx) throws InterruptedException {
        BatchManager batchManager = BatchManager.getInstance();
        long start = System.currentTimeMillis();
        for (FileSet fileSet : fileSets) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            BatchManager.BatchItemObject item = consumeFileSet(fileSet, ctx);
            String pid = item == null ? null : item.getPid();
            BatchItem.FileState state = item == null ? BatchItem.FileState.SKIPPED : BatchItem.FileState.OK;
            batchManager.addFileItem(batch.getId(), pid, state, fileSet.getFiles());
            if (item != null) {
                if (BatchItem.ObjectState.LOADING_FAILED == item.getState()) {
                    batch.setState(Batch.State.LOADING_FAILED);
                    batch.setLog(item.getFile() + "\n" + item.getLog());
                    return ;
                }
            }
        }
        LOG.log(Level.FINE, "Total time: {0} ms", System.currentTimeMillis() - start);
    }

    private BatchManager.BatchItemObject consumeFileSet(FileSet fileSet, ImportProcess.ImportOptions ctx) {
        long start = System.currentTimeMillis();
        List<ImageImporter> consumers = ImportProcess.getConsumers();
        for (ImageImporter consumer : consumers) {
            BatchManager.BatchItemObject item = consumer.consume(fileSet, ctx);
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
