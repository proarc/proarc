/*
 * Copyright (C) 2023 Lukas Sykora
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
package cz.cas.lib.proarc.common.process.imports.replaceStream;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.process.export.mets.JhoveUtility;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.imports.ImportHandler;
import cz.cas.lib.proarc.common.process.imports.replaceStream.FileReader.ImportSession;
import cz.cas.lib.proarc.common.process.imports.ImportProcess;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * A main class of Replace Stream import.
 *
 * @author Lukas Sykora
 */
public class ReplaceStreamImport implements ImportHandler {

    private ImportSession iSession;
    private static final Logger LOG = Logger.getLogger(ReplaceStreamImport.class.getName());

    @Override
    public int estimateItemNumber(ImportProcess.ImportOptions importConfig) throws IOException {
        File importFolder = importConfig.getImportFolder();
        List<File> importFiles = ReplaceStreamScanner.findFile(importFolder);
        return importFiles.size();
    }

    @Override
    public boolean isImportable(File folder) {
        return ReplaceStreamScanner.isImportable(folder);
    }

    @Override
    public void start(ImportProcess.ImportOptions importConfig, BatchManager batchManager, AppConfiguration config) throws Exception {
        iSession = new ImportSession(BatchManager.getInstance(), importConfig, config);
        importConfig.setJhoveContext(JhoveUtility.createContext());
        load(importConfig);
        ingest(importConfig);
    }

    private void ingest(ImportProcess.ImportOptions importConfig) throws InterruptedException {
        Batch batch = importConfig.getBatch();
        batch.setState(Batch.State.INGESTING);
        iSession.getImportManager().update(batch);

        try {
            List<File> importFiles = ReplaceStreamScanner.findFile(importConfig.getImportFolder());
            for (File file : importFiles) {
                if (Thread.interrupted()) {
                    throw new InterruptedException();
                }
                ingestFile(file, importConfig);
            }
            importFiles = ReplaceStreamScanner.findFile(importConfig.getTargetFolder());
            for (File file : importFiles) {
                if (Thread.interrupted()) {
                    throw new InterruptedException();
                }
                ingestFile(file, importConfig);
            }
            batch.setState(Batch.State.INGESTED);
            batch.setLog(null);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, String.valueOf(batch), t);
            batch.setState(Batch.State.LOADING_FAILED);
            batch.setLog(BatchManager.toString(t));
        } finally {
            iSession.getImportManager().update(batch);
        }

    }

    private void ingestFile(File file, ImportProcess.ImportOptions importConfig) {
        FileIngest ingest = new FileIngest(iSession);
        ingest.ingest(file, importConfig);
    }

    private void load(ImportProcess.ImportOptions importConfig) throws Exception {
        File importFolder = importConfig.getImportFolder();
        List<File> importFiles = ReplaceStreamScanner.findFile(importFolder);
        consume(importFiles, importConfig);
        Batch batch = importConfig.getBatch();
        batch.setState(Batch.State.LOADED);
        iSession.getImportManager().update(batch);
    }

    public void consume(List<File> importFiles, ImportProcess.ImportOptions context) throws InterruptedException {
        int index = 1;
        for (File file : importFiles) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            consumeFile(file, context, index++);
        }
    }

    private void consumeFile(File file, ImportProcess.ImportOptions context, int index) {
        FileReader reader = new FileReader(iSession);
        reader.read(file, context);
    }


}
