/*
 * Copyright (C) 2019 Lukas Sykora
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
package cz.cas.lib.proarc.common.process.imports.kramerius;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.Storage;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraImport;
import cz.cas.lib.proarc.common.process.imports.FedoraImport;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.imports.ImportHandler;
import cz.cas.lib.proarc.common.process.imports.ImportProcess;
import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * Imports Kramerius packages.
 *
 * @author Lukas Sykora
 */
public class KrameriusImport implements ImportHandler {

    private FileReader.ImportSession isession;
    private String type;

    public KrameriusImport(String type) {
        this.type = type;
    }

    @Override
    public boolean isImportable(File folder) {
       return KrameriusScanner.isImportable(folder);
    }

    @Override
    public int estimateItemNumber(ImportProcess.ImportOptions importConfig) throws IOException {
        File importFolder = importConfig.getImportFolder();
        List<File> importFiles = KrameriusScanner.findImportableFiles(importFolder);
        return importFiles.size();
    }


    @Override
    public void start(ImportProcess.ImportOptions importConfig, BatchManager batchManager, AppConfiguration configuration) throws Exception {
        isession = new FileReader.ImportSession(BatchManager.getInstance(), importConfig, configuration);
        load(importConfig);
        ingest(importConfig);
    }

    public void load(ImportProcess.ImportOptions importConfig) throws Exception {
        File importFolder = importConfig.getImportFolder();
        List<File> importFiles = KrameriusScanner.findImportableFiles(importFolder);
        try {
            consume(importFiles, importConfig);
        } catch (DigitalObjectException ex) {
            if (ex != null && ex.getPid() != null && ex.getPid().contains("The repository already contains pid:")) {
                Batch batch = importConfig.getBatch();
                batch.setState(Batch.State.LOADING_CONFLICT);
                isession.getImportManager().update(batch);
                throw new IllegalStateException(ex.getMessage(), ex);
            } else {
                throw new IllegalStateException(ex != null && ex.getMessage() != null ? ex.getMessage() : ex.getPid(), ex);
            }
        }
        Batch batch = importConfig.getBatch();
        batch.setState(Batch.State.LOADED);
        isession.getImportManager().update(batch);
    }

    public void ingest(ImportProcess.ImportOptions importConfig) throws Exception {
        BatchManager ibm = BatchManager.getInstance();
        Batch batch = importConfig.getBatch();
        AppConfiguration config = AppConfigurationFactory.getInstance().defaultInstance();
        if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
            FedoraImport ingest = new FedoraImport(config, RemoteStorage.getInstance(config), ibm, null, importConfig);
            ingest.importBatch(batch, importConfig.getUsername(), null);
        } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
            AkubraConfiguration akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(config.getConfigHome());
            AkubraImport ingest = new AkubraImport(config, akubraConfiguration, ibm, null, importConfig);
            ingest.importBatch(batch, importConfig.getUsername(), null);
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
        }
    }

    public void consume(List<File> importFiles, ImportProcess.ImportOptions ctx) throws InterruptedException, DigitalObjectException {
        int index = 1;
        for (File file : importFiles) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            consumeKrameriusFile(file, ctx, index++);
        }
    }

    private void consumeKrameriusFile(File file, ImportProcess.ImportOptions ctx, int index) throws DigitalObjectException {
        File targetFolder = ctx.getTargetFolder();
        FileReader reader = new FileReader(targetFolder, isession, type);
        reader.read(file, ctx, index);
    }

}
