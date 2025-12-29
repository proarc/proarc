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
package cz.cas.lib.proarc.common.process.imports.ndk;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraImport;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.imports.FedoraImport;
import cz.cas.lib.proarc.common.process.imports.ImportHandler;
import cz.cas.lib.proarc.common.process.imports.ImportProcess.ImportOptions;
import cz.cas.lib.proarc.common.process.imports.ndk.FileReader.ImportSession;
import java.io.File;
import java.io.IOException;
import java.sql.Timestamp;
import java.util.List;

/**
 * Imports ndk psp packages.
 *
 * @author Lukas Sykora
 */
public class NdkImport implements ImportHandler {

    private ImportSession iSession;

    @Override
    public int estimateItemNumber(ImportOptions importConfig) throws IOException {
        File importFolder = importConfig.getImportFolder();
        List<File> mets = NdkScanner.findMets(importFolder);
        return mets.size();
    }

    @Override
    public boolean isImportable(File folder) {
        return NdkScanner.isImportable(folder);
    }

    @Override
    public void start(ImportOptions importConfig, BatchManager batchManager, AppConfiguration config) throws Exception {
        iSession = new ImportSession(BatchManager.getInstance(), importConfig, config);

        Batch batch = importConfig.getBatch();
        batch.setState(Batch.State.LOADING);
        batch.setUpdated(new Timestamp(System.currentTimeMillis()));
        batch = batchManager.update(batch);
        importConfig.setBatch(batch);

        load(importConfig, config);
        ingest(importConfig, config);
        batch = batchManager.update(batch);
        if (Batch.State.INGESTED.equals(batch.getState())) {
            batch.setParentPid(iSession.getRootPid());
            iSession.getImportManager().update(batch);
        }
    }

    private void ingest(ImportOptions importConfig, AppConfiguration config) throws Exception {
        BatchManager bm = BatchManager.getInstance();
        Batch batch = importConfig.getBatch();
        if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
            FedoraImport ingest = new FedoraImport(config, FedoraStorage.getInstance(config), bm, null, importConfig);
            ingest.importBatch(batch, importConfig.getUsername(), null);
        } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
            AkubraConfiguration akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(config.getConfigHome());
            AkubraImport ingest = new AkubraImport(config, akubraConfiguration, bm, null, importConfig);
            ingest.importBatch(batch, importConfig.getUsername(), null);
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
        }
    }

    public void load(ImportOptions importConfig, AppConfiguration config) throws Exception {
        File importFolder = importConfig.getImportFolder();
        List<File> metsFiles = NdkScanner.findMets(importFolder);
        consume(metsFiles, importConfig, config);
        Batch batch = importConfig.getBatch();
        if (Batch.State.LOADING.equals(batch.getState())) {
            batch.setState(Batch.State.LOADED);
            batch.setUpdated(new Timestamp(System.currentTimeMillis()));
            iSession.getImportManager().update(batch);
        }
    }

    public void consume(List<File> metsFiles, ImportOptions ctx, AppConfiguration config) throws InterruptedException {
        for (File metsFile : metsFiles) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            consumeNdk(metsFile, ctx, config);
        }
    }

    private void consumeNdk(File metsFile, ImportOptions ctx, AppConfiguration config) {
        File targetFolder = ctx.getTargetFolder();
        FileReader reader = new FileReader(targetFolder, iSession, config);
        reader.read(metsFile, ctx);
    }
}
