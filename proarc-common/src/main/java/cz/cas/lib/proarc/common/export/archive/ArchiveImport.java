/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.export.archive;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.Batch.State;
import cz.cas.lib.proarc.common.export.archive.PackageReader.ImportSession;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.Storage;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraImport;
import cz.cas.lib.proarc.common.imports.FedoraImport;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.imports.ImportHandler;
import cz.cas.lib.proarc.common.imports.ImportProcess.ImportOptions;
import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * Imports proarc archive packages.
 *
 * @author Jan Pokorsky
 */
public class ArchiveImport implements ImportHandler {

    private ImportSession isession;

    @Override
    public boolean isImportable(File folder) {
        return ArchiveScanner.isImportable(folder);
    }

    @Override
    public int estimateItemNumber(ImportOptions importConfig) throws IOException {
        File importFolder = importConfig.getImportFolder();
        List<File> mets = ArchiveScanner.findMets(importFolder);
        return mets.size();
    }

    @Override
    public void start(ImportOptions importConfig, ImportBatchManager batchManager, AppConfiguration config) throws Exception {
        isession = new ImportSession(ImportBatchManager.getInstance(), importConfig, config);
        load(importConfig, config);
        ingest(importConfig);
    }

    public void load(ImportOptions importConfig, AppConfiguration config) throws Exception {
        File importFolder = importConfig.getImportFolder();
        List<File> metsFiles = ArchiveScanner.findMets(importFolder);
        consume(metsFiles, importConfig, config);
        Batch batch = importConfig.getBatch();
        batch.setState(State.LOADED);
        isession.getImportManager().update(batch);
    }

    public void ingest(ImportOptions importConfig) throws Exception {
        ImportBatchManager ibm = ImportBatchManager.getInstance();
        Batch batch = importConfig.getBatch();
        AppConfiguration config = AppConfigurationFactory.getInstance().defaultInstance();
        if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
            FedoraImport ingest = new FedoraImport(config, RemoteStorage.getInstance(), ibm, null);
            ingest.importBatch(batch, importConfig.getUsername(), null);
        } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
            AkubraConfiguration akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(config.getConfigHome());
            AkubraImport ingest = new AkubraImport(config, akubraConfiguration, ibm, null);
            ingest.importBatch(batch, importConfig.getUsername(), null);
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
        }
    }

    public void consume(List<File> metsFiles, ImportOptions ctx, AppConfiguration config) throws InterruptedException {
        for (File metsFile : metsFiles) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            consumeArchive(metsFile, ctx, config);
        }
    }

    public void consumeArchive(File metsFile, ImportOptions ctx, AppConfiguration config) {
        File targetFolder = ctx.getTargetFolder();
        PackageReader reader = new PackageReader(targetFolder, isession, config);
        reader.read(metsFile, ctx);
    }

}
