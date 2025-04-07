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
package cz.cas.lib.proarc.common.process.imports.archive;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.Batch.State;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraImport;
import cz.cas.lib.proarc.common.process.imports.FedoraImport;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.imports.ImportHandler;
import cz.cas.lib.proarc.common.process.imports.ImportProcess.ImportOptions;
import java.io.File;
import java.io.IOException;
import java.util.List;
import org.apache.commons.io.FileUtils;

/**
 * Imports proarc archive packages.
 *
 * @author Jan Pokorsky
 */
public class ArchiveImport implements ImportHandler {

    private PackageReader.ImportSession isession;

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
    public void start(ImportOptions importConfig, BatchManager batchManager, AppConfiguration config) throws Exception {
        isession = new PackageReader.ImportSession(BatchManager.getInstance(), importConfig, config);
        load(importConfig, config);
        storeArchivalCopies(importConfig);
        ingest(importConfig, config);
        Batch batch = importConfig.getBatch();
        if (Batch.State.INGESTED.equals(batch.getState())) {
            batch.setParentPid(isession.getRootPid());
            isession.getImportManager().update(batch);
        }

    }

    private void storeArchivalCopies(ImportOptions importConfig) throws Exception {
        AppConfiguration config = AppConfigurationFactory.getInstance().defaultInstance();
        File archivalCopiesHome = new File(config.getConfigHome(), "archival_copies");
        if (!archivalCopiesHome.exists()) {
            archivalCopiesHome.mkdir();
            if (!archivalCopiesHome.exists()) {
                throw new Exception("Nepodarilo se vytvorit slozku: " + archivalCopiesHome.getAbsolutePath());
            }
        }
        File importFolder = getImportFolder(config, importConfig.getImportFolder());
        if (importFolder != null && importFolder.exists()) {
            String folderName = isession.getRootPid();
            if (folderName != null && !folderName.isEmpty()) {
                folderName = folderName.replace("uuid:", "");
            } else {
                folderName = importFolder.getName();
            }
            File destination = new File(archivalCopiesHome, folderName);
            if (destination.exists()) {
                MetsUtils.deleteFolder(destination);
            }
            destination.mkdir();
            FileUtils.copyDirectory(importFolder, destination);

            for (File file : destination.listFiles()) {
                if (file.getName().endsWith(".txt") || file.getName().endsWith(".md5")) {
                    MetsUtils.deleteFolder(file);
                }
                if (config.getArchiveExportOptions().getNoTifAvailableFileName().equals(file.getName())) {
                    MetsUtils.deleteFolder(file);
                }
            }
        }
    }

    private File getImportFolder(AppConfiguration config, File folder) {
        if (config.getArchiveExportOptions().getArchivalCopyFolderName().equals(folder.getName())) {
            return folder;
        }
        if (folder.isDirectory()) {
            for (File file : folder.listFiles()) {
                File requiredFile =  getImportFolder(config, file);
                if (requiredFile != null) {
                    return requiredFile;
                }
            }
        }
        return null;
    }

    public void load(ImportOptions importConfig, AppConfiguration config) throws Exception {
        File importFolder = importConfig.getImportFolder();
        List<File> metsFiles = ArchiveScanner.findMets(importFolder);
        try {
            consume(metsFiles, importConfig, config);
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
        batch.setState(State.LOADED);
        isession.getImportManager().update(batch);
    }

    public void ingest(ImportOptions importConfig, AppConfiguration config) throws Exception {
        BatchManager ibm = BatchManager.getInstance();
        Batch batch = importConfig.getBatch();
        if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
            FedoraImport ingest = new FedoraImport(config, FedoraStorage.getInstance(config), ibm, null, importConfig);
            ingest.importBatch(batch, importConfig.getUsername(), null);
        } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
            AkubraConfiguration akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(config.getConfigHome());
            AkubraImport ingest = new AkubraImport(config, akubraConfiguration, ibm, null, importConfig);
            ingest.importBatch(batch, importConfig.getUsername(), null);
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
        }
    }

    public void consume(List<File> metsFiles, ImportOptions ctx, AppConfiguration config) throws InterruptedException, DigitalObjectException {
        for (File metsFile : metsFiles) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            consumeArchive(metsFile, ctx, config);
        }
    }

    public void consumeArchive(File metsFile, ImportOptions ctx, AppConfiguration config) throws DigitalObjectException {
        File targetFolder = ctx.getTargetFolder();
        PackageReader reader = new PackageReader(targetFolder, isession, config);
        reader.read(metsFile, ctx);
    }

}
