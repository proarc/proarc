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
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.imports.FedoraImport;
import cz.cas.lib.proarc.common.process.imports.ImportHandler;
import cz.cas.lib.proarc.common.process.imports.ImportProcess;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraImport;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.mods.ModsDefinition;
import java.io.File;
import java.io.IOException;
import java.util.List;

import static cz.cas.lib.proarc.common.actions.ChangeModels.fixModsFromK4;
import static cz.cas.lib.proarc.common.process.imports.kramerius.FileReader.getK4Model;

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

    private void updateModsIfNeeded(ImportProcess.ImportOptions importConfig) throws DigitalObjectException {
        if (importConfig.wasK4Model()) {
            Batch batch = isession.getImportManager().get(importConfig.getBatch().getId());
            if (batch != null && batch.getParentPid() != null && !importConfig.getPidsToUpdate().isEmpty()) {
                String parentPid = batch.getParentPid();

                BatchManager.BatchItemObject parentImportItem = isession.findItem(parentPid);
                LocalStorage.LocalObject parentObj = isession.findLocalObject(parentImportItem);

                RelationEditor parentRelationEditor = new RelationEditor(parentObj);
                if (!(NdkPlugin.MODEL_PERIODICAL.equals(parentRelationEditor.getModel()) || NdkPlugin.MODEL_MONOGRAPHTITLE.equals(parentRelationEditor.getModel()))) {
                    return;
                }
                String parentModelId = parentRelationEditor.getModel();

                ModsStreamEditor parentModsEditor = new ModsStreamEditor(parentObj);
                ModsDefinition parentMods = parentModsEditor.read();

                for (String pidToUpdate : importConfig.getPidsToUpdate()) {
                    BatchManager.BatchItemObject importItem = isession.findItem(pidToUpdate);
                    LocalStorage.LocalObject lObj = isession.findLocalObject(importItem);
                    RelationEditor relationEditor = new RelationEditor(lObj);
                    String modelId = relationEditor.getModel();

                    ModsStreamEditor modsEditor = new ModsStreamEditor(lObj);
                    ModsDefinition mods = modsEditor.read();

                    mods = fixModsFromK4(pidToUpdate, mods, modelId, getK4Model(modelId), parentMods);

                    modsEditor.write(mods, modsEditor.getLastModified(), "Update MODS po zmene modelu z K4 na NDK.");
                    lObj.flush();
                }
            }
        }
    }

    public void load(ImportProcess.ImportOptions importConfig) throws Exception {
        File importFolder = importConfig.getImportFolder();
        List<File> importFiles = KrameriusScanner.findImportableFiles(importFolder);
        try {
            consume(importFiles, importConfig);
            updateModsIfNeeded(importConfig);
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
            FedoraImport ingest = new FedoraImport(config, FedoraStorage.getInstance(config), ibm, null, importConfig);
            ingest.importBatch(batch, importConfig.getUsername(), null);
        } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
            AkubraConfiguration akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(config.getConfigHome());
            AkubraImport ingest = new AkubraImport(config, akubraConfiguration, ibm, null, importConfig);

            String parentPid = batch.getParentPid();
            batch.setParentPid(null); // nastaveni parentPid na null, protoze stale neni v repozitory, ale jen lokalne. Po ingestu se nastavi na

            ingest.importBatch(batch, importConfig.getUsername(), null);

            batch.setParentPid(parentPid);
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
