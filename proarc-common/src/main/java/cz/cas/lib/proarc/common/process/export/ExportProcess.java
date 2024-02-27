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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.process.export;

import com.google.common.hash.HashCode;
import com.google.common.hash.Hashing;
import com.google.common.io.ByteSource;
import com.google.common.io.Files;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.process.export.archive.ArchiveOldPrintProducer;
import cz.cas.lib.proarc.common.process.export.archive.ArchiveProducer;
import cz.cas.lib.proarc.common.process.export.bagit.BagitExport;
import cz.cas.lib.proarc.common.process.export.cejsh.CejshConfig;
import cz.cas.lib.proarc.common.process.export.cejsh.CejshExport;
import cz.cas.lib.proarc.common.process.export.cejsh.CejshStatusHandler;
import cz.cas.lib.proarc.common.process.export.crossref.CrossrefExport;
import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.process.export.mets.NdkExport;
import cz.cas.lib.proarc.common.process.export.mets.NdkSttExport;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.process.export.sip.NdkSipExport;
import cz.cas.lib.proarc.common.process.export.workflow.WorkflowExport;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.kramerius.KUtils;
import cz.cas.lib.proarc.common.kramerius.KrameriusOptions;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import cz.cas.lib.proarc.common.workflow.WorkflowActionHandler;
import cz.cas.lib.proarc.common.workflow.WorkflowException;
import cz.cas.lib.proarc.common.workflow.WorkflowManager;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.MaterialView;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.model.TaskView;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.math.BigDecimal;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Logger;
import org.apache.commons.io.FileUtils;

import static cz.cas.lib.proarc.common.dao.BatchUtils.finishedExportWithError;
import static cz.cas.lib.proarc.common.process.export.bagit.BagitExport.findNdkExportFolder;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_FAILED;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_FINISHED;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_WARNING;
import static cz.cas.lib.proarc.common.kramerius.KrameriusOptions.KRAMERIUS_INSTANCE_LOCAL;

/**
 * Export process
 * {@link #start() runs} the export
 *
 * @author Lukas Sykora
 */
public final class ExportProcess implements Runnable {

    private static final Logger LOG = Logger.getLogger(ExportProcess.class.getName());

    private final BatchManager batchManager;
    private final ExportOptions exportOptions;
    private final AppConfiguration config;
    private final AkubraConfiguration akubraConfiguration;
    private final UserProfile user;

    ExportProcess(ExportOptions exportOptions, BatchManager batchManager, AppConfiguration config, AkubraConfiguration akubraConfiguration, UserProfile user) {
        this.exportOptions = exportOptions;
        this.batchManager = batchManager;
        this.config = config;
        this.akubraConfiguration = akubraConfiguration;
        this.user = user;
    }

    /**
     * Prepares a new export process.
     * to run with {@link #start} immediately or later with {@link ExportDispatcher}.
     */
    public static ExportProcess prepare(AppConfiguration config, AkubraConfiguration akubraConfig, Batch batch, BatchManager batchManager, UserProfile user, String log, Locale locale) throws IOException {
        ExportOptions options = new ExportOptions(config, batch, log, locale);
        ExportProcess process = new ExportProcess(options, batchManager, config, akubraConfig, user);
        return process;
    }

    @Override
    public void run() {
        start();
    }

    /**
     * Starts the process.
     *
     * @return the import batch
     */
    public Batch start() {
        Batch batch = exportOptions.getBatch();
        if (batch == null) {
            throw new IllegalStateException("Batch is null!");
        }
        String profileId = batch.getProfileId();
        BatchParams params = batch.getParamsAsObject();
        if (params == null) {
            return finishedExportWithError(batchManager, batch, batch.getFolder(), new Exception("Batch params are null."));
        }
        try {
            batch = BatchUtils.startWaitingExportBatch(batchManager, batch);
            switch (profileId) {
                case Batch.EXPORT_KRAMERIUS:
                    return krameriusExport(batch, params);
                case Batch.EXPORT_DATASTREAM:
                    return datastreamExport(batch, params);
                case Batch.EXPORT_NDK:
                    return ndkExport(batch, params);
                case Batch.EXPORT_DESA:
                    return desaExport(batch, params);
                case Batch.EXPORT_CEJSH:
                    return cejshExport(batch, params);
                case Batch.EXPORT_CROSSREF:
                    return crossrefExport(batch, params);
                case Batch.EXPORT_ARCHIVE:
                    return archiveExport(batch, params);
                case Batch.EXPORT_KWIS:
                    return kwisExport(batch, params);
                default:
                    return finishedExportWithError(batchManager, batch, batch.getFolder(), new Exception("Unknown export profile."));
            }
        } catch (InterruptedException ex) {
            // rollback files on batch resume
            return null;
        } catch (Throwable t) {
            return BatchUtils.finishedExportWithError(batchManager, batch, batch.getFolder(), t);
        }
    }

    public static void resumeAll(BatchManager ibm, ExportDispatcher dispatcher, AppConfiguration config, AkubraConfiguration akubraConfiguration) {
        List<Batch> batches2schedule = ibm.findWaitingExportBatches();
        for (Batch batch : batches2schedule) {
            try {
                ExportProcess resume = ExportProcess.resume(batch, ibm, config, akubraConfiguration);
                dispatcher.addExport(resume);
            } catch (Exception ex) {
                BatchUtils.finishedExportWithError(ibm, batch, batch.getFolder(), ex);
            }
        }
    }

    public static ExportProcess resume(Batch batch, BatchManager ibm, AppConfiguration config, AkubraConfiguration akubraConfiguration) throws IOException {
        UserManager users = UserUtil.getDefaultManger();
        UserProfile user = users.find(batch.getUserId());
        // if necessary reset old computed batch items
        ExportProcess process = ExportProcess.prepare(config, akubraConfiguration, batch, ibm, user, "Resume export", new Locale("cs", "CZ"));
        return process;
    }

    private Batch kwisExport(Batch batch, BatchParams params) {
        try {
            String outputPath = config.getKwisExportOptions().getKwisPath();
            if (outputPath == null) {
                outputPath = user.getExportFolder().getPath();
            }
            if (!outputPath.endsWith(File.separator)) {
                outputPath = outputPath + File.separator;
            }

            URI imagesPath = runDatastreamExport(outputPath, params, Collections.singletonList("NDK_USER"));
            URI k4Path = runK4Export(outputPath, params, "public");

            String imp = imagesPath.getPath();
            String k4p = k4Path.getPath();
            String exportPackPath = outputPath + params.getPids().get(0).substring(5) + "_KWIS";
            File exportFolder = new File(exportPackPath);
            exportFolder.mkdir();

            KwisExport export = new KwisExport(config, imp, k4p, exportPackPath);

            try {
                export.run();
                return BatchUtils.finishedExportSuccessfully(this.batchManager, batch, exportFolder.getAbsolutePath());
            } catch (Exception ex) {
                String exportPath = MetsUtils.renameFolder(new File(outputPath), exportFolder, null);
                return finishedExportWithError(this.batchManager, batch, exportPath, ex);
            } finally {
                FileUtils.deleteDirectory(new File(imp));
                FileUtils.deleteDirectory(new File(k4p));
            }
        } catch (Exception ex) {
            return finishedExportWithError(this.batchManager, batch, batch.getFolder(), ex);
        }
    }

    private Batch archiveExport(Batch batch, BatchParams params) {
        try {
            URI exportUri = user.getExportFolder();
            File exportFolder = new File(exportUri);
            ArchiveProducer export = null;
            String typeOfPackage = getTypeOfPackage(params.getPids(), params.getTypeOfPackage());
            switch (typeOfPackage) {
                case Const.EXPORT_NDK_BASIC:
                case Const.EXPORT_NDK4SIP:
                    export = new ArchiveProducer(config, akubraConfiguration);
                    break;
                case Const.EXPORT_NDK4STT:
                    export = new ArchiveOldPrintProducer(config, akubraConfiguration);
                    break;
                default:
                    throw new IllegalArgumentException("Unsupported type of package");
            }

            File targetFolder = ExportUtils.createFolder(exportFolder, "archive_" + FoxmlUtils.pidAsUuid(params.getPids().get(0)), config.getExportParams().isOverwritePackage());
            try {
                //File archiveRootFolder = ExportUtils.createFolder(targetFolder, "archive_" + FoxmlUtils.pidAsUuid(pids.get(0)));
                targetFolder = export.archive(params.getPids(), targetFolder, params.isIgnoreMissingUrnNbn());
            } catch (Exception ex) {
                LOG.warning(ex.getMessage() + "\n" + ex.getCause());
            }
            ExportResultLog reslog = export.getResultLog();
            for (ExportResultLog.ExportResult logResult : reslog.getExports()) {
                for (ExportResultLog.ResultError error : logResult.getError()) {
                    if (isMissingURNNBN(error) && config.getExportParams().isDeletePackage()) {
                        MetsUtils.deleteFolder(targetFolder);
                        error.setDetails(error.getMessage());
                        return BatchUtils.finishedExportWithError(this.batchManager, batch, "Folder deleted.", error);
                    } else {
                        String exportPath = MetsUtils.renameFolder(exportFolder, targetFolder, null);
                        return BatchUtils.finishedExportWithError(this.batchManager, batch, exportPath, error);
                    }
                }
            }
            NdkExport exportNdk;
            typeOfPackage = getTypeOfPackage(params.getPids(), typeOfPackage);
            switch (typeOfPackage) {
                case Const.EXPORT_NDK_BASIC:
                    exportNdk = new NdkExport(config, akubraConfiguration);
                    break;
                case Const.EXPORT_NDK4SIP:
                    exportNdk = new NdkSipExport(config, akubraConfiguration);
                    break;
                case Const.EXPORT_NDK4STT:
                    exportNdk = new NdkSttExport(config, akubraConfiguration);
                    break;
                default:
                    throw new IllegalArgumentException("Unsupported type of package");
            }
            File target = null;
            for (File file : targetFolder.listFiles()) {
                deleteRawFolder(typeOfPackage, file);
                if (file.isDirectory()) {
                    target = ExportUtils.createFolder(file, "NDK", false);
                    continue;
                }
            }
            if (target == null) {
                target = targetFolder;
            }
            List<NdkExport.Result> ndkResults = exportNdk.exportNdkArchive(targetFolder, params.getPids(), true, true, true, params.isIgnoreMissingUrnNbn(), exportOptions.getLog());
            for (NdkExport.Result r : ndkResults) {
                if (r.getError() != null) {
                    String exportPath = MetsUtils.renameFolder(exportFolder, targetFolder, target);
                    finishedExportWithError(this.batchManager, batch, exportPath, r.getError());
                    throw r.getError();
                }
                if (r.getValidationError() != null) {
                    if (isMissingURNNBN(r) && config.getExportParams().isDeletePackage()) {
                        //MetsUtils.deleteFolder(r.getTargetFolder());
                        return BatchUtils.finishedExportWithError(this.batchManager, batch, targetFolder.getAbsolutePath(), r.getValidationError().getMessage());
                    } else {
                        String exportPath = MetsUtils.renameFolder(exportFolder, targetFolder, target);
                        return BatchUtils.finishedExportWithError(this.batchManager, batch, exportPath, r.getValidationError().getMessage());
                    }
                }
            }
            if (Const.EXPORT_NDK4SIP.equals(typeOfPackage)) {
                ArchiveProducer.fixPdfFile(targetFolder);
            }
            try {
                if (config.getArchiveExportOptions().isExtendedPackage() && params.isExtendedArchivePackage()) { // pokud neni tak normalne jedu dal
                    for (File folder : targetFolder.listFiles()) {
                        if (folder.isDirectory()) {
                            String pid = "uuid:" + folder.getName();
                            File archivalCopiesSource = getSourceLocation(pid, folder);
                            if (archivalCopiesSource == null || !archivalCopiesSource.exists()) {
                                if (params.getNoTifAvailableMessage() != null && !params.getNoTifAvailableMessage().isEmpty()) {
                                    File archivalCopiesDestination = new File(folder, config.getArchiveExportOptions().getArchivalCopyFolderName());
                                    if (!archivalCopiesDestination.mkdir()) {
                                        String exportPath = MetsUtils.renameFolder(exportFolder, targetFolder, target);
                                        return BatchUtils.finishedExportWithWarning(this.batchManager, batch, exportPath, "Nepodařilo se vytvořit složku k archivním kopiím: " + archivalCopiesDestination.getAbsolutePath());
                                    }
                                    if (!archivalCopiesDestination.exists()) {
                                        String exportPath = MetsUtils.renameFolder(exportFolder, targetFolder, target);
                                        return BatchUtils.finishedExportWithWarning(this.batchManager, batch, exportPath, "Nepodařilo se dostat do složky archivních kopií: " + archivalCopiesDestination.getAbsolutePath());
                                    }
                                    String filename = config.getArchiveExportOptions().getNoTifAvailableFileName();
                                    writeToFile(new File(archivalCopiesDestination, filename.endsWith(".txt") ? filename : filename + ".txt"), params.getNoTifAvailableMessage());
                                    String noTifAvailablePath = config.getArchiveExportOptions().getNoTifAvailablePath();
                                    File noTifAvailableSource = new File(noTifAvailablePath);
                                    if (noTifAvailableSource == null || !noTifAvailableSource.exists()) {
                                        String exportPath = MetsUtils.renameFolder(exportFolder, targetFolder, target);
                                        return BatchUtils.finishedExportWithWarning(this.batchManager, batch, exportPath, "Nenalezen soubor, ktery se kopiruje v pripade chybejicich skenu: " + noTifAvailableSource.getAbsolutePath());
                                    }
                                    FileUtils.copyFile(noTifAvailableSource, new File(archivalCopiesDestination, noTifAvailableSource.getName()));
                                    createMd5File(archivalCopiesDestination);
                                } else {
                                    if (archivalCopiesSource == null) {
                                        String exportPath = MetsUtils.renameFolder(exportFolder, targetFolder, target);
                                        return BatchUtils.finishedExportWithWarning(this.batchManager, batch, exportPath, "Nedefinovana cesta k archivnim kopiim.");
                                    }
                                    String exportPath = MetsUtils.renameFolder(exportFolder, targetFolder, target);
                                    return BatchUtils.finishedExportWithWarning(this.batchManager, batch, exportPath, "Nenalezena cesta k archivnim kopiim: " + archivalCopiesSource.getAbsolutePath());
                                }
                            } else {
                                File archivalCopiesDestination = new File(folder, config.getArchiveExportOptions().getArchivalCopyFolderName());
                                if (!archivalCopiesDestination.mkdir()) {
                                    String exportPath = MetsUtils.renameFolder(exportFolder, targetFolder, target);
                                    return BatchUtils.finishedExportWithWarning(this.batchManager, batch, exportPath, "Nepodařilo se vytvořit složku k archivním kopiím: " + archivalCopiesDestination.getAbsolutePath());
                                }
                                if (!archivalCopiesDestination.exists()) {
                                    String exportPath = MetsUtils.renameFolder(exportFolder, targetFolder, target);
                                    return BatchUtils.finishedExportWithWarning(this.batchManager, batch, exportPath, "Nepodařilo se dostat do složky archivních kopií: " + archivalCopiesDestination.getAbsolutePath());
                                }
                                try {
                                    FileUtils.copyDirectory(archivalCopiesSource, archivalCopiesDestination);
                                } catch (IOException ex) {
                                    ex.printStackTrace();
                                    String exportPath = MetsUtils.renameFolder(exportFolder, targetFolder, target);
                                    return BatchUtils.finishedExportWithWarning(this.batchManager, batch, exportPath, "Nepodařilo se překopírovat obsah z " + archivalCopiesSource.getAbsolutePath() + " do " + archivalCopiesDestination.getAbsolutePath());
                                }
                                if (params.getAdditionalInfoMessage() != null && !params.getAdditionalInfoMessage().isEmpty()) {
                                    String filename = config.getArchiveExportOptions().getAdditionalInfoFileName();
                                    writeToFile(new File(archivalCopiesDestination, filename.endsWith(".txt") ? filename : filename + ".txt"), params.getAdditionalInfoMessage());
                                }
                                createMd5File(archivalCopiesDestination);
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                ex.printStackTrace();
                String exportPath = MetsUtils.renameFolder(exportFolder, targetFolder, target);
                return BatchUtils.finishedExportWithWarning(this.batchManager, batch, exportPath, "Neočekávaná chyba " + ex.getMessage());
            }

            ExportUtils.writeExportResult(targetFolder, export.getResultLog());
            if (params.isBagit()) {
                for (File targetFile : targetFolder.listFiles()) {
                    if (targetFile.isDirectory()) {
                        BagitExport bagitExport = new BagitExport(config, targetFile);
                        bagitExport.prepare();
                        bagitExport.bagit();
                        bagitExport.zip();
                        bagitExport.moveToBagitFolder();
                        bagitExport.createMd5File();
                        bagitExport.moveToSpecifiedDirectories();
                        bagitExport.deleteExportFolder();
                    }
                }
                targetFolder.renameTo(new File(targetFolder.getParentFile(), "bagit_" + targetFolder.getName()));
            }
            for (NdkExport.Result r : ndkResults) {
                try {
                    setWorkflowExport("task.exportArchive", "param.exportArchive", r.getPageIndexCount(), params, getRoot(r.getPid(), exportFolder));
                } catch (MetsExportException | DigitalObjectException e) {
                    return BatchUtils.finishedExportWithWarning(batchManager, batch, batch.getFolder(), "Vyexportovano ale nepodarilo se propojit s RDflow.");
                } catch (WorkflowException e) {
                    if ("Task is blocked by other tasks!".equals(e.getMessage())) {
                        String targetLog = "Zapsan\u00ed \u00fakolu je blokov\u00e1no!";
                        return BatchUtils.finishedExportWithWarning(batchManager, batch, batch.getFolder(), targetLog);
                    } else {
                        return BatchUtils.finishedExportWithWarning(batchManager, batch, batch.getFolder(), "Vyexportovano ale nepodarilo se propojit s RDflow.");
                    }
                }
            }
            WorkflowExport exportWorkflow = new WorkflowExport(config, akubraConfiguration, user, exportOptions.getLocale());
            try {
                exportWorkflow.export(targetFolder, ndkResults, exportOptions.getLog());
            } catch (Exception ex) {
                return finishedExportWithError(batchManager, batch, batch.getFolder(), ex);
            }
            return BatchUtils.finishedExportSuccessfully(this.batchManager, batch, targetFolder.getAbsolutePath());
        } catch (Exception ex) {
            return finishedExportWithError(this.batchManager, batch, batch.getFolder(), ex);
        } catch (Throwable t) {
            IOException ex = new IOException(t.getMessage(), t);
            return finishedExportWithError(this.batchManager, batch, batch.getFolder(), ex);
        }
    }

    private void createMd5File(File folder) throws IOException {
        StringBuilder checksumBuilder = new StringBuilder();
        for (File file : folder.listFiles()) {
            ByteSource byteSource = Files.asByteSource(file);
            HashCode hc = byteSource.hash(Hashing.md5());
            checksumBuilder.append(hc.toString().toLowerCase()).append(" ").append("./" + folder.getName() + "/"+ file.getName()).append("\n");
        }
        writeToFile(new File(folder.getParentFile(), folder.getName() + ".md5"), checksumBuilder.toString());
    }

    private void writeToFile(File file, String message) throws IOException {
        BufferedWriter writer = null;
        try {
            writer = new BufferedWriter(new FileWriter(file));
            writer.append(message);
        } finally {
            if (writer != null) {
                writer.close();
            }
        }
    }

    private File getSourceLocation(String pid, File targetFolder) {
        File source = null;

        if (source == null || !source.exists()) {
            // hledani archivnich skenu v ATM
            try {
                DigitalObjectManager dom = DigitalObjectManager.getDefault();
                ProArcObject proArcObject = dom.find(pid, null);
                DigitalObjectHandler handler = dom.createHandler(proArcObject);
                RelationEditor relationEditor = handler.relations();
                String sourcePath = relationEditor.getArchivalCopiesPath();
                if (sourcePath != null && !sourcePath.isEmpty()) {
                    source = new File(sourcePath);
                }
            } catch (Throwable t) {
                t.printStackTrace();
                LOG.warning("Nepodarilo se hledat v ATM/archivalCopies");
            }
        }
        if (source == null || !source.exists()) {
            // hledani archivnich skenu v ${PROARC.HOME}/archival_copies
            File archivalCopies = new File(config.getConfigHome(), "archival_copies");
            if (archivalCopies.exists()) {
                source = new File(archivalCopies, pid.substring(5));
            }
        }
        if (source == null || !source.exists()) {
            try {
                IMetsElement element = getRoot(pid, targetFolder);
                String sourcePath = findArchivalCopiesInWorkFlow(element);
                if (sourcePath != null && !sourcePath.isEmpty()) {
                    source = new File(sourcePath);
                }
            } catch (Throwable t) {
                t.printStackTrace();
                LOG.warning("Nepodarilo se hledat ve WorkFlow");
            }

        }

        return source != null && source.exists() ? source : null;
    }

    private String findArchivalCopiesInWorkFlow(IMetsElement element) throws DigitalObjectException {
        if (element != null) {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            DigitalObjectManager.CreateHandler handler = dom.create(element.getModel(), element.getOriginalPid(), null, user, null, exportOptions.getLog());
            Job selectedJob = handler.getWfJob(element.getOriginalPid(), exportOptions.getLocale());
            if (selectedJob == null) {
                return null;
            }
            List<MaterialView> materials = handler.getMaterial(selectedJob.getId(), exportOptions.getLocale());
            for (MaterialView material : materials) {
                if ("material.folder.rawScan".equals(material.getName())) {
                    return material.getPath();
                }
            }
        }
        return null;
    }

    private Batch crossrefExport(Batch batch, BatchParams params) {
        try {
            URI exportUri = user.getExportFolder();
            File exportFolder = new File(exportUri);
//        CejshConfig cejshConfig = CejshConfig.from(appConfig.getAuthenticators());
            CrossrefExport export = new CrossrefExport(
                    DigitalObjectManager.getDefault(), config, akubraConfiguration);
            CejshStatusHandler status = new CejshStatusHandler();
            export.export(exportFolder, params.getPids(), status);
            File targetFolder = status.getTargetFolder();
//            ExportResult result = new ExportResult();
//            if (targetFolder != null) {
//                result.setTarget(user.getUserHomeUri().relativize(targetFolder.toURI()).toASCIIString());
//            }
            if (!status.isOk()) {
//                result.setErrors(new ArrayList<>());
//                for (ExportResultLog.ExportResult logResult : status.getReslog().getExports()) {
//                    for (ExportResultLog.ResultError error : logResult.getError()) {
//                        result.getErrors().add(new ExportError(
//                                error.getPid(), error.getMessage(), false, error.getDetails()));
//                    }
//                }
                String exportPath = MetsUtils.renameFolder(exportFolder, targetFolder, null);
                return finishedExportWithError(this.batchManager, batch, exportPath, status.getReslog().getExports());
            } else {
                return BatchUtils.finishedExportSuccessfully(this.batchManager, batch, status.getTargetFolder().getAbsolutePath());
            }
        } catch (Exception ex) {
            return finishedExportWithError(this.batchManager, batch, batch.getFolder(), ex);
        }
    }

    private Batch cejshExport(Batch batch, BatchParams params) {
        try {
            URI exportUri = user.getExportFolder();
            File exportFolder = new File(exportUri);
            CejshConfig cejshConfig = CejshConfig.from(config.getAuthenticators());
            CejshExport export = new CejshExport(
                    DigitalObjectManager.getDefault(), config, akubraConfiguration);
            CejshStatusHandler status = export.export(exportFolder, params.getPids());
            File targetFolder = status.getTargetFolder();
//            ExportResult result = new ExportResult();
//            if (targetFolder != null) {
//                result.setTarget(user.getUserHomeUri().relativize(targetFolder.toURI()).toASCIIString());
//            }
            if (!status.isOk()) {
//                result.setErrors(new ArrayList<>());
//                for (ExportResultLog.ExportResult logResult : status.getReslog().getExports()) {
//                    for (ExportResultLog.ResultError error : logResult.getError()) {
//                        result.getErrors().add(new ExportError(
//                                error.getPid(), error.getMessage(), false, error.getDetails()));
//                    }
//                }
                String exportPath = MetsUtils.renameFolder(exportFolder, targetFolder, null);
                return finishedExportWithError(this.batchManager, batch, exportPath, status.getReslog().getExports());
            } else {
                return BatchUtils.finishedExportSuccessfully(this.batchManager, batch, status.getTargetFolder().getAbsolutePath());
            }
        } catch (Exception ex) {
            return finishedExportWithError(this.batchManager, batch, batch.getFolder(), ex);
        }
    }

    private Batch ndkExport(Batch batch, BatchParams params) {
        try {
//            URI exportUri = user.getExportFolder();
//            File exportFolder = new File(exportUri);
//            List<ExportResult> result = new ArrayList<>(params.getPids().size());
            NdkExport export;

            String typeOfPackage = getTypeOfPackage(params.getPids(), params.getTypeOfPackage());

            switch (typeOfPackage) {
                case Const.EXPORT_NDK_BASIC:
                    export = new NdkExport(config, akubraConfiguration);
                    break;
                case Const.EXPORT_NDK4SIP:
                    export = new NdkSipExport(config, akubraConfiguration);
                    break;
                case Const.EXPORT_NDK4STT:
                    export = new NdkSttExport(config, akubraConfiguration);
                    break;
                case Const.EXPORT_NDK4CHRONICLE:
                    export = new NdkExport(config, akubraConfiguration);
                    break;
                default:
                    throw new IllegalArgumentException("Unsupported type of package");
            }

            File exportFolder = KrameriusOptions.getExportFolder(params.getKrameriusInstanceId(), user.getExportFolder(), config, KUtils.EXPORT_NDK);
            List<NdkExport.Result> ndkResults = export.export(exportFolder, params.getPids(), true, true, null, params.isIgnoreMissingUrnNbn(), exportOptions.getLog(), params.getKrameriusInstanceId(), params.getPolicy());
            for (NdkExport.Result r : ndkResults) {
                if (r.getError() != null) {
                    String exportPath = MetsUtils.renameFolder(exportFolder, r.getTargetFolder(), null);
                    batch = finishedExportWithError(this.batchManager, batch, exportPath, r.getError());
                    throw r.getError();
                } else if (r.getValidationError() != null) {
                    if (isMissingURNNBN(r) && config.getExportParams().isDeletePackage()) {
                        MetsUtils.deleteFolder(r.getTargetFolder());
                        batch = finishedExportWithError(batchManager, batch, r.getValidationError().getExceptions());
                    } else {
                        String exportPath = MetsUtils.renameFolder(exportFolder, r.getTargetFolder(), null);
                        batch = finishedExportWithError(batchManager, batch, r.getValidationError().getExceptions());
                    }
                } else {
                    // XXX not used for now
                    batch = BatchUtils.finishedExportSuccessfully(batchManager, batch, r.getTargetFolder().getAbsolutePath());
                }
            }
            if (Batch.State.EXPORT_DONE.equals(batch.getState())) {
                if (params.getKrameriusInstanceId() == null || params.getKrameriusInstanceId().isEmpty() || KRAMERIUS_INSTANCE_LOCAL.equals(params.getKrameriusInstanceId())) {
                    LOG.info("Export " + batch.getId() + " done.");
                    if (params.isBagit() || params.isLtpCesnet()) {
                        LOG.info("Export " + batch.getId() + " - doing bagit.");
                        File targetFolder = findNdkExportFolder(batch.getFolder());
                        if (targetFolder != null) {
                            for (File targetFile : targetFolder.listFiles()) {
                                if (targetFile.isDirectory()) {
                                    BagitExport bagitExport = new BagitExport(config, targetFile);
                                    bagitExport.prepare();
                                    bagitExport.bagit();
                                    bagitExport.zip();
                                    bagitExport.moveToBagitFolder();
                                    bagitExport.createMd5File();
                                    bagitExport.moveToSpecifiedDirectories();
                                    bagitExport.deleteExportFolder();
                                    if (params.isLtpCesnet() && !params.getLtpCesnetToken().isEmpty()) {
                                        LOG.info("Bagit " + batch.getId() + " finished - uploading to ltp cesnet");
                                        bagitExport.uploadToLtpCesnet(params.getLtpCesnetToken(), params.getPids().get(0));
                                    }
                                }
                            }
                            targetFolder.renameTo(new File(targetFolder.getParentFile(), "bagit_" + targetFolder.getName()));
                        }
                    }
                    for (NdkExport.Result r : ndkResults) {
                        try {
                            setWorkflowExport("task.exportNdkPsp", "param.exportNdkPsp.numberOfPackages", r.getPageIndexCount(), params, getRoot(r.getPid(), exportFolder));
                        } catch (MetsExportException | DigitalObjectException | WorkflowException e) {
                            return BatchUtils.finishedExportWithWarning(batchManager, batch, batch.getFolder(), "Vyexportovano ale nepodarilo se propojit s RDflow.");
                        }
                    }
                    return BatchUtils.finishedExportSuccessfully(this.batchManager, batch, ndkResults.get(0).getTargetFolder().getAbsolutePath());
                } else {
                    for (NdkExport.Result r : ndkResults) {
                        if (r.getKrameriusImportState() != null && KRAMERIUS_PROCESS_FAILED.equals(r.getKrameriusImportState())) {
                            batch = finishedExportWithError(this.batchManager, batch, r.getTargetFolder().getAbsolutePath(), r.getMessage());
                        } else if (r.getKrameriusImportState() != null && KRAMERIUS_PROCESS_WARNING.equals(r.getKrameriusImportState())) {
                            batch = BatchUtils.finishedExportWithWarning(this.batchManager, batch, r.getTargetFolder().getAbsolutePath(), r.getMessage());
                        } else if (r.getKrameriusImportState() != null && KRAMERIUS_PROCESS_FINISHED.equals(r.getKrameriusImportState())) {
                            batch = BatchUtils.finishedExportSuccessfully(this.batchManager, batch, r.getTargetFolder().getAbsolutePath(), r.getMessage());
                        }
                    }
                    return batch;
                }
            } else {
                LOG.info("Export " + batch.getId() + " undone.");
                return null;
            }
        } catch (Throwable t) {
            IOException ex = new IOException(t.getMessage(), t);
            return finishedExportWithError(this.batchManager, batch, batch.getFolder(), ex);
        }
    }

    private Batch desaExport(Batch batch, BatchParams params) {
        try {
            DesaExport export = new DesaExport(config, akubraConfiguration, MetaModelRepository.getInstance());
            URI exportUri = user.getExportFolder();
            File exportFolder = new File(exportUri);
            List<MetsExportException.MetsExportExceptionElement> exceptions = new ArrayList<>();
            if (params.isForDownload()) {
                DesaExport.Result r = export.exportDownload(exportFolder, params.getPids().get(0));
                if (r.getValidationError() != null) {
                    return finishedExportWithError(this.batchManager, batch, r.getValidationError().getExceptions());
                } else {
                    return BatchUtils.finishedExportSuccessfully(this.batchManager, batch, r.getDownloadToken());
                }
            } else {
                if (params.isDryRun()) {
                    for (String pid : params.getPids()) {
                        List<MetsExportException.MetsExportExceptionElement> errors = export.validate(exportFolder, pid, params.isHierarchy());
                        exceptions.addAll(errors);
                    }
                    if (exceptions.isEmpty()) {
                        return BatchUtils.finishedExportSuccessfully(batchManager, batch, batch.getFolder());
                    } else {
                        return finishedExportWithError(batchManager, batch, exceptions);
                    }
                } else {
                    for (String pid : params.getPids()) {
                        DesaExport.Result r = export.export(exportFolder, pid, null, false, params.isHierarchy(), false, exportOptions.getLog(), user);
                        if (r.getValidationError() != null) {
                            exceptions.addAll(r.getValidationError().getExceptions());
                        }
//                        return BatchUtils.finishedExportSuccessfully(this.batchManager, batch, r.getDownloadToken());
                    }
                    if (exceptions.isEmpty()) {
                        return BatchUtils.finishedExportSuccessfully(batchManager, batch, batch.getFolder());
                    } else {
                        return finishedExportWithError(batchManager, batch, exceptions);
                    }
                }
            }
        } catch (Exception ex) {
            return finishedExportWithError(this.batchManager, batch, batch.getFolder(), ex);
        }
    }

    private Batch datastreamExport(Batch batch, BatchParams params) {
        try {
            DataStreamExport export = new DataStreamExport(config, akubraConfiguration);
            URI exportUri = user.getExportFolder();
            File exportFolder = new File(exportUri);
            File target = export.export(exportFolder, params.isHierarchy(), params.getPids(), params.getDsIds());
//            URI targetPath = user.getUserHomeUri().relativize(target.toURI());
            return BatchUtils.finishedExportSuccessfully(batchManager, batch, target.getAbsolutePath());
        } catch (Exception e) {
            return finishedExportWithError(batchManager, batch, batch.getFolder(), e);
        }
    }

    private Batch krameriusExport(Batch batch, BatchParams params) throws Exception {
        try {
            Kramerius4Export export = new Kramerius4Export(config, akubraConfiguration, params.getPolicy(), params.getLicense(), params.isArchive());
            File exportFolder = KrameriusOptions.getExportFolder(params.getKrameriusInstanceId(), user.getExportFolder(), config, KUtils.EXPORT_KRAMERIUS);
            Kramerius4Export.Result k4Result = export.export(exportFolder, params.isHierarchy(), exportOptions.getLog(), params.getKrameriusInstanceId(), params.getPids().toArray(new String[params.getPids().size()]));
            if (k4Result.getException() != null) {
                String exportPath = MetsUtils.renameFolder(exportFolder, k4Result.getFile(), null);
                finishedExportWithError(this.batchManager, batch, exportPath, k4Result.getException());
                throw k4Result.getException();
            } else if (k4Result.getValidationError() != null) {
                String exportPath = MetsUtils.renameFolder(exportFolder, k4Result.getFile(), null);
                return BatchUtils.finishedExportWithWarning(this.batchManager, batch, exportPath, k4Result.getValidationError().getExceptions());
//                return logBatchFailure(batch, k4Result.getValidationError().getExceptions());
            } else {
//                URI targetPath = user.getUserHomeUri().relativize(k4Result.getFile().toURI());
                if (params.getKrameriusInstanceId() == null || params.getKrameriusInstanceId().isEmpty() || KRAMERIUS_INSTANCE_LOCAL.equals(params.getKrameriusInstanceId())) {
                    if (params.isBagit()) {
                        File targetFolder = k4Result.getFile();
                        if (targetFolder.isDirectory()) {
                            BagitExport bagitExport = new BagitExport(config, targetFolder);
                            bagitExport.prepareFoxml();
                            bagitExport.bagit();
                            bagitExport.zip();
                            bagitExport.moveToBagitFolder();
                            bagitExport.createMd5File();
                            bagitExport.moveToSpecifiedFoxmlDirectories();
                            bagitExport.deleteExportFolder();
                            bagitExport.deleteTargetFolderContent(targetFolder);
                        }
                        targetFolder.renameTo(new File(targetFolder.getParentFile(), "bagit_" + targetFolder.getName()));
                    }
                    batch = BatchUtils.finishedExportSuccessfully(this.batchManager, batch, k4Result.getFile().getAbsolutePath());
//                    return BatchUtils.finishedExportSuccessfully(batchManager, batch, k4Result.getFile().getAbsolutePath());
                } else {
                    if (k4Result.getKrameriusImportState() != null && KRAMERIUS_PROCESS_FAILED.equals(k4Result.getKrameriusImportState())) {
                        batch = finishedExportWithError(this.batchManager, batch, k4Result.getFile().getAbsolutePath(), k4Result.getMessage());
                    } else if (k4Result.getKrameriusImportState() != null && KRAMERIUS_PROCESS_WARNING.equals(k4Result.getKrameriusImportState())) {
                        batch = BatchUtils.finishedExportWithWarning(this.batchManager, batch, k4Result.getFile().getAbsolutePath(), k4Result.getMessage());
                    } else if (k4Result.getKrameriusImportState() != null && KRAMERIUS_PROCESS_FINISHED.equals(k4Result.getKrameriusImportState())) {
                        batch = BatchUtils.finishedExportSuccessfully(this.batchManager, batch, k4Result.getFile().getAbsolutePath(), k4Result.getMessage());
                    }
                }
                for (String pid : params.getPids()) {
                    setWorkflowExport("task.exportK4", "param.exportK4", k4Result.getPageCount(), params, getRoot(pid, exportFolder));
                }
            }
            return batch;
        } catch (Exception ex) {
            return finishedExportWithError(this.batchManager, batch, batch.getFolder(), ex);
//            if (ex instanceof DigitalObjectException || ex instanceof MetsExportException || ex instanceof WorkflowException) {
//                throw new IOException(ex);
//            }
//            throw ex;
        }
    }

    private URI runK4Export(String path, BatchParams params, String exportPageContext) throws Exception {
        Kramerius4Export export = new Kramerius4Export(config, akubraConfiguration, params.getPolicy(), params.getLicense(), params.isArchive());
        if (path == null || path.isEmpty()) {
            path = user.getExportFolder().getPath();
        }
        File exportFolder = new File(path);
        Kramerius4Export.Result target = export.export(exportFolder, params.isHierarchy(), exportOptions.getLog(), null, params.getPids().toArray(new String[params.getPids().size()]));
        if (target.getException() != null) {
            MetsUtils.renameFolder(exportFolder, target.getFile(), null);
            throw target.getException();
        }
        return user.getUserHomeUri().relativize(target.getFile().toURI());
    }

    private URI runDatastreamExport(String path, BatchParams params, List<String> dsIds) throws IOException, ExportException {
        DataStreamExport export = new DataStreamExport(config, akubraConfiguration);
        if (path == null || path.isEmpty()) {
            path = user.getExportFolder().getPath();
        }
        File exportFolder = new File(path);
        File target = export.export(exportFolder, params.isHierarchy(), params.getPids(), dsIds);
        return user.getUserHomeUri().relativize(target.toURI());
    }

    private IMetsElement getRoot(String pid, File exportFolder) throws MetsExportException {
        MetsContext metsContext = null;
        ProArcObject object = null;

        try {
            if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
                FedoraStorage fedoraStorage = FedoraStorage.getInstance();
                object = fedoraStorage.find(pid);
                metsContext = MetsContext.buildFedoraContext(object, null, exportFolder, fedoraStorage, config.getNdkExportOptions());
            } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                object = akubraStorage.find(pid);
                metsContext = MetsContext.buildAkubraContext(object, null, exportFolder, akubraStorage, config.getNdkExportOptions());
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
            }
        } catch (Exception ex) {
            throw new IllegalStateException(ex);
        }
        return getMetsElement(object, metsContext, true);
    }

    private MetsElement getMetsElement(ProArcObject fo, MetsContext dc, boolean hierarchy) throws MetsExportException {
        dc.resetContext();
        DigitalObject dobj = MetsUtils.readFoXML(fo.getPid(), dc);
        if (dobj == null) {
            throw new MetsExportException("Missing uuid");
        }
        return MetsElement.getElement(dobj, null, dc, hierarchy);
    }

    private void setWorkflowExport(String taskName, String parameterName, Integer pageCount, BatchParams params, IMetsElement root) throws DigitalObjectException, WorkflowException {
        if (root != null) {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            DigitalObjectManager.CreateHandler handler = dom.create(root.getModel(), root.getOriginalPid(), null, user, null, exportOptions.getLog());
            Job job = handler.getWfJob(root.getOriginalPid(), exportOptions.getLocale());
            if (job == null) {
                return;
            }
            List<TaskView> tasks = handler.getTask(job.getId(), exportOptions.getLocale());
            Task editedTask = null;
            for (TaskView task : tasks) {
                if (taskName.equals(task.getTypeRef())) {
                    editedTask = task;
                    break;
                }
            }
            if (editedTask != null) {
                editedTask.setOwnerId(new BigDecimal(user.getId()));
                editedTask.setState(Task.State.FINISHED);
                WorkflowProfiles workflowProfiles = WorkflowProfiles.getInstance();
                WorkflowDefinition workflow = workflowProfiles.getProfiles();
                WorkflowManager workflowManager = WorkflowManager.getInstance();

                try {
                /*    TaskFilter taskFilter = new TaskFilter();
                    taskFilter.setId(editedTask.getId());
                    taskFilter.setLocale(locale);
                    Task.State previousState = workflowManager.tasks().findTask(taskFilter, workflow).stream()
                            .findFirst().get().getState();
                 */
                    if (parameterName != null) {
                        Map<String, Object> parameters = new HashMap<>();
                        parameters.put(parameterName, pageCount);
                        workflowManager.tasks().updateTask(editedTask, parameters, workflow);
                    } else {
                        workflowManager.tasks().updateTask(editedTask, (Map<String, Object>) null, workflow);
                    }
                    // List<TaskView> result = workflowManager.tasks().findTask(taskFilter, workflow);

                    //if (result != null && !result.isEmpty() && result.get(0).getState() != previousState) {
                    WorkflowActionHandler workflowActionHandler = new WorkflowActionHandler(workflow, exportOptions.getLocale());
                    workflowActionHandler.runAction(editedTask);
                    //}
                } catch (IOException e) {
                    throw new DigitalObjectException(e.getMessage());
                }
            }
        }
    }

    private String getTypeOfPackage(List<String> pids, String typeOfPackage) throws MetsExportException {
        if (pids == null || pids.isEmpty()) {
            return typeOfPackage;
        }
        if (!Const.EXPORT_NDK_BASIC.equals(typeOfPackage)) {
            return typeOfPackage;
        }
        MetsContext metsContext = null;
        ProArcObject object = null;

        try {
            if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
                FedoraStorage fedoraStorage = FedoraStorage.getInstance();
                object = fedoraStorage.find(pids.get(0));
                metsContext = MetsContext.buildFedoraContext(object, null, null, fedoraStorage, config.getNdkExportOptions());
            } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                object = akubraStorage.find(pids.get(0));
                metsContext = MetsContext.buildAkubraContext(object, null, null, akubraStorage, config.getNdkExportOptions());
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
            }
        } catch (Exception ex) {
            throw new IllegalStateException(ex);
        }
        IMetsElement element = getMetsElement(object, metsContext, true);
        if (element != null) {
            String modelId = element.getModel().substring(12);
            if (NdkMapper.isOldprintModel(modelId)) {
                return Const.EXPORT_NDK4STT;
            } else if (NdkMapper.isChronicleModel(modelId)) {
                return Const.EXPORT_NDK4CHRONICLE;
            } else if (NdkMapper.isNdkEModel(modelId)) {
                return Const.EXPORT_NDK4SIP;
            } else if (NdkMapper.isNdkModel(modelId)) {
                return Const.EXPORT_NDK_BASIC;
            }
        }
        return typeOfPackage;
    }

    private void deleteRawFolder(String typeOfPackage, File archiveFolder) {
        for (File file : archiveFolder.listFiles()) {
            if (Const.EXPORT_NDK4SIP.equals(typeOfPackage) && file.isDirectory() && "RAW".equals(file.getName())) {
                MetsUtils.deleteFolder(file);
                break;
            }
        }
    }

    /*
     * @return true if at least one exception contains "URNNBN misssing" otherwise @return false
     */
    private boolean isMissingURNNBN(NdkExport.Result r) {
        for (MetsExportException.MetsExportExceptionElement exception : r.getValidationError().getExceptions()) {
            if ("URNNBN identifier is missing".equals(exception.getMessage())) {
                return true;
            }
        }
        return false;
    }

    private boolean isMissingURNNBN(ExportResultLog.ResultError error) {
        return "URNNBN identifier is missing".equals(error.getMessage());
    }

    public static final class ExportOptions {

        private File targetFolder;

        private AppConfiguration configuration;
        private Batch batch;
        private String log;
        private Locale locale;

        public ExportOptions(AppConfiguration config, Batch batch, String log, Locale locale) {
            this.configuration = config;
            this.batch = batch;
            this.log = log;
            this.locale = locale;
        }

        public File getTargetFolder() {
            return targetFolder;
        }

        public void setTargetFolder(File targetFolder) {
            this.targetFolder = targetFolder;
        }

        public AppConfiguration getConfiguration() {
            return configuration;
        }

        public void setConfiguration(AppConfiguration configuration) {
            this.configuration = configuration;
        }

        public Batch getBatch() {
            return batch;
        }

        public void setBatch(Batch batch) {
            this.batch = batch;
        }

        public String getLog() {
            return log;
        }

        public void setLog(String log) {
            this.log = log;
        }

        public Locale getLocale() {
            return locale;
        }

        public void setLocale(Locale locale) {
            this.locale = locale;
        }
    }
}
