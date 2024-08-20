/*
 * Copyright (C) 2014 Jan Pokorsky, Robert Simonovsky
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
package cz.cas.lib.proarc.common.process.export.mets;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.actions.CatalogRecord;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.kramerius.KImporter;
import cz.cas.lib.proarc.common.kramerius.KUtils;
import cz.cas.lib.proarc.common.kramerius.KrameriusOptions;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.export.ExportException;
import cz.cas.lib.proarc.common.process.export.ExportResultLog;
import cz.cas.lib.proarc.common.process.export.ExportResultLog.ItemList;
import cz.cas.lib.proarc.common.process.export.ExportResultLog.ResultError;
import cz.cas.lib.proarc.common.process.export.ExportResultLog.ResultStatus;
import cz.cas.lib.proarc.common.process.export.ExportUtils;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElementVisitor;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElementVisitor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.mets.info.Info;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.logging.Logger;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import org.apache.commons.lang.Validate;

import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_FAILED_V5;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_FAILED_V7;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_FINISHED_V5;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_FINISHED_V7;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_KILLED_V7;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_NO_BATCH_V5;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_FAILED;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_FINISHED;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_WARNING;
import static cz.cas.lib.proarc.common.kramerius.KrameriusOptions.KRAMERIUS_INSTANCE_LOCAL;
import static cz.cas.lib.proarc.common.kramerius.KrameriusOptions.findKrameriusInstance;

/**
 * Exports digital object and transforms its data streams to NDK format.
 *
 * @author Jan Pokorsky
 * @see <a href='http://ndk.cz/digitalizace/nove-standardy-digitalizace-od-roku-2011'>NDK</a>
 */
public class NdkExport {

    private static final Logger LOG = Logger.getLogger(NdkExport.class.getName());
    private final AppConfiguration appConfig;
    private final AkubraConfiguration akubraConfiguration;
    private FedoraStorage fedoraStorage;

    public NdkExport(FedoraStorage fedoraStorage, AppConfiguration appConfig, AkubraConfiguration akubraConfiguration) {
        this.appConfig = appConfig;
        this.akubraConfiguration = akubraConfiguration;
        this.fedoraStorage = fedoraStorage;
    }

    public NdkExport(AppConfiguration config, AkubraConfiguration akubraConfiguration) {
        this.appConfig = config;
        this.akubraConfiguration = akubraConfiguration;
        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            this.fedoraStorage = FedoraStorage.getInstance();
        }
    }

    /**
     * Exports PIDs in Mets format
     *
     * @param exportsFolder folder with user exports
     * @param pids          PID to export
     * @param hierarchy     export PID and its children
     * @param keepResult    delete or not export folder on exit
     * @param log           message for storage logging
     * @return the result
     * @throws ExportException unexpected failure
     */
    public List<Result> export(File exportsFolder, List<String> pids,
                               boolean hierarchy, boolean keepResult, Boolean overwrite,
                               boolean ignoreMissingUrnNbn, String log, String krameriusInstanceId,
                               String policy, Batch batch) throws ExportException {
        Validate.notEmpty(pids, "Pids to export are empty");

        ExportResultLog reslog = new ExportResultLog();
        File target;
        if (exportsFolder != null && "NDK".equals(exportsFolder.getName())) {
            target = exportsFolder;
        } else {
            target = ExportUtils.createFolder(exportsFolder, FoxmlUtils.pidAsUuid(pids.get(0)), overwrite(overwrite, this.appConfig.getExportParams().isOverwritePackage()));
        }
        if (batch != null) {
            BatchUtils.updateExportingBatch(BatchManager.getInstance(), batch, target);
        }
        List<Result> results = new ArrayList<>(pids.size());
        KUtils.ImportState state = null;
        for (String pid : pids) {
            ExportResultLog.ExportResult logItem = new ExportResultLog.ExportResult();
            logItem.setInputPid(pid);
            reslog.getExports().add(logItem);
            try {
                Result result = export(target, pid, hierarchy, keepResult, ignoreMissingUrnNbn, log);
                results.add(result);
                deleteUnnecessaryFolder(target);
                Info info = getInfo(getInfoFile(target));
                if (info != null) {
                    logItem.getItemList().add(new ItemList(getTotalSize(info), getFileSize(info, "alto"), getFileSize(info, "txt"),
                            getFileSize(info, "usercopy"), getFileSize(info, "mastercopy"), getFileSize(info, "amdsec"), getFileSize(info, "original")));
                }
                logResult(result, logItem);
                if (!(krameriusInstanceId == null || krameriusInstanceId.isEmpty() || KRAMERIUS_INSTANCE_LOCAL.equals(krameriusInstanceId))) {
                    KrameriusOptions.KrameriusInstance instance = findKrameriusInstance(appConfig.getKrameriusOptions().getKrameriusInstances(), krameriusInstanceId);
                    KImporter kImporter = new KImporter(appConfig, instance);
                    state = kImporter.importToKramerius(result.getTargetFolder(), false, KUtils.EXPORT_NDK, policy);
                    LOG.fine("PROCESS " + state.getProcessState() + " BATCH " + state.getBatchState() + " DELETE " + instance.deleteAfterImport());
                    if (KRAMERIUS_PROCESS_FINISHED.equals(state.getProcessState())  && (KRAMERIUS_BATCH_FINISHED_V5.equals(state.getBatchState()) || KRAMERIUS_BATCH_FINISHED_V7.equals(state.getBatchState()))) {
                        if (instance.deleteAfterImport()) {
                            LOG.fine("Mazu soubor " + result.getTargetFolder());
                            MetsUtils.deleteFolder(result.getTargetFolder());
                        }
                    }
                    switch (state.getBatchState()) {
                        case KRAMERIUS_BATCH_FINISHED_V5:
                        case KRAMERIUS_BATCH_FINISHED_V7:
                            result.setMessage("Import do Krameria (" + instance.getId() + " --> " + instance.getUrl() + ") prošel bez chyby.");
                            result.setKrameriusImportState(KRAMERIUS_PROCESS_FINISHED);
                            if (instance.uploadToCatalog() != null && !instance.uploadToCatalog().isEmpty()) {
                                LOG.info("Nahravam informace do katalogu.");
                                CatalogRecord catalogRecord = new CatalogRecord(appConfig, akubraConfiguration);
                                catalogRecord.update(instance.uploadToCatalog(), FoxmlUtils.pidAsUuid(pid));
                            } else {
                                LOG.info("Neni zapnuta volba nahrani informaci do katalogu.");
                            }
                            break;
                        case KRAMERIUS_BATCH_FAILED_V5:
                        case KRAMERIUS_BATCH_FAILED_V7:
                        case KRAMERIUS_BATCH_KILLED_V7:
                            result.setMessage("Import do Krameria (" + instance.getId() + " --> " + instance.getUrl() + ") selhal.");
                            result.setKrameriusImportState(KRAMERIUS_PROCESS_FAILED);
                            break;
                        case KRAMERIUS_BATCH_NO_BATCH_V5:
                            switch (state.getProcessState()) {
                                case KRAMERIUS_PROCESS_FINISHED:
                                    result.setMessage("Import do Krameria (" + instance.getId() + " --> " + instance.getUrl() + ") prošel, ale nebyla spuštěna indexace.");
                                    result.setKrameriusImportState(KRAMERIUS_PROCESS_WARNING);
                                    break;
                                case KRAMERIUS_PROCESS_FAILED:
                                    result.setMessage("Import do Krameria (" + instance.getId() + " --> " + instance.getUrl() + ") selhal.");
                                    result.setKrameriusImportState(KRAMERIUS_PROCESS_FAILED);
                                    break;
                                case KRAMERIUS_PROCESS_WARNING:
                                    result.setMessage("Import do Krameria (" + instance.getId() + " --> " + instance.getUrl() + ") prošel s chybou.");
                                    result.setKrameriusImportState(KRAMERIUS_PROCESS_WARNING);
                                    break;
                            }
                            break;
                        default:
                            result.setMessage("Import do Krameria (" + instance.getId() + " --> " + instance.getUrl() + ") selhal.");
                            result.setKrameriusImportState(KRAMERIUS_PROCESS_FAILED);
                            break;
                    }
                }
            } catch (ExportException ex) {
                logItem.setStatus(ResultStatus.FAILED);
                logItem.getError().add(new ResultError(null, ex));
                ExportUtils.writeExportResult(target, reslog);
                throw ex;
            } catch (JAXBException ex) {
                logItem.setStatus(ResultStatus.FAILED);
                logItem.getError().add(new ResultError(null, ex));
                ExportUtils.writeExportResult(target, reslog);
                throw new ExportException(ex);
            } catch (Exception ex) {
                logItem.setStatus(ResultStatus.FAILED);
                logItem.getError().add(new ResultError(null, ex));
                ExportUtils.writeExportResult(target, reslog);
                Result r = new Result();
                r.setError(ex);
                r.setPid(pid);
                r.setTargetFolder(target);
                results.add(r);
                return results;
            } finally {
                logItem.setEnd();
            }
        }
        if (krameriusInstanceId == null || krameriusInstanceId.isEmpty() || KRAMERIUS_INSTANCE_LOCAL.equals(krameriusInstanceId)) {
            ExportUtils.writeExportResult(target, reslog);
        } else {
            if (!(KRAMERIUS_PROCESS_FINISHED.equals(state.getProcessState())  && (KRAMERIUS_BATCH_FINISHED_V5.equals(state.getBatchState()) || KRAMERIUS_BATCH_FINISHED_V7.equals(state.getBatchState())))) {
                ExportUtils.writeExportResult(target, reslog);
            }
        }
        return results;
    }

    public List<Result> exportNdkArchive(File exportsFolder, List<String> pids,
                                         boolean hierarchy, boolean keepResult, Boolean overwrite,
                                         boolean ignoreMissingUrnNbn, String log) throws ExportException {
        Validate.notEmpty(pids, "Pids to export are empty");

        ExportResultLog reslog = new ExportResultLog();
        File target;
        /*if (exportsFolder != null && "NDK".equals(exportsFolder.getName())) {
            target = exportsFolder;
        } else {
            target = ExportUtils.createFolder(exportsFolder, FoxmlUtils.pidAsUuid(pids.get(0)), overwrite(overwrite, exportOptions.isOverwritePackage()));
        }*/
        target = exportsFolder;
        List<Result> results = new ArrayList<>(pids.size());
        for (String pid : pids) {
            ExportResultLog.ExportResult logItem = new ExportResultLog.ExportResult();
            logItem.setInputPid(pid);
            reslog.getExports().add(logItem);
            try {
                Result r = exportNdk(target, pid, hierarchy, keepResult, ignoreMissingUrnNbn, log);
                target = r.getTargetFolder();
                results.add(r);
                deleteUnnecessaryFolder(target);
                Info info = getInfo(getInfoFile(target));
                if (info != null) {
                    logItem.getItemList().add(new ItemList(getTotalSize(info), getFileSize(info, "alto"), getFileSize(info, "txt"),
                            getFileSize(info, "usercopy"), getFileSize(info, "mastercopy"), getFileSize(info, "amdsec"), getFileSize(info, "original")));
                }
                logResult(r, logItem);
            } catch (ExportException ex) {
                logItem.setStatus(ResultStatus.FAILED);
                logItem.getError().add(new ResultError(null, ex));
                ExportUtils.writeExportResult(target, reslog);
                throw ex;
            } catch (JAXBException ex) {
                logItem.setStatus(ResultStatus.FAILED);
                logItem.getError().add(new ResultError(null, ex));
                ExportUtils.writeExportResult(target, reslog);
                throw new ExportException(ex);
            } catch (Exception ex) {
                logItem.setStatus(ResultStatus.FAILED);
                logItem.getError().add(new ResultError(null, ex));
                ExportUtils.writeExportResult(target, reslog);
                Result r = new Result();
                r.setError(ex);
                r.setPid(pid);
                r.setTargetFolder(target);
                results.add(r);
                return results;
            } finally {
                logItem.setEnd();
            }
        }
        //ExportUtils.writeExportResult(target, reslog);
        return results;
    }

    private Result exportNdk(File target, String pid,
                             boolean hierarchy, boolean keepResult, boolean ignoreMissingUrnNbn, String log) throws ExportException {

        Result result = new Result();
        result.setPid(pid);

        if (keepResult) {
            result.setTargetFolder(target);
        }
        ProArcObject object = null;


        try {
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                object = fedoraStorage.find(pid);
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                object = akubraStorage.find(pid);
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
            MetsContext metsContext = buildContext(pid, target);
            File targetFolder = null;
            MetsElement metsElement = getMetsElement(object, metsContext, hierarchy);
            if (Const.SOUND_COLLECTION.equals(metsElement.getElementType())) {
                metsContext.resetContext();
                String outputPath = metsContext.getOutputPath() + File.separator + getUuidName(pid) + File.separator + "NDK";
                targetFolder = new File(outputPath);
                metsContext.setOutputPath(outputPath);
                result.setTargetFolder(targetFolder);
                DigitalObject dobj = MetsUtils.readFoXML(pid, metsContext);
                MetsElement mElm = MetsElement.getElement(dobj, null, metsContext, hierarchy);
                mElm.setIgnoreValidation(ignoreMissingUrnNbn);
                mElm.accept(createMetsVisitor());
            } else {
                List<String> PSPs = MetsUtils.findPSPPIDs(object.getPid(), metsContext, hierarchy);
                if (PSPs.size() == 0) {
                    throw new MetsExportException(pid, "Pod tímto modelem je očekáván model s přiděleným urn:nbn. Tento model chybí. Opravte a poté znovu exportujte.", false, null);
                }
                String output = metsContext.getOutputPath();
                for (String pspPid : PSPs) {
                    metsContext.resetContext();
                    String outputPath = output + File.separator + getUuidName(pspPid) + File.separator + "NDK";
                    targetFolder = new File(outputPath);
                    metsContext.setOutputPath(outputPath);
                    result.setTargetFolder(targetFolder);
                    DigitalObject dobj = MetsUtils.readFoXML(pspPid, metsContext);
                    MetsElement mElm = MetsElement.getElement(dobj, null, metsContext, hierarchy);
                    mElm.setIgnoreValidation(ignoreMissingUrnNbn);
                    mElm.accept(createMetsVisitor());
                    // XXX use relative path to users folder?
                }
            }
            if (targetFolder == null) {
                targetFolder = target;
            }
            storeExportResult(metsContext, targetFolder.toURI().toASCIIString(), "ARCHIVE", log);
            result.setPageIndexCount(countPageIndex(target));
            return result;
        } catch (MetsExportException ex) {
            if (ex.getExceptions().isEmpty()) {
                throw new ExportException(pid, ex);
            }
            return result.setValidationError(ex);
        } catch (NoSuchElementException exel) {
            return result.setValidationError(new MetsExportException(pid, "Model obsahuje neočekávaný element {" + exel.getMessage() + "}.", false, null));
        } catch (Throwable ex) {
            throw new ExportException(pid, ex);
        }
    }

    public MetsContext buildContext(String pid, File target) {
        ProArcObject object = null;
        try {
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                object = fedoraStorage.find(pid);
                return MetsContext.buildFedoraContext(object, null, target, fedoraStorage, appConfig.getNdkExportOptions());
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                object = akubraStorage.find(pid);
                return MetsContext.buildAkubraContext(object, null, target, akubraStorage, appConfig.getNdkExportOptions());
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
        } catch (Exception ex) {
            throw new IllegalStateException(ex);
        }
    }

    private String getUuidName(String name) {
        return name.substring(5);
    }

    private boolean overwrite(Boolean overwrite_constant, Boolean overwrite_config) {
        if (overwrite_constant != null) {
            return overwrite_constant;
        }
        return overwrite_config;
    }

    private String getFileSize(Info info, String value) {
        int values = 0;
        if (info.getItemlist() != null) {
            for (String path : info.getItemlist().getItem()) {
                if (path.contains(value)) {
                    values++;
                }
            }
        }
        if (values == 0) {
            return null;
        } else {
            return String.valueOf(values);
        }
    }

    private String getTotalSize(Info info) {
        if (info.getItemlist() != null) {
            return info.getItemlist().getItemtotal().toString();
        }
        return "nepodarilo se spocitat";
    }

    private Info getInfo(File infoFile) throws JAXBException {
        if (infoFile == null) {
            return null;
        }
        JAXBContext jaxbContext = JAXBContext.newInstance(Info.class);
        Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
        return (Info) unmarshaller.unmarshal(infoFile);
    }


    private File getInfoFile(File target) {
        if (target.isFile() && target.getName().startsWith("info_")) {
            return target;
        }
        if (target.isFile()) {
            return null;
        }
        if (target.isDirectory()) {
            for (File file : target.listFiles()) {
                File fileInfo = getInfoFile(file);
                if (fileInfo != null) {
                    return fileInfo;
                }
            }
        }
        return null;
    }

    /**
     * All folders are necessary, nothing is deleted
     * Used in NdkSttExport
     */
    protected void deleteUnnecessaryFolder(File target) {
    }

    /**
     * Exports packages. These can be split by PSP identifier (some level of model, for example issue for periodical)
     * <p>
     * Each digital object is processed by visitor. This visitor create package on his own (and can be overriden for other NDK formats)
     *
     * @param target     filepath to export
     * @param pid        pid of exported object. This can be a root of object.
     * @param hierarchy  recursive search for packages
     * @param keepResult delete or not export folder on exit
     * @param log        message for storage logging
     * @return Result with target path and possible errors
     * @throws ExportException contains PID and exception
     */
    private Result export(File target, String pid,
                          boolean hierarchy, boolean keepResult, boolean ignoreMissingUrnNbn, String log) throws ExportException {

        Result result = new Result();
        result.setPid(pid);

        if (keepResult) {
            result.setTargetFolder(target);
        }
        MetsContext metsContext = null;
        ProArcObject object = null;
        try {
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                object = fedoraStorage.find(pid);
                metsContext = MetsContext.buildFedoraContext(object, null, target, fedoraStorage, appConfig.getNdkExportOptions());
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                object = akubraStorage.find(pid);
                metsContext = MetsContext.buildAkubraContext(object, null, target, akubraStorage, appConfig.getNdkExportOptions());
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
            MetsElement metsElement = getMetsElement(object, metsContext, hierarchy);
            if (Const.SOUND_COLLECTION.equals(metsElement.getElementType())) {
                metsElement.setIgnoreValidation(ignoreMissingUrnNbn);
                metsElement.accept(new MetsElementVisitor());
            } else {
                List<String> PSPs = MetsUtils.findPSPPIDs(object.getPid(), metsContext, hierarchy);
                if (PSPs.size() == 0) {
                    throw new MetsExportException(pid, "Pod tímto modelem je očekáván model s přiděleným urn:nbn. Tento model chybí. Opravte a poté znovu exportujte.", false, null);
                }
                for (String pspPid : PSPs) {
                    metsContext.resetContext();
                    DigitalObject dobj = MetsUtils.readFoXML(pspPid, metsContext);
                    MetsElement mElm = MetsElement.getElement(dobj, null, metsContext, hierarchy);
                    mElm.setIgnoreValidation(ignoreMissingUrnNbn);
                    mElm.accept(createMetsVisitor());
                    // XXX use relative path to users folder?
                }
            }
            storeExportResult(metsContext, target.toURI().toASCIIString(), "NDK", log);
            result.setPageIndexCount(countPageIndex(target));
            return result;
        } catch (MetsExportException ex) {
            if (ex.getExceptions().isEmpty()) {
                throw new ExportException(pid, ex);
            }
            return result.setValidationError(ex);
        } catch (NoSuchElementException exel) {
            return result.setValidationError(new MetsExportException(pid, "Model obsahuje neočekávaný element {" + exel.getMessage() + "}.", false, null));
        } catch (Throwable ex) {
            throw new ExportException(pid, ex);
        }
    }

    private Integer countPageIndex(File folder) {
        File[] files = folder.listFiles();
        if (files != null) {
            for (File f : files) {
                if (f.isDirectory() && f.getName().equals("mastercopy")) {
                    return f.listFiles().length;
                } else if (f.isDirectory()) {
                    int count = countPageIndex(f);
                    if (count > 0) {
                        return count;
                    }
                }
            }
        }
        return 0;
    }

    protected IMetsElementVisitor createMetsVisitor() {
        return new MetsElementVisitor();
    }

    private MetsElement getMetsElement(ProArcObject fo, MetsContext metsContext, boolean hierarchy) throws MetsExportException {
        metsContext.resetContext();
        DigitalObject dobj = MetsUtils.readFoXML(fo.getPid(), metsContext);
        return MetsElement.getElement(dobj, null, metsContext, hierarchy);
    }

    /**
     * Stores logs to the digital object hierarchy.
     *
     * @param metsContext context with exported elements
     * @throws MetsExportException write failure
     */
    private void storeExportResult(MetsContext metsContext, String target, String type, String log) throws MetsExportException {
        for (String pid : metsContext.getPidElements().keySet()) {
            try {
                ExportUtils.storeObjectExportResult(pid, target, type, log);
            } catch (DigitalObjectException ex) {
                throw new MetsExportException(pid, "Cannot store logs!", false, ex);
            }
        }
    }

    private void logResult(Result r, ExportResultLog.ExportResult logItem) {
        if (r.getValidationError() != null) {
            logItem.setStatus(ResultStatus.FAILED);
            List<MetsExportException.MetsExportExceptionElement> exceptions = r.getValidationError().getExceptions();
            for (MetsExportException.MetsExportExceptionElement mex : exceptions) {
                List<String> validations = mex.getValidationErrors();
                String pid = mex.getPid();
                if (validations != null && !validations.isEmpty()) {
                    logItem.getError().add(new ResultError(pid, mex.getMessage(), validations));
                } else {
                    logItem.getError().add(new ResultError(pid, mex.getMessage(), mex.getEx()));
                }
            }
        } else {
            logItem.setStatus(ResultStatus.OK);
        }
    }

    /**
     * The export result.
     */
    public static class Result {

        private File targetFolder;
        private MetsExportException validationError;
        private Exception error;
        private String pid;
        private Integer pageIndexCount;
        private String krameriusImportState;
        private String message;

        public MetsExportException getValidationError() {
            return validationError;
        }

        public Result setValidationError(MetsExportException validationError) {
            this.validationError = validationError;
            return this;
        }

        public Exception getError() {
            return error;
        }

        public Result setError(Exception exception) {
            this.error = exception;
            return this;
        }

        /**
         * Gets the folder with exported packages.
         *
         * @return {@code null} if the result should not be kept
         */
        public File getTargetFolder() {
            return targetFolder;
        }

        Result setTargetFolder(File targetFolder) {
            this.targetFolder = targetFolder;
            return this;
        }

        public String getPid() {
            return pid;
        }

        public void setPid(String pid) {
            this.pid = pid;
        }

        public Integer getPageIndexCount() {
            return pageIndexCount;
        }

        public void setPageIndexCount(Integer pageIndexCount) {
            this.pageIndexCount = pageIndexCount;
        }

        public String getKrameriusImportState() {
            return krameriusImportState;
        }

        public void setKrameriusImportState(String krameriusImportState) {
            this.krameriusImportState = krameriusImportState;
        }

        public String getMessage() {
            return message;
        }

        public void setMessage(String message) {
            this.message = message;
        }
    }
}
