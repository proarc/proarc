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
package cz.cas.lib.proarc.common.export.mets;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.export.ExportException;
import cz.cas.lib.proarc.common.export.ExportResultLog;
import cz.cas.lib.proarc.common.export.ExportResultLog.ResultError;
import cz.cas.lib.proarc.common.export.ExportResultLog.ItemList;
import cz.cas.lib.proarc.common.export.ExportResultLog.ResultStatus;
import cz.cas.lib.proarc.common.export.ExportUtils;
import cz.cas.lib.proarc.common.export.mets.MetsExportException.MetsExportExceptionElement;
import cz.cas.lib.proarc.common.export.mets.structure.IMetsElementVisitor;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElementVisitor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.logging.Logger;
import cz.cas.lib.proarc.mets.info.Info;
import org.apache.commons.lang.Validate;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

/**
 * Exports digital object and transforms its data streams to NDK format.
 *
 * @author Jan Pokorsky
 * @see <a href='http://ndk.cz/digitalizace/nove-standardy-digitalizace-od-roku-2011'>NDK</a>
 */
public class NdkExport {

    private static final Logger LOG = Logger.getLogger(NdkExport.class.getName());
    protected final RemoteStorage rstorage;
    protected final NdkExportOptions options;

    public NdkExport(RemoteStorage rstorage, NdkExportOptions options) {
        this.rstorage = rstorage;
        this.options = options;
    }

    /**
     * Exports PIDs in Mets format
     *
     * @param exportsFolder
     *            folder with user exports
     * @param pids
     *            PID to export
     * @param hierarchy
     *            export PID and its children
     * @param keepResult
     *            delete or not export folder on exit
     * @param log
     *            message for storage logging
     * @return the result
     * @throws ExportException
     *             unexpected failure
     */
    public List<Result> export(File exportsFolder, List<String> pids,
            boolean hierarchy, boolean keepResult, String log
            ) throws ExportException {
        Validate.notEmpty(pids, "Pids to export are empty");

        ExportResultLog reslog = new ExportResultLog();
        File target = ExportUtils.createFolder(exportsFolder, FoxmlUtils.pidAsUuid(pids.get(0)));
        List<Result> results = new ArrayList<>(pids.size());
        for (String pid : pids) {
            ExportResultLog.ExportResult logItem = new ExportResultLog.ExportResult();
            logItem.setInputPid(pid);
            reslog.getExports().add(logItem);
            try {
                Result r = export(target, pid, hierarchy, keepResult, log);
                results.add(r);
                deleteUnnecessaryFolder(target);
                Info info = getInfo(getInfoFile(target));
                if (info != null) {
                    logItem.getItemList().add(new ItemList(getTotalSize(info), getFileSize(info, "alto"), getFileSize(info, "txt"),
                            getFileSize(info, "usercopy"),getFileSize(info, "mastercopy"),getFileSize(info, "amdsec")));
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
            } finally {
                logItem.setEnd();
            }
        }
        ExportUtils.writeExportResult(target, reslog);
        return results;
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
            return "Nepodarilo se spocitat " + value + " soubory.";
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
        if (target.isDirectory()){
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
     *
     * Each digital object is processed by visitor. This visitor create package on his own (and can be overriden for other NDK formats)
     *
     * @param target filepath to export
     * @param pid pid of exported object. This can be a root of object.
     * @param hierarchy recursive search for packages
     * @param keepResult delete or not export folder on exit
     * @param log message for storage logging
     * @return Result with target path and possible errors
     * @throws ExportException contains PID and exception
     */
    private Result export(File target, String pid,
                          boolean hierarchy, boolean keepResult, String log) throws ExportException {

        Result result = new Result();

        if (keepResult) {
            result.setTargetFolder(target);
        }
        RemoteObject fo = rstorage.find(pid);
        MetsContext dc = buildContext(fo, null, target);
        try {
            MetsElement metsElement = getMetsElement(fo, dc, hierarchy);
            if (Const.SOUND_COLLECTION.equals(metsElement.getElementType())) {
                metsElement.accept(new MetsElementVisitor());
            } else {
                List<String> PSPs = MetsUtils.findPSPPIDs(fo.getPid(), dc, hierarchy);
                if (PSPs.size() == 0) {
                    throw new MetsExportException(pid   , "Pod tímto modelem je očekáván model s přiděleným urn:nbn. Tento model chybí. Opravte a poté znovu exportujte.", false, null);
                }
                for (String pspPid : PSPs) {
                    dc.resetContext();
                    DigitalObject dobj = MetsUtils.readFoXML(pspPid, fo.getClient());
                    MetsElement mElm = MetsElement.getElement(dobj, null, dc, hierarchy);
                    mElm.accept(createMetsVisitor());
                    // XXX use relative path to users folder?
                }
            }
            storeExportResult(dc, target.toURI().toASCIIString(), log);
            return result;
        } catch (MetsExportException ex) {
            if (ex.getExceptions().isEmpty()) {
                throw new ExportException(pid, ex);
            }
            return result.setValidationError(ex);
        } catch (NoSuchElementException exel) {
            return result.setValidationError(new MetsExportException(pid, "Model obsahuje neočekávaný element {" + exel.getMessage() +"}.", false, null));
        }catch (Throwable ex) {
            throw new ExportException(pid, ex);
        }
    }

    protected IMetsElementVisitor createMetsVisitor() {
        return new MetsElementVisitor();
    }

    private MetsElement getMetsElement(RemoteObject fo, MetsContext dc, boolean hierarchy) throws MetsExportException {
        dc.resetContext();
        DigitalObject dobj = MetsUtils.readFoXML(fo.getPid(), fo.getClient());
         return MetsElement.getElement(dobj, null, dc, hierarchy);
    }

    protected MetsContext buildContext(RemoteObject fo, String packageId, File targetFolder) {
        MetsContext mc = new MetsContext();
        mc.setFedoraClient(fo.getClient());
        mc.setRemoteStorage(rstorage);
        mc.setPackageID(packageId);
        mc.setOutputPath(targetFolder.getAbsolutePath());
        mc.setAllowNonCompleteStreams(false);
        mc.setAllowMissingURNNBN(false);
        mc.setConfig(options);
        return mc;
    }

    /**
     * Stores logs to the digital object hierarchy.
     *
     * @param metsContext
     *            context with exported elements
     * @throws MetsExportException
     *             write failure
     */
    private void storeExportResult(MetsContext metsContext, String target, String log) throws MetsExportException {
        for (String pid : metsContext.getPidElements().keySet()) {
            try {
                ExportUtils.storeObjectExportResult(pid, target, log);
            } catch (DigitalObjectException ex) {
                throw new MetsExportException(pid, "Cannot store logs!", false, ex);
            }
        }
    }

    private void logResult(Result r, ExportResultLog.ExportResult logItem) {
        if (r.getValidationError() != null) {
            logItem.setStatus(ResultStatus.FAILED);
            List<MetsExportExceptionElement> exceptions = r.getValidationError().getExceptions();
            for (MetsExportExceptionElement mex : exceptions) {
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

        public MetsExportException getValidationError() {
            return validationError;
        }

        Result setValidationError(MetsExportException validationError) {
            this.validationError = validationError;
            return this;
        }

        /**
         * Gets the folder with exported packages.
         * @return {@code null} if the result should not be kept
         */
        public File getTargetFolder() {
            return targetFolder;
        }

        Result setTargetFolder(File targetFolder) {
            this.targetFolder = targetFolder;
            return this;
        }

    }
}
