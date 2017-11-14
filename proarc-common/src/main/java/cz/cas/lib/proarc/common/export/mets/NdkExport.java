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
import cz.cas.lib.proarc.common.export.ExportResultLog.ResultStatus;
import cz.cas.lib.proarc.common.export.ExportUtils;
import cz.cas.lib.proarc.common.export.mets.MetsExportException.MetsExportExceptionElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElementVisitor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

/**
 * Exports digital object and transforms its data streams to NDK format.
 *
 * @author Jan Pokorsky
 * @see <a href='http://ndk.cz/digitalizace/nove-standardy-digitalizace-od-roku-2011'>NDK</a>
 */
public final class NdkExport {

    private static final Logger LOG = Logger.getLogger(NdkExport.class.getName());
    private final RemoteStorage rstorage;
    private final NdkExportOptions options;

    public NdkExport(RemoteStorage rstorage, NdkExportOptions options) {
        this.rstorage = rstorage;
        this.options = options;
    }

//    /**
//     * Runs export to validate inputs. It cleans outputs on exit.
//     * @param exportsFolder folder with user exports
//     * @param pid PID to validate
//     * @param hierarchy export PID ant its children
//     * @return validation report
//     * @throws ExportException unexpected failure
//     */
//    public List<MetsExportExceptionElement> validate(File exportsFolder, String pid,
//            boolean hierarchy) throws ExportException {
//
//        Result export = export(exportsFolder, pid, "ValPKGID", hierarchy, false, null);
//        if (export.getValidationError() != null) {
//            return export.getValidationError().getExceptions();
//        } else {
//            return null;
//        }
//    }

    /**
     * Prepares export package of a single PID without children for later download.
     * @param exportsFolder folder with user exports
     * @param pid PID to export
     * @return the result with token or validation errors
     * @throws ExportException unexpected failure
     */
    public Result exportDownload(File exportsFolder, String pid) throws ExportException {
        return export(exportsFolder, pid, null, true, false, null);
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

        ExportResultLog reslog = new ExportResultLog();
        File target = ExportUtils.createFolder(exportsFolder, FoxmlUtils.pidAsUuid(pids.get(0)));
        ArrayList<Result> results = new ArrayList<Result>(pids.size());
        for (String pid : pids) {
            ExportResultLog.ExportResult logItem = new ExportResultLog.ExportResult();
            logItem.setInputPid(pid);
            reslog.getExports().add(logItem);
            try {
                Result r = export(target, pid, null, hierarchy, keepResult, log);
                results.add(r);
                logResult(r, logItem);
            } catch (ExportException ex) {
                logItem.setStatus(ResultStatus.FAILED);
                logItem.getError().add(new ResultError(null, ex));
                ExportUtils.writeExportResult(target, reslog);
                throw ex;
            } finally {
                logItem.setEnd();
            }
        }
        ExportUtils.writeExportResult(target, reslog);
        return results;
    }

    Result export(File target, String pid, String packageId,
            boolean hierarchy, boolean keepResult, String log
            ) throws ExportException {

        Result result = new Result();
        try {
            if (keepResult) {
                result.setTargetFolder(target);
            }
            RemoteObject fo = rstorage.find(pid);
            MetsContext dc = buildContext(fo, packageId, target);
            try {
                List<String> PSPs = MetsUtils.findPSPPIDs(fo.getPid(), dc, hierarchy);
                for (String pspPid : PSPs) {
                    dc.resetContext();
                    DigitalObject dobj = MetsUtils.readFoXML(pspPid, fo.getClient());
                    MetsElement mElm = MetsElement.getElement(dobj, null, dc, hierarchy);
                    mElm.accept(new MetsElementVisitor());
                    // XXX use relative path to users folder?
                }
                storeExportResult(dc, target.toURI().toASCIIString(), log);
                return result;
            } catch (MetsExportException ex) {
                keepResult = false;
                // do not clean folder as i it is possilbe to write status.log
                if (ex.getExceptions().isEmpty()) {
                    throw new ExportException(pid, ex);
                }
                return result.setValidationError(ex);
            } catch (Throwable ex) {
                keepResult = false;
                // do not clean folder as i it is possilbe to write status.log
                throw new ExportException(pid, ex);
            }
        } finally {
            if (!keepResult) {
//                // run asynchronously not to block client request?
//                boolean deleted = FileUtils.deleteQuietly(target);
//                if (!deleted) {
//                    LOG.warning("Cannot delete: " + target.toString());
//                }
            }
        }
    }

    private MetsContext buildContext(RemoteObject fo, String packageId, File targetFolder) {
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
     * @param mElm
     *            exported elements
     * @throws MetsExportException
     *             write failure
     */
    void storeExportResult(MetsContext metsContext, String target, String log) throws MetsExportException {
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

        /**
         * Gets the token for future requests.
         * @return the token
         */
        public String getDownloadToken() {
            return targetFolder == null ? null: targetFolder.getName();
        }

    }
}
