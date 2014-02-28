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
package cz.cas.lib.proarc.common.export;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;

import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsExportException.MetsExportExceptionElement;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElementVisitor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;

import java.io.File;
import java.util.List;
import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;

/**
 * The exporter of digital objects.
 *
 * @author Jan Pokorsky, Robert Simonovsky
 */
public final class MetsExport {

    private static final Logger LOG = Logger.getLogger(MetsExport.class.getName());
    private final RemoteStorage rstorage;

    public MetsExport(RemoteStorage rstorage) {
        this.rstorage = rstorage;
    }

    /**
     * Runs export to validate inputs. It cleans outputs on exit.
     * @param exportsFolder folder with user exports
     * @param pid PID to validate
     * @param hierarchy export PID ant its children
     * @return validation report
     * @throws ExportException unexpected failure
     */
    public List<MetsExportExceptionElement> validate(File exportsFolder, String pid,
            boolean hierarchy) throws ExportException {

        Result export = export(exportsFolder, pid, "ValPKGID", hierarchy, false, null);
        if (export.getValidationError() != null) {
            return export.getValidationError().getExceptions();
        } else {
            return null;
        }
    }

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
     * @param pid
     *            PID to export
     * @param hierarchy
     *            export PID ant its children
     * @param keepResult
     *            delete or not export folder on exit
     * @param log
     *            message for storage logging
     * @return the result
     * @throws ExportException
     *             unexpected failure
     */
    public Result export(File exportsFolder, String pid, String packageId, boolean hierarchy, boolean keepResult, String log
            ) throws ExportException {

        File target = ExportUtils.createFolder(exportsFolder, FoxmlUtils.pidAsUuid(pid));
        Result result = new Result();
        try {
            if (keepResult) {
                result.setTargetFolder(target);
            }
            RemoteObject fo = rstorage.find(pid);
            MetsContext dc = buildContext(fo, packageId, target);
            try {
                DigitalObject dobj = MetsUtils.readFoXML(fo.getPid(), fo.getClient());
                MetsElement mElm = MetsElement.getElement(dobj, null, dc, hierarchy);
                mElm.accept(new MetsElementVisitor());
                storeExportResult(mElm, log);
                return result;
            } catch (MetsExportException ex) {
                keepResult = false;
                if (ex.getExceptions().isEmpty()) {
                    throw new ExportException(pid, ex);
                }
                return result.setValidationError(ex);
            } catch (Throwable ex) {
                keepResult = false;
                throw new ExportException(pid, ex);
            }
        } finally {
            if (!keepResult) {
                // run asynchronously not to block client request?
                boolean deleted = FileUtils.deleteQuietly(target);
                if (!deleted) {
                    LOG.warning("Cannot delete: " + target.toString());
                }
            }
        }
    }

    private MetsContext buildContext(RemoteObject fo, String packageId, File targetFolder) {
        MetsContext mc = new MetsContext();
        mc.setFedoraClient(fo.getClient());
        mc.setRemoteStorage(rstorage);
        mc.setPackageID(packageId);
        mc.setOutputPath(targetFolder.getAbsolutePath());
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
    void storeExportResult(MetsElement mElm, String log) throws MetsExportException {
        for (MetsElement childElm : mElm.getChildren()) {
            storeExportResult(childElm, log);
        }
        String pid = mElm.getOriginalPid();
        storeObjectExportResult(pid, log);
    }

    void storeObjectExportResult(String pid, String log) throws MetsExportException {
        try {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            FedoraObject fo = dom.find(pid, null);
            DigitalObjectHandler doh = dom.createHandler(fo);
            RelationEditor relations = doh.relations();
            relations.write(relations.getLastModified(), log);
            doh.commit();
        } catch (DigitalObjectException ex) {
            throw new MetsExportException(pid, "Cannot store logs!", false, ex);
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
