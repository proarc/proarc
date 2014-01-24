/*
 * Copyright (C) 2014 Jan Pokorsky
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
import cz.cas.lib.proarc.common.export.desa.DesaContext;
import cz.cas.lib.proarc.common.export.desa.structure.DesaElement;
import cz.cas.lib.proarc.common.export.desa.structure.DesaElementVisitor;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsExportException.MetsExportExceptionElement;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Logger;
import org.apache.commons.io.FileUtils;

/**
 * The exporter of digital objects in format expected by the DESA repository.
 *
 * @author Jan Pokorsky
 */
public final class DesaExport {

    private static final Logger LOG = Logger.getLogger(DesaExport.class.getName());
    private final RemoteStorage rstorage;

    public DesaExport(RemoteStorage rstorage, Object desaService) {
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

        Result export = export(exportsFolder, pid, null, true, hierarchy, false);
        return export.getValidationError().getExceptions();
    }

    /**
     * Prepares export package of a single PID without children for later download.
     * @param exportsFolder folder with user exports
     * @param pid PID to export
     * @return the result with token or validation errors
     * @throws ExportException unexpected failure
     */
    public Result exportDownload(File exportsFolder, String pid) throws ExportException {
        return export(exportsFolder, pid, null, true, false, true);
    }

    /**
     * Finds zipped SIP from a previous export.
     * @param exportsFolder folder with user exports to scan
     * @param token folder name of the requested export
     * @return zip file or null
     */
    public static File findExportedPackage(File exportsFolder, String token) {
        // take the first .zip
        File target = new File(exportsFolder, token);
        if (!target.exists()) {
            return null;
        }
        File[] files = target.listFiles();
        for (File file : files) {
            if (file.isFile() && file.getName().endsWith(".zip")) {
                return file;
            }
        }
        return null;
    }

    /**
     * Exports PIDs in format suitable for DESA storage.
     * @param exportsFolder folder with user exports
     * @param pid PID to export
     * @param hierarchy export PID ant its children
     * @param dryRun build package without sending to the repository.
     * @param keepResult delete or not export folder on exit
     * @return the result
     * @throws ExportException unexpected failure
     */
    public Result export(File exportsFolder, String pid, String packageId, boolean dryRun, boolean hierarchy, boolean keepResult) throws ExportException {
        File target = ExportUtils.createFolder(exportsFolder, FoxmlUtils.pidAsUuid(pid));
        Result result = new Result();
        try {
            if (keepResult) {
                result.setTargetFolder(target);
            }
            RemoteObject fo = rstorage.find(pid);
            DesaContext dc = new DesaContext();
            dc.setFedoraClient(fo.getClient());
            dc.setRemoteStorage(rstorage);
            dc.setPackageID(packageId);
            dc.setOutputPath(target.getAbsolutePath());
            // transporter logs
            dc.setDesaResultPath(new File(target, "transporter").getAbsolutePath());
            try {
                DigitalObject dobj = MetsUtils.readFoXML(fo.getPid(), fo.getClient());
                DesaElement dElm = DesaElement.getElement(dobj, null, dc, hierarchy);
                // XXX if not dryRun configure DESA transporter here
                HashMap<String, String> desaServiceCfg = null;
                dElm.accept(new DesaElementVisitor(), desaServiceCfg);
                result.setValidationError(dc.getMetsExportException());
                return result;
            } catch (MetsExportException ex) {
                keepResult = false;
                if (ex.getExceptions().isEmpty()) {
                    throw new ExportException(ex);
                }
                return result.setValidationError(ex);
            }
        } finally {
            if (!keepResult || result.getValidationError() != null) {
                // run asynchronously not to block client request?
                boolean deleted = FileUtils.deleteQuietly(target);
                if (!deleted) {
                    LOG.warning("Cannot delete: " + target.toString());
                }
            }
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
