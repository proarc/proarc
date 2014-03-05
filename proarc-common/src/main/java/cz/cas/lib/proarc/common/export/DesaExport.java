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
import cz.cas.lib.proarc.common.export.desa.Const;
import cz.cas.lib.proarc.common.export.desa.DesaContext;
import cz.cas.lib.proarc.common.export.desa.DesaServices;
import cz.cas.lib.proarc.common.export.desa.DesaServices.DesaConfiguration;
import cz.cas.lib.proarc.common.export.desa.structure.DesaElement;
import cz.cas.lib.proarc.common.export.desa.structure.DesaElementVisitor;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.fedora.relation.RelationResource;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import java.io.File;
import java.util.HashMap;
import java.util.logging.Logger;
import org.apache.commons.io.FileUtils;

/**
 * The exporter of digital objects. It can build and validate SIP in format
 * expected by the DESA registry.
 *
 * @author Jan Pokorsky
 */
public final class DesaExport {

    private static final Logger LOG = Logger.getLogger(DesaExport.class.getName());
    private final RemoteStorage rstorage;
    private final DesaServices desaServices;
    private final MetaModelRepository models;

    public DesaExport(RemoteStorage rstorage, DesaServices desaServices, MetaModelRepository models) {
        this.rstorage = rstorage;
        this.desaServices = desaServices;
        this.models = models;
    }

    /**
     * Runs export to validate inputs. It cleans outputs on exit.
     * @param exportsFolder folder with user exports
     * @param pid PID to validate
     * @param hierarchy export PID ant its children
     * @return validation report
     * @throws ExportException unexpected failure
     */
    public MetsExportException validate(File exportsFolder, String pid,
            boolean hierarchy) throws ExportException {

        Result export = export(exportsFolder, pid, null, true, hierarchy, false, null);
        return export.getValidationError();
    }

    /**
     * Prepares export package of a single PID without children for later download.
     * @param exportsFolder folder with user exports
     * @param pid PID to export
     * @return the result with token or validation errors
     * @throws ExportException unexpected failure
     */
    public Result exportDownload(File exportsFolder, String pid) throws ExportException {
        return export(exportsFolder, pid, null, true, false, true, null);
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
     * Exports PIDs in format suitable for DESA registry. It also writes SIP IDs
     * as a hasExport relation to corresponding digital objects in case of
     * successful package delivery to the registry.
     *
     * @param exportsFolder folder with user exports
     * @param pid PID to export
     * @param hierarchy export PID ant its children
     * @param dryRun build package without sending to the repository.
     * @param keepResult delete or not export folder on exit
     * @param log message for storage logging
     * @return the result
     * @throws ExportException unexpected failure
     */
    public Result export(File exportsFolder, String pid, String packageId,
            boolean dryRun, boolean hierarchy, boolean keepResult, String log
            ) throws ExportException {

        File target = ExportUtils.createFolder(exportsFolder, FoxmlUtils.pidAsUuid(pid));
        Result result = new Result();
        try {
            if (keepResult) {
                result.setTargetFolder(target);
            }
            RemoteObject fo = rstorage.find(pid);
            DesaContext dc = buildContext(fo, packageId, target);
            try {
                DigitalObject dobj = MetsUtils.readFoXML(fo.getPid(), fo.getClient());
                DesaElement dElm = DesaElement.getElement(dobj, null, dc, hierarchy);
                DesaConfiguration desaCfg = transporterProperties(dryRun, dElm);
                HashMap<String, String> tProps = null;
                if (desaCfg != null) {
                    tProps = desaCfg.toTransporterConfig();
                    dc.setTransporter(desaServices.getDesaClient(desaCfg).getSipTransporter(
                            // XXX replace with real operator and producer code from http request
                            tProps.get("desa." + DesaConfiguration.PROPERTY_OPERATOR),
                            tProps.get("desa." + DesaConfiguration.PROPERTY_PRODUCER)));
                }
                dElm.accept(new DesaElementVisitor(), tProps);
                // dc.getMetsExportException() should be ignored now; validation warnings are always thrown
                // result.setValidationError(dc.getMetsExportException());
                if (!dryRun) {
                    storeExportResult(dElm, log);
                }
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

    private DesaContext buildContext(RemoteObject fo, String packageId, File targetFolder) {
        DesaContext dc = new DesaContext();
        dc.setFedoraClient(fo.getClient());
        dc.setRemoteStorage(rstorage);
        dc.setPackageID(packageId);
        dc.setOutputPath(targetFolder.getAbsolutePath());
        // transporter logs
        dc.setDesaResultPath(new File(targetFolder, "transporter").getAbsolutePath());
        return dc;
    }

    /**
     * Finds a transporter configuration for the element or {@code null} when
     * {@code dryRun} is {@code true}.
     * @throws MetsExportException no configuration found
     */
    private DesaConfiguration transporterProperties(boolean dryRun, DesaElement dElm) throws MetsExportException {
        if (!dryRun) {
            String modelId = dElm.getModel();
            if (modelId.startsWith(Const.FEDORAPREFIX)) {
                // uff, model is not model but fedora resource reference
                modelId = new RelationResource(modelId).getPid();
            }
            DesaConfiguration desaCfg = desaServices.findConfiguration(models.find(modelId));
            if (desaCfg != null) {
                return desaCfg;
            } else {
                throw new MetsExportException(dElm.getOriginalPid(),
                        String.format("No configuration of the DESA registry found for type %s!", modelId),
                        false, null);
            }
        }
        return null;
    }

    /**
     * Stores SIP ID to all digital objects referenced by the DESA element
     * hierarchy.
     * @param dElm exported elements
     * @throws MetsExportException write failure
     */
    void storeExportResult(DesaElement dElm, String log) throws MetsExportException {
        for (DesaElement childElm : dElm.getChildren()) {
            storeExportResult(childElm, log);
        }
        String idSIPVersion = dElm.getIdSIPVersion();
        String pid = dElm.getOriginalPid();
        storeObjectExportResult(pid, idSIPVersion, log);
    }

    void storeObjectExportResult(String pid, String idSIPVersion, String log) throws MetsExportException {
        try {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            FedoraObject fo = dom.find(pid, null);
            DigitalObjectHandler doh = dom.createHandler(fo);
            RelationEditor relations = doh.relations();
            relations.setExportResult(idSIPVersion);
            relations.write(relations.getLastModified(), log);
            doh.commit();
        } catch (DigitalObjectException ex) {
            throw new MetsExportException(pid, "Cannot store SIP ID Version!", false, ex);
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
