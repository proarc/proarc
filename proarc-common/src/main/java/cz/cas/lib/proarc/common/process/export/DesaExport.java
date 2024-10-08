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
package cz.cas.lib.proarc.common.process.export;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.export.desa.Const;
import cz.cas.lib.proarc.common.process.export.desa.DesaContext;
import cz.cas.lib.proarc.common.process.export.desa.DesaServices;
import cz.cas.lib.proarc.common.process.export.desa.structure.DesaElement;
import cz.cas.lib.proarc.common.process.export.desa.structure.DesaElementVisitor;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationResource;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.desa.SIP2DESATransporter;
import java.io.File;
import java.io.IOException;
import java.util.List;
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
//    private final RemoteStorage rstorage;
    private final AppConfiguration appConfiguration;
    private final AkubraConfiguration akubraConfiguration;
    private final MetaModelRepository models;

    public DesaExport(AppConfiguration appConfig, AkubraConfiguration akubraConfiguration, MetaModelRepository models) {
        this.appConfiguration = appConfig;
        this.akubraConfiguration = akubraConfiguration;
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
    public List<MetsExportException.MetsExportExceptionElement> validate(File exportsFolder, String pid,
                                                                         boolean hierarchy, Batch batch) throws ExportException {

        Result export = export(exportsFolder, pid, null, true, hierarchy, false, null, null, batch);
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
    public Result exportDownload(File exportsFolder, String pid, Batch batch) throws ExportException {
        return export(exportsFolder, pid, null, true, false, true, null, null, batch);
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
            boolean dryRun, boolean hierarchy, boolean keepResult, String log,
            UserProfile user, Batch batch
            ) throws ExportException {

        File target = ExportUtils.createFolder(exportsFolder, FoxmlUtils.pidAsUuid(pid), this.appConfiguration.getExportParams().isOverwritePackage());
        if (batch != null) {
            BatchUtils.updateExportingBatch(BatchManager.getInstance(), batch, target);
        }
        Result result = new Result();
        try {
            if (keepResult) {
                result.setTargetFolder(target);
            }
            ProArcObject object = null;
            DesaContext context = null;
            if (Storage.FEDORA.equals(appConfiguration.getTypeOfStorage())) {
                FedoraStorage fedoraStorage = FedoraStorage.getInstance(appConfiguration);
                object = fedoraStorage.find(pid);
                context = DesaContext.buildFedoraContext(object, null, null, fedoraStorage);
            } else if (Storage.AKUBRA.equals(appConfiguration.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                object = akubraStorage.find(pid);
                context = DesaContext.buildAkubraContext(object, null, null, akubraStorage);
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfiguration.getTypeOfStorage());
            }
            try {
                DigitalObject dobj = MetsUtils.readFoXML(context, object);
                if (dobj == null) {
                    throw new ExportException(pid);
                }
                DesaElement dElm = DesaElement.getElement(dobj, null, context, hierarchy);
                DesaServices.DesaConfiguration desaCfg = transporterProperties(dryRun, dElm);
                if (desaCfg != null) {
                    context.setTransporter(getSipTransporter(desaCfg, user));
                }
                dElm.accept(new DesaElementVisitor(), null);
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
        } catch (IOException e) {
            throw new ExportException(pid, e);
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

    private SIP2DESATransporter getSipTransporter(DesaServices.DesaConfiguration desaCfg, UserProfile operator) throws ExportException {
        DesaServices desaServices = this.appConfiguration.getDesaServices();
        String operatorName = desaServices.getOperatorName(operator, desaCfg);
        String producerCode = desaServices.getProducerCode(operator, desaCfg);
        if (operatorName == null || producerCode == null) {
            throw new ExportException("Requires operator name and producer code!");
        }
        return desaServices.getDesaClient(desaCfg).getSipTransporter(operatorName, producerCode);
    }

    /**
     * Finds a transporter configuration for the element or {@code null} when
     * {@code dryRun} is {@code true}.
     * @throws MetsExportException no configuration found
     */
    private DesaServices.DesaConfiguration transporterProperties(boolean dryRun, DesaElement dElm) throws MetsExportException {
        if (!dryRun) {
            String modelId = dElm.getModel();
            if (modelId.startsWith(Const.FEDORAPREFIX)) {
                // uff, model is not model but fedora resource reference
                modelId = new RelationResource(modelId).getPid();
            }
            DesaServices.DesaConfiguration desaCfg = this.appConfiguration.getDesaServices().findConfiguration(models.find(modelId));
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
            ExportUtils.storeObjectExportResult(pid, idSIPVersion, "DESA", log);
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
