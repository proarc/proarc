/*
 * Copyright (C) 2012 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.server.rest.v1;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.dao.BatchView;
import cz.cas.lib.proarc.common.dao.BatchViewFilter;
import cz.cas.lib.proarc.common.export.AcceptedExports;
import cz.cas.lib.proarc.common.export.DesaExport;
import cz.cas.lib.proarc.common.export.ExportDispatcher;
import cz.cas.lib.proarc.common.export.ExportException;
import cz.cas.lib.proarc.common.export.ExportProcess;
import cz.cas.lib.proarc.common.export.ExportUtils;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsExportException.MetsExportExceptionElement;
import cz.cas.lib.proarc.common.fedora.Storage;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.kramerius.KrameriusOptions;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
import cz.cas.lib.proarc.webapp.server.ServerMessages;
import cz.cas.lib.proarc.webapp.server.rest.RestException;
import cz.cas.lib.proarc.webapp.server.rest.SessionContext;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.SecurityContext;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import org.apache.commons.io.FileUtils;
import org.glassfish.jersey.server.CloseableService;

import static cz.cas.lib.proarc.common.kramerius.KrameriusOptions.KRAMERIUS_INSTANCE_LOCAL;
import static cz.cas.lib.proarc.common.kramerius.KrameriusOptions.findKrameriusInstance;

/**
 * REST resource to export data from the system.
 *
 * @author Jan Pokorsky
 */
@Deprecated
@Path(RestConfig.URL_API_VERSION_1 + "/" + ExportResourceApi.PATH)
public class ExportResourceV1 {

    protected final AppConfiguration appConfig;
    private final AkubraConfiguration akubraConfiguration;
    protected final UserProfile user;
    private final SessionContext session;
    private final ImportBatchManager batchManager;
    private HttpHeaders httpHeaders;

    public ExportResourceV1(
            @Context SecurityContext securityCtx,
            @Context HttpServletRequest httpRequest
    ) throws AppConfigurationException {

        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            this.akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfig.getConfigHome());
        } else {
            this.akubraConfiguration = null;
        }
        this.httpHeaders = httpHeaders;
        session = SessionContext.from(httpRequest);
        this.batchManager = ImportBatchManager.getInstance();
        user = session.getUser();
    }

    public ExportResourceV1(
            @Context SecurityContext securityCtx,
            @Context HttpServletRequest httpRequest,
            @Context HttpHeaders httpHeaders
            ) throws AppConfigurationException {

        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            this.akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfig.getConfigHome());
        } else {
            this.akubraConfiguration = null;
        }
        this.httpHeaders = httpHeaders;
        session = SessionContext.from(httpRequest);
        this.batchManager = ImportBatchManager.getInstance();
        user = session.getUser();
    }

    @GET
    @Path(ExportResourceApi.VALID_EXPORTS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<List<String>> validExports(
            @QueryParam(ExportResourceApi.VALUD_EXPORTS_MODEL_PARAM) String modelId
    ) {

        Set<String> models = MetaModelRepository.getInstance().find()
                .stream().map(metaModel -> metaModel.getPid()).collect(Collectors.toSet());

        if (modelId == null || !models.contains(modelId)) {
            throw RestException.plainBadRequest(DigitalObjectResourceApi.DIGITALOBJECT_MODEL, modelId);
        }

        AcceptedExports ae = new AcceptedExports(modelId);
        List<String> validExportItems = ae.getList();

        return new SmartGwtResponse<List<String>>(validExportItems);
    }

    public String returnLocalizedMessage(String key, Object... arguments) {
        Locale locale = session.getLocale(httpHeaders);
        ServerMessages msgs = ServerMessages.get(locale);
        return msgs.getFormattedMessage(key, arguments);
    }

    @GET
    @Path(ExportResourceApi.KRAMERIUS4_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<KrameriusDescriptor> krameriusInstances(
            @QueryParam(ExportResourceApi.KRAMERIUS_INSTANCE_ID) String id) {

        List<KrameriusOptions.KrameriusInstance> tmpKrameriusInstances;
        if (id == null) {
            tmpKrameriusInstances = appConfig.getKrameriusOptions().getKrameriusInstances();
        } else {
            List<KrameriusOptions.KrameriusInstance> listOfInstances = appConfig.getKrameriusOptions().getKrameriusInstances();
            KrameriusOptions.KrameriusInstance krameriusInstance = KrameriusOptions.findKrameriusInstance(listOfInstances, id);
            tmpKrameriusInstances = krameriusInstance != null ? Arrays.asList(krameriusInstance) : Collections.<KrameriusOptions.KrameriusInstance>emptyList();
        }
        List<KrameriusOptions.KrameriusInstance> krameriusInstances;
        if (user.hasPermissionToImportToProdFunction()) {
            krameriusInstances = new ArrayList<>(tmpKrameriusInstances);
        } else {
            krameriusInstances = new ArrayList<>();
            for (KrameriusOptions.KrameriusInstance instance : tmpKrameriusInstances) {
                if (KRAMERIUS_INSTANCE_LOCAL.equals(instance.getId())) {
                    krameriusInstances.add(instance);
                } else {
                    if (instance.isTestType()) {
                        krameriusInstances.add(instance);
                    }
                }
            }
        }
        ArrayList<KrameriusDescriptor> result = new ArrayList<>(krameriusInstances.size());
        for (KrameriusOptions.KrameriusInstance kc : krameriusInstances) {
            result.add(KrameriusDescriptor.create(kc));
        }
        return new SmartGwtResponse<KrameriusDescriptor>(result);
    }

    @GET
    @Path(ExportResourceApi.BATCHES_IN_PROCESS_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<BatchView> listProcessingBatches(
            @QueryParam(ImportResourceApi.IMPORT_BATCH_STATE) Set<Batch.State> batchState

    ) throws IOException {
        if (batchState.isEmpty()) {
            batchState.add(Batch.State.EXPORTING);
            batchState.add(Batch.State.EXPORT_PLANNED);
        }

        BatchViewFilter filterAll = new BatchViewFilter()
                .setState(batchState)
                .setMaxCount(1000)
                .setSortBy("id");

        List<BatchView> plannedBatches = batchManager.viewProcessingBatches(filterAll, user, UserRole.ROLE_USER);

        int endRow = 0 + plannedBatches.size() - 1;
        int total = plannedBatches.size();
        return new SmartGwtResponse<BatchView>(SmartGwtResponse.STATUS_SUCCESS, 0, endRow, total, plannedBatches);
    }

    @POST
    @Path(ExportResourceApi.REEXPORT_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ExportResult> reexport(
            @FormParam(ExportResourceApi.BATCH_ID) Integer batchId
    ) throws Exception {

        Locale locale = session.getLocale(httpHeaders);

        if (batchId == null || batchId < 1) {
            throw RestException.plainText(Status.BAD_REQUEST, ServerMessages.get(locale).getFormattedMessage("Resource_Missing_Value", ExportResourceApi.BATCH_ID));
        }
        Batch batch = this.batchManager.get(batchId);
        if (batch == null) {
            ExportResult result = new ExportResult(new ExportError(String.valueOf(batchId), ServerMessages.get(locale).getFormattedMessage("Resource_Unsupported_Value", batchId)));
            return new SmartGwtResponse<>(result);
        } else {
            if (!Batch.State.EXPORT_FAILED.equals(batch.getState())) {
                ExportResult result = new ExportResult(new ExportError(String.valueOf(batchId), ServerMessages.get(locale).getFormattedMessage("ExportResouce_ReexportNotSupported_WrongState", batch.getState())));
                return new SmartGwtResponse<>(result);
            } else {
                String profileId = batch.getProfileId();
                BatchParams params = batch.getParamsAsObject();
                if (params == null) {
                    ExportResult result = new ExportResult(new ExportError(String.valueOf(batchId), ServerMessages.get(locale).getFormattedMessage("ExportResouce_ReexportNotSupported_MissingParams")));
                    return new SmartGwtResponse<>(result);
                }
                ExportProcess process = ExportProcess.prepare(appConfig, akubraConfiguration, batch, batchManager, user, session.asFedoraLog(), session.getLocale(httpHeaders));
                ExportDispatcher.getDefault().addExport(process);
                ExportResult result = new ExportResult(batch.getId(), "Proces naplánován.");
                return new SmartGwtResponse<ExportResult>(result);
            }
        }
    }

    @POST
    @Path(ExportResourceApi.DATASTREAM_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ExportResult> datastream(
            @FormParam(ExportResourceApi.DATASTREAM_PID_PARAM) List<String> pids,
            @FormParam(ExportResourceApi.DATASTREAM_DSID_PARAM) List<String> dsIds,
            @FormParam(ExportResourceApi.DATASTREAM_HIERARCHY_PARAM) @DefaultValue("true") boolean hierarchy
            ) throws IOException, ExportException {

        if (pids.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing " + ExportResourceApi.DATASTREAM_PID_PARAM);
        }
        if (dsIds.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing " + ExportResourceApi.DATASTREAM_DSID_PARAM);
        }

        BatchParams params = new BatchParams(pids, hierarchy, dsIds);
        Batch batch = BatchUtils.addNewExportBatch(this.batchManager, pids, user, Batch.EXPORT_DATASTREAM, params);

        ExportProcess process = ExportProcess.prepare(appConfig, akubraConfiguration, batch, batchManager, user, session.asFedoraLog(), session.getLocale(httpHeaders));
        ExportDispatcher.getDefault().addExport(process);
        ExportResult result = new ExportResult(batch.getId(), "Proces naplánován.");
        return new SmartGwtResponse<ExportResult>(result);
    }

    @POST
    @Path(ExportResourceApi.KRAMERIUS4_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ExportResult> kramerius4(
            @FormParam(ExportResourceApi.KRAMERIUS4_PID_PARAM) List<String> pids,
            @FormParam(ExportResourceApi.KRAMERIUS4_POLICY_PARAM) String policy,
            @FormParam(ExportResourceApi.KRAMERIUS4_HIERARCHY_PARAM) @DefaultValue("true") boolean hierarchy,
            @FormParam(ExportResourceApi.KRAMERIUS_INSTANCE) String krameriusInstanceId,
            @DefaultValue("false") @FormParam(ExportResourceApi.EXPORT_BAGIT) boolean isBagit
            ) throws Exception {

        if (pids.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing " + ExportResourceApi.KRAMERIUS4_PID_PARAM);
        }
        KrameriusOptions.KrameriusInstance instance = findKrameriusInstance(appConfig.getKrameriusOptions().getKrameriusInstances(), krameriusInstanceId);
        if (!KRAMERIUS_INSTANCE_LOCAL.equals(instance.getId()) && !instance.isTestType() && !user.hasPermissionToImportToProdFunction()) {
                throw RestException.plainText(Status.BAD_REQUEST, "Permission denied for " + ExportResourceApi.KRAMERIUS_INSTANCE);
        }
        BatchParams params = new BatchParams(pids, policy, hierarchy, krameriusInstanceId, isBagit);
        Batch batch = BatchUtils.addNewExportBatch(this.batchManager, pids, user, Batch.EXPORT_KRAMERIUS, params);

        ExportProcess process = ExportProcess.prepare(appConfig, akubraConfiguration, batch, batchManager, user, session.asFedoraLog(), session.getLocale(httpHeaders));
        ExportDispatcher.getDefault().addExport(process);
        ExportResult result = new ExportResult(batch.getId(), "Proces naplánován.");
        return new SmartGwtResponse<ExportResult>(result);
    }

    /**
     * Starts a new export to DESA repository.
     *
     * @param pids PIDs to export
     * @param hierarchy export also children hierarchy of requested PIDs. Default is {@code false}.
     * @param forDownload export to file system for later client download. If {@code true} dryRun is ignored.
     *              Default is {@code false}.
     * @param dryRun use to build packages without sending to the repository. Default is {@code false}.
     * @return the list of results for requested PIDs
     * @throws IOException unexpected failure
     * @throws ExportException unexpected failure
     */
    @POST
    @Path(ExportResourceApi.DESA_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ExportResult> newDesaExport(
            @FormParam(ExportResourceApi.DESA_PID_PARAM) List<String> pids,
            @FormParam(ExportResourceApi.DESA_HIERARCHY_PARAM) @DefaultValue("false") boolean hierarchy,
            @FormParam(ExportResourceApi.DESA_FORDOWNLOAD_PARAM) @DefaultValue("false") boolean forDownload,
            @FormParam(ExportResourceApi.DESA_DRYRUN_PARAM) @DefaultValue("false") boolean dryRun
            ) throws IOException, ExportException {

        if (pids.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing " + ExportResourceApi.DESA_PID_PARAM);
        }
        BatchParams params = new BatchParams(pids, hierarchy, forDownload, dryRun);
        Batch batch = BatchUtils.addNewExportBatch(this.batchManager, pids, user, Batch.EXPORT_DESA, params);

        ExportProcess process = ExportProcess.prepare(appConfig, akubraConfiguration, batch, batchManager, user, session.asFedoraLog(), session.getLocale(httpHeaders));
        ExportDispatcher.getDefault().addExport(process);
        ExportResult result = new ExportResult(batch.getId(), "Proces naplánován.");
        return new SmartGwtResponse<ExportResult>(result);
    }

    /**
     * Gets the exported package built by {@link #newDesaExport(List, boolean, boolean, boolean)}  } with {@code forDownload=true}.
     * The package data are removed after completion of the response.
     *
     * @param token token to identify the prepared package
     * @return the package contents in ZIP format
     */
    @GET
    @Path(ExportResourceApi.DESA_PATH)
    @Produces(MediaType.APPLICATION_OCTET_STREAM)
    public Response getDesaExport(
            @QueryParam(ExportResourceApi.RESULT_TOKEN) String token,
            @Context CloseableService finalizer
            ) {

        URI exportUri = user.getExportFolder();
        File exportFolder = new File(exportUri);
        final File file = DesaExport.findExportedPackage(exportFolder, token);
        if (file == null) {
            return Response.status(Status.NOT_FOUND).type(MediaType.TEXT_PLAIN_TYPE)
                    .entity("The contents not found!").build();
        }
        finalizer.add(new Closeable() {

            @Override
            public void close() {
                FileUtils.deleteQuietly(file.getParentFile());
            }
        });
        return Response.ok(file, MediaType.APPLICATION_OCTET_STREAM)
                .header("Content-Disposition", "attachment; filename=\"" + file.getName() + '"')
                .build();
    }

    /**
     * Gets the exported package as PSP (default) or SIP (simpler format for eborn documents)
     * @param pids identifiers of saved objects in fedora repository
     * @param typeOfPackage (PSP | SIP]
     * @return ExportResult with path to package and possible errors from export
     * @throws ExportException
     */
    @POST
    @Path(ExportResourceApi.NDK_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ExportResult> newNdkExport(
            @FormParam(ExportResourceApi.NDK_PID_PARAM) List<String> pids,
            @FormParam(ExportResourceApi.NDK_PACKAGE) @DefaultValue("PSP") String typeOfPackage,
            @FormParam(ExportResourceApi.IGNORE_MISSING_URNNBN) boolean ignoreMissingUrnNbn,
            @DefaultValue("false") @FormParam(ExportResourceApi.EXPORT_BAGIT) boolean isBagit,
            @DefaultValue("false") @FormParam(ExportResourceApi.EXPORT_LTP_CESNET) boolean ltpCesnet,
            @FormParam(ExportResourceApi.EXPORT_LTP_CESNET_TOKEN) String token,
            @FormParam(ExportResourceApi.KRAMERIUS_INSTANCE) String krameriusInstanceId,
            @FormParam(ExportResourceApi.KRAMERIUS4_POLICY_PARAM) String policy
            ) throws Exception {
        if (pids.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing " + ExportResourceApi.DESA_PID_PARAM);
        }
        try {
            ExportUtils.missingUrnNbn(pids, ignoreMissingUrnNbn, appConfig, akubraConfiguration);
        } catch (MetsExportException ex) {
            ExportResult result = new ExportResult(ex.getExceptions());
            result.setIgnoreMissingUrnNbn(true);
            return new SmartGwtResponse<ExportResult>(result);
        }
        BatchParams params = new BatchParams(pids, typeOfPackage, ignoreMissingUrnNbn, isBagit, ltpCesnet, token, krameriusInstanceId, policy);
        Batch batch = BatchUtils.addNewExportBatch(this.batchManager, pids, user, Batch.EXPORT_NDK, params);

        ExportProcess process = ExportProcess.prepare(appConfig, akubraConfiguration, batch, batchManager, user, session.asFedoraLog(), session.getLocale(httpHeaders));
        ExportDispatcher.getDefault().addExport(process);
        ExportResult result = new ExportResult(batch.getId(), "Proces naplánován.");
        return new SmartGwtResponse<ExportResult>(result);
    }

    /**
     * Starts a new CEJSH export.
     * @param pids PIDs to export
     * @return the export result
     */
    @POST
    @Path(ExportResourceApi.CEJSH_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ExportResult> newCejshExport(
            @FormParam(ExportResourceApi.CEJSH_PID_PARAM) List<String> pids
            ) throws Exception {

        if (pids.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing " + ExportResourceApi.CEJSH_PID_PARAM);
        }

        BatchParams params = new BatchParams(pids);
        Batch batch = BatchUtils.addNewExportBatch(this.batchManager, pids, user, Batch.EXPORT_CEJSH, params);

        ExportProcess process = ExportProcess.prepare(appConfig, akubraConfiguration, batch, batchManager, user, session.asFedoraLog(), session.getLocale(httpHeaders));
        ExportDispatcher.getDefault().addExport(process);
        ExportResult result = new ExportResult(batch.getId(), "Proces naplánován.");
        return new SmartGwtResponse<ExportResult>(result);
    }

    /**
     * Starts a new Crossref export.
     * @param pids PIDs to export
     * @return the export result
     */
    @POST
    @Path(ExportResourceApi.CROSSREF_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ExportResult> newCrossrefExport(
            @FormParam(ExportResourceApi.CROSSREF_PID_PARAM) List<String> pids
            ) throws Exception {

        if (pids.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing " + ExportResourceApi.CROSSREF_PID_PARAM);
        }
        BatchParams params = new BatchParams(pids);
        Batch batch = BatchUtils.addNewExportBatch(this.batchManager, pids, user, Batch.EXPORT_CROSSREF, params);

        ExportProcess process = ExportProcess.prepare(appConfig, akubraConfiguration, batch, batchManager, user, session.asFedoraLog(), session.getLocale(httpHeaders));
        ExportDispatcher.getDefault().addExport(process);
        ExportResult result = new ExportResult(batch.getId(), "Proces naplánován.");
        return new SmartGwtResponse<ExportResult>(result);
    }

    /**
     * Starts new archiving.
     * @param pids PIDs to export
     * @return the export result
     */
    @POST
    @Path(ExportResourceApi.ARCHIVE_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ExportResult> newArchive(
            @FormParam(ExportResourceApi.ARCHIVE_PID_PARAM) List<String> pids,
            @FormParam(ExportResourceApi.NDK_PACKAGE) @DefaultValue("PSP") String typeOfPackage,
            @FormParam(ExportResourceApi.IGNORE_MISSING_URNNBN) boolean ignoreMissingUrnNbn,
            @DefaultValue("false") @FormParam(ExportResourceApi.EXPORT_BAGIT) boolean isBagit,
            @FormParam(ExportResourceApi.ARCHIVE_NO_TIF_AVAILABLE_MESSAGE) String noTifAvailableMessage,
            @FormParam(ExportResourceApi.ARCHIVE_ADDITIONAL_INFO_MESSAGE) String additionalInfoMessage
            ) throws Exception {

        if (pids.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing " + ExportResourceApi.ARCHIVE_PID_PARAM);
        }

        try {
            ExportUtils.missingUrnNbn(pids, ignoreMissingUrnNbn, appConfig, akubraConfiguration);
        } catch (MetsExportException ex) {
            ExportResult result = new ExportResult(ex.getExceptions());
            result.setIgnoreMissingUrnNbn(true);
            return new SmartGwtResponse<ExportResult>(result);
        }
        BatchParams params = new BatchParams(pids, typeOfPackage, ignoreMissingUrnNbn, isBagit, noTifAvailableMessage, additionalInfoMessage);
        Batch batch = BatchUtils.addNewExportBatch(this.batchManager, pids, user, Batch.EXPORT_ARCHIVE, params);

        ExportProcess process = ExportProcess.prepare(appConfig, akubraConfiguration, batch, batchManager, user, session.asFedoraLog(), session.getLocale(httpHeaders));
        ExportDispatcher.getDefault().addExport(process);
        ExportResult result = new ExportResult(batch.getId(), "Proces naplánován.");
        return new SmartGwtResponse<ExportResult>(result);
    }

    @POST
    @Path(ExportResourceApi.KWIS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<ExportResult> newKwisExport(
            @FormParam(ExportResourceApi.KWIS_PID_PARAM) List<String> pids,
            @FormParam(ExportResourceApi.KRAMERIUS4_POLICY_PARAM) String policy,
            @FormParam(ExportResourceApi.KWIS_HIERARCHY_PARAM) @DefaultValue("true") boolean hierarchy
    ) throws Exception {

        if (pids.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing " + ExportResourceApi.KRAMERIUS4_PID_PARAM);
        }
        BatchParams params = new BatchParams(pids, policy, hierarchy, null, false);
        Batch batch = BatchUtils.addNewExportBatch(this.batchManager, pids, user, Batch.EXPORT_KWIS, params);

        ExportProcess process = ExportProcess.prepare(appConfig, akubraConfiguration, batch, batchManager, user, session.asFedoraLog(), session.getLocale(httpHeaders));
        ExportDispatcher.getDefault().addExport(process);
        ExportResult result = new ExportResult(batch.getId(), "Proces naplánován.");
        return new SmartGwtResponse<ExportResult>(result);
    }

    @GET
    @Path("alephexport")
    @Produces({MediaType.APPLICATION_JSON})
    public List<String> alephExportState() {
        File[] listOfFiles = new File("/tmp/aleph").listFiles();

        if (listOfFiles == null) {
            return new ArrayList<>();
        }

        List<String> fileNames = new ArrayList<>();

        for (File file : listOfFiles) {
            fileNames.add(file.getName());
        }

        return fileNames;
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class KrameriusDescriptor {

        public static KrameriusDescriptor create(KrameriusOptions.KrameriusInstance krameriusInstance) {
            return new KrameriusDescriptor(krameriusInstance.getId(), krameriusInstance.getTitle());
        }

        @XmlElement(name = ExportResourceApi.KRAMERIUS_INSTANCE_ID)
        private String id;
        @XmlElement(name = ExportResourceApi.KRAMERIUS_INSTANCE_NAME)
        private String name;

        public KrameriusDescriptor(String id, String name) {
            this.id = id;
            this.name = name;
        }
    }


    /**
     * The export result.
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class ExportResult {

        @XmlElement(name = ExportResourceApi.RESULT_ID)
        private Integer exportId;

        @XmlElement(name = ExportResourceApi.RESULT_TOKEN)
        private String token;

        /**
         * The target folder path.
         */
        @XmlElement(name = ExportResourceApi.RESULT_TARGET)
        private String target;

        @XmlElement(name = ExportResourceApi.RESULT_ERRORS)
        private List<ExportError> errors;

        @XmlElement(name = ExportResourceApi.IGNORE_MISSING_URNNBN)
        private Boolean ignoreMissingUrnNbn;

        public ExportResult() {
        }

        public ExportResult(URI targetPath) {
            this.target = targetPath.toASCIIString();
        }

        public ExportResult(String token) {
            this.token = token;
        }

        public ExportResult(Integer exportId, String target) {
            this.exportId = exportId;
            this.target = target;
        }

        public ExportResult(List<MetsExportExceptionElement> validations) {
            if (validations != null) {
                errors = new ArrayList<>();
                for (MetsExportExceptionElement me : validations) {
                    errors.add(new ExportError(me));
                }
            }
        }

        public ExportResult(ExportError error) {
            this.errors = new ArrayList<>();
            this.errors.add(error);
        }

        public Integer getExportId() {
            return exportId;
        }

        public String getTarget() {
            return target;
        }

        @SuppressWarnings("AssignmentOrReturnOfFieldWithMutableType")
        public List<ExportError> getErrors() {
            if (errors == null) {
                errors = new ArrayList<>();
            }
            return errors;
        }

        public void setExportId(Integer exportId) {
            this.exportId = exportId;
        }

        public void setTarget(String target) {
            this.target = target;
        }

        public void setErrors(List<ExportError> errors) {
            this.errors = new ArrayList<>(errors);
        }

        public String getToken() {
            return token;
        }

        public void setToken(String token) {
            this.token = token;
        }

        public Boolean getIgnoreMissingUrnNbn() {
            return ignoreMissingUrnNbn;
        }

        public void setIgnoreMissingUrnNbn(boolean ignoreMissingUrnNbn) {
            this.ignoreMissingUrnNbn = ignoreMissingUrnNbn;
        }
    }

    /**
     * The export error.
     */
    @XmlRootElement(name = ExportResourceApi.RESULT_ERROR)
    public static class ExportError {

        @XmlElement(name = ExportResourceApi.RESULT_ERROR_PID)
        private String pid;

        @XmlElement(name = ExportResourceApi.RESULT_ERROR_MESSAGE)
        private String message;

        @XmlElement(name = ExportResourceApi.RESULT_ERROR_WARNING)
        private boolean warning;

        @XmlElement(name = ExportResourceApi.RESULT_ERROR_LOG)
        private String log;

        @XmlElement(name = ExportResourceApi.IGNORE_MISSING_URNNBN)
        private boolean ignoreMissingUrnNbn;

        public ExportError() {
        }

        public ExportError(String pid, String message, boolean warning, String log) {
            this.pid = pid;
            this.message = message;
            this.warning = warning;
            this.log = log;
        }

        public ExportError(MetsExportExceptionElement me) {
            this.pid = me.getPid();
            this.message = me.getMessage();
            this.warning = me.isWarning();
            List<String> validations = me.getValidationErrors();
            Exception ex = me.getEx();
            if (validations != null && !validations.isEmpty()) {
                StringBuilder sb = new StringBuilder();
                for (String validation : validations) {
                    if (message == null) {
                        message = validation;
                    }
                    sb.append(validation).append('\n');
                }
                this.log = sb.toString();
            } else if (ex != null) {
                StringWriter sw = new StringWriter();
                PrintWriter pw = new PrintWriter(sw);
                ex.printStackTrace(pw);
                pw.close();
                this.log = sw.toString();
            }
        }

        public ExportError(String pid, String message) {
            this.pid = pid;
            this.message = message;
        }

        public void setIgnoreMissingUrnNbn(boolean ignoreMissingUrnNbn) {
            this.ignoreMissingUrnNbn = ignoreMissingUrnNbn;
        }
    }

}
