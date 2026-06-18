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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.server.rest.v2;

import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchView;
import cz.cas.lib.proarc.common.kramerius.KrameriusOptions;
import cz.cas.lib.proarc.webapp.server.rest.ProArcResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.ExportResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.DefaultValue;
import jakarta.ws.rs.FormParam;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.SecurityContext;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.common.kramerius.KrameriusOptions.KRAMERIUS_INSTANCE_LOCAL;
import static cz.cas.lib.proarc.common.kramerius.KrameriusOptions.findKrameriusInstance;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_MISSING_PARAMETER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_NO_PERMISSION;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.URL_API_VERSION_2;

/**
 * REST resource to export data from the system.
 *
 * @author Lukas Sykora
 */
@Path(URL_API_VERSION_2 + "/" + ExportResourceApi.PATH)
public class ExportResource extends ExportResourceV1 {

    private static final Logger LOG = Logger.getLogger(ExportResource.class.getName());

    public ExportResource(
            @Context SecurityContext securityCtx,
            @Context HttpServletRequest httpRequest
    ) throws AppConfigurationException {
        super(securityCtx, httpRequest);
    }

    public ExportResource(
            @Context SecurityContext securityCtx,
            @Context HttpServletRequest httpRequest,
            @Context HttpHeaders httpHeaders
    ) throws AppConfigurationException {
        super(securityCtx, httpRequest, httpHeaders);
    }

    @GET
    @Path(ExportResourceApi.VALID_EXPORTS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<List<String>> validExports(
            @QueryParam(ExportResourceApi.VALUD_EXPORTS_MODEL_PARAM) String modelId
    ) {
        if (modelId == null) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, ExportResourceApi.VALUD_EXPORTS_MODEL_PARAM));
        }
        try {
            return super.validExports(modelId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @GET
    @Path(ExportResourceApi.KRAMERIUS4_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public ProArcResponse<KrameriusDescriptor> krameriusInstances(
            @QueryParam(ExportResourceApi.KRAMERIUS_INSTANCE_ID) String id
    ) {
        try {
            return super.krameriusInstances(id);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @GET
    @Path(ExportResourceApi.BATCHES_IN_PROCESS_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public ProArcResponse<BatchView> listProcessingBatches(
            @QueryParam(ImportResourceApi.IMPORT_BATCH_STATE) Set<Batch.State> batchState
    ) {
        try {
            return super.listProcessingBatches(batchState);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @POST
    @Path(ExportResourceApi.REEXPORT_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<ExportResult> reexport(
            @FormParam(ExportResourceApi.BATCH_ID) Integer batchId
    ) {
        if (batchId == null || batchId < 1) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, ExportResourceApi.BATCH_ID));
        }
        try {
            return super.reexport(batchId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @POST
    @Path(ExportResourceApi.DATASTREAM_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<ExportResult> datastream(
            @FormParam(ExportResourceApi.DATASTREAM_PID_PARAM) List<String> pids,
            @FormParam(ExportResourceApi.DATASTREAM_DSID_PARAM) List<String> dsIds,
            @FormParam(ExportResourceApi.DATASTREAM_HIERARCHY_PARAM) @DefaultValue("true") boolean hierarchy,
            @FormParam(ExportResourceApi.BATCH_NIGHT_ONLY) @DefaultValue("false") Boolean isNightOnly
    ) {
        if (pids == null) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, ExportResourceApi.DATASTREAM_PID_PARAM));
        }
        if (dsIds == null || dsIds.isEmpty()) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, ExportResourceApi.DATASTREAM_DSID_PARAM));
        }
        try {
            return super.datastream(pids, dsIds, hierarchy, isNightOnly);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @POST
    @Path(ExportResourceApi.KRAMERIUS4_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<ExportResult> kramerius4(
            @FormParam(ExportResourceApi.KRAMERIUS4_PID_PARAM) List<String> pids,
            @FormParam(ExportResourceApi.KRAMERIUS4_POLICY_PARAM) String policy,
            @FormParam(ExportResourceApi.KRAMERIUS4_LICENSE_PARAM) String license,
            @FormParam(ExportResourceApi.KRAMERIUS4_HIERARCHY_PARAM) @DefaultValue("true") boolean hierarchy,
            @FormParam(ExportResourceApi.KRAMERIUS_INSTANCE) String krameriusInstanceId,
            @DefaultValue("false") @FormParam(ExportResourceApi.EXPORT_BAGIT) boolean isBagit,
            @FormParam(ExportResourceApi.BATCH_NIGHT_ONLY) @DefaultValue("false") Boolean isNightOnly
    ) {
        if (pids.isEmpty()) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, ExportResourceApi.KRAMERIUS4_PID_PARAM));
        }
        KrameriusOptions.KrameriusInstance instance = findKrameriusInstance(appConfig.getKrameriusOptions().getKrameriusInstances(), krameriusInstanceId);
        if (!KRAMERIUS_INSTANCE_LOCAL.equals(instance.getId()) && !instance.isTestType() && !user.hasPermissionToImportToProdFunction()) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        try {
            return super.kramerius4(pids, policy, license, hierarchy, krameriusInstanceId, isBagit, isNightOnly);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @POST
    @Path(ExportResourceApi.NDK_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<ExportResult> newNdkExport(
            @FormParam(ExportResourceApi.NDK_PID_PARAM) List<String> pids,
            @FormParam(ExportResourceApi.NDK_PACKAGE) @DefaultValue("PSP") String typeOfPackage,
            @FormParam(ExportResourceApi.IGNORE_MISSING_URNNBN) boolean ignoreMissingUrnNbn,
            @DefaultValue("false") @FormParam(ExportResourceApi.EXPORT_BAGIT) boolean isBagit,
            @DefaultValue("false") @FormParam(ExportResourceApi.EXPORT_LTP_CESNET) boolean ltpCesnet,
            @FormParam(ExportResourceApi.EXPORT_LTP_CESNET_TOKEN) String token,
            @FormParam(ExportResourceApi.KRAMERIUS_INSTANCE) String krameriusInstanceId,
            @FormParam(ExportResourceApi.KRAMERIUS4_POLICY_PARAM) String policy,
            @FormParam(ExportResourceApi.KRAMERIUS4_LICENSE_PARAM) String license,
            @FormParam(ExportResourceApi.BATCH_NIGHT_ONLY) @DefaultValue("false") Boolean isNightOnly
    ) {
        if (pids.isEmpty()) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, ExportResourceApi.NDK_PID_PARAM));
        }
        try {
            return super.newNdkExport(pids, typeOfPackage, ignoreMissingUrnNbn, isBagit, ltpCesnet, token, krameriusInstanceId, policy, license, isNightOnly);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @POST
    @Path(ExportResourceApi.CEJSH_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<ExportResult> newCejshExport(
            @FormParam(ExportResourceApi.CEJSH_PID_PARAM) List<String> pids,
            @FormParam(ExportResourceApi.BATCH_NIGHT_ONLY) @DefaultValue("false") Boolean isNightOnly
    ) {
        if (pids.isEmpty()) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, ExportResourceApi.CEJSH_PID_PARAM));
        }
        try {
            return super.newCejshExport(pids, isNightOnly);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @POST
    @Path(ExportResourceApi.CROSSREF_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<ExportResult> newCrossrefExport(
            @FormParam(ExportResourceApi.CROSSREF_PID_PARAM) List<String> pids,
            @FormParam(ExportResourceApi.BATCH_NIGHT_ONLY) @DefaultValue("false") Boolean isNightOnly
    ) {
        if (pids.isEmpty()) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, ExportResourceApi.CROSSREF_PID_PARAM));
        }
        try {
            return super.newCrossrefExport(pids, isNightOnly);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @POST
    @Path(ExportResourceApi.ARCHIVE_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<ExportResult> newArchive(
            @FormParam(ExportResourceApi.ARCHIVE_PID_PARAM) List<String> pids,
            @FormParam(ExportResourceApi.NDK_PACKAGE) @DefaultValue("PSP") String typeOfPackage,
            @FormParam(ExportResourceApi.IGNORE_MISSING_URNNBN) boolean ignoreMissingUrnNbn,
            @DefaultValue("false") @FormParam(ExportResourceApi.EXPORT_BAGIT) boolean isBagit,
            @FormParam(ExportResourceApi.ARCHIVE_NO_TIF_AVAILABLE_MESSAGE) String noTifAvailableMessage,
            @FormParam(ExportResourceApi.ARCHIVE_ADDITIONAL_INFO_MESSAGE) String additionalInfoMessage,
            @FormParam(ExportResourceApi.ARCHIVE_EXTENDED_PACKAGE_PARAM) @DefaultValue("false") boolean extendedArchivePackage,
            @FormParam(ExportResourceApi.BATCH_NIGHT_ONLY) @DefaultValue("false") Boolean isNightOnly
    ) {
        if (pids.isEmpty()) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, ExportResourceApi.ARCHIVE_PID_PARAM));
        }
        try {
            return super.newArchive(pids, typeOfPackage, ignoreMissingUrnNbn, isBagit, noTifAvailableMessage, additionalInfoMessage, extendedArchivePackage, isNightOnly);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @POST
    @Path(ExportResourceApi.KWIS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<ExportResult> newKwisExport(
            @FormParam(ExportResourceApi.KWIS_PID_PARAM) List<String> pids,
            @FormParam(ExportResourceApi.KRAMERIUS4_POLICY_PARAM) String policy,
            @FormParam(ExportResourceApi.KRAMERIUS4_LICENSE_PARAM) String license,
            @FormParam(ExportResourceApi.KWIS_HIERARCHY_PARAM) @DefaultValue("true") boolean hierarchy,
            @FormParam(ExportResourceApi.BATCH_NIGHT_ONLY) @DefaultValue("false") Boolean isNightOnly
    ) {
        if (pids.isEmpty()) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, ExportResourceApi.KWIS_PID_PARAM));
        }
        try {
            return super.newKwisExport(pids, policy, license, hierarchy, isNightOnly);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @GET
    @Path("alephexport")
    @Produces({MediaType.APPLICATION_JSON})
    public List<String> alephExportState() {
        return super.alephExportState();
    }
}
