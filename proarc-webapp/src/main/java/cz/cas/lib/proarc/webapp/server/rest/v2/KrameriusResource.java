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
import cz.cas.lib.proarc.common.kramerius.KUtils;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.KrameriusResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.KrameriusResourceApi;
import java.util.logging.Level;
import java.util.logging.Logger;
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
import javax.ws.rs.core.Request;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;

import static cz.cas.lib.proarc.common.kramerius.KrameriusOptions.KRAMERIUS_INSTANCE_LOCAL;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_MISSING_PARAMETER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_UNSUPPORTED_VALUE;

@Path(RestConfig.URL_API_VERSION_2 + "/" + KrameriusResourceApi.PATH)
public class KrameriusResource extends KrameriusResourceV1 {

    private static final Logger LOG = Logger.getLogger(KrameriusResource.class.getName());

    public KrameriusResource(
            @Context Request request,
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest
    ) throws AppConfigurationException {
        super(request, securityCtx, httpHeaders, uriInfo, httpRequest);
    }

    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<String> connectionTest() {
        try {
            return super.connectionTest();
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(KrameriusResourceApi.VIEW_MODS)
    @Produces({MediaType.APPLICATION_JSON})
    public Object viewMods(
            @QueryParam(KrameriusResourceApi.KRAMERIUS_OBJECT_PID) String pid,
            @QueryParam(KrameriusResourceApi.KRAMERIUS_INSTANCE) String krameriusInstanceId,
            @DefaultValue("true")
            @QueryParam(KrameriusResourceApi.KRAMERIUD_RERUN) boolean rerun
    ) {
        if (pid == null || pid.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, KrameriusResourceApi.KRAMERIUS_OBJECT_PID));
        }
        if (krameriusInstanceId == null || krameriusInstanceId.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        if (KRAMERIUS_INSTANCE_LOCAL.equals(krameriusInstanceId)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_UNSUPPORTED_VALUE, KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        try {
            return super.viewMods(pid, krameriusInstanceId, rerun);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @GET
    @Path(KrameriusResourceApi.VIEW_IMAGE)
    @Produces("*/*")
    public Object viewImage(
            @QueryParam(KrameriusResourceApi.KRAMERIUS_OBJECT_PID) String pid,
            @QueryParam(KrameriusResourceApi.KRAMERIUS_INSTANCE) String krameriusInstanceId
    ) {
        if (pid == null || pid.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, KrameriusResourceApi.KRAMERIUS_OBJECT_PID));
        }
        if (krameriusInstanceId == null || krameriusInstanceId.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        if (KRAMERIUS_INSTANCE_LOCAL.equals(krameriusInstanceId)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_UNSUPPORTED_VALUE, KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        try {
            return super.viewImage(pid, krameriusInstanceId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(KrameriusResourceApi.UPDATE_MODS)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DescriptionMetadata<Object>> updateMods(
            @FormParam(KrameriusResourceApi.KRAMERIUS_OBJECT_PID) String pid,
            @FormParam(KrameriusResourceApi.KRAMERIUS_INSTANCE) String krameriusInstanceId,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMJSONDATA) String jsonData,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMXMLDATA) String xmlData,
            @FormParam(MetaModelDataSource.FIELD_MODELOBJECT) String model,
            @DefaultValue("false")
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_IGNOREVALIDATION) boolean ignoreValidation,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_STANDARD) String standard
    ) {
        if (pid == null || pid.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, KrameriusResourceApi.KRAMERIUS_OBJECT_PID));
        }
        if (krameriusInstanceId == null || krameriusInstanceId.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        if (timestamp == null) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.TIMESTAMP_PARAM));
        }
        if (KRAMERIUS_INSTANCE_LOCAL.equals(krameriusInstanceId)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_UNSUPPORTED_VALUE, KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        try {
            return super.updateMods(pid, krameriusInstanceId, editorId, timestamp, jsonData, xmlData, model, ignoreValidation, standard);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(KrameriusResourceApi.IMPORT_2_PROARC)
    public SmartGwtResponse<KUtils.ImportResult> import2ProArc(
        @FormParam(KrameriusResourceApi.KRAMERIUS_OBJECT_PID) String pid,
        @FormParam(KrameriusResourceApi.KRAMERIUS_INSTANCE) String krameriusInstanceId
    ) {
        if (pid == null || pid.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, KrameriusResourceApi.KRAMERIUS_OBJECT_PID));
        }
        if (krameriusInstanceId == null || krameriusInstanceId.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        if (KRAMERIUS_INSTANCE_LOCAL.equals(krameriusInstanceId)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_UNSUPPORTED_VALUE, KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        try {
            return super.import2ProArc(pid, krameriusInstanceId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(KrameriusResourceApi.IMPORT_2_KRAMERIUS)
    public SmartGwtResponse<KUtils.ImportResult> import2Kramerius(
            @FormParam(KrameriusResourceApi.KRAMERIUS_OBJECT_PID) String pid,
            @FormParam(KrameriusResourceApi.KRAMERIUS_INSTANCE) String krameriusInstanceId,
            @FormParam(KrameriusResourceApi.KRAMERIUS_IMPORT_INSTANCE) String krameriusImportInstanceId
    ) {
        if (pid == null || pid.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, KrameriusResourceApi.KRAMERIUS_OBJECT_PID));
        }
        if (krameriusInstanceId == null || krameriusInstanceId.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        if (KRAMERIUS_INSTANCE_LOCAL.equals(krameriusInstanceId)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_UNSUPPORTED_VALUE, KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        if (krameriusImportInstanceId == null || krameriusImportInstanceId.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, KrameriusResourceApi.KRAMERIUS_IMPORT_INSTANCE));
        }
        if (KRAMERIUS_INSTANCE_LOCAL.equals(krameriusImportInstanceId)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_UNSUPPORTED_VALUE, KrameriusResourceApi.KRAMERIUS_IMPORT_INSTANCE));
        }
        try {
            return super.import2Kramerius(pid, krameriusInstanceId, krameriusImportInstanceId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }
}
