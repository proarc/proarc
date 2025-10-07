/*
 * Copyright (C) 2025 Lukas Sykora
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
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.software.Software;
import cz.cas.lib.proarc.common.software.SoftwareException;
import cz.cas.lib.proarc.common.software.SoftwareNotFoundException;
import cz.cas.lib.proarc.common.software.SoftwareRepository;
import cz.cas.lib.proarc.common.software.SoftwareUtils;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
import cz.cas.lib.proarc.webapp.server.ServerMessages;
import cz.cas.lib.proarc.webapp.server.rest.RestException;
import cz.cas.lib.proarc.webapp.server.rest.SessionContext;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.shared.rest.SoftwareResourceApi;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;
import javax.xml.transform.stream.StreamSource;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_SOFTWARE_IN_USE;
import static cz.cas.lib.proarc.webapp.server.rest.UserPermission.checkPermission;

/**
 * Resource to manage softwares producing digital objects.
 *
 * @author Lukas Sykora
 */
@Deprecated
@Path(RestConfig.URL_API_VERSION_1 + "/" + SoftwareResourceApi.PATH)
public class SoftwareResourceV1 {

    private static final Logger LOG = Logger.getLogger(SoftwareResourceV1.class.getName());
    private final AppConfiguration appConfig;
    private final AkubraConfiguration akubraConfiguration;
    private final Request httpRequest;
    private final HttpHeaders httpHeaders;
    private final SessionContext session;
    private final SoftwareRepository devRepo;
    protected final UserProfile user;

    public SoftwareResourceV1(
            @Context Request request,
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest
            ) throws AppConfigurationException, IOException {

        this.httpRequest = request;
        this.httpHeaders = httpHeaders;
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        this.session = SessionContext.from(httpRequest);
        this.user = session.getUser();
        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            this.devRepo = new SoftwareRepository(FedoraStorage.getInstance(appConfig));
            this.akubraConfiguration = null;
        } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            this.akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfig.getConfigHome());
            this.devRepo = new SoftwareRepository(AkubraStorage.getInstance(akubraConfiguration));
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }
    }

    @DELETE
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Software> deleteSoftware(
            @QueryParam(SoftwareResourceApi.SOFTWARE_ITEM_ID) String id
            ) throws SoftwareException {

        checkPermission(user, UserRole.PERMISSION_FUNCTION_DEVICE);

        try {
            boolean deleted = devRepo.deleteSoftware(id, session.asFedoraLog());
            if (!deleted) {
                Locale locale = session.getLocale(httpHeaders);
//                throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
                return SmartGwtResponse.asError(returnLocalizedMessage(ERR_SOFTWARE_IN_USE));
            }
            Software software = new Software();
            software.setId(id);
            return new SmartGwtResponse<Software>(software);
        } catch (SoftwareNotFoundException ex) {
//            LOG.log(Level.SEVERE, id, ex);
            throw RestException.plainNotFound(SoftwareResourceApi.SOFTWARE_ITEM_ID, id);
        }
    }

    /**
     * Gets list of softwares.
     *
     * @param id software ID. If omitted all software are returned but without description.
     * @return list of softwares
     */
    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Software> getSoftwares(
            @QueryParam(SoftwareResourceApi.SOFTWARE_ITEM_ID) String id,
            @QueryParam(SoftwareResourceApi.SOFTWARE_ITEM_MODEL) String model,
            @QueryParam(SoftwareResourceApi.SOFTWARE_START_ROW_PARAM) int startRow
            ) throws SoftwareException, IOException {

        int total = 0;
        boolean fetchDescription = id != null && !id.isEmpty();
        List<Software> result = new ArrayList<>();

        if (id == null && model == null) {
            total = devRepo.findAllSoftware(appConfig, 0).size();
            result = devRepo.findAllSoftware(appConfig, startRow);
        } else {
            result = devRepo.find(null, id, model, fetchDescription, startRow);
            total = result.size();
        }
        int endRow = startRow + result.size() - 1;
        return new SmartGwtResponse<Software>(SmartGwtResponse.STATUS_SUCCESS, startRow, endRow, total, result);
    }

    @GET
    @Path(SoftwareResourceApi.PATH_SET)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Software> getSoftwareSet() {
        try {
            return getSoftwares(null, SoftwareRepository.METAMODEL_SET_ID, 0);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    /**
     * Gets list of software with define preview.
     *
     * @param id software ID
     * @return list of softwares
     */
    @GET
    @Path(SoftwareResourceApi.PATH_PREVIEW)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Software> getSoftwarePreview(
            @QueryParam(SoftwareResourceApi.SOFTWARE_ITEM_ID) String id
            ) throws SoftwareException {
        if (id == null) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing id!");
        }
        List<Software> result = devRepo.find(id);
        int total = result.size();
        int startRow = 0;
        int endRow = startRow + result.size() - 1;
        return new SmartGwtResponse<Software>(SmartGwtResponse.STATUS_SUCCESS, startRow, endRow, total, result);
    }

    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Software> newSoftware(
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_LABEL) String label,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_MODEL) String model,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_MEMBERS) List<String> setOfIds,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_ADD_DEFAULT_METADATA) String type,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_DESCRIPTION) String description,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_TIMESTAMP) Long timestamp
            ) {

        checkPermission(user, UserRole.PERMISSION_FUNCTION_DEVICE);


        try {
            String owner = session.getUser().getUserName();
            Software software = devRepo.addSoftware(owner, model, label == null || label.isEmpty() ? "?" : label, session.asFedoraLog());
            boolean update = false;
            if ((description != null && !description.isEmpty())) {
                Mets mets = MetsUtils.unmarshalMets(new StreamSource(new StringReader(description)));
                mets = devRepo.fixMetsAccordingModel(model, mets);
                software.setDescription(mets);
                software.setTimestamp(timestamp);
                update = true;
            }
            if (type != null && !type.isEmpty()) {
                String defaultDescription = SoftwareUtils.createDefaultDescription(model, type);
                Mets defaultMets = MetsUtils.unmarshalMets(new StreamSource(new StringReader(defaultDescription)));
                defaultMets = devRepo.fixMetsAccordingModel(model, defaultMets);
                software.setDescription(defaultMets);
                software.setTimestamp(0L);
                update = true;
            }
            if (setOfIds != null && !setOfIds.isEmpty()) {
                setOfIds = devRepo.checkSetOfIds(setOfIds, software.getModel());
                software.setSetOfLinkedIds(setOfIds);
                update = true;
            }
            if (update) {
                software = devRepo.update(software, session.asFedoraLog());
            }
            return new SmartGwtResponse<Software>(software);
        } catch (SoftwareException ex) {

            throw new WebApplicationException(ex);
        }
    }

    @PUT
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Software> updateSoftware(
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_ID) String id,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_LABEL) String label,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_MODEL) String model,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_MEMBERS) List<String> setOfIds,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_DESCRIPTION) String description,
            @FormParam(SoftwareResourceApi.SOFTWARE_ITEM_TIMESTAMP) Long timestamp
            ) throws SoftwareException {

        checkPermission(user, UserRole.PERMISSION_FUNCTION_DEVICE);

        if (id == null || label == null || label.isEmpty() || model == null || model.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing software!");
        }
        Software update = new Software();
        update.setId(id);
        update.setLabel(label);
        update.setModel(model);
        update.setTimestamp(timestamp);
        if (description != null && !description.isEmpty()) {
            Mets mets = MetsUtils.unmarshalMets(new StreamSource(new StringReader(description)));
            mets = devRepo.fixMetsAccordingModel(model, mets);
            update.setDescription(mets);
        } else {
            update.setDescription(new Mets());
        }
        if (setOfIds != null && !setOfIds.isEmpty()) {
            setOfIds = devRepo.checkSetOfIds(setOfIds, update.getModel());
            update.setSetOfLinkedIds(setOfIds);
        } else {
            update.setSetOfLinkedIds(new ArrayList<>());
        }
        Software updated = devRepo.update(update, session.asFedoraLog());
        return new SmartGwtResponse<Software>(updated);
    }

    protected String returnLocalizedMessage(String key, Object... arguments) {
        Locale locale = session.getLocale(httpHeaders);
        ServerMessages msgs = ServerMessages.get(locale);
        return msgs.getFormattedMessage(key, arguments);
    }

}
