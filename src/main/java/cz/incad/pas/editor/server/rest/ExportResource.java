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
package cz.incad.pas.editor.server.rest;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.export.DataStreamExport;
import cz.cas.lib.proarc.common.export.ExportException;
import cz.cas.lib.proarc.common.export.Kramerius4Export;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import cz.incad.pas.editor.shared.rest.ExportResourceApi;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.security.Principal;
import java.util.List;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.FormParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.SecurityContext;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * REST resource to export data from the system.
 *
 * @author Jan Pokorsky
 */
@Path(ExportResourceApi.PATH)
public class ExportResource {

    private final AppConfiguration appConfig;
    private final UserManager userManager;
    private final UserProfile user;

    public ExportResource(
            @Context SecurityContext securityCtx
            ) throws AppConfigurationException {
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        this.userManager = UserUtil.getDefaultManger();

        Principal userPrincipal = securityCtx.getUserPrincipal();
        String userName;
        if (userPrincipal != null) {
            userName = userPrincipal.getName();
        } else {
            userName = UserManager.GUEST_ID;
        }
        user = userManager.find(userName);
    }

    @POST
    @Path(ExportResourceApi.DATASTREAM_PATH)
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
        DataStreamExport export = new DataStreamExport(RemoteStorage.getInstance(appConfig));
        URI exportUri = user.getExportFolder();
        File exportFolder = new File(exportUri);
        File target = export.export(exportFolder, hierarchy, pids, dsIds);
        URI targetPath = user.getUserHomeUri().relativize(target.toURI());
        return new SmartGwtResponse<ExportResult>(new ExportResult(targetPath.toASCIIString()));
    }

    @POST
    @Path(ExportResourceApi.KRAMERIUS4_PATH)
    public SmartGwtResponse<ExportResult> kramerius4(
            @FormParam(ExportResourceApi.KRAMERIUS4_PID_PARAM) List<String> pids,
            @FormParam(ExportResourceApi.KRAMERIUS4_HIERARCHY_PARAM) @DefaultValue("true") boolean hierarchy
            ) throws IOException {

        if (pids.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing " + ExportResourceApi.KRAMERIUS4_PID_PARAM);
        }
        Kramerius4Export export = new Kramerius4Export(
                RemoteStorage.getInstance(appConfig), appConfig.getKramerius4Export());
        URI exportUri = user.getExportFolder();
        File exportFolder = new File(exportUri);
        File target = export.export(exportFolder, hierarchy, pids.toArray(new String[pids.size()]));
        URI targetPath = user.getUserHomeUri().relativize(target.toURI());
        return new SmartGwtResponse<ExportResult>(new ExportResult(targetPath.toASCIIString()));
    }

    /**
     * Export result.
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class ExportResult {

        /**
         * The target folder path.
         */
        @XmlElement(name = ExportResourceApi.RESULT_TARGET)
        private String target;

        public ExportResult() {
        }

        public ExportResult(String target) {
            this.target = target;
        }

    }
}
