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
package cz.cas.lib.proarc.webapp.server.rest;

import com.sun.jersey.spi.CloseableService;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.export.DataStreamExport;
import cz.cas.lib.proarc.common.export.DesaExport;
import cz.cas.lib.proarc.common.export.DesaExport.Result;
import cz.cas.lib.proarc.common.export.ExportException;
import cz.cas.lib.proarc.common.export.Kramerius4Export;
import cz.cas.lib.proarc.common.export.mets.MetsExportException.MetsExportExceptionElement;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.webapp.shared.rest.ExportResourceApi;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.SecurityContext;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import org.apache.commons.io.FileUtils;

/**
 * REST resource to export data from the system.
 *
 * @author Jan Pokorsky
 */
@Path(ExportResourceApi.PATH)
public class ExportResource {

    private final AppConfiguration appConfig;
    private final UserProfile user;

    public ExportResource(
            @Context SecurityContext securityCtx,
            @Context HttpServletRequest httpRequest
            ) throws AppConfigurationException {

        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        SessionContext session = SessionContext.from(httpRequest);
        user = session.getUser();
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
    public SmartGwtResponse<ExportResult> newDesaExport(
            @FormParam(ExportResourceApi.DESA_PID_PARAM) List<String> pids,
            @FormParam(ExportResourceApi.DESA_HIERARCHY_PARAM) @DefaultValue("false") boolean hierarchy,
            @FormParam(ExportResourceApi.DESA_FORDOWNLOAD_PARAM) @DefaultValue("false") boolean forDownload,
            @FormParam(ExportResourceApi.DESA_DRYRUN_PARAM) @DefaultValue("false") boolean dryRun
            ) throws IOException, ExportException {

        if (pids.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing " + ExportResourceApi.DESA_PID_PARAM);
        }
        DesaExport export = new DesaExport(RemoteStorage.getInstance(appConfig),
                appConfig.getDesaServices(), MetaModelRepository.getInstance());
        URI exportUri = user.getExportFolder();
        File exportFolder = new File(exportUri);
        List<ExportResult> result = new ArrayList<ExportResult>(pids.size());
        if (forDownload) {
            Result r = export.exportDownload(exportFolder, pids.get(0));
            result.add(r.getValidationError() != null
                    ? new ExportResult(r.getValidationError().getExceptions())
                    : new ExportResult(r.getDownloadToken()));
        } else {
            if (dryRun) {
                for (String pid : pids) {
                    List<MetsExportExceptionElement> errors = export.validate(exportFolder, pid, hierarchy);
                    result.add(new ExportResult(errors));
                }
            } else {
                for (String pid : pids) {
                    Result r = export.export(exportFolder, pid, null, false, hierarchy, false);
                    if (r.getValidationError() != null) {
                        result.add(new ExportResult(r.getValidationError().getExceptions()));
                    } else {
                        URI targetPath = user.getUserHomeUri().relativize(r.getTargetFolder().toURI());
                        result.add(new ExportResult((Integer) null, targetPath.toASCIIString()));
                    }
                }
            }
        }
        return new SmartGwtResponse<ExportResult>(result);
    }

    /**
     * Gets the exported package built by {@link #newDesaExport() } with {@code forDownload=true}.
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
            public void close() throws IOException {
                FileUtils.deleteQuietly(file.getParentFile());
            }
        });
        return Response.ok(file, MediaType.APPLICATION_OCTET_STREAM)
                .header("Content-Disposition", "attachment; filename=" + file.getName())
                .build();
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

        public ExportResult() {
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
                errors = new ArrayList<ExportError>();
                for (MetsExportExceptionElement me : validations) {
                    errors.add(new ExportError(me));
                }
            }
        }

        public Integer getExportId() {
            return exportId;
        }

        public String getTarget() {
            return target;
        }

        public List<ExportError> getErrors() {
            return errors;
        }

        public void setExportId(Integer exportId) {
            this.exportId = exportId;
        }

        public void setTarget(String target) {
            this.target = target;
        }

        public void setErrors(List<ExportError> errors) {
            this.errors = errors;
        }

        public String getToken() {
            return token;
        }

        public void setToken(String token) {
            this.token = token;
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

        public ExportError(MetsExportExceptionElement me) {
            this.pid = me.getPid();
            this.message = me.getMessage();
            this.warning = me.isWarning();
            Exception ex = me.getEx();
            if (ex != null) {
                StringWriter sw = new StringWriter();
                PrintWriter pw = new PrintWriter(sw);
                ex.printStackTrace(pw);
                pw.close();
                this.log = sw.toString();
            }
        }

    }

}
