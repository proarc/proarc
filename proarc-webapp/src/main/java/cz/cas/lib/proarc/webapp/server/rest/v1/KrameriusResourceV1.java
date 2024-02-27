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
package cz.cas.lib.proarc.webapp.server.rest.v1;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.storage.DigitalObjectConcurrentModificationException;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.storage.DigitalObjectValidationException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.StringEditor;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.kramerius.K7Authenticator;
import cz.cas.lib.proarc.common.kramerius.K7Downloader;
import cz.cas.lib.proarc.common.kramerius.KDataHandler;
import cz.cas.lib.proarc.common.kramerius.KImporter;
import cz.cas.lib.proarc.common.kramerius.KUtils;
import cz.cas.lib.proarc.common.kramerius.KrameriusOptions;
import cz.cas.lib.proarc.common.mods.custom.IdentifierMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkPageMapper;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.server.ServerMessages;
import cz.cas.lib.proarc.webapp.server.rest.RestException;
import cz.cas.lib.proarc.webapp.server.rest.SessionContext;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.KrameriusResourceApi;
import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.Locale;
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
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicHeader;
import org.fcrepo.utilities.FileUtils;

import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_FAILED_V5;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_FAILED_V7;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_FINISHED_V5;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_FINISHED_V7;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_KILLED_V7;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_NO_BATCH_V5;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_FAILED;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_FINISHED;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_WARNING;
import static cz.cas.lib.proarc.common.kramerius.KUtils.findHandler;
import static cz.cas.lib.proarc.common.kramerius.KUtils.getExpectedDestinationPath;
import static cz.cas.lib.proarc.common.kramerius.KUtils.getExpectedSourcePath;
import static cz.cas.lib.proarc.common.kramerius.KUtils.transformKrameriusModel;
import static cz.cas.lib.proarc.common.kramerius.KrameriusOptions.KRAMERIUS_INSTANCE_LOCAL;
import static cz.cas.lib.proarc.common.kramerius.KrameriusOptions.findKrameriusInstance;
import static cz.cas.lib.proarc.webapp.server.rest.v1.DigitalObjectResourceV1.toValidationError;
import static java.net.HttpURLConnection.HTTP_OK;

@Deprecated
@Path(RestConfig.URL_API_VERSION_1 + "/" + KrameriusResourceApi.PATH)
public class KrameriusResourceV1 {

    private static final Logger LOG = Logger.getLogger(KrameriusResourceV1.class.getName());

    private final AppConfiguration appConfig;
    private final BatchManager batchManager;
    private final HttpHeaders httpHeaders;
    private final UserProfile user;
    private final SessionContext session;

    public KrameriusResourceV1(
            @Context Request request,
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest
    ) throws AppConfigurationException {

        this.httpHeaders = httpHeaders;
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        this.batchManager = BatchManager.getInstance(appConfig);
        session = SessionContext.from(httpRequest);
        user = session.getUser();
        LOG.fine(user.toString());
    }

    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<String> connectionTest() {
        LOG.fine("Succesfull attempt to connected to ProArc");
        return new SmartGwtResponse<>("Connected to ProArc");
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

        LOG.fine(String.format("pid: %s, krameriusInstanceId: %s", pid, krameriusInstanceId));
        Locale locale = session.getLocale(httpHeaders);

        if (pid == null || pid.isEmpty()) {
            throw RestException.plainText(Response.Status.BAD_REQUEST, ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Missing_Value", KrameriusResourceApi.KRAMERIUS_OBJECT_PID));
        }
        if (krameriusInstanceId == null || krameriusInstanceId.isEmpty()) {
            return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Missing_Value", KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        if (KRAMERIUS_INSTANCE_LOCAL.equals(krameriusInstanceId)) {
            return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Unsupported_Value", KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }

        try {
            DigitalObjectHandler handler = findHandler(pid, krameriusInstanceId);
            MetadataHandler<?> metadataHandler = handler.metadata();
            String model = handler.getModel().getPid();
            if (NdkPlugin.MODEL_PAGE.equals(model) || NdkPlugin.MODEL_NDK_PAGE.equals(model) || OldPrintPlugin.MODEL_PAGE.equals(model)) {
                DescriptionMetadata<Object> metadata = handler.metadata().getMetadataAsJsonObject(null);
                if (((NdkPageMapper.Page)metadata.getData()).getIdentifiers().isEmpty()) {
                    ((NdkPageMapper.Page)metadata.getData()).getIdentifiers().add(new IdentifierMapper.IdentifierItem("uuid", pid.substring(5)));
                }
                String title = ((NdkPageMapper.Page) metadata.getData()).getType();
                if (title != null && !title.isEmpty()) {
                    title = title.substring(0, 1).toLowerCase() + title.substring(1);
                    ((NdkPageMapper.Page) metadata.getData()).setType(title);
                }
                DescriptionMetadata<String> metadataAsXml = metadataHandler.getMetadataAsXml();
                metadata.setContent(metadataAsXml.getData());
                metadata.setKrameriusInstanceId(krameriusInstanceId);
                metadata.setModel(transformKrameriusModel(appConfig, handler.getModel().getPid()));
                return new SmartGwtResponse<DescriptionMetadata<Object>>(metadata);
            } else {
                DescriptionMetadata<String> metadataAsXml = metadataHandler.getMetadataAsXml();
                StringEditor.StringRecord result = new StringEditor.StringRecord(metadataAsXml.getData(), metadataAsXml.getTimestamp(), metadataAsXml.getPid());
                result.setKrameriusInstanceId(krameriusInstanceId);
                result.setModel(transformKrameriusModel(appConfig, handler.getModel().getPid()));
                return result;
            }
        } catch (DigitalObjectNotFoundException ex) {
            if (rerun) {
                KrameriusOptions.KrameriusInstance instance = findKrameriusInstance(appConfig.getKrameriusOptions().getKrameriusInstances(), krameriusInstanceId);
                if (instance == null) {
                    StringEditor.StringRecord result = new StringEditor.StringRecord();
                    result.setStatus(StringEditor.StringRecord.STATUS_FAILURE);
                    result.setData(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Unknown_Value", krameriusInstanceId, KrameriusResourceApi.KRAMERIUS_INSTANCE));
                    return result;
                } else {
                    try {
                        LOG.fine(String.format("Downloading pid: %s, krameriusInstanceId: %s", pid, instance.getId()));

                        K7Authenticator authenticator = new K7Authenticator(instance);
                        String token = authenticator.authenticate();
                        if (token != null || !token.isEmpty()) {
                            K7Downloader downloader = new K7Downloader(appConfig, instance);
                            String foxml = downloader.downloadFromK7(pid, token);
                            downloader.saveFoxml(foxml, pid);
                        }

                        return viewMods(pid, krameriusInstanceId, false);
                    } catch (Exception e) {
                        LOG.severe("Error in downloading object " + pid);
                        e.printStackTrace();
                        StringEditor.StringRecord result = new StringEditor.StringRecord();
                        result.setStatus(StringEditor.StringRecord.STATUS_FAILURE);
                        result.setData(e.getMessage());
                        return result;
                    }
                }
            } else {
                LOG.severe("Error in getting object " + pid);
                ex.printStackTrace();
                StringEditor.StringRecord result = new StringEditor.StringRecord();
                result.setStatus(StringEditor.StringRecord.STATUS_FAILURE);
                result.setData(ex.getMessage());
                return result;
            }
        } catch (Exception ex) {
            LOG.severe("Error in getting object " + pid);
            ex.printStackTrace();
            StringEditor.StringRecord result = new StringEditor.StringRecord();
            result.setStatus(StringEditor.StringRecord.STATUS_FAILURE);
            result.setData(ex.getMessage());
            return result;
        }
    }

    @GET
    @Path(KrameriusResourceApi.VIEW_IMAGE)
    @Produces("*/*")
    public Object viewImage(
            @QueryParam(KrameriusResourceApi.KRAMERIUS_OBJECT_PID) String pid,
            @QueryParam(KrameriusResourceApi.KRAMERIUS_INSTANCE) String krameriusInstanceId
    ) {
        Locale locale = session.getLocale(httpHeaders);
        if (pid == null || pid.isEmpty()) {
            throw RestException.plainText(Response.Status.BAD_REQUEST, ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Missing_Value", KrameriusResourceApi.KRAMERIUS_OBJECT_PID));
        }
        if (krameriusInstanceId == null || krameriusInstanceId.isEmpty()) {
            return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Missing_Value", KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        if (KRAMERIUS_INSTANCE_LOCAL.equals(krameriusInstanceId)) {
            return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Unsupported_Value", KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }

        KrameriusOptions.KrameriusInstance instance = findKrameriusInstance(appConfig.getKrameriusOptions().getKrameriusInstances(), krameriusInstanceId);
        if (instance == null) {
            return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Unknown_Value", krameriusInstanceId, KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }

        KUtils.RedirectedResult result = new KUtils.RedirectedResult(pid);
        try {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            ProArcObject proArcObject = dom.find2(pid, null, krameriusInstanceId);
            RelationEditor relationEditor = new RelationEditor(proArcObject);
            if (relationEditor.getModel() == null || relationEditor.getModel().isEmpty()) {
                result.setStatus("Failed");
                result.setMessage(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_ImpossibleToFindModel", pid));
                return new SmartGwtResponse<>(result);
            }
            if (NdkPlugin.MODEL_PAGE.equals(relationEditor.getModel()) || NdkPlugin.MODEL_NDK_PAGE.equals(relationEditor.getModel()) || OldPrintPlugin.MODEL_PAGE.equals(relationEditor.getModel())) {
                K7Authenticator authenticator = new K7Authenticator(instance);
                String token = authenticator.authenticate();
                String url = instance.getUrl() + instance.getUrlImage() + pid + "/full/max/0/default.jpg";
                LOG.info("Redirected to " + url);
//                result.setStatus("Successful");
//                result.setMessage("Redirected to ProArc Image View");
//                result.setUrl(url);
                HttpClient httpClient = HttpClients.createDefault();
                HttpGet httpGet = new HttpGet(url);

                httpGet.setHeader(new BasicHeader("Keep-Alive", "timeout=600, max=1000"));
                if (token != null && !token.isEmpty()) {
                    httpGet.setHeader(new BasicHeader("Authorization", "Bearer " + token));
                }
                httpGet.setHeader(new BasicHeader("Connection", "Keep-Alive, Upgrade"));

                HttpResponse response = httpClient.execute(httpGet);
                if (HTTP_OK == response.getStatusLine().getStatusCode()) {
                    Response proarcResponse = Response.ok(response.getEntity().getContent(), response.getFirstHeader("Content-Type").getValue()).header("Content-Disposition", "default.jpg").build();
                    return proarcResponse;
                } else {
                    result.setStatus("Failed");
                    result.setMessage(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_ImpossibleToDownloadJpg", pid));
                    return new SmartGwtResponse<>(result);
                }
            } else {
                result.setStatus("Failed");
                result.setMessage(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_NoJpg4Object", pid));
                result.setUrl(null);
                return new SmartGwtResponse<>(result);
            }
        } catch (Exception ex) {
            LOG.severe("Error in getting object " + pid);
            ex.printStackTrace();
            result.setStatus("Failed");
            result.setUrl(null);
            result.setMessage(ex.getMessage());
            return new SmartGwtResponse<>(result);
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
    ) throws DigitalObjectException {

        LOG.fine(String.format("pid: %s, krameriusInstanceId: %s, editor: %s, timestamp: %s, ignoreValidation: %s, json: %s, xml: %s",
                pid, krameriusInstanceId, editorId, timestamp, ignoreValidation, jsonData, xmlData));

        Locale locale = session.getLocale(httpHeaders);
        if (pid == null || pid.isEmpty()) {
            throw RestException.plainText(Response.Status.BAD_REQUEST, ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Missing_Value", KrameriusResourceApi.KRAMERIUS_OBJECT_PID));
        }
        if (krameriusInstanceId == null || krameriusInstanceId.isEmpty()) {
            return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Missing_Value", KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        if (KRAMERIUS_INSTANCE_LOCAL.equals(krameriusInstanceId)) {
            return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Unsupported_Value", KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }


        if (timestamp == null) {
            return SmartGwtResponse.asError(DigitalObjectResourceApi.TIMESTAMP_PARAM, pid);
        }
        final boolean isJsonData = xmlData == null;
        String data = isJsonData ? jsonData : xmlData;
        DigitalObjectHandler doHandler;
        MetadataHandler<?> mHandler;
        try {
            doHandler = findHandler(pid, krameriusInstanceId);
            mHandler = doHandler.metadata();
            DescriptionMetadata<String> dMetadata = new DescriptionMetadata<>();
            dMetadata.setPid(pid);
            dMetadata.setKrameriusInstanceId(krameriusInstanceId);
            dMetadata.setEditor(editorId);
            dMetadata.setData(data);
            dMetadata.setTimestamp(timestamp);
            dMetadata.setStandard(standard);
            dMetadata.setIgnoreValidation(ignoreValidation);
            if (isJsonData) {
                mHandler.setMetadataAsJson(dMetadata, session.asFedoraLog(), NdkMetadataHandler.OPERATION_UPDATE);
            } else {
                mHandler.setMetadataAsXml(dMetadata, session.asFedoraLog(), NdkMetadataHandler.OPERATION_UPDATE);
            }
        } catch (DigitalObjectValidationException ex) {
            return toValidationError(ex, session.getLocale(httpHeaders));
        } catch (DigitalObjectNotFoundException ex) {
            return SmartGwtResponse.asError(ex);
        }
//        DigitalObjectStatusUtils.setState(doHandler.getFedoraObject(), STATUS_PROCESSING);
        doHandler.commit();
        return new SmartGwtResponse<>(mHandler.getMetadataAsJsonObject(editorId));
    }

    @POST
    @Path(KrameriusResourceApi.IMPORT_2_PROARC)
    public SmartGwtResponse<KUtils.ImportResult> import2ProArc(
        @FormParam(KrameriusResourceApi.KRAMERIUS_OBJECT_PID) String pid,
        @FormParam(KrameriusResourceApi.KRAMERIUS_INSTANCE) String krameriusInstanceId
    ) {

        LOG.fine(String.format("pid: %s, krameriusInstanceId: %s", pid, krameriusInstanceId));

        Locale locale = session.getLocale(httpHeaders);
        if (pid == null || pid.isEmpty()) {
            throw RestException.plainText(Response.Status.BAD_REQUEST, ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Missing_Value", KrameriusResourceApi.KRAMERIUS_OBJECT_PID));
        }
        if (krameriusInstanceId == null || krameriusInstanceId.isEmpty()) {
            return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Missing_Value", KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        if (KRAMERIUS_INSTANCE_LOCAL.equals(krameriusInstanceId)) {
            return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Unsupported_Value", KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }


        KUtils.ImportResult importResult = new KUtils.ImportResult(pid, "ProArc");

        BatchParams params = new BatchParams(Collections.singletonList(pid), krameriusInstanceId);
        Batch batch = BatchUtils.addNewUploadBatch(this.batchManager, pid, user, Batch.UPLOAD_PROARC, params);

        try {
            KDataHandler dataHandler = new KDataHandler(appConfig);
            DescriptionMetadata<String> metadata = dataHandler.getDescriptionMetadata(pid, krameriusInstanceId);
            if (metadata == null) {
                BatchUtils.finishedUploadWithError(this.batchManager, batch, "ProArc", new IOException("No metadata content for this object with pid: " + pid));
                return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_NoMetadata4Object", pid));
            }
            boolean status = dataHandler.setDescriptionMetadataToProArc(pid, metadata, krameriusInstanceId);
            if (status) {
                importResult.setStatus("Successful");
                importResult.setReason(null);
                BatchUtils.finishedUploadSuccessfully(this.batchManager, batch, "ProArc");
            } else {
                importResult.setStatus("Failed");
                importResult.setReason("Reason not known.");
                BatchUtils.finishedUploadWithError(this.batchManager, batch, "ProArc", new IOException(importResult.getReason()));
            }
        } catch (DigitalObjectException ex) {
            BatchUtils.finishedUploadWithError(this.batchManager, batch, "ProArc", ex);
            if (ex instanceof DigitalObjectNotFoundException) {
                LOG.severe("Import (" + pid + ") to ProArc failed, because Object with this pid not found in storage.");
                importResult.setReason(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_ObjectNotFound", pid));
            } else if (ex instanceof DigitalObjectConcurrentModificationException) {
                LOG.severe("Concurrent modification for object with " + pid + ".");
                importResult.setReason(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_ConcurrentModification", pid));
            } else {
                if (ex.getMessage().contains("RELS-EXT, lastModified: -1")) {
                    importResult.setReason(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_ObjectNotFound", pid));
                    importResult.setReason(ServerMessages.get(locale).KrameriusResource_Upload_Proarc_Msg());
                } else {
                    LOG.severe("Import (" + pid + ") to ProArc failed, because " + ex.getMessage());
                    importResult.setReason(ex.getMessage());
                }
            }
            importResult.setStatus("Failed");
        }
        return new SmartGwtResponse<>(importResult);
    }

    @POST
    @Path(KrameriusResourceApi.IMPORT_2_KRAMERIUS)
    public SmartGwtResponse<KUtils.ImportResult> import2Kramerius(
            @FormParam(KrameriusResourceApi.KRAMERIUS_OBJECT_PID) String pid,
            @FormParam(KrameriusResourceApi.KRAMERIUS_INSTANCE) String krameriusInstanceId,
            @FormParam(KrameriusResourceApi.KRAMERIUS_IMPORT_INSTANCE) String krameriusImportInstanceId
    ) {

        LOG.fine(String.format("pid: %s, krameriusInstanceId: %s krameriusImportInstanceId: %s", pid, krameriusInstanceId, krameriusImportInstanceId));

        Locale locale = session.getLocale(httpHeaders);
        if (pid == null || pid.isEmpty()) {
            throw RestException.plainText(Response.Status.BAD_REQUEST, ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Missing_Value", KrameriusResourceApi.KRAMERIUS_OBJECT_PID));
        }
        if (krameriusInstanceId == null || krameriusInstanceId.isEmpty()) {
            return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Missing_Value", KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        if (KRAMERIUS_INSTANCE_LOCAL.equals(krameriusInstanceId)) {
            return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Unsupported_Value", KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        if (krameriusImportInstanceId == null || krameriusImportInstanceId.isEmpty()) {
            return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Missing_Value", KrameriusResourceApi.KRAMERIUS_IMPORT_INSTANCE));
        }
        if (KRAMERIUS_INSTANCE_LOCAL.equals(krameriusImportInstanceId)) {
            return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Unsupported_Value", KrameriusResourceApi.KRAMERIUS_IMPORT_INSTANCE));
        }

        BatchParams params = new BatchParams(Collections.singletonList(pid), krameriusInstanceId, krameriusImportInstanceId);
        Batch batch = BatchUtils.addNewUploadBatch(this.batchManager, pid, user, Batch.UPLOAD_KRAMERIUS, params);

        KUtils.ImportResult importResult = new KUtils.ImportResult(pid, krameriusImportInstanceId);

        KrameriusOptions.KrameriusInstance instance = findKrameriusInstance(appConfig.getKrameriusOptions().getKrameriusInstances(), krameriusImportInstanceId);
        if (instance == null) {
            return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_Unknown_Value", krameriusInstanceId, KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }
        if (!instance.isTestType() && !user.hasPermissionToImportToProdFunction()) {
            return SmartGwtResponse.asError(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_No_Permission", krameriusInstanceId, KrameriusResourceApi.KRAMERIUS_INSTANCE));
        }

        KDataHandler dataHandler = new KDataHandler(appConfig);
        try {
            File sourceFile = dataHandler.getSourceFile(pid, krameriusInstanceId);
            if (sourceFile == null || !sourceFile.exists()) {
                throw new IOException(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_SourceFileDooesntExists", pid, getExpectedSourcePath(appConfig, krameriusInstanceId, pid)));
            }
            File destinationFile = dataHandler.getDestinationFile(pid, instance);
            if (destinationFile == null || !destinationFile.exists()) {
                throw new IOException(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_DestinationFileDooesntExists", pid, getExpectedDestinationPath(instance, pid)));
            }
            if (!FileUtils.copy(sourceFile, destinationFile)) {
                throw new IOException(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_CantCopyContent", sourceFile.getAbsolutePath(), destinationFile.getAbsolutePath()));
            }
            KImporter kImporter = new KImporter(appConfig, instance);
            KUtils.ImportState state = kImporter.importToKramerius(destinationFile.getParentFile(), true, KUtils.EXPORT_KRAMERIUS, params.getPolicy());
            if (KRAMERIUS_PROCESS_FINISHED.equals(state.getProcessState()) && (KRAMERIUS_BATCH_FINISHED_V5.equals(state.getBatchState()) || KRAMERIUS_BATCH_FINISHED_V7.equals(state.getBatchState()))) {
                if (instance.deleteAfterImport()) {
                    MetsUtils.deleteFolder(destinationFile.getParentFile());
                }
            }
            switch (state.getBatchState()) {
                case KRAMERIUS_BATCH_FINISHED_V5:
                case KRAMERIUS_BATCH_FINISHED_V7:
                    importResult.setStatus("Successful");
                    BatchUtils.finishedUploadSuccessfully(this.batchManager, batch, instance.getUrl());
                    break;
                case KRAMERIUS_BATCH_FAILED_V5:
                case KRAMERIUS_BATCH_FAILED_V7:
                case KRAMERIUS_BATCH_KILLED_V7:
                    importResult.setStatus("Failed");
                    importResult.setReason(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_ImportKrameriuFailed_status", instance.getId(), instance.getUrl()));
                    BatchUtils.finishedUploadWithError(this.batchManager, batch, instance.getUrl(), new IOException("Import selhal."));
                    break;
                case KRAMERIUS_BATCH_NO_BATCH_V5:
                    switch (state.getProcessState()) {
                        case KRAMERIUS_PROCESS_FINISHED:
                            importResult.setStatus("Failed");
                            importResult.setReason(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_ImportKrameriuFailed_status", instance.getId(), instance.getUrl()));
                            BatchUtils.finishedUploadWithError(this.batchManager, batch, instance.getUrl(), new IOException("Import selhal."));
                            break;
                        case KRAMERIUS_PROCESS_FAILED:
                            importResult.setStatus("Failed");
                            importResult.setReason(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_ImportKrameriuFailed_status", instance.getId(), instance.getUrl()));
                            BatchUtils.finishedUploadWithError(this.batchManager, batch, instance.getUrl(), new IOException("Import selhal."));
                            break;
                        case KRAMERIUS_PROCESS_WARNING:
                            importResult.setStatus("Failed");
                            importResult.setReason(ServerMessages.get(locale).getFormattedMessage("KrameriusResource_ImportKrameriuWarning_status", instance.getId(), instance.getUrl()));
                            BatchUtils.finishedUploadWithError(this.batchManager, batch, instance.getUrl(), new IOException("Import pro3el s chybou."));
                            break;
                    }
                    break;
                default:
                    importResult.setStatus("Failed");
                    importResult.setReason("Unknown status: " + state.getBatchState());
                    BatchUtils.finishedExportWithError(this.batchManager, batch, instance.getUrl(), new IOException("Import selhal - neznamy status: "+ state.getBatchState()));
                    break;
            }
        } catch (Exception ex) {
            LOG.severe(ex.getMessage());
            importResult.setStatus("Failed");
            importResult.setReason(ex.getMessage());
            BatchUtils.finishedUploadWithError(this.batchManager, batch, instance.getUrl(), ex);
        }
        return new SmartGwtResponse<>(importResult);
    }

    protected String returnLocalizedMessage(String key, Object... arguments) {
        Locale locale = session.getLocale(httpHeaders);
        ServerMessages msgs = ServerMessages.get(locale);
        return msgs.getFormattedMessage(key, arguments);
    }
}
