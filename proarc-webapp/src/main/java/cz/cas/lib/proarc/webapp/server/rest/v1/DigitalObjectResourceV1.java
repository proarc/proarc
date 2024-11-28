/*
 * Copyright (C) 2011 Jan Pokorsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.server.rest.v1;

import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import cz.cas.lib.proarc.common.actions.AddReference;
import cz.cas.lib.proarc.common.actions.CatalogRecord;
import cz.cas.lib.proarc.common.actions.ChangeModels;
import cz.cas.lib.proarc.common.actions.CopyObject;
import cz.cas.lib.proarc.common.actions.LockObject;
import cz.cas.lib.proarc.common.actions.ReindexDigitalObjects;
import cz.cas.lib.proarc.common.actions.UpdateObjects;
import cz.cas.lib.proarc.common.actions.UpdatePages;
import cz.cas.lib.proarc.common.actions.UpdatePagesMetadata;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.mods.AuthorityMetadataInjector;
import cz.cas.lib.proarc.common.mods.MetadataInjector;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.DigitalObjectExistException;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.DigitalObjectManager.CreateHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager.CreateHierarchyHandler;
import cz.cas.lib.proarc.common.object.DisseminationHandler;
import cz.cas.lib.proarc.common.object.DisseminationInput;
import cz.cas.lib.proarc.common.object.K4Plugin;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.collectionOfClippings.CollectionOfClippingsPlugin;
import cz.cas.lib.proarc.common.object.emods.BornDigitalModsPlugin;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.object.technicalMetadata.TechnicalMetadataMapper;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.BatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.process.InternalExternalDispatcher;
import cz.cas.lib.proarc.common.process.InternalExternalProcess;
import cz.cas.lib.proarc.common.process.export.ExportUtils;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.storage.AesEditor;
import cz.cas.lib.proarc.common.storage.AtmEditor;
import cz.cas.lib.proarc.common.storage.AtmEditor.AtmItem;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.CodingHistoryEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.storage.DigitalObjectValidationException;
import cz.cas.lib.proarc.common.storage.DigitalObjectValidationException.ValidationResult;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.storage.MixEditor;
import cz.cas.lib.proarc.common.storage.PremisEditor;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.SearchViewQuery;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.StringEditor;
import cz.cas.lib.proarc.common.storage.StringEditor.StringRecord;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage.AkubraObject;
import cz.cas.lib.proarc.common.storage.akubra.PurgeAkubraObject;
import cz.cas.lib.proarc.common.storage.akubra.SolrSearchView;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage.RemoteObject;
import cz.cas.lib.proarc.common.storage.fedora.PurgeFedoraObject;
import cz.cas.lib.proarc.common.storage.fedora.PurgeFedoraObject.PurgeException;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.urnnbn.UrnNbnConfiguration;
import cz.cas.lib.proarc.common.urnnbn.UrnNbnConfiguration.ResolverConfiguration;
import cz.cas.lib.proarc.common.urnnbn.UrnNbnService;
import cz.cas.lib.proarc.common.urnnbn.UrnNbnStatusHandler;
import cz.cas.lib.proarc.common.urnnbn.UrnNbnStatusHandler.PidResult;
import cz.cas.lib.proarc.common.urnnbn.UrnNbnStatusHandler.StatusEntry;
import cz.cas.lib.proarc.common.user.Group;
import cz.cas.lib.proarc.common.user.Permissions;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import cz.cas.lib.proarc.common.workflow.WorkflowActionHandler;
import cz.cas.lib.proarc.common.workflow.WorkflowException;
import cz.cas.lib.proarc.common.workflow.WorkflowManager;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.MaterialFilter;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.MaterialView;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.model.TaskFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskView;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import cz.cas.lib.proarc.urnnbn.ResolverUtils;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
import cz.cas.lib.proarc.webapp.server.ServerMessages;
import cz.cas.lib.proarc.webapp.server.rest.AnnotatedMetaModel;
import cz.cas.lib.proarc.webapp.server.rest.LocalDateParam;
import cz.cas.lib.proarc.webapp.server.rest.ProArcRequest;
import cz.cas.lib.proarc.webapp.server.rest.RestException;
import cz.cas.lib.proarc.webapp.server.rest.SessionContext;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse.ErrorBuilder;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi.SearchSort;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi.SearchType;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.UserResourceApi;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.MissingResourceException;
import java.util.Set;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.DefaultValue;
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
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.transform.stream.StreamSource;
import org.apache.commons.io.FileUtils;
import org.codehaus.jettison.json.JSONException;
import org.glassfish.jersey.media.multipart.FormDataBodyPart;
import org.glassfish.jersey.media.multipart.FormDataContentDisposition;
import org.glassfish.jersey.media.multipart.FormDataParam;

import static cz.cas.lib.proarc.common.process.export.mets.MetsContext.buildAkubraContext;
import static cz.cas.lib.proarc.common.process.export.mets.MetsContext.buildFedoraContext;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_ADDING_REFERENCE_FAILED;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_CHANGING_MODEL_FAILED;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_IN_GETTING_CHILDREN;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_IS_LOCKED;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_MISSING_PARAMETER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_UNLOCKING_OBJECT_FAILED;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.STATUS_DONT_BE_IGNORED;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.STATUS_LOCKED;
import static cz.cas.lib.proarc.webapp.server.rest.UserPermission.checkPermission;

/**
 * Resource to manage digital objects.
 * <p>
 * /object/{pid}/ GET - read DigObjDesc:{pid, displayname, date, owner};
 * /object/ GET - lists all DigObjDesc
 * /object/{pid}/foxml
 * /object/{pid}/scan
 * /object/{pid}/preview
 * /object/{pid}/thumb
 * /object/{pid}/ocr
 * /object/{pid}/metadata
 * /object/{pid}/relations
 * /object/metamodel/ GET - lists model:{pid, displayname, type:(TOP|LEAF)}
 *
 * @author Jan Pokorsky
 */
@Deprecated
@Path(RestConfig.URL_API_VERSION_1 + "/" + DigitalObjectResourceApi.PATH)
public class DigitalObjectResourceV1 {

    private static final Logger LOG = Logger.getLogger(DigitalObjectResourceV1.class.getName());

    private final AppConfiguration appConfig;
    private final AkubraConfiguration akubraConfiguration;
    private final MetaModelRepository metamodels = MetaModelRepository.getInstance();
    private final BatchManager importManager;
    private final Request httpRequest;
    private final HttpHeaders httpHeaders;
    private final UserProfile user;
    private final SessionContext session;
    private final BatchManager batchManager;


    public DigitalObjectResourceV1(
            @Context Request request,
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest
    ) throws AppConfigurationException {

        this.httpRequest = request;
        this.httpHeaders = httpHeaders;
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            this.akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfig.getConfigHome());
        } else {
            this.akubraConfiguration = null;
        }
        this.importManager = BatchManager.getInstance(appConfig);
        session = SessionContext.from(httpRequest);
        user = session.getUser();
        this.batchManager = BatchManager.getInstance();
        LOG.fine(user.toString());
    }

    /**
     * Creates a new digital object
     *
     * @param modelId              model ID (model:page, ...) of the digital object; required
     * @param pid                  PID of the digital object from external Kramerius. PID must not be already assigned. Optional
     * @param parentPid            optional PID of parent object to link the newly created object
     * @param seriesDateFrom       an optional start ISO date used to generate a series of objects.
     * @param seriesDateTo         an optional end ISO date used to limit a series of objects.
     *                             If missing the last day of the year of the start date is used.
     * @param seriesDaysIncluded   an optional set of days of the week that should be included to generate the series.
     *                             Use 1 for Monday and 7 for Sunday as defined by ISO. The set of all days is used in case of no value.
     * @param seriesPartNumberFrom an optional number to generate a series of MODS objects
     * @param xmlMetadata          XML used to create new object; optional
     * @param workflowJobId        ID of workflow job (save PID of created object into workflow); optional
     * @return the list of created objects
     * @throws DigitalObjectException failure
     */
    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> newObject(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parentPid,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_DATE_FROM_PARAM) LocalDateParam seriesDateFrom,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_DATE_TO_PARAM) LocalDateParam seriesDateTo,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_DAYS_INCLUDED_PARAM) List<Integer> seriesDaysIncluded,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_DAYS_IN_RANGE_PARAM) List<Integer> seriesDaysInRange,
            @DefaultValue("false") @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_MISSING_DAYS_INCLUDED_PARAM) boolean seriesMissingDaysIncluded,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_PARTNUMBER_FROM_PARAM) Integer seriesPartNumberFrom,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_SIGNATURA) String seriesSignatura,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_FREQUENCY) String seriesFrequency,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SERIES_DATE_FORMAT) String seriesDateFormat,
            @FormParam(DigitalObjectResourceApi.NEWOBJECT_XML_PARAM) String xmlMetadata,
            @FormParam(WorkflowModelConsts.PARAMETER_JOBID) BigDecimal workflowJobId,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CATALOGID) String catalogId,
            @DefaultValue("true") @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CREATE_OBJECT) boolean createObject,
            @DefaultValue("true") @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_VALIDATE_OBJECT) boolean validation
    ) throws DigitalObjectException {

        Set<String> models = MetaModelRepository.getInstance().find()
                .stream().map(metaModel -> metaModel.getPid()).collect(Collectors.toSet());

        if (isLocked(parentPid)) {
            throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
        }

        if (modelId == null || !models.contains(modelId)) {
            throw RestException.plainBadRequest(DigitalObjectResourceApi.DIGITALOBJECT_MODEL, modelId);
        }
        xmlMetadata = (xmlMetadata == null || xmlMetadata.isEmpty() || "null".equals(xmlMetadata)) ? null : xmlMetadata;

        if (pid != null) {
            boolean invalid = pid.length() < 5;
            try {
                if (!invalid) {
                    UUID uuid = UUID.fromString(FoxmlUtils.pidAsUuid(pid));
                    pid = FoxmlUtils.pidFromUuid(uuid.toString());
                }
            } catch (IllegalArgumentException e) {
                invalid = true;
            }
            if (invalid) {
                return SmartGwtResponse.<SearchViewItem>asError().error(
                        DigitalObjectResourceApi.DIGITALOBJECT_PID, "Invalid PID!").build();
            }
        } else {
            if (xmlMetadata != null) {
                pid = FoxmlUtils.identifierAsPid(ResolverUtils.getIdentifier("uuid", ModsUtils.unmarshalModsType(new StreamSource(new StringReader(xmlMetadata)))));
            }
        }

        LOG.log(Level.FINE, "model: {0}, pid: {3}, parent: {2}, XML: {1}",
                new Object[]{modelId, xmlMetadata, parentPid, pid});


        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        try {
            CreateHandler handler = dom.create(modelId, pid, parentPid, user, xmlMetadata, session.asFedoraLog());
            if (seriesDateFrom != null) {
                handler.issueSeries(seriesDateFrom.getLocalDate(),
                        seriesDateTo == null ? null : seriesDateTo.getLocalDate(),
                        seriesDaysIncluded, seriesMissingDaysIncluded, seriesDaysInRange, seriesPartNumberFrom, seriesFrequency, seriesDateFormat, seriesSignatura);
            }
            List<SearchViewItem> items;
            if (workflowJobId != null) {
                Locale locale = session.getLocale(httpHeaders);
                WorkflowManager workflowManager = WorkflowManager.getInstance();
                Job job = workflowManager.getJob(workflowJobId);
                if (job != null) {
                    BigDecimal parentJobId = job.getParentId();
                    if (parentJobId != null) {
                        MaterialFilter filter = new MaterialFilter();
                        filter.setLocale(session.getLocale(httpHeaders));
                        filter.setJobId(parentJobId);
                        filter.setType(MaterialType.DIGITAL_OBJECT);
                        List<MaterialView> materialViews = workflowManager.findMaterial(filter);
                        for (MaterialView material : materialViews) {
                            if (material.getPid() != null) {
                                parentPid = material.getPid();
                                break;
                            }
                        }
                    }
                }
                handler.setParentPid(parentPid);
                items = handler.createAndConnectToWorkflowJob(workflowJobId, locale, createObject, validation);
            } else {
                items = handler.create(createObject, validation, catalogId);
            }

            if (OldPrintPlugin.MODEL_CONVOLUTTE.equals(modelId) && createObject) {
                CreateHandler hierarchyModelsHandler = dom.create(OldPrintPlugin.MODEL_MONOGRAPHVOLUME, null, items.get(0).getPid(), user, xmlMetadata, session.asFedoraLog());
                hierarchyModelsHandler.create();
                CreateHierarchyHandler hierarchyHandler = dom.createHierarchyHandler(OldPrintPlugin.MODEL_MONOGRAPHVOLUME, pid, items.get(0).getPid(), user, xmlMetadata, session.asFedoraLog());
                if (catalogId != null && !catalogId.equals("")) {
                    hierarchyHandler.prepareCatalog(catalogId);
                    hierarchyHandler.prepareChildList(xmlMetadata);

                    List<Locale> acceptableLanguages = httpHeaders.getAcceptableLanguages();
                    Locale locale = acceptableLanguages.isEmpty() ? null : acceptableLanguages.get(0);
                    hierarchyHandler.createChild(locale);
                }
            }

            return new SmartGwtResponse<>(items);
        } catch (DigitalObjectExistException ex) {
            return SmartGwtResponse.<SearchViewItem>asError().error("pid", ex.getMessage()).build();
        } catch (WorkflowException ex) {
            return SmartGwtResponse.asError(ex);
        } catch (DigitalObjectValidationException ex) {
            return toValidationError(ex, session.getLocale(httpHeaders));
        }
    }

    /**
     * @see PurgeFedoraObject
     */
    @DELETE
    @Consumes({MediaType.APPLICATION_FORM_URLENCODED})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DigitalObject> deleteObject(
            @QueryParam(DigitalObjectResourceApi.DELETE_PID_PARAM) List<String> pids,
            @QueryParam(DigitalObjectResourceApi.DELETE_HIERARCHY_PARAM)
            @DefaultValue("true") boolean hierarchy,
            @QueryParam(DigitalObjectResourceApi.DELETE_PURGE_PARAM)
            @DefaultValue("false") boolean purge,
            @QueryParam(DigitalObjectResourceApi.DELETE_RESTORE_PARAM)
            @DefaultValue("false") boolean restore
    ) throws IOException, PurgeException {

        if (isLocked(pids)) {
            throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
        }

        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            FedoraStorage fedora = FedoraStorage.getInstance(appConfig);
            ArrayList<DigitalObject> result = new ArrayList<DigitalObject>(pids.size());
            PurgeFedoraObject service = new PurgeFedoraObject(fedora);
            for (String pid : pids) {
                try {
                    setWorkflow("task.deletionPA", getIMetsElement(pid, false));
                } catch (MetsExportException | DigitalObjectException | WorkflowException e) {
                    if (e.getMessage() != null && e.getMessage().contains("low-level storage")) {
                        LOG.warning("Skiped setting task in workflow, " + e.getMessage() + " " + e.getStackTrace());
                    } else if (e.getMessage() != null && e.getMessage().contains("Unable to get")) {
                        LOG.warning("Skiped setting task in workflow, " + e.getMessage() + " " + e.getStackTrace());
                    } else {
                        throw new IOException(e);
                    }
                }
            }
            if (purge) {
                session.requirePermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN);
                service.purge(pids, hierarchy, session.asFedoraLog());
            } else if (restore) {
                service.restore(pids, session.asFedoraLog());
            } else {
                service.delete(pids, hierarchy, session.asFedoraLog());
            }
            for (String pid : pids) {
                result.add(new DigitalObject(pid, null));
            }
            return new SmartGwtResponse<DigitalObject>(result);
        } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            AkubraStorage akubra = AkubraStorage.getInstance(akubraConfiguration);
            ArrayList<DigitalObject> result = new ArrayList<>(pids.size());
            PurgeAkubraObject service = new PurgeAkubraObject(akubra);
            for (String pid : pids) {
                try {
                    setWorkflow("task.deletionPA", getIMetsElement(pid, false));
                } catch (MetsExportException | DigitalObjectException | WorkflowException e) {
                    if (e.getMessage() != null && e.getMessage().contains("low-level storage")) {
                        LOG.warning("Skiped setting task in workflow, " + e.getMessage() + " " + e.getStackTrace());
                    } else if (e.getMessage() != null && e.getMessage().contains("Unable to get")) {
                        LOG.warning("Skiped setting task in workflow, " + e.getMessage() + " " + e.getStackTrace());
                    } else {
                        throw new IOException(e);
                    }
                }
            }
            if (purge) {
                session.requirePermission(UserRole.ROLE_SUPERADMIN, Permissions.ADMIN);
                service.purge(pids, hierarchy, session.asFedoraLog());
            } else if (restore) {
                service.restore(pids, session.asFedoraLog());
            } else {
                service.delete(pids, hierarchy, session.asFedoraLog());
            }
            for (String pid : pids) {
                result.add(new DigitalObject(pid, null));
            }
            return new SmartGwtResponse<DigitalObject>(result);
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }
    }

    @DELETE
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DigitalObject> deleteObject(
            ProArcRequest.DeleteObjectRequest deleteObjectRequest,
            @QueryParam(DigitalObjectResourceApi.DELETE_PID_PARAM) List<String> pids,
            @QueryParam(DigitalObjectResourceApi.DELETE_HIERARCHY_PARAM)
            @DefaultValue("true") boolean hierarchy,
            @QueryParam(DigitalObjectResourceApi.DELETE_PURGE_PARAM)
            @DefaultValue("false") boolean purge,
            @QueryParam(DigitalObjectResourceApi.DELETE_RESTORE_PARAM)
            @DefaultValue("false") boolean restore) throws PurgeException, IOException {
        if (deleteObjectRequest == null) {
            return deleteObject(pids, hierarchy, purge, restore);
        } else {
            return deleteObject(deleteObjectRequest.pids, deleteObjectRequest.hierarchy, deleteObjectRequest.purge, deleteObjectRequest.restore);
        }
    }

    public SmartGwtResponse<SearchViewItem> search(String pid) throws IOException, FedoraClientException {
        return search(null, SearchType.PIDS, Collections.singletonList(pid), null, null, null, null, null, null, null, null, null, null, 0, null, null);
    }

//    @DELETE
//    @Consumes({MediaType.APPLICATION_JSON})
//    @Produces({MediaType.APPLICATION_JSON})
//    public SmartGwtResponse<DigitalObject> deleteObject(
//            ProArcRequest.DeleteObjectRequest deleteObjectRequest
//    ) throws PurgeException, IOException {
//        return deleteObject(deleteObjectRequest.pids, deleteObjectRequest.hierarchy, deleteObjectRequest.purge, deleteObjectRequest.restore);
//    }
//
//    public SmartGwtResponse<SearchViewItem> search(String pid) throws IOException, FedoraClientException {
//        return search(null, SearchType.PIDS, Collections.singletonList(pid), null, null, null, null, null, null, null, null, null, null, 0, null, null);
//    }

    @GET
    @Path(DigitalObjectResourceApi.SEARCH_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> search(
            @QueryParam(DigitalObjectResourceApi.SEARCH_OWNER_PARAM) String owner,
            @DefaultValue(SearchType.DEFAULT)
            @QueryParam(DigitalObjectResourceApi.SEARCH_TYPE_PARAM) SearchType type,
            @QueryParam(DigitalObjectResourceApi.SEARCH_PID_PARAM) List<String> pids,
            @QueryParam(DigitalObjectResourceApi.SEARCH_BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.SEARCH_PHRASE_PARAM) String phrase,
            @QueryParam(DigitalObjectResourceApi.SEARCH_QUERY_CREATOR_PARAM) String queryCreator,
            @QueryParam(DigitalObjectResourceApi.SEARCH_QUERY_IDENTIFIER_PARAM) String queryIdentifier,
            @QueryParam(DigitalObjectResourceApi.SEARCH_QUERY_LABEL_PARAM) String queryLabel,
            @QueryParam(DigitalObjectResourceApi.SEARCH_QUERY_MODEL_PARAM) String queryModel,
            @QueryParam(DigitalObjectResourceApi.SEARCH_QUERY_TITLE_PARAM) String queryTitle,
            @QueryParam(DigitalObjectResourceApi.SEARCH_STATUS_PARAM) String queryStatus,
            @QueryParam(DigitalObjectResourceApi.SEACH_ORGANIZATION_PARAM) String queryOrganization,
            @QueryParam(DigitalObjectResourceApi.SEARCH_PROCESSOR_PARAM) String queryProcessor,
            @QueryParam(DigitalObjectResourceApi.SEARCH_START_ROW_PARAM) int startRow,
            @DefaultValue(SearchSort.DEFAULT_DESC)
            @QueryParam(DigitalObjectResourceApi.SEARCH_SORT_PARAM) SearchSort sort,
            @QueryParam(DigitalObjectResourceApi.SEARCH_SORT_FIELD_PARAM) String sortField
    ) throws FedoraClientException, IOException {

        Locale locale = session.getLocale(httpHeaders);
        SearchView search = null;
        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            FedoraStorage remote = FedoraStorage.getInstance(appConfig);
            search = remote.getSearch(locale);
        } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            AkubraStorage akubra = AkubraStorage.getInstance(akubraConfiguration);
            search = akubra.getSearch(locale);
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }
        String organization = user.getRole() == null || user.getRole().isEmpty() || UserRole.ROLE_SUPERADMIN.equals(user.getRole()) ? null : user.getOrganization();
        String username = user.getRole() == null
                || user.getRole().isEmpty()
                || UserRole.ROLE_SUPERADMIN.equals(user.getRole())
                || UserRole.ROLE_ADMIN.equals(user.getRole())
                || !appConfig.getSearchOptions().getSearchFilterProcessor() ? null : user.getUserName();

        Boolean allowAllForProcessor = appConfig.getSearchOptions().getSearchFilterAllowAllForProcessor();
        Boolean filterWithoutExtension = appConfig.getSearchOptions().getSearchFilterWithoutExtension();

        List<SearchViewItem> items;
        int total = 0;
        int page = 20;
        switch (type) {
            case ALPHABETICAL:
                total = search.countModels(queryModel, filterOwnObjects(user), organization, username, filterWithoutExtension);
                items = search.findAlphabetical(startRow, queryModel, filterOwnObjects(user), organization, username, filterWithoutExtension, 100, sort.toString());
                items = sortItems(items, sort);
                break;
            case LAST_MODIFIED:
                total = search.countModels(queryModel, filterOwnObjects(user), organization, username, filterWithoutExtension);
                items = search.findLastModified(startRow, queryModel, filterOwnObjects(user), organization, username, filterWithoutExtension, 100, sort.toString());
                break;
            case QUERY:
                items = search.findQuery(new SearchViewQuery().setTitle(queryTitle)
                        .setLabel(queryLabel).setIdentifier(queryIdentifier)
                        .setOwner(owner).setModel(queryModel).setCreator(queryCreator).setStatus(queryStatus)
                        .setHasOwners(filterGroups(user)), "active");
                total = items.size();
                page = 1;
                break;
            case PIDS:
                items = search.find(pids);
                total = items.size();
                page = 1;
                break;
            case PHRASE:
                if (session.checkPermission(Permissions.REPO_SEARCH_GROUPOWNER)) {
                    // unsupported type
                    throw new WebApplicationException(Status.FORBIDDEN);
                }
                if (username == null) {
                    username = queryProcessor;
                }
                if (organization == null) {
                    organization = queryOrganization;
                }
                items = search.findPhrase(phrase, null, null, null, queryModel, false, filterWithoutExtension, sortField, sort.toString(), startRow, 100);
                total = items.size();
                page = 1;
                break;
            case PARENT:
                items = searchParent(batchId, pids, search);
                total = items.size();
                page = 1;
                break;
            case DELETED:
                items = search.findQuery(new SearchViewQuery().setTitle(queryTitle)
                        .setLabel(queryLabel).setIdentifier(queryIdentifier)
                        .setOwner(owner).setModel(queryModel).setCreator(queryCreator)
                        .setHasOwners(filterGroups(user)), "deleted");
                ;
                total = items.size();
                page = 1;
                break;
            case ALL:
                items = search.findAllObjects();
                total = items.size();
                break;
            case ADVANCED:
                if (username == null) {
                    username = queryProcessor;
                }
                if (organization == null) {
                    organization = queryOrganization;
                }
                total = search.findAdvancedSearchCount(queryIdentifier, queryLabel, owner, queryStatus, organization, username, queryModel, queryCreator, allowAllForProcessor, filterWithoutExtension);
                items = search.findAdvancedSearchItems(queryIdentifier, queryLabel, owner, queryStatus, organization, username, queryModel, queryCreator, allowAllForProcessor, filterWithoutExtension, sortField, sort.toString(), startRow, 100);
                if (sortField == null || sortField.isEmpty() || "label".equals(sortField)) {
                    items = sortItems(items, sort);
                }
                break;
            default:
                total = search.countModels(queryModel, filterOwnObjects(user), organization, username, filterWithoutExtension);
                items = search.findLastCreated(startRow, queryModel, filterOwnObjects(user), organization, username, filterWithoutExtension, 100, sort.toString());
        }
        repairItemsModel(items);
        int count = items.size();
        int endRow = startRow + count - 1;
        //int total = count == 0 ? startRow : endRow + page;
        return new SmartGwtResponse<SearchViewItem>(SmartGwtResponse.STATUS_SUCCESS, startRow, endRow, total, items);
    }

    private List<SearchViewItem> sortItems(List<SearchViewItem> items, SearchSort sort) {
        List<SearchViewItem> normal = new ArrayList<>();
        List<SearchViewItem> lower = new ArrayList<>();
        List<SearchViewItem> upper = new ArrayList<>();
        for (SearchViewItem item : items) {
            if (item.getLabel() != null && item.getLabel().startsWith("\"")) {
                upper.add(item);
            } else if (item.getLabel() != null && item.getLabel().startsWith("„")) {
                lower.add(item);
            } else {
                normal.add(item);
            }
        }
        items.clear();
        if (SearchSort.ASC.equals(sort)) {
            items.addAll(lower);
            items.addAll(upper);
            items.addAll(normal);
        } else {
            items.addAll(normal);
            items.addAll(upper);
            items.addAll(lower);
        }
        return items;
    }

    private void repairItemsModel(List<SearchViewItem> items) {
        for (SearchViewItem item : items) {
            if (item.getOrganization() != null && item.getOrganization().startsWith("info:fedora/")) {
                item.setOrganization(item.getOrganization().substring(12));
            }
            if (item.getUser() != null && item.getUser().startsWith("info:fedora/")) {
                item.setUser(item.getUser().substring(12));
            }
            if (item.getStatus() != null && item.getStatus().startsWith("info:fedora/")) {
                item.setStatus(item.getStatus().substring(12));
            }
        }
    }

    private String filterOwnObjects(UserProfile user) {
        boolean checkPermission = session.checkPermission(Permissions.REPO_SEARCH_GROUPOWNER);
        return checkPermission ? user.getUserNameAsPid() : null;
    }

    private Collection<String> filterGroups(UserProfile user) {
        boolean checkPermission = session.checkPermission(Permissions.REPO_SEARCH_GROUPOWNER);
        if (checkPermission && user.getDefaultGroup() != null) {
            UserManager userManager = UserUtil.getDefaultManger();
            // FedoraClient.findObjects() does not support OR operator!
            // Filter just the default group.
            Group defGroup = userManager.findGroup(user.getDefaultGroup());
            if (defGroup != null) {
                return Collections.singletonList(UserUtil.toGroupPid(defGroup));
            }
//            List<Group> groups = userManager.findUserGroups(user.getId());
//            return UserUtil.toGroupPid(groups);
        }
        return Collections.emptyList();
    }

    private List<SearchViewItem> searchParent(Integer batchId, List<String> pids, SearchView search)
            throws IOException, FedoraClientException {

        if (batchId != null) {
            Batch batch = importManager.get(batchId);
            String parentPid = batch == null ? null : batch.getParentPid();
            if (parentPid == null) {
                return Collections.emptyList();
            } else {
                return search.find(parentPid);
            }
        } else {
            if (pids == null || pids.size() != 1) {
                throw RestException.plainText(Status.BAD_REQUEST, "parent search requires single pid parameter");
            }
            return search.findReferrers(pids.get(0));
        }
    }

    /**
     * Gets members of a digital object.
     *
     * @param parent PID of digital object to query its members. {@code root} parameter is ignored.
     * @param root   PID of digital object to return itself as a member with parent as {@code null}.
     *               Useful to show root of the member hierarchy.
     * @return ordered list of members
     */
    @GET
    @Path(DigitalObjectResourceApi.MEMBERS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> findMembers(
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parent,
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ROOT_PARAM) String root
    ) throws FedoraClientException, IOException, DigitalObjectException {

        SearchView search = null;
        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            search = FedoraStorage.getInstance(appConfig).getSearch(session.getLocale(httpHeaders));
        } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            search = AkubraStorage.getInstance(akubraConfiguration).getSearch(session.getLocale(httpHeaders));
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }
        List<SearchViewItem> items;
        String parentPid;
        if (parent == null || "null".equals(parent)) {
            items = search.find(root);
            parentPid = null;
        } else {
            items = search.findSortedChildren(parent);
            parentPid = parent;
        }
        for (SearchViewItem item : items) {
            item.setParentPid(parentPid);
        }
        return new SmartGwtResponse<SearchViewItem>(items);
    }


    /**
     * Sets new member sequence of given parent digital object.
     *
     * @param request params posted inside the request body
     */
    @PUT
    @Path(DigitalObjectResourceApi.MEMBERS_PATH)
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> setMembers(
            ProArcRequest.SetMemberRequest request
    ) throws IOException, FedoraClientException, DigitalObjectException {

        return setMembers(request.parentPid, request.batchId, request.toSetPids);
    }

    /**
     * Sets new member sequence of given parent digital object.
     *
     * @param parentPid parent PID
     * @param batchId   batch import ID
     * @param toSetPids list of member PIDS
     * @return ordered list of members
     * @throws RestException
     */
    @PUT
    @Path(DigitalObjectResourceApi.MEMBERS_PATH)
    @Consumes({MediaType.APPLICATION_FORM_URLENCODED})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> setMembers(
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parentPid,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PID) List<String> toSetPids
            // XXX long timestamp
    ) throws IOException, FedoraClientException, DigitalObjectException {

//        LOG.log(Level.INFO, "parentPid: {0}, batchId: {1}, toSetPids: {2}",
//                new Object[]{parentPid, batchId, toSetPids});
        if (batchId == null && parentPid == null) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT, null);
        }

        if (isLocked(parentPid)) {
            throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
        }
        boolean batchImportMembers = batchId != null;
        if (toSetPids == null || toSetPids.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.MEMBERS_ITEM_PID, null);
        }
        if (!batchImportMembers && toSetPids.contains(parentPid)) {
            throw RestException.plainText(Status.BAD_REQUEST, "parent and pid are same!");
        }

        HashSet<String> toSetPidSet = new HashSet<String>(toSetPids);
        if (toSetPidSet.size() != toSetPids.size()) {
            throw RestException.plainText(Status.BAD_REQUEST, "duplicates in PIDs to set!\n" + toSetPids.toString());
        }

        Batch batch = batchId == null ? null : importManager.get(batchId);

        // fetch PID[] -> Item[]
        Map<String, SearchViewItem> memberSearchMap;
        if (batchImportMembers) {
            memberSearchMap = loadLocalSearchItems(batch);
            checkSearchedMembers(toSetPidSet, memberSearchMap);
        } else {
            memberSearchMap = loadSearchItems(toSetPidSet);
        }
        // load current members
        DigitalObjectHandler doHandler = findHandler(parentPid, batch, false);
        RelationEditor editor = doHandler.relations();
        List<String> members = editor.getMembers();
        members.clear();
        // add new members
        ArrayList<SearchViewItem> added = new ArrayList<SearchViewItem>();
        for (String addPid : toSetPids) {
            if (!members.contains(addPid)) {
                members.add(addPid);
                SearchViewItem item = memberSearchMap.get(addPid);
                if (item == null) {
                    throw RestException.plainNotFound(DigitalObjectResourceApi.MEMBERS_ITEM_PID,
                            toSetPids.toString());
                }
                item.setParentPid(parentPid);
                added.add(item);
            }
        }
        editor.setMembers(members);
        editor.write(editor.getLastModified(), session.asFedoraLog());
        doHandler.commit();
        return new SmartGwtResponse<SearchViewItem>(added);
    }

    /**
     * Fetches object descriptions from the index. Useful to check whether object exists.
     *
     * @param pids object IDs to search
     * @return the map of found PIDs and descriptions
     */
    private Map<String, SearchViewItem> loadSearchItems(Set<String> pids) throws IOException, FedoraClientException {
        SearchView search = null;
        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            FedoraStorage storage = FedoraStorage.getInstance(appConfig);
            search = storage.getSearch(session.getLocale(httpHeaders));
        } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
            search = akubraStorage.getSearch(session.getLocale(httpHeaders));
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }

        List<SearchViewItem> memberSearch = search.find(new ArrayList<String>(pids));
        HashMap<String, SearchViewItem> memberSearchMap = new HashMap<String, SearchViewItem>(memberSearch.size());
        for (SearchViewItem item : memberSearch) {
            memberSearchMap.put(item.getPid(), item);
        }
        checkSearchedMembers(pids, memberSearchMap);
        return memberSearchMap;
    }

    private Map<String, SearchViewItem> loadLocalSearchItems(Batch batch) throws DigitalObjectException {
        if (batch == null) {
            throw new NullPointerException();
        }
        HashMap<String, SearchViewItem> memberSearchMap = new HashMap<String, SearchViewItem>();
        List<BatchItemObject> batchObjects = importManager.findLoadedObjects(batch);
        for (BatchItemObject batchObject : batchObjects) {
            DigitalObjectHandler doh = findHandler(batchObject.getPid(), batch);
            LocalObject lfo = (LocalObject) doh.getFedoraObject();
            SearchViewItem item = new SearchViewItem(batchObject.getPid());
            item.setBatchId(batch.getId());
            item.setLabel(lfo.getLabel());
            item.setOwner(lfo.getOwner());
            RelationEditor relationEditor = doh.relations();
            item.setModel(relationEditor.getModel());
            memberSearchMap.put(batchObject.getPid(), item);
        }
        return memberSearchMap;
    }

    private void checkSearchedMembers(Set<String> pids, Map<String, SearchViewItem> memberSearchMap) throws RestException {
        if (!pids.equals(memberSearchMap.keySet())) {
            HashSet<String> notMembers = new HashSet<String>(pids);
            notMembers.removeAll(memberSearchMap.keySet());
            HashSet<String> missingPids = new HashSet<String>(memberSearchMap.keySet());
            missingPids.removeAll(pids);
            throw RestException.plainNotFound(DigitalObjectResourceApi.MEMBERS_ITEM_PID,
                    "Not member PIDs: " + notMembers.toString()
                            + "\nMissing PIDs: " + missingPids.toString());
        }
    }

    /**
     * Adds new object members. Members that already exists remain untouched.
     *
     * @param parentPid PID of parent object
     * @param toAddPids list of PIDs to add; cannot contain parent PID
     * @return list of added members
     */
    @POST
    @Path(DigitalObjectResourceApi.MEMBERS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> addMembers(
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parentPid,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PID) List<String> toAddPids,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId
    ) throws IOException, FedoraClientException, DigitalObjectException {

        if (parentPid == null) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT, null);
        }
        if (toAddPids == null || toAddPids.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.MEMBERS_ITEM_PID, null);
        }
        if (toAddPids.contains(parentPid)) {
            throw RestException.plainText(Status.BAD_REQUEST, "parent and pid are same!");
        }
        if (isLocked(parentPid) || isLocked(toAddPids)) {
            throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
        }
        HashSet<String> addPidSet = new HashSet<String>(toAddPids);
        if (addPidSet.size() != toAddPids.size()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Duplicate children in the request!");
        }

        // XXX loadLocalSearchItems
        Map<String, SearchViewItem> memberSearchMap = loadSearchItems(addPidSet);
        DigitalObjectHandler handler = findHandler(parentPid, batchId, false);

        checkModelRelations(toAddPids, batchId, handler);

        List<SearchViewItem> added = addMembers(handler, toAddPids, memberSearchMap);
        handler.commit();
        return new SmartGwtResponse<SearchViewItem>(added);
    }

    private void checkModelRelations(
            List<String> toAddPids,
            Integer batchId,
            DigitalObjectHandler parentHandler
    ) throws DigitalObjectException {
        for (String pid : toAddPids) {
            MetaModel child = findHandler(pid, batchId).getModel();

            StringBuilder reason = new StringBuilder();

            if (!child.isAllowedRelation(findHandler(pid, batchId, true), parentHandler.getModel().getPid(), reason)) {
                throw RestException.plainText(Status.BAD_REQUEST,
                        "NDK restrictions do not allow: " + child.getPid() + "<br/> " +
                                "to be in direct relation with: " + parentHandler.getModel().getPid() + "<br/> " +
                                "<br/>" +
                                reason.toString());
            }
        }
    }

    private List<SearchViewItem> addMembers(DigitalObjectHandler parent,
                                            List<String> toAddPids,
                                            Map<String, SearchViewItem> memberSearchMap
    ) throws DigitalObjectException {

        String parentPid = parent.getFedoraObject().getPid();
        HashSet<String> toAddPidSet = new HashSet<String>(toAddPids);
        ArrayList<SearchViewItem> added = new ArrayList<SearchViewItem>(toAddPidSet.size());
        if (toAddPidSet.isEmpty()) {
            return added;
        }
        RelationEditor editor = parent.relations();
        List<String> members = editor.getMembers();
        // add new members
        for (String addPid : toAddPids) {
            if (!members.contains(addPid)) {
                members.add(addPid);
                SearchViewItem item = memberSearchMap.get(addPid);
                if (item == null) {
                    throw RestException.plainNotFound("pid", toAddPidSet.toString());
                }
                item.setParentPid(parentPid);
                added.add(item);
            } else {
                throw RestException.plainText(Status.BAD_REQUEST,
                        parentPid + " already contains: " + addPid);
            }
        }
        // write if any change
        if (!added.isEmpty()) {
            editor.setMembers(members);
            editor.write(editor.getLastModified(), session.asFedoraLog());
        }
        return added;
    }

    /**
     * Deletes object members from digital object.
     *
     * @param parentPid    digital object ID
     * @param toRemovePids member IDs to remove
     * @param batchId      optional batch import ID
     * @return list of removed IDs
     */
    @DELETE
    @Path(DigitalObjectResourceApi.MEMBERS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> deleteMembers(
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parentPid,
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ITEM_PID) List<String> toRemovePids,
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId
    ) throws IOException, DigitalObjectException {

        if (parentPid == null) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing parent parameter!");
        }
        if (toRemovePids == null || toRemovePids.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing pid parameter!");
        }
        if (toRemovePids.contains(parentPid)) {
            throw RestException.plainText(Status.BAD_REQUEST, "parent and pid are same!");
        }

        if (isLocked(parentPid) || isLocked(toRemovePids)) {
            throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
        }

        HashSet<String> toRemovePidSet = new HashSet<String>(toRemovePids);
        if (toRemovePidSet.isEmpty()) {
            return new SmartGwtResponse<SearchViewItem>(Collections.<SearchViewItem>emptyList());
        }

        DigitalObjectHandler parent = findHandler(parentPid, batchId, false);
        deleteMembers(parent, toRemovePidSet);
        parent.commit();

        ArrayList<SearchViewItem> removed = new ArrayList<SearchViewItem>(toRemovePidSet.size());
        for (String removePid : toRemovePidSet) {
            SearchViewItem item = new SearchViewItem(removePid);
            item.setParentPid(parentPid);
            removed.add(item);
        }

        return new SmartGwtResponse<SearchViewItem>(removed);
    }

    /**
     * Removes given children from a parent.
     * <p><b>Requires handler commit!</b>
     *
     * @param parent         parent PID
     * @param toRemovePidSet PIDs of children to remove
     */
    private void deleteMembers(DigitalObjectHandler parent, Set<String> toRemovePidSet) throws DigitalObjectException {
        RelationEditor editor = parent.relations();
        List<String> members = editor.getMembers();
        // check that PIDs being removed are members of parent object
        HashSet<String> toRemovePidSetCopy = new HashSet<String>(toRemovePidSet);
        toRemovePidSetCopy.removeAll(members);
        if (!toRemovePidSetCopy.isEmpty()) {
            String msg = String.format("Parent: %s does not contain members: %s",
                    parent.getFedoraObject().getPid(), toRemovePidSetCopy.toString());
            throw RestException.plainText(Status.BAD_REQUEST, msg);
        }
        // remove
        if (members.removeAll(toRemovePidSet)) {
            editor.setMembers(members);
            editor.write(editor.getLastModified(), session.asFedoraLog());
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.MEMBERS_PATH + '/' + DigitalObjectResourceApi.MEMBERS_MOVE_PATH)
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> moveMembers(
            ProArcRequest.MoveMembersRequest request
    ) throws IOException, DigitalObjectException, FedoraClientException {

        return moveMembers(request.srcParentPid, request.dstParentPid, request.batchId, request.pids);
    }

    /**
     * Moves members from a source object to a destination object.
     *
     * @param srcParentPid PID of source
     * @param dstParentPid PID of destination
     * @param batchId      optional batch import ID
     * @param movePids     member PIDs to move
     * @return the list of updated members
     */
    @PUT
    @Path(DigitalObjectResourceApi.MEMBERS_PATH + '/' + DigitalObjectResourceApi.MEMBERS_MOVE_PATH)
    @Consumes({MediaType.APPLICATION_FORM_URLENCODED})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> moveMembers(
            @FormParam(DigitalObjectResourceApi.MEMBERS_MOVE_SRCPID) String srcParentPid,
            @FormParam(DigitalObjectResourceApi.MEMBERS_MOVE_DSTPID) String dstParentPid,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PID) List<String> movePids
    ) throws IOException, DigitalObjectException, FedoraClientException {

        if (srcParentPid == null) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing source PID!");
        }
        if (dstParentPid == null) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing target PID!");
        }
        if (srcParentPid.equals(dstParentPid)) {
            throw RestException.plainText(Status.BAD_REQUEST, "src == dst!");
        }
        if (movePids == null || movePids.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing children PIDs!");
        }

        if (isLocked(srcParentPid) || isLocked(dstParentPid) || isLocked(movePids)) {
            throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
        }

        HashSet<String> movePidSet = new HashSet<String>(movePids);
        if (movePidSet.isEmpty()) {
            return new SmartGwtResponse<SearchViewItem>(Collections.<SearchViewItem>emptyList());
        } else if (movePidSet.size() != movePids.size()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Duplicate children in the request!");
        }
        if (movePidSet.contains(dstParentPid)) {
            throw RestException.plainText(Status.BAD_REQUEST, "The target parent listed as child!");
        }

        Batch batch = batchId == null ? null : importManager.get(batchId);
        DigitalObjectHandler srcHandler = findHandler(srcParentPid, batch, false);
        DigitalObjectHandler dstHandler = findHandler(dstParentPid, batch, false);

        checkModelRelations(movePids, batchId, dstHandler);

        deleteMembers(srcHandler, movePidSet);

        // XXX loadLocalSearchItems
        Map<String, SearchViewItem> memberSearchMap = loadSearchItems(movePidSet);

        List<SearchViewItem> added = addMembers(dstHandler, movePids, memberSearchMap);

        srcHandler.commit();
        dstHandler.commit();

        try {
            setWorkflow("task.metadataDescriptionInProArc", getIMetsElement(dstParentPid, true));
        } catch (MetsExportException | DigitalObjectException | WorkflowException e) {
            LOG.severe("Nepodarilo se ukoncit ukol \"task.metadataDescriptionInProArc\" pro " + dstParentPid + " - " + e.getMessage());
        }

        SmartGwtResponse<SearchViewItem> result = new SmartGwtResponse<SearchViewItem>(added);
        return result;
    }

    @GET
    @Path(DigitalObjectResourceApi.DC_PATH)
    @Produces(MediaType.APPLICATION_XML)
    public DublinCoreRecord getDublinCore(
            @QueryParam(DigitalObjectResourceApi.DUBLINCORERECORD_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.DUBLINCORERECORD_BATCHID) Integer batchId
    ) throws IOException, DigitalObjectException {

        ProArcObject fobject = findFedoraObject(pid, batchId);
        DcStreamEditor dcEditor = new DcStreamEditor(fobject);
        try {
            DublinCoreRecord dc = dcEditor.read();
            dc.setBatchId(batchId);
            return dc;
        } catch (DigitalObjectNotFoundException ex) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.CREATE_PID_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord createPid() {
        String pid = FoxmlUtils.createPid();
        StringRecord record = new StringRecord();
        record.setPid(pid);
        record.setTimestamp(System.nanoTime());
        return record;
    }

    @GET
    @Path(DigitalObjectResourceApi.DC_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public DublinCoreRecord getDublinCoreJson(
            @QueryParam(DigitalObjectResourceApi.DUBLINCORERECORD_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.DUBLINCORERECORD_BATCHID) Integer batchId
    ) throws IOException, DigitalObjectException {

        ProArcObject fobject = findFedoraObject(pid, batchId);
        DcStreamEditor dcEditor = new DcStreamEditor(fobject);
        try {
            DublinCoreRecord dc = dcEditor.read();
            dc.setBatchId(batchId);
            return dc;
        } catch (DigitalObjectNotFoundException ex) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.DC_PATH)
    @Consumes({MediaType.TEXT_XML, MediaType.APPLICATION_XML})
    @Produces(MediaType.APPLICATION_XML)
    public DublinCoreRecord updateDublinCore(DublinCoreRecord update) throws IOException, DigitalObjectException {
        if (update == null || update.getDc() == null) {
            throw new IllegalArgumentException();
        }
        ProArcObject fobject = findFedoraObject(update.getPid(), update.getBatchId(), false);
        DcStreamEditor dcEditor = new DcStreamEditor(fobject);
        dcEditor.write(update, session.asFedoraLog());

        fobject.flush();
        DublinCoreRecord result = dcEditor.read();
        result.setBatchId(update.getBatchId());
        return result;
    }

    /**
     * Gets subset of MODS properties in JSON.
     *
     * @param pid      PID of requested digital object
     * @param editorId view defining subset of MODS properties
     */
    @GET
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<DescriptionMetadata<Object>> getDescriptionMetadata(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId
    ) throws IOException, DigitalObjectException {

        if (pid == null || pid.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }

        DigitalObjectHandler doHandler = findHandler(pid, batchId);
        DescriptionMetadata<Object> metadata = doHandler.metadata().getMetadataAsJsonObject(editorId);
        metadata.setBatchId(batchId);
        return new SmartGwtResponse<DescriptionMetadata<Object>>(metadata);
    }

    @PUT
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DescriptionMetadata<Object>> updateDescriptionMetadata(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMJSONDATA) String jsonData,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMXMLDATA) String xmlData,
            @FormParam(WorkflowModelConsts.PARAMETER_JOBID) BigDecimal jobId,
            @FormParam(MetaModelDataSource.FIELD_MODELOBJECT) String model,
            @DefaultValue("false")
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_IGNOREVALIDATION) boolean ignoreValidation,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_STANDARD) String standard
    ) throws DigitalObjectException {

        LOG.fine(String.format("pid: %s, editor: %s, timestamp: %s, ignoreValidation: %s, json: %s, xml: %s, standard: %s",
                pid, editorId, timestamp, ignoreValidation, jsonData, xmlData, standard));
        if (pid == null || pid.isEmpty()) {
            if (jobId == null) {
                throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
            } else {
                // MODS Custom editor doesnt use WorkFlowResource, if there is a validation error
                return WorkflowResourceV1.updateDescriptionMetadataFix(jobId, model, timestamp, editorId, jsonData, xmlData, ignoreValidation, standard, session, httpHeaders);
            }
        }

        if (isLocked(pid)) {
            DigitalObjectValidationException validationException = new DigitalObjectValidationException(pid, null, null, "Locked", null);
            validationException.addValidation("Locked", ERR_IS_LOCKED, false);
            return toValidationError(validationException, STATUS_LOCKED, session.getLocale(httpHeaders));
        }
        if (timestamp == null) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.TIMESTAMP_PARAM, pid);
        }
        final boolean isJsonData = xmlData == null;
        String data = isJsonData ? jsonData : xmlData;
        DigitalObjectHandler doHandler = findHandler(pid, batchId, false);
        MetadataHandler<?> mHandler = doHandler.metadata();
        DescriptionMetadata<String> dMetadata = new DescriptionMetadata<String>();
        dMetadata.setPid(pid);
        dMetadata.setBatchId(batchId);
        dMetadata.setEditor(editorId);
        dMetadata.setData(data);
        dMetadata.setTimestamp(timestamp);
        dMetadata.setStandard(standard);
        dMetadata.setIgnoreValidation(ignoreValidation);

        try {
            List<SearchViewItem> parents = new ArrayList<>();
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                FedoraStorage remote = FedoraStorage.getInstance(appConfig);
                parents = searchParent(batchId, pidToList(pid), remote.getSearch(session.getLocale(httpHeaders)));
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                parents = searchParent(batchId, pidToList(pid), akubraStorage.getSearch(session.getLocale(httpHeaders)));
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
            if (parents.size() > 0) {
                ProArcObject parentObject = find(parents.get(0).getPid(), null);
                DigitalObjectHandler parentHandler = DigitalObjectManager.getDefault().createHandler(parentObject);
                doHandler.setParameterParent(parentHandler);
            }
        } catch (Exception ex) {
            LOG.info("Impossible to find parent handler.");
        }

        try {
            if (isJsonData) {
                mHandler.setMetadataAsJson(dMetadata, session.asFedoraLog(), NdkMetadataHandler.OPERATION_UPDATE);
            } else {
                mHandler.setMetadataAsXml(dMetadata, session.asFedoraLog(), NdkMetadataHandler.OPERATION_UPDATE);
            }
        } catch (DigitalObjectValidationException ex) {
            return toValidationError(ex, session.getLocale(httpHeaders));
        }
//        DigitalObjectStatusUtils.setState(doHandler.getFedoraObject(), STATUS_PROCESSING);
        doHandler.commit();
        return new SmartGwtResponse<DescriptionMetadata<Object>>(mHandler.getMetadataAsJsonObject(editorId));
    }

    @POST
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_VALIDATE_OBJECT_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DescriptionMetadata<Object>> validateObject(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) throws DigitalObjectException {


        if (pid == null || pid.isEmpty()) {
            throw new DigitalObjectException(null, "Missing PID to validate");
//            if (jobId == null) {
//                throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
//            } else {
//                // MODS Custom editor doesnt use WorkFlowResource, if there is a validation error
//                return WorkflowResource.updateDescriptionMetadataFix(jobId, model, timestamp, editorId, jsonData, xmlData, ignoreValidation, session, httpHeaders);
//            }
        }

        DigitalObjectHandler doHandler = findHandler(pid, batchId);
        DescriptionMetadata<Object> descriptionMetadata = doHandler.metadata().getMetadataAsJsonObject(null);
        descriptionMetadata.setBatchId(batchId);

        MetadataHandler<?> mHandler = doHandler.metadata();


        try {
            List<SearchViewItem> parents = new ArrayList<>();
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                FedoraStorage remote = FedoraStorage.getInstance(appConfig);
                parents = searchParent(batchId, pidToList(pid), remote.getSearch(session.getLocale(httpHeaders)));
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                parents = searchParent(batchId, pidToList(pid), akubraStorage.getSearch(session.getLocale(httpHeaders)));
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
            if (parents.size() > 0) {
                ProArcObject parentObject = find(parents.get(0).getPid(), null);
                DigitalObjectHandler parentHandler = DigitalObjectManager.getDefault().createHandler(parentObject);
                doHandler.setParameterParent(parentHandler);
            }
        } catch (Exception ex) {
            LOG.info("Impossible to find parent handler.");
        }

        try {
            mHandler.validateMetadataAsJson(descriptionMetadata);
        } catch (DigitalObjectValidationException ex) {
            return toValidationError(ex, session.getLocale(httpHeaders));
        } catch (DigitalObjectException ex) {
            return SmartGwtResponse.asError(ex);
        }
//        DigitalObjectStatusUtils.setState(doHandler.getFedoraObject(), STATUS_PROCESSING);
        return new SmartGwtResponse<DescriptionMetadata<Object>>();
    }

    private ProArcObject find(String pid, Integer batchId) throws DigitalObjectNotFoundException {
        return DigitalObjectManager.getDefault().find(pid, batchId);
    }

    private List<String> pidToList(String pid) {
        List<String> pids = new ArrayList<>();
        pids.add(pid);
        return pids;
    }

    @PUT
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_ADD_AUTHORITY)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DescriptionMetadata<Object>> addAuthority(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMJSONDATA) String jsonData,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId
    ) throws DigitalObjectException {
        if (pid == null || pid.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
        if (isLocked(pid)) {
            DigitalObjectValidationException validationException = new DigitalObjectValidationException(pid, null, null, "Locked", null);
            validationException.addValidation("Locked", ERR_IS_LOCKED, false);
            return toValidationError(validationException, STATUS_LOCKED, session.getLocale(httpHeaders));
        }
        if (timestamp == null) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.TIMESTAMP_PARAM, pid);
        }
        DigitalObjectHandler doHandler = findHandler(pid, batchId, false);
        MetadataHandler<?> mHandler = doHandler.metadata();
        DescriptionMetadata<String> dMetadata = new DescriptionMetadata<String>();
        dMetadata.setPid(pid);
        dMetadata.setBatchId(batchId);
        dMetadata.setEditor(editorId);
        dMetadata.setData(jsonData);
        dMetadata.setTimestamp(timestamp);

        try {
            MetadataInjector metadataInjector = new AuthorityMetadataInjector(mHandler);
            metadataInjector.addMetadata(dMetadata);
        } catch (DigitalObjectValidationException ex) {
            return toValidationError(ex, session.getLocale(httpHeaders));
        }

        doHandler.commit();
        return new SmartGwtResponse<DescriptionMetadata<Object>>(mHandler.getMetadataAsJsonObject(editorId));
    }

    @PUT
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_EDITOR_PAGES)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DescriptionMetadata<Object>> updateDescriptionMetadataPages(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PIDS) String pidsArray,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_APPLY_TO) String applyTo,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_APPLY_TO_FIRST_PAGE) String applyToFirstPage,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_NUMBER_PREFIX) String prefix,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_NUMBER_SUFFIX) String suffix,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_USE_BRACKETS) @DefaultValue("false") String useBrackets,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_NUMBER_SEQUENCE_TYPE) String sequenceType,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_NUMBER_START_NUMBER) String startNumber,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_NUMBER_INCREMENT_NUMBER) String incrementNumber,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_INDEX_START_NUMBER) String startIndex,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_TYPE_PAGE) String pageType,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_DOUBLE_COLUMNS) String doubleColumns,
            @FormParam(DigitalObjectResourceApi.MODS_PAGE_RULES_PAGE_POSITION) String pagePosition,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId
    ) throws IOException, DigitalObjectException {
        LOG.fine(String.format("pid: %s", pidsArray));

        if (pidsArray == null || pidsArray.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pidsArray);
        }
        if (batchId != null) {
            Batch batch = importManager.get(batchId);
            List<BatchItemObject> objects = importManager.findLoadedObjects(batch);
            List<String> pids = UpdatePages.createListFromArray(pidsArray);
            UpdatePages updatePages = new UpdatePages(applyTo, applyToFirstPage, doubleColumns);
            updatePages.createIndex(startIndex);
            updatePages.createListOfPids(pids);
            updatePages.updatePagesLocal(objects, sequenceType, startNumber, incrementNumber, prefix, suffix, pageType, useBrackets, pagePosition);
            return new SmartGwtResponse<>();
        } else {
            List<String> pids = UpdatePages.createListFromArray(pidsArray);
            if (isLocked(pids)) {
                DigitalObjectValidationException validationException = new DigitalObjectValidationException(pids.get(0), null, null, "Locked", null);
                validationException.addValidation("Locked", ERR_IS_LOCKED, false);
                return toValidationError(validationException, STATUS_LOCKED, session.getLocale(httpHeaders));
            }
            UpdatePages updatePages = new UpdatePages(applyTo, applyToFirstPage, doubleColumns);
            updatePages.createListOfPids(pids);
            updatePages.createIndex(startIndex);
            updatePages.updatePages(sequenceType, startNumber, incrementNumber, prefix, suffix, pageType, useBrackets, pagePosition);
            return new SmartGwtResponse<>();
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_EDITOR_PAGES_COPY_METADATA)
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DescriptionMetadata<Object>> copyDescriptionMetadataToPages(
            ProArcRequest.CopyPagesMetadataRequest request
    ) throws IOException, DigitalObjectException {
        return copyDescriptionMetadataToPages(request.sourcePidsArray, request.destinationPidsArray, request.copyPageNumber, request.copyPageType, request.copyPageIndex, request.copyPagePosition, request.copyPageRepre, request.batchId);
    }

    @POST
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_EDITOR_PAGES_COPY_METADATA)
    @Consumes({MediaType.APPLICATION_FORM_URLENCODED})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DescriptionMetadata<Object>> copyDescriptionMetadataToPages(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_SOURCE_PIDS) List<String> sourcePids,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_DESTINATION_PIDS) List<String> destinationPids,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_NUMBER) Boolean copyPageNumber,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_TYPE) Boolean copyPageType,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_INDEX) Boolean copyPageIndex,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_POSITION) Boolean copyPagePosition,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_COPY_PAGE_REPRE) Boolean copyPageRepre,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId
    ) throws IOException, DigitalObjectException {

        if (batchId != null) {
            Batch batch = importManager.get(batchId);
            List<BatchItemObject> objects = importManager.findLoadedObjects(batch);

            UpdatePagesMetadata updatePagesMetadata = new UpdatePagesMetadata(sourcePids, destinationPids, copyPageIndex, copyPageNumber, copyPageType, copyPagePosition, copyPageRepre);
            updatePagesMetadata.updatePagesLocal(objects);
            return new SmartGwtResponse(SmartGwtResponse.STATUS_SUCCESS, 0, 0, -1, null);
        } else {
            if (isLocked(destinationPids)) {
                DigitalObjectValidationException validationException = new DigitalObjectValidationException(destinationPids.get(0), null, null, "Locked", null);
                validationException.addValidation("Locked", ERR_IS_LOCKED, false);
                return toValidationError(validationException, STATUS_LOCKED, session.getLocale(httpHeaders));
            }
            UpdatePagesMetadata updatePagesMetadata = new UpdatePagesMetadata(sourcePids, destinationPids, copyPageIndex, copyPageNumber, copyPageType, copyPagePosition, copyPageRepre);
            updatePagesMetadata.updatePages();
            return new SmartGwtResponse(SmartGwtResponse.STATUS_SUCCESS, 0, 0, -1, null);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_FUNCTION_ADD_BRACKETS)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> updatePagesAddBrackets(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PIDS) String pidsArray,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId
    ) throws DigitalObjectException {
        LOG.fine(String.format("pid: %s", pidsArray));

        if (pidsArray == null || pidsArray.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pidsArray);
        }
        if (batchId != null) {
            Batch batch = importManager.get(batchId);
            List<BatchItemObject> objects = importManager.findLoadedObjects(batch);
            List<String> pids = UpdatePages.createListFromArray(pidsArray);
            UpdatePages updatePages = new UpdatePages();
            updatePages.editBracketsLocal(objects, pids, true, false);
            return returnFunctionSuccess();
        } else {
            List<String> pids = UpdatePages.createListFromArray(pidsArray);
            if (isLocked(pids)) {
                DigitalObjectValidationException validationException = new DigitalObjectValidationException(pids.get(0), null, null, "Locked", null);
                validationException.addValidation("Locked", ERR_IS_LOCKED, false);
                return toValidationError(validationException, STATUS_LOCKED, session.getLocale(httpHeaders));
            }

            UpdatePages updatePages = new UpdatePages();
            updatePages.editBrackets(pids, true, false);
            return returnFunctionSuccess();
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_FUNCTION_REMOVE_BRACKETS)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> updatePagesRemoveBrackets(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PIDS) String pidsArray,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId
    ) throws DigitalObjectException {
        LOG.fine(String.format("pid: %s", pidsArray));

        if (pidsArray == null || pidsArray.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pidsArray);
        }
        if (batchId != null) {
            Batch batch = importManager.get(batchId);
            List<BatchItemObject> objects = importManager.findLoadedObjects(batch);
            List<String> pids = UpdatePages.createListFromArray(pidsArray);
            UpdatePages updatePages = new UpdatePages();
            updatePages.editBracketsLocal(objects, pids, false, true);
            return returnFunctionSuccess();
        } else {
            List<String> pids = UpdatePages.createListFromArray(pidsArray);
            if (isLocked(pids)) {
                DigitalObjectValidationException validationException = new DigitalObjectValidationException(pids.get(0), null, null, "Locked", null);
                validationException.addValidation("Locked", ERR_IS_LOCKED, false);
                return toValidationError(validationException, STATUS_LOCKED, session.getLocale(httpHeaders));
            }

            UpdatePages updatePages = new UpdatePages();
            updatePages.editBrackets(pids, false, true);
            return returnFunctionSuccess();
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_FUNCTION_ADD_REFERENCE)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> addReferencesToMods(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_FUNCTION_ADD_REFERENCE_STRUCTURED) Boolean structured,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_FUNCTION_ADD_REFERENCE_VALUE) String reference
    ) throws DigitalObjectException {

        LOG.fine(String.format("pid: %s, structured: %s, reference: %s", pid, structured, reference));

        if (pid == null || pid.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }

        if (reference == null || reference.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.MODS_CUSTOM_FUNCTION_ADD_REFERENCE_VALUE, reference);
        }

        if (isLocked(pid)) {
            DigitalObjectValidationException validationException = new DigitalObjectValidationException(pid, null, null, "Locked", null);
            validationException.addValidation("Locked", ERR_IS_LOCKED, false);
            return toValidationError(validationException, STATUS_LOCKED, session.getLocale(httpHeaders));
        }

        AddReference addReference = new AddReference(structured, reference);
        AddReference.Result result = addReference.toPid(pid);
        if (result == null || !"ok".equalsIgnoreCase(result.getStatus())) {
            return returnFunctionError(ERR_ADDING_REFERENCE_FAILED, result.getEx());
        }
        return returnFunctionSuccess();
    }

    public static <T> SmartGwtResponse<T> toValidationError(DigitalObjectValidationException ex, Locale locale) {
        return toValidationError(ex, null, locale);
    }

    private static <T> SmartGwtResponse<T> toValidationError(DigitalObjectValidationException ex, String type, Locale locale) {
        if (ex.getValidations().isEmpty()) {
            return SmartGwtResponse.asError(ex);
        }
        ErrorBuilder<T> error = SmartGwtResponse.asError();
        ServerMessages msgs = ServerMessages.get(locale);
        boolean canBeIgnored = true;
        for (ValidationResult validation : ex.getValidations()) {
            String msg;
            if (!validation.isCanBeIgnored()) {
                canBeIgnored = false;
            }
            try {
                msg = msgs.getFormattedMessage(validation.getBundleKey(), validation.getValues());
            } catch (MissingResourceException mrex) {
                LOG.log(Level.WARNING, validation.getBundleKey(), mrex);
                msg = validation.getBundleKey();
            }
            error.error(validation.getName(), msg);
        }
        return canBeIgnored ? error.build(type) : type == null ? error.build(STATUS_DONT_BE_IGNORED) : error.build(type);
    }

    @GET
    @Path(DigitalObjectResourceApi.METAMODEL_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<AnnotatedMetaModel> listModels() {
        Locale locale = session.getLocale(httpHeaders);

        Collection<MetaModel> models = metamodels.find();
        ArrayList<AnnotatedMetaModel> result = new ArrayList<AnnotatedMetaModel>(models.size());
        for (MetaModel model : models) {
            result.add(new AnnotatedMetaModel(model, locale));
        }
        return new SmartGwtResponse<AnnotatedMetaModel>(result);
    }

    /**
     * Gets list of data profiles. Only with digitized contents.
     *
     * @param pid     object ID
     * @param batchId optional import ID
     * @param dsId    optional profile ID to filter result
     * @return the list of profiles
     */
    @GET
    @Path(DigitalObjectResourceApi.STREAMPROFILE_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DatastreamResult> getStreamProfile(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.STREAMPROFILE_ID) String dsId
    ) throws IOException, DigitalObjectException, MetsExportException {

        if (pid == null || pid.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
        ProArcObject fo = findFedoraObject(pid, batchId, true);
        List<DatastreamProfile> profiles = fo.getStreamProfile(dsId);
        ArrayList<DatastreamResult> result = new ArrayList<DatastreamResult>(profiles.size());
        for (DatastreamProfile profile : profiles) {
            String profileDsId = profile.getDsID();
            if (BinaryEditor.isMediaStream(profileDsId)) {
                result.add(DatastreamResult.from(profile));
            }
        }
        return new SmartGwtResponse<DatastreamResult>(result);
    }

    private void setWorkflow(String type, IMetsElement root) throws DigitalObjectException, WorkflowException {
        if (root != null) {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            DigitalObjectManager.CreateHandler handler = dom.create(root.getModel(), root.getOriginalPid(), null, user, null, session.asFedoraLog());
            Locale locale = session.getLocale(httpHeaders);
            Job job = handler.getWfJob(root.getOriginalPid(), locale);
            if (job == null) {
                return;
            }
            List<TaskView> tasks = handler.getTask(job.getId(), locale);
            Task editedTask = null;
            for (TaskView task : tasks) {
                if (type.equals(task.getTypeRef())) {
                    editedTask = task;
                    break;
                }
            }
            if (editedTask != null) {
                editedTask.setOwnerId(new BigDecimal(session.getUser().getId()));
                editedTask.setState(Task.State.FINISHED);
                WorkflowProfiles workflowProfiles = WorkflowProfiles.getInstance();
                WorkflowDefinition workflow = workflowProfiles.getProfiles();
                WorkflowManager workflowManager = WorkflowManager.getInstance();

                try {
                    TaskFilter taskFilter = new TaskFilter();
                    taskFilter.setId(editedTask.getId());
                    taskFilter.setLocale(locale);
                    Task.State previousState = workflowManager.tasks().findTask(taskFilter, workflow).stream()
                            .findFirst().get().getState();
                    workflowManager.tasks().updateTask(editedTask, (Map<String, Object>) null, workflow);
                    List<TaskView> result = workflowManager.tasks().findTask(taskFilter, workflow);

                    if (result != null && !result.isEmpty() && result.get(0).getState() != previousState) {
                        WorkflowActionHandler workflowActionHandler = new WorkflowActionHandler(workflow, locale);
                        workflowActionHandler.runAction(editedTask);
                    }
                } catch (IOException e) {
                    throw new DigitalObjectException(e.getMessage());
                }
            }
        }
    }

    private IMetsElement getIMetsElement(String pid, boolean validation) throws MetsExportException, IOException {
        MetsContext metsContext = null;
        ProArcObject object = null;
        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            FedoraStorage rstorage = FedoraStorage.getInstance(appConfig);
            object = rstorage.find(pid);
            metsContext = buildFedoraContext(object, null, null, rstorage, appConfig.getNdkExportOptions());
        } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
            object = akubraStorage.find(pid);
            metsContext = buildAkubraContext(object, null, null, akubraStorage, appConfig.getNdkExportOptions());
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }

        IMetsElement element = getMetsElement(object, metsContext, true, validation);
        return element == null ? null : element;
    }

    private MetsElement getMetsElement(ProArcObject fo, MetsContext metsContext, boolean hierarchy, boolean validation) throws MetsExportException {
        metsContext.resetContext();
        com.yourmediashelf.fedora.generated.foxml.DigitalObject dobj = MetsUtils.readFoXML(metsContext, fo);
        if (dobj == null) {
            return null;
        }
        return MetsElement.getElement(dobj, null, metsContext, hierarchy, validation);
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class DatastreamResult {
        @XmlElement(name = DigitalObjectResourceApi.STREAMPROFILE_ID)
        private String id;
        @XmlElement(name = DigitalObjectResourceApi.STREAMPROFILE_MIME)
        private String mime;
        @XmlElement(name = DigitalObjectResourceApi.STREAMPROFILE_HEIGHT)
        private Integer height;
        @XmlElement(name = DigitalObjectResourceApi.STREAMPROFILE_WIDTH)
        private Integer width;

        public static DatastreamResult from(DatastreamProfile profile) {
            DatastreamResult d = new DatastreamResult();
            d.id = profile.getDsID();
            d.mime = profile.getDsMIME();
            return d;
        }

        public static DatastreamResult from(DatastreamProfile profile, int height, int width) {
            DatastreamResult d = new DatastreamResult();
            d.id = profile.getDsID();
            d.mime = profile.getDsMIME();
            d.height = height;
            d.width = width;
            return d;
        }

        public DatastreamResult(String id, String mime) {
            this.id = id;
            this.mime = mime;
        }

        public DatastreamResult() {
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.PREVIEW_PATH)
    @Produces("*/*")
    public Response getPreview(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) throws IOException, DigitalObjectException {

        return getDissemination(pid, batchId, BinaryEditor.PREVIEW_ID);
    }

    /**
     * Default alias for FULL dissemination.
     *
     * @param pid     digital object PID (required)
     * @param batchId import batch ID (optional)
     * @return raw version of the archived object
     */
    @GET
    @Path(DigitalObjectResourceApi.FULL_PATH)
    @Produces("*/*")
    public Response getFull(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) throws IOException, DigitalObjectException {

        return getDissemination(pid, batchId, BinaryEditor.FULL_ID);
    }

    /**
     * Default alias for raw dissemination.
     *
     * @param pid     digital object PID (required)
     * @param batchId import batch ID (optional)
     * @return raw version of the archived object
     */
    @GET
    @Path(DigitalObjectResourceApi.RAW_PATH)
    @Produces("*/*")
    public Response getRaw(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) throws IOException, DigitalObjectException {

        return getDissemination(pid, batchId, BinaryEditor.RAW_ID);
    }

    /**
     * Gets digital object dissemination.
     *
     * @param pid     PID (required)
     * @param batchId import batch ID (optional)
     * @param dsId    data stream ID. If missing the whole digital object is returned as XML.
     * @return digital object dissemination
     * @throws IOException
     */
    @GET
    @Path(DigitalObjectResourceApi.DISSEMINATION_PATH)
    @Produces("*/*")
    public Response getDissemination(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.DISSEMINATION_DATASTREAM) String dsId
    ) throws DigitalObjectException {

        DigitalObjectHandler doHandler = findHandler(pid, batchId);
        DisseminationHandler dissemination = doHandler.dissemination(dsId);
        return dissemination.getDissemination(httpRequest);
    }

    /**
     * Updates dissemination of digital object with binary data sent as
     * {@link MediaType#MULTIPART_FORM_DATA}. It allows to upload file from
     * client.
     * <p>For now only RAW stream is supported.
     *
     * @param pid        PID (required)
     * @param batchId    import batch ID (optional)
     * @param dsId       data stream ID.
     * @param file       contents
     * @param fileInfo   contents description metadata (injected by the server)
     * @param mimeType   MIME type of the sent contents (optional)
     * @param jsonErrors include error in JSON response with HTTP status 200
     * @return JSON response with process ID (needs process API)
     */
    @POST
    @Path(DigitalObjectResourceApi.DISSEMINATION_PATH)
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Map<String, Object>> updateDissemination(
            @FormDataParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormDataParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_DATASTREAM) String dsId,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_FILE) InputStream file,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_FILE) FormDataContentDisposition fileInfo,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_FILE) FormDataBodyPart fileBodyPart,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_MIME) String mimeType,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_ERROR) @DefaultValue("false") boolean jsonErrors
    ) {

        try {
            return updateDisseminationImpl(pid, batchId, dsId, file, fileInfo, fileBodyPart, mimeType);
        } catch (Throwable ex) {
            if (jsonErrors) {
                return SmartGwtResponse.<Map<String, Object>>asError(ex);
            } else {
                if (!(ex instanceof WebApplicationException)) {
                    ex = new WebApplicationException(ex);
                }
                throw (WebApplicationException) ex;
            }
        }
    }

    private SmartGwtResponse<Map<String, Object>> updateDisseminationImpl(
            @FormDataParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormDataParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_DATASTREAM) String dsId,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_FILE) InputStream fileContent,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_FILE) FormDataContentDisposition fileInfo,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_FILE) FormDataBodyPart fileBodyPart,
            @FormDataParam(DigitalObjectResourceApi.DISSEMINATION_MIME) String mimeType
    ) throws IOException, DigitalObjectException {

        if (pid == null) {
            return SmartGwtResponse.<Map<String, Object>>asError(DigitalObjectResourceApi.DIGITALOBJECT_PID, "Missing PID!");
        }
        if (fileContent == null) {
            return SmartGwtResponse.<Map<String, Object>>asError(DigitalObjectResourceApi.DISSEMINATION_FILE, "Missing file!");
        }

        if (dsId != null && !dsId.equals(BinaryEditor.RAW_ID)) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing or unsupported datastream ID: " + dsId);
        }
        String filename = getFilename(fileInfo.getFileName());
        File file = File.createTempFile("proarc_", null);
        try {
            FileUtils.copyToFile(fileContent, file);
            // XXX add config property or user permission
            if (file.length() > 1 * 1024 * 1024 * 1024) { // 1GB
                throw RestException.plainText(Status.BAD_REQUEST, "File contents too large!");
            }
            MediaType mime;
            try {
                mime = mimeType != null ? MediaType.valueOf(mimeType) : fileBodyPart.getMediaType();
            } catch (IllegalArgumentException ex) {
                return SmartGwtResponse.<Map<String, Object>>asError(
                        DigitalObjectResourceApi.DISSEMINATION_MIME, "Invalid MIME type! " + mimeType);
            }
            LOG.log(Level.FINE, "filename: {0}, user mime: {1}, resolved mime: {2}, {3}/{4}", new Object[]{filename, mimeType, mime, pid, dsId});
            DigitalObjectHandler doHandler = findHandler(pid, batchId);
            DisseminationHandler dissemination = doHandler.dissemination(BinaryEditor.RAW_ID);
            DisseminationInput input = new DisseminationInput(file, filename, mime);
            dissemination.setDissemination(input, appConfig.getTypeOfStorage(), session.asFedoraLog());
            doHandler.commit();
        } finally {
            file.delete();
        }
        return new SmartGwtResponse<Map<String, Object>>(Collections.singletonMap("processId", (Object) 0L));
    }

    /**
     * Removes specified datastream from object
     *
     * @param pid     PID (required)
     * @param batchId import batch ID (optional)
     * @param dsId    Datastream ID (required)
     * @return Log result of operation
     */
    @DELETE
    @Path(DigitalObjectResourceApi.DISSEMINATION_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse deleteDissemination(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.DISSEMINATION_DATASTREAM) String dsId
    ) throws DigitalObjectException, IOException, PurgeException {

        String message = session.asFedoraLog();

        DigitalObjectHandler doHandler = findHandler(pid, batchId);
        DisseminationHandler disseminationHandler = doHandler.dissemination(dsId);

        disseminationHandler.deleteDissemination(message);

        return new SmartGwtResponse<String>(message);
    }

    @GET
    @Path(DigitalObjectResourceApi.THUMB_PATH)
    @Produces("image/*")
    public Response getThumbnail(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) throws IOException, DigitalObjectException {

        return getDissemination(pid, batchId, BinaryEditor.THUMB_ID);
    }

    @GET
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_PLAIN_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getDescriptionMetadataTxt(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) throws DigitalObjectException {

        DigitalObjectHandler handler = findHandler(pid, batchId, false);
        MetadataHandler<?> metadataHandler = handler.metadata();
        DescriptionMetadata<String> metadataAsXml = metadataHandler.getMetadataAsXml();
        StringRecord result = new StringRecord(
                metadataAsXml.getData(), metadataAsXml.getTimestamp(), metadataAsXml.getPid());
        result.setBatchId(batchId);
        result.setModel(handler.getModel().getPid());
        return result;
    }

    @GET
    @Path(DigitalObjectResourceApi.OCR_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getOcr(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) throws IOException, DigitalObjectException {

        ProArcObject fobject = findFedoraObject(pid, batchId);
        StringEditor ocrEditor = StringEditor.ocr(fobject);
        try {
            StringRecord ocr = ocrEditor.readRecord();
            ocr.setBatchId(batchId);
            return ocr;
        } catch (DigitalObjectNotFoundException ex) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.OCR_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord updateOcr(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.STRINGRECORD_CONTENT) String content
    ) throws IOException, DigitalObjectException {

        if (isLocked(pid)) {
            throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
        }

        if (timestamp == null) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing timestamp!");
        }
        ProArcObject fobject = findFedoraObject(pid, batchId, false);
        StringEditor ocrEditor = StringEditor.ocr(fobject);
        try {
            ocrEditor.write(content, timestamp, session.asFedoraLog());
            fobject.flush();
            StringRecord result = ocrEditor.readRecord();
            result.setBatchId(batchId);
            return result;
        } catch (DigitalObjectNotFoundException ex) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.GENERATE_ALTO_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<InternalExternalProcessResult> generateAlto(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid
    ) throws IOException {
        if (pid == null || pid.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing " + DigitalObjectResourceApi.ATM_ITEM_PID);
        }
        BatchParams params = new BatchParams(Collections.singletonList(pid));
        Batch batch = BatchUtils.addNewExternalBatch(this.batchManager, pid, user, Batch.EXTERNAL_PERO, params);

        InternalExternalProcess process = InternalExternalProcess.prepare(appConfig, akubraConfiguration, batch, batchManager, user, session.asFedoraLog(), session.getLocale(httpHeaders));
        InternalExternalDispatcher.getDefault().addInternalExternalProcess(process);
        InternalExternalProcessResult result = new InternalExternalProcessResult(batch.getId(), "Proces naplánován.");
        return new SmartGwtResponse<InternalExternalProcessResult>(result);
    }

    @POST
    @Path(DigitalObjectResourceApi.GENERATE_PDFA)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<InternalExternalProcessResult> generatePdfA(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid
    ) throws IOException {
        if (pid == null || pid.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing " + DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        BatchParams params = new BatchParams(Collections.singletonList(pid));
        Batch batch = BatchUtils.addNewExternalBatch(this.batchManager, pid, user, Batch.EXTERNAL_PDFA, params);

        InternalExternalProcess process = InternalExternalProcess.prepare(appConfig, akubraConfiguration, batch, batchManager, user, session.asFedoraLog(), session.getLocale(httpHeaders));
        InternalExternalDispatcher.getDefault().addInternalExternalProcess(process);
        InternalExternalProcessResult result = new InternalExternalProcessResult(batch.getId(), "Proces naplánován.");
        return new SmartGwtResponse<InternalExternalProcessResult>(result);
    }

    @POST
    @Path(DigitalObjectResourceApi.VALIDATE_OBJECT_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<InternalExternalProcessResult> validate(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws IOException, MetsExportException {
        if (pids == null || pids.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing " + DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }

        ProArcObject object = null;
        MetsContext metsContext = null;
        Set<String> pidsToValidate = new LinkedHashSet<>();
        try {
            for (String pid : pids) {
                if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                    FedoraStorage fedoraStorage = FedoraStorage.getInstance();
                    object = fedoraStorage.find(pid);
                    metsContext = MetsContext.buildFedoraContext(object, null, null, fedoraStorage, appConfig.getNdkExportOptions());
                } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                    AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                    object = akubraStorage.find(pid);
                    metsContext = MetsContext.buildAkubraContext(object, null, null, akubraStorage, appConfig.getNdkExportOptions());
                } else {
                    throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
                }

                MetsElement metsElement = getMetsElement(object, metsContext, true, false);
                List<String> PSPs = new ArrayList<>();
                if (NdkAudioPlugin.MODEL_PHONOGRAPH.equals(ExportUtils.getModel(metsElement.getModel())) || NdkAudioPlugin.MODEL_MUSICDOCUMENT.equals(ExportUtils.getModel(metsElement.getModel()))) {
                    PSPs = Collections.singletonList(metsElement.getOriginalPid());
                } else {
                    PSPs = MetsUtils.findPSPPIDs(object.getPid(), metsContext, true);
                }

                pidsToValidate.addAll(PSPs);
            }
        } catch (MetsExportException e) {
            throw new MetsExportException("Proces se nepodařilo vytvořit.", false, e);
        }

        int batchId = 0;
        for (String pid : pidsToValidate) {
            BatchParams params = new BatchParams(Collections.singletonList(pid));
            Batch batch = new BatchUtils().addNewInternalBatch(this.batchManager, pid, user, Batch.INTERNAL_VALIDATION, params);
            batchId = batch.getId();

            InternalExternalProcess process = InternalExternalProcess.prepare(appConfig, akubraConfiguration, batch, batchManager, user, session.asFedoraLog(), session.getLocale(httpHeaders));
            InternalExternalDispatcher.getDefault().addInternalExternalProcess(process);
        }
        InternalExternalProcessResult result = new InternalExternalProcessResult(batchId, "Proces(y) naplánován(y).");
        return new SmartGwtResponse<InternalExternalProcessResult>(result);
    }

    @GET
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_XML_AES_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getTechnicalMetadataAesTxt(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) throws IOException, DigitalObjectException {

        ProArcObject fobject = findFedoraObject(pid, batchId);
        RelationEditor relationEditor = new RelationEditor(fobject);
        if (relationEditor == null) {
            return null;
        }
        if (NdkAudioPlugin.MODEL_PAGE.equals(relationEditor.getModel())) {
            AesEditor aesEditor = AesEditor.ndkArchival(fobject);
            try {
                TechnicalMetadataMapper mapper = new TechnicalMetadataMapper(relationEditor.getModel(), batchId, pid, appConfig, akubraConfiguration);
                StringRecord technicalMetadata = new StringRecord(mapper.getMetadataAsXml(fobject, appConfig, relationEditor.getImportFile(), TechnicalMetadataMapper.AES), aesEditor.getLastModified(), fobject.getPid());
                technicalMetadata.setBatchId(batchId);
                technicalMetadata.setModel(relationEditor.getModel());
                return technicalMetadata;
            } catch (DigitalObjectNotFoundException ex) {
                throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
            }
        } else if (relationEditor.getModel().contains("page")) {
            MixEditor mixEditor = MixEditor.ndkArchival(fobject);
            try {
                StringRecord technicalMedata = new StringRecord(mixEditor.readAsString(), mixEditor.getLastModified(), fobject.getPid());
                technicalMedata.setBatchId(batchId);
                technicalMedata.setModel(relationEditor.getModel());
                return technicalMedata;
            } catch (DigitalObjectNotFoundException ex) {
                throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
            }
        } else {
            return null;
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_AES_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<DescriptionMetadata<Object>> updateTechnicalMetadataAes(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.TECHNICAL_CUSTOM_XMLDATA) String xmlData,
            @FormParam(DigitalObjectResourceApi.TECHNICAL_CUSTOM_JSONDATA) String jsonData
    ) throws IOException, DigitalObjectException {

        if (timestamp == null) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing timestamp!");
        }
        if (isLocked(pid)) {
            throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
        }
        if ((xmlData == null || xmlData.length() == 0) && (jsonData == null || jsonData.length() == 0)) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing technical metadata!");
        }
        ProArcObject fobject = findFedoraObject(pid, batchId, false);
        RelationEditor relationEditor = new RelationEditor(fobject);
        if (relationEditor == null) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
        String data = xmlData == null ? jsonData : xmlData;

        TechnicalMetadataMapper mapper = new TechnicalMetadataMapper(relationEditor.getModel(), batchId, pid, appConfig, akubraConfiguration);
        if (xmlData == null) {
            mapper.updateMetadataAsJson(fobject, data, timestamp, session.asFedoraLog(), TechnicalMetadataMapper.AES);
        } else {
            mapper.updateMetadataAsXml(fobject, data, timestamp, session.asFedoraLog(), TechnicalMetadataMapper.AES);
        }
        DescriptionMetadata<Object> metadata = mapper.getMetadataAsJsonObject(fobject, relationEditor.getImportFile(), TechnicalMetadataMapper.AES);
        return new SmartGwtResponse<DescriptionMetadata<Object>>(metadata);
    }

    @GET
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_AES_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<DescriptionMetadata<Object>> getTechnicalMetadataAes(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId
    ) throws IOException, DigitalObjectException {
        if (pid == null || pid.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }

        ProArcObject fobject = findFedoraObject(pid, batchId, false);
        RelationEditor editor = new RelationEditor(fobject);

        if (editor == null) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
        TechnicalMetadataMapper mapper = new TechnicalMetadataMapper(editor.getModel(), batchId, pid, appConfig, akubraConfiguration);
        DescriptionMetadata<Object> metadata = mapper.getMetadataAsJsonObject(fobject, editor.getImportFile(), TechnicalMetadataMapper.AES);
        return new SmartGwtResponse<DescriptionMetadata<Object>>(metadata);
    }

    @GET
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_XML_PREMIS_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getTechnicalMetadataPremisTxt(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) throws IOException, DigitalObjectException {

        ProArcObject fobject = findFedoraObject(pid, batchId);
        RelationEditor relationEditor = new RelationEditor(fobject);
        if (relationEditor == null) {
            return null;
        }
        if (NdkPlugin.MODEL_PAGE.equals(relationEditor.getModel()) || NdkPlugin.MODEL_NDK_PAGE.equals(relationEditor.getModel()) || OldPrintPlugin.MODEL_PAGE.equals(relationEditor.getModel())) {
            PremisEditor premisEditor = PremisEditor.ndkArchival(fobject);
            try {
                TechnicalMetadataMapper mapper = new TechnicalMetadataMapper(relationEditor.getModel(), batchId, pid, appConfig, akubraConfiguration);
                StringRecord technicalMetadata = new StringRecord(mapper.getMetadataAsXml(fobject, appConfig, relationEditor.getImportFile(), TechnicalMetadataMapper.PREMIS), premisEditor.getLastModified(), fobject.getPid());
                technicalMetadata.setBatchId(batchId);
                technicalMetadata.setModel(relationEditor.getModel());
                return technicalMetadata;
            } catch (DigitalObjectNotFoundException ex) {
                throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
            }
        } else {
            return null;
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_PREMIS_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<DescriptionMetadata<Object>> updateTechnicalMetadataPremis(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.TECHNICAL_CUSTOM_XMLDATA) String xmlData,
            @FormParam(DigitalObjectResourceApi.TECHNICAL_CUSTOM_JSONDATA) String jsonData
    ) throws IOException, DigitalObjectException {

        if (timestamp == null) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing timestamp!");
        }
        if (isLocked(pid)) {
            throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
        }
        if ((xmlData == null || xmlData.length() == 0) && (jsonData == null || jsonData.length() == 0)) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing technical metadata!");
        }
        ProArcObject fobject = findFedoraObject(pid, batchId, false);
        RelationEditor relationEditor = new RelationEditor(fobject);
        if (relationEditor == null) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
        String data = xmlData == null ? jsonData : xmlData;

        TechnicalMetadataMapper mapper = new TechnicalMetadataMapper(relationEditor.getModel(), batchId, pid, appConfig, akubraConfiguration);
        if (xmlData == null) {
            mapper.updateMetadataAsJson(fobject, data, timestamp, session.asFedoraLog(), TechnicalMetadataMapper.PREMIS);
        } else {
            mapper.updateMetadataAsXml(fobject, data, timestamp, session.asFedoraLog(), TechnicalMetadataMapper.PREMIS);
        }
        DescriptionMetadata<Object> metadata = mapper.getMetadataAsJsonObject(fobject, relationEditor.getImportFile(), TechnicalMetadataMapper.PREMIS);
        return new SmartGwtResponse<DescriptionMetadata<Object>>(metadata);
    }


    @GET
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_XML_CODING_HISTORY_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getCodingHistoryTxt(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) throws IOException, DigitalObjectException {

        ProArcObject fobject = findFedoraObject(pid, batchId);
        RelationEditor relationEditor = new RelationEditor(fobject);
        if (relationEditor == null) {
            return null;
        }
        if (NdkAudioPlugin.MODEL_PAGE.equals(relationEditor.getModel())) {
            CodingHistoryEditor codingHistoryEditor = CodingHistoryEditor.ndkArchival(fobject);
            try {
                TechnicalMetadataMapper mapper = new TechnicalMetadataMapper(relationEditor.getModel(), batchId, pid, appConfig, akubraConfiguration);
                StringRecord technicalMetadata = new StringRecord(mapper.getMetadataAsXml(fobject, appConfig, relationEditor.getImportFile(), TechnicalMetadataMapper.CODING_HISTORY), codingHistoryEditor.getLastModified(), fobject.getPid());
                technicalMetadata.setBatchId(batchId);
                technicalMetadata.setModel(relationEditor.getModel());
                return technicalMetadata;
            } catch (DigitalObjectNotFoundException ex) {
                throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
            }
        } else {
            return null;
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_CODING_HISTORY_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<DescriptionMetadata<Object>> updateCodingHistory(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.TECHNICAL_CUSTOM_XMLDATA) String xmlData,
            @FormParam(DigitalObjectResourceApi.TECHNICAL_CUSTOM_JSONDATA) String jsonData
    ) throws IOException, DigitalObjectException {

        if (timestamp == null) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing timestamp!");
        }
        if (isLocked(pid)) {
            throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
        }
        if ((xmlData == null || xmlData.length() == 0) && (jsonData == null || jsonData.length() == 0)) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing technical metadata!");
        }
        ProArcObject fobject = findFedoraObject(pid, batchId, false);
        RelationEditor relationEditor = new RelationEditor(fobject);
        if (relationEditor == null) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
        String data = xmlData == null ? jsonData : xmlData;

        TechnicalMetadataMapper mapper = new TechnicalMetadataMapper(relationEditor.getModel(), batchId, pid, appConfig, akubraConfiguration);
        if (xmlData == null) {
            mapper.updateMetadataAsJson(fobject, data, timestamp, session.asFedoraLog(), TechnicalMetadataMapper.CODING_HISTORY);
        } else {
            mapper.updateMetadataAsXml(fobject, data, timestamp, session.asFedoraLog(), TechnicalMetadataMapper.CODING_HISTORY);
        }
        DescriptionMetadata<Object> metadata = mapper.getMetadataAsJsonObject(fobject, relationEditor.getImportFile(), TechnicalMetadataMapper.CODING_HISTORY);
        return new SmartGwtResponse<DescriptionMetadata<Object>>(metadata);
    }

    @GET
    @Path(DigitalObjectResourceApi.TECHNICALMETADATA_CODING_HISTORY_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<DescriptionMetadata<Object>> getCodingHistoryMetadata(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId
    ) throws IOException, DigitalObjectException {
        if (pid == null || pid.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }

        ProArcObject fobject = findFedoraObject(pid, batchId, false);
        RelationEditor editor = new RelationEditor(fobject);

        if (editor == null) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
        TechnicalMetadataMapper mapper = new TechnicalMetadataMapper(editor.getModel(), batchId, pid, appConfig, akubraConfiguration);
        DescriptionMetadata<Object> metadata = mapper.getMetadataAsJsonObject(fobject, editor.getImportFile(), TechnicalMetadataMapper.CODING_HISTORY);
        return new SmartGwtResponse<DescriptionMetadata<Object>>(metadata);
    }

    @GET
    @Path(DigitalObjectResourceApi.PRIVATENOTE_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getPrivateNote(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
    ) throws IOException, DigitalObjectException {

        ProArcObject fobject = findFedoraObject(pid, batchId);
        StringEditor editor = StringEditor.privateNote(fobject);
        try {
            StringRecord content = editor.readRecord();
            content.setBatchId(batchId);
            return content;
        } catch (DigitalObjectNotFoundException ex) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.PRIVATENOTE_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord updatePrivateNote(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.STRINGRECORD_CONTENT) String content
    ) throws IOException, DigitalObjectException {

        if (timestamp == null) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing timestamp!");
        }
        ProArcObject fobject = findFedoraObject(pid, batchId, false);
        StringEditor editor = StringEditor.privateNote(fobject);
        editor.write(content, timestamp, session.asFedoraLog());
        fobject.flush();
        StringRecord result = editor.readRecord();
        result.setBatchId(batchId);
        return result;
    }

    /**
     * Gets digital object administration and technical data.
     *
     * @param pid     PID (required)
     * @param batchId import batch ID (optional)
     * @return digital object dissemination
     * @throws IOException
     */
    @GET
    @Path(DigitalObjectResourceApi.ATM_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<AtmItem> getAtm(
            @QueryParam(DigitalObjectResourceApi.ATM_ITEM_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.ATM_ITEM_BATCHID) Integer batchId
    ) throws IOException, DigitalObjectException, FedoraClientException {

        if (pid == null) {
            return new SmartGwtResponse<AtmItem>();
        }
        ProArcObject fobject = findFedoraObject(pid, batchId);
        Locale locale = session.getLocale(httpHeaders);
        if (fobject instanceof RemoteObject) {
            FedoraStorage storage = FedoraStorage.getInstance(appConfig);
            AtmEditor editor = new AtmEditor(fobject, storage.getSearch(locale));
            AtmItem atm = editor.read();
            atm.setBatchId(batchId);
            return new SmartGwtResponse<AtmItem>(atm);
        } else if (fobject instanceof AkubraObject) {
            AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
            AtmEditor editor = new AtmEditor(fobject, akubraStorage.getSearch(locale));
            AtmItem atm = editor.read();
            atm.setBatchId(batchId);
            return new SmartGwtResponse<AtmItem>(atm);
        } else {
            return new SmartGwtResponse<>();
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.ATM_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<AtmItem> updateAtm(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) Set<String> pids,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_OWNER) String owner,
            @FormParam(DigitalObjectResourceApi.ATM_ITEM_DEVICE) String deviceId,
            @FormParam(DigitalObjectResourceApi.ATM_ITEM_ORGANIZATION) String organization,
            @FormParam(DigitalObjectResourceApi.ATM_ITEM_STATUS) String status,
            @FormParam(DigitalObjectResourceApi.ATM_ITEM_USER) String userName,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String model,
            @FormParam(DigitalObjectResourceApi.ATM_ITEM_DONATOR) String donator,
            @FormParam(DigitalObjectResourceApi.ATM_ITEM_ARCHIVAL_COPIES) String archivalCopiesPath
    ) throws IOException, DigitalObjectException, AppConfigurationException {

        if (isLocked(transform(pids))) {
            throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
        }

        ArrayList<AtmItem> result = new ArrayList<AtmItem>(pids.size());
        Locale locale = session.getLocale(httpHeaders);
        SearchView search = null;
        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            FedoraStorage storage = FedoraStorage.getInstance(appConfig);
            search = storage.getSearch(locale);
        } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
            search = akubraStorage.getSearch(locale);
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }

        if (userName != null && !userName.isEmpty()) {
            UserProfile processor = UserUtil.getDefaultManger().find(userName);
            if (processor != null) {
                organization = processor.getOrganization();
            }
        }

        for (String pid : pids) {
            ProArcObject fobject = findFedoraObject(pid, batchId);
            AtmEditor editor = new AtmEditor(fobject, search);
            editor.write(deviceId, organization, userName, status, donator, archivalCopiesPath, session.asFedoraLog(), user.getRole());
            fobject.flush();
            if (!(model != null && model.length() > 0 && model.contains("page"))) {
                editor.setChild(pid, organization, userName, status, donator, appConfig, akubraConfiguration, search, session.asFedoraLog());
            }
            AtmItem atm = editor.read();
            atm.setBatchId(batchId);
            result.add(atm);
        }
        return new SmartGwtResponse<AtmItem>(result);
    }

    @POST
    @Path(DigitalObjectResourceApi.URNNBN_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<UrnNbnResult> registerUrnNbn(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids,
            @FormParam(DigitalObjectResourceApi.URNNBN_RESOLVER) String resolverId,
            @FormParam(DigitalObjectResourceApi.URNNBN_HIERARCHY) @DefaultValue("true") boolean hierarchy
    ) {

        if (isLocked(pids)) {
            throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
        }
        List<UrnNbnResult> result = new LinkedList<UrnNbnResult>();
        if (!pids.isEmpty()) {
            UrnNbnConfiguration config = appConfig.getUrnNbnConfiguration();
            ResolverConfiguration resolverConfig = null;
            if (resolverId == null) {
                // no resolver passed, try the first registered
                List<ResolverConfiguration> confs = config.getResolverConfigurations();
                if (!confs.isEmpty()) {
                    resolverConfig = confs.get(0);
                }
            } else {
                resolverConfig = config.findResolverConfiguration(resolverId);
            }
            if (resolverConfig == null) {
                throw RestException.plainText(Status.BAD_REQUEST,
                        String.format("Unknown property '%s' = '%s'. Check server configuration!",
                                DigitalObjectResourceApi.URNNBN_RESOLVER, resolverId));
            }
            UrnNbnService service = new UrnNbnService(appConfig, resolverConfig);
            UrnNbnStatusHandler status = service.register(pids, hierarchy);
            for (Entry<String, PidResult> entry : status.getPids().entrySet()) {
                PidResult pidResult = entry.getValue();
                String entryPid = entry.getKey();
                for (StatusEntry statusEntry : pidResult.getErrors()) {
                    result.add(new UrnNbnResult(entryPid, statusEntry, false, pidResult.getPid()));
                }
                for (StatusEntry statusEntry : pidResult.getWarnings()) {
                    result.add(new UrnNbnResult(entryPid, statusEntry, true, pidResult.getPid()));
                }
                if (pidResult.getUrnNbn() != null) {
                    result.add(new UrnNbnResult(entryPid, pidResult.getUrnNbn(), pidResult.getPid()));
                }
            }
        }
        return new SmartGwtResponse<UrnNbnResult>(result);
    }

    @POST
    @Path(DigitalObjectResourceApi.URNNBN_PATH + "/" + DigitalObjectResourceApi.URNNBN_INVALIDATE_LOCAL_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<UrnNbnResult> invalidateLocalUrnNbn(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids,
            @FormParam(DigitalObjectResourceApi.URNNBN_HIERARCHY) @DefaultValue("true") boolean hierarchy
    ) {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_CZIDLO_FUNCTION);

        if (isLocked(pids)) {
            throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
        }
        List<UrnNbnResult> result = new LinkedList<UrnNbnResult>();
        if (!pids.isEmpty()) {
            UrnNbnService service = new UrnNbnService(appConfig);
            UrnNbnStatusHandler status = service.invalidateValue(pids);
            for (Entry<String, PidResult> entry : status.getPids().entrySet()) {
                PidResult pidResult = entry.getValue();
                String entryPid = entry.getKey();
                for (StatusEntry statusEntry : pidResult.getErrors()) {
                    result.add(new UrnNbnResult(entryPid, statusEntry, false, pidResult.getPid()));
                }
                for (StatusEntry statusEntry : pidResult.getWarnings()) {
                    result.add(new UrnNbnResult(entryPid, statusEntry, true, pidResult.getPid()));
                }
                if (pidResult.getUrnNbn() != null) {
                    result.add(new UrnNbnResult(entryPid, pidResult.getUrnNbn(), pidResult.getPid()));
                }
            }
        }
        return new SmartGwtResponse<UrnNbnResult>(result);
    }

    @POST
    @Path(DigitalObjectResourceApi.URNNBN_PATH + "/" + DigitalObjectResourceApi.URNNBN_CREATE_SUCCESSOR_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<UrnNbnResult> createSuccessorUrnNbn(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids,
            @FormParam(DigitalObjectResourceApi.URNNBN_RESOLVER) String resolverId,
            @FormParam(DigitalObjectResourceApi.URNNBN_HIERARCHY) @DefaultValue("true") boolean hierarchy
    ) {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_CZIDLO_FUNCTION);


        if (isLocked(pids)) {
            throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
        }
        List<UrnNbnResult> result = new LinkedList<UrnNbnResult>();
        if (!pids.isEmpty()) {
            UrnNbnConfiguration config = appConfig.getUrnNbnConfiguration();
            ResolverConfiguration resolverConfig = null;
            if (resolverId == null) {
                // no resolver passed, try the first registered
                List<ResolverConfiguration> confs = config.getResolverConfigurations();
                if (!confs.isEmpty()) {
                    resolverConfig = confs.get(0);
                }
            } else {
                resolverConfig = config.findResolverConfiguration(resolverId);
            }
            if (resolverConfig == null) {
                throw RestException.plainText(Status.BAD_REQUEST,
                        String.format("Unknown property '%s' = '%s'. Check server configuration!",
                                DigitalObjectResourceApi.URNNBN_RESOLVER, resolverId));
            }
            UrnNbnService service = new UrnNbnService(appConfig, resolverConfig);
            UrnNbnStatusHandler status = service.createSuccessor(pids, hierarchy);
            for (Entry<String, PidResult> entry : status.getPids().entrySet()) {
                PidResult pidResult = entry.getValue();
                String entryPid = entry.getKey();
                for (StatusEntry statusEntry : pidResult.getErrors()) {
                    result.add(new UrnNbnResult(entryPid, statusEntry, false, pidResult.getPid()));
                }
                for (StatusEntry statusEntry : pidResult.getWarnings()) {
                    result.add(new UrnNbnResult(entryPid, statusEntry, true, pidResult.getPid()));
                }
                if (pidResult.getUrnNbn() != null) {
                    result.add(new UrnNbnResult(entryPid, pidResult.getUrnNbn(), pidResult.getPid()));
                }
            }
        }
        return new SmartGwtResponse<UrnNbnResult>(result);
    }

    @POST
    @Path(DigitalObjectResourceApi.URNNBN_PATH + "/" + DigitalObjectResourceApi.URNNBN_INVALIDATE_REMOTE_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<UrnNbnResult> invalidateRemoteUrnNbn(
            @FormParam(DigitalObjectResourceApi.URNNBN_VALUE_TO_DEACTIVATE) String urnNbnValue,
            @FormParam(DigitalObjectResourceApi.URNNBN_RESOLVER) String resolverId,
            @FormParam(DigitalObjectResourceApi.URNNBN_HIERARCHY) @DefaultValue("true") boolean hierarchy
    ) {
        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_CZIDLO_FUNCTION);

        List<UrnNbnResult> result = new LinkedList<UrnNbnResult>();
        if (urnNbnValue != null && !urnNbnValue.isEmpty()) {
            UrnNbnConfiguration config = appConfig.getUrnNbnConfiguration();
            ResolverConfiguration resolverConfig = null;
            if (resolverId == null) {
                // no resolver passed, try the first registered
                List<ResolverConfiguration> confs = config.getResolverConfigurations();
                if (!confs.isEmpty()) {
                    resolverConfig = confs.get(0);
                }
            } else {
                resolverConfig = config.findResolverConfiguration(resolverId);
            }
            if (resolverConfig == null) {
                throw RestException.plainText(Status.BAD_REQUEST,
                        String.format("Unknown property '%s' = '%s'. Check server configuration!",
                                DigitalObjectResourceApi.URNNBN_RESOLVER, resolverId));
            }
            UrnNbnService service = new UrnNbnService(appConfig, resolverConfig);
            UrnNbnStatusHandler status = service.invalidateRemoteValue(urnNbnValue, hierarchy);
            for (Entry<String, PidResult> entry : status.getPids().entrySet()) {
                PidResult pidResult = entry.getValue();
                String entryPid = entry.getKey();
                for (StatusEntry statusEntry : pidResult.getErrors()) {
                    result.add(new UrnNbnResult(entryPid, statusEntry, false, pidResult.getPid()));
                }
                for (StatusEntry statusEntry : pidResult.getWarnings()) {
                    result.add(new UrnNbnResult(entryPid, statusEntry, true, pidResult.getPid()));
                }
                if (pidResult.getUrnNbn() != null) {
                    result.add(new UrnNbnResult(entryPid, pidResult.getUrnNbn(), pidResult.getPid()));
                }
            }
        }
        return new SmartGwtResponse<UrnNbnResult>(result);
    }

    @POST
    @Path(DigitalObjectResourceApi.URNNBN_PATH + "/" + DigitalObjectResourceApi.URNNBN_REGISTER_AGAIN_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<UrnNbnResult> registerAgainUrnNbn(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids,
            @FormParam(DigitalObjectResourceApi.URNNBN_RESOLVER) String resolverId,
            @FormParam(DigitalObjectResourceApi.URNNBN_HIERARCHY) @DefaultValue("true") boolean hierarchy
    ) {
        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_CZIDLO_FUNCTION);

        if (isLocked(pids)) {
            throw RestException.plainText(Status.BAD_REQUEST, returnLocalizedMessage(ERR_IS_LOCKED));
        }
        List<UrnNbnResult> result = new LinkedList<UrnNbnResult>();
        if (!pids.isEmpty()) {
            UrnNbnConfiguration config = appConfig.getUrnNbnConfiguration();
            ResolverConfiguration resolverConfig = null;
            if (resolverId == null) {
                // no resolver passed, try the first registered
                List<ResolverConfiguration> confs = config.getResolverConfigurations();
                if (!confs.isEmpty()) {
                    resolverConfig = confs.get(0);
                }
            } else {
                resolverConfig = config.findResolverConfiguration(resolverId);
            }
            if (resolverConfig == null) {
                throw RestException.plainText(Status.BAD_REQUEST,
                        String.format("Unknown property '%s' = '%s'. Check server configuration!",
                                DigitalObjectResourceApi.URNNBN_RESOLVER, resolverId));
            }
            UrnNbnService service = new UrnNbnService(appConfig, resolverConfig);
            UrnNbnStatusHandler status = service.registerAgain(pids, hierarchy);
            for (Entry<String, PidResult> entry : status.getPids().entrySet()) {
                PidResult pidResult = entry.getValue();
                String entryPid = entry.getKey();
                for (StatusEntry statusEntry : pidResult.getErrors()) {
                    result.add(new UrnNbnResult(entryPid, statusEntry, false, pidResult.getPid()));
                }
                for (StatusEntry statusEntry : pidResult.getWarnings()) {
                    result.add(new UrnNbnResult(entryPid, statusEntry, true, pidResult.getPid()));
                }
                if (pidResult.getUrnNbn() != null) {
                    result.add(new UrnNbnResult(entryPid, pidResult.getUrnNbn(), pidResult.getPid()));
                }
            }
        }
        return new SmartGwtResponse<UrnNbnResult>(result);
    }

    @POST
    @Path(DigitalObjectResourceApi.URNNBN_PATH + "/" + DigitalObjectResourceApi.URNNBN_UPDATE_IDENTIFIER_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<UrnNbnResult> updateIdentifierUrnNbn(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids,
            @FormParam(DigitalObjectResourceApi.URNNBN_IDENTIFIER) String identifier,
            @FormParam(DigitalObjectResourceApi.URNNBN_OPERATION) String operation,
            @FormParam(DigitalObjectResourceApi.URNNBN_RESOLVER) String resolverId,
            @FormParam(DigitalObjectResourceApi.URNNBN_HIERARCHY) @DefaultValue("true") boolean hierarchy
    ) {
        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_CZIDLO_FUNCTION);

        if (isLocked(pids)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_IS_LOCKED));
        }
        if (operation == null || operation.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.URNNBN_OPERATION));
        }
        if (identifier == null || identifier.isEmpty()) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.URNNBN_IDENTIFIER));
        }

        List<UrnNbnResult> result = new LinkedList<UrnNbnResult>();
        if (!pids.isEmpty()) {
            UrnNbnConfiguration config = appConfig.getUrnNbnConfiguration();
            ResolverConfiguration resolverConfig = null;
            if (resolverId == null) {
                // no resolver passed, try the first registered
                List<ResolverConfiguration> confs = config.getResolverConfigurations();
                if (!confs.isEmpty()) {
                    resolverConfig = confs.get(0);
                }
            } else {
                resolverConfig = config.findResolverConfiguration(resolverId);
            }
            if (resolverConfig == null) {
                throw RestException.plainText(Status.BAD_REQUEST,
                        String.format("Unknown property '%s' = '%s'. Check server configuration!",
                                DigitalObjectResourceApi.URNNBN_RESOLVER, resolverId));
            }
            UrnNbnService service = new UrnNbnService(appConfig, resolverConfig);
            UrnNbnStatusHandler status = service.updateCzidloRecord(pids, identifier, operation, hierarchy);
            for (Entry<String, PidResult> entry : status.getPids().entrySet()) {
                PidResult pidResult = entry.getValue();
                String entryPid = entry.getKey();
                for (StatusEntry statusEntry : pidResult.getErrors()) {
                    result.add(new UrnNbnResult(entryPid, statusEntry, false, pidResult.getPid()));
                }
                for (StatusEntry statusEntry : pidResult.getWarnings()) {
                    result.add(new UrnNbnResult(entryPid, statusEntry, true, pidResult.getPid()));
                }
                if (pidResult.getUrnNbn() != null) {
                    if (pidResult.getInfos() == null || pidResult.getInfos().isEmpty()) {
                        result.add(new UrnNbnResult(entryPid, pidResult.getUrnNbn(), pidResult.getPid()));
                    } else {
                        for (StatusEntry statusEntry : pidResult.getInfos()) {
                            result.add(new UrnNbnResult(entryPid, pidResult.getUrnNbn(), pidResult.getPid(), statusEntry));
                        }
                    }

                }
            }
        }
        return new SmartGwtResponse<UrnNbnResult>(result);
    }

    @POST
    @Path(DigitalObjectResourceApi.LOCK_OBJECT_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> lockObject(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_LOCK_OBJECT_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        for (String pid : pids) {
            LockObject lockObject = new LockObject(appConfig, akubraConfiguration, pid, user.getUserName());
            lockObject.findObjects();
            LockObject.LockObjectResult result = lockObject.setLocked();
            if (result != null) {
                if (!result.getEx().getMyMessage().equals(LockObject.MSG_ALREADY_LOCKED)) {
                    lockObject.setUnlocked();
                }
                return returnFunctionError(ERR_UNLOCKING_OBJECT_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.UNLOCK_OBJECT_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> unlockObject(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_UNLOCK_OBJECT_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        for (String pid : pids) {
            LockObject lockObject = new LockObject(appConfig, akubraConfiguration, pid, user.getUserName());
            lockObject.findObjects();
            LockObject.LockObjectResult result = lockObject.setUnlocked();
            if (result != null) {
                if (!result.getEx().getMyMessage().equals(LockObject.MSG_ALREADY_UNLOCKED)) {
                    lockObject.setLocked();
                }
                return returnFunctionError(ERR_UNLOCKING_OBJECT_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.COPYOBJECT_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> copyObject(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pidOld,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId
    ) throws DigitalObjectException, IOException, FedoraClientException {
        if (isLocked(pidOld)) {
            return returnValidationError(ERR_IS_LOCKED);
        }

        CopyObject copyObject = new CopyObject(appConfig, akubraConfiguration, user, pidOld, modelId);
        List<SearchViewItem> items = null;
        try {
            items = copyObject.copy();
            copyObject.copyMods();
        } catch (DigitalObjectValidationException ex) {
            return toValidationError(ex, session.getLocale(httpHeaders));
        }
        if (items != null && items.size() > 0) {
            SearchViewItem item = items.get(0);
            return search(item.getPid());
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.GENERATE_JP2_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> generateJp2(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.GENERATE_TYPE) String type,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId
    ) throws DigitalObjectException {


        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_PAGE_TO_NDK_PAGE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changePageToNdkPage(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }

        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }

        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_PAGE, NdkPlugin.MODEL_NDK_PAGE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(null);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_PAGE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }


    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_PAGE_TO_PAGE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkPageToPage(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_NDK_PAGE, NdkPlugin.MODEL_PAGE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(null);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_NDK_PAGE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_PAGE_TO_STT_PAGE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changePageToSttPage(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_PAGE, OldPrintPlugin.MODEL_PAGE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(null);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_PAGE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_PAGE_TO_PAGE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeSttPageToPage(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, OldPrintPlugin.MODEL_PAGE, NdkPlugin.MODEL_PAGE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(null);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), OldPrintPlugin.MODEL_PAGE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_PAGE_TO_NDK_PAGE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeSttPageToNdkPage(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, OldPrintPlugin.MODEL_PAGE, NdkPlugin.MODEL_NDK_PAGE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(null);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), OldPrintPlugin.MODEL_PAGE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_PAGE_TO_STT_PAGE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkPageToSttPage(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_NDK_PAGE, OldPrintPlugin.MODEL_PAGE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(null);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_NDK_PAGE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_CLIPPINGS_VOLUME_TO_NDK_MONOGRAPH_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeClippingsVolumeToNdkMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_VOLUME, NdkPlugin.MODEL_MONOGRAPHVOLUME);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_VOLUME);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_VOLUME_TO_CLIPPINGS_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographVolumeToClippingsVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_MONOGRAPHVOLUME, CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_VOLUME);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_MONOGRAPHVOLUME);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_TITLE_TO_CLIPPINGS_TITLE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographTitleToClippingsTitle(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_MONOGRAPHTITLE, CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_TITLE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(null);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_MONOGRAPHTITLE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }


    @POST
    @Path(DigitalObjectResourceApi.CHANGE_CLIPPINGS_TITLE_TO_NDK_MONOGRAPH_TITLE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeClippingsTitleToNdkMonographTitle(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_TITLE, NdkPlugin.MODEL_MONOGRAPHTITLE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(null);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_TITLE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_K4_PERIODICAL_TO_NDK_PERIODICAL)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeK4PeriodicalToNdkPeriodical(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, K4Plugin.MODEL_PERIODICAL, NdkPlugin.MODEL_PERIODICAL);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(null);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), K4Plugin.MODEL_PERIODICAL);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_K4_PERIODICAL_VOLUME_TO_NDK_PERIODICAL_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeK4PeriodicalVolumeToNdkPeriodicalVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, K4Plugin.MODEL_PERIODICALVOLUME, NdkPlugin.MODEL_PERIODICALVOLUME);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), K4Plugin.MODEL_PERIODICALVOLUME);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_K4_PERIODICAL_ISSUE_TO_NDK_PERIODICAL_ISSUE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeK4PeriodicalIssueToNdkPeriodicalIssue(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, K4Plugin.MODEL_PERIODICALITEM, NdkPlugin.MODEL_PERIODICALISSUE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), K4Plugin.MODEL_PERIODICALITEM);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_K4_MONOGRAPH_TO_NDK_MONOGRAPH_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeK4MonographToNdkMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, K4Plugin.MODEL_MONOGRAPH, NdkPlugin.MODEL_MONOGRAPHVOLUME);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), K4Plugin.MODEL_MONOGRAPH);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_K4_MONOGRAPH_UNIT_TO_NDK_MONOGRAPH_VOLUME)
     @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeK4MonographUnitToNdkMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, K4Plugin.MODEL_MONOGRAPHUNIT, NdkPlugin.MODEL_MONOGRAPHVOLUME);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), K4Plugin.MODEL_MONOGRAPHUNIT);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_K4_MONOGRAPH_UNIT_TO_NDK_MONOGRAPH_UNIT)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeK4MonographUnitToNdkMonographUnit(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, K4Plugin.MODEL_MONOGRAPHUNIT, NdkPlugin.MODEL_MONOGRAPHUNIT);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), K4Plugin.MODEL_MONOGRAPHUNIT);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_TITLE_TO_NDK_MONOGRAPH_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographTitleToNdkMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_MONOGRAPHTITLE, NdkPlugin.MODEL_MONOGRAPHVOLUME);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_MONOGRAPHTITLE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_UNIT_TO_NDK_MONOGRAPH_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographUnitToNdkMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_MONOGRAPHUNIT, NdkPlugin.MODEL_MONOGRAPHVOLUME);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_MONOGRAPHUNIT);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_VOLUME_TO_NDK_MONOGRAPH_TITLE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographVolumeToNdkMonographTitle(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_MONOGRAPHVOLUME, NdkPlugin.MODEL_MONOGRAPHTITLE);
            changeModels.findObjects(false);

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_MONOGRAPHVOLUME);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_VOLUME_TO_NDK_MONOGRAPH_UNIT)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographVolumeToNdkMonographUnit(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_MONOGRAPHVOLUME, NdkPlugin.MODEL_MONOGRAPHUNIT);
            changeModels.findObjects(false);

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_MONOGRAPHVOLUME);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MUSICSHEET_TO_STT_MUSICSHEET)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMusicsheetToOldprintMusicsheet(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_SHEETMUSIC, OldPrintPlugin.MODEL_SHEETMUSIC);
            changeModels.findObjects(false);

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_SHEETMUSIC);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_MUSICSHEET_TO_NDK_MUSICSHEET)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeSttMusicsheetToNdkMusicsheet(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, OldPrintPlugin.MODEL_SHEETMUSIC, NdkPlugin.MODEL_SHEETMUSIC);
            changeModels.findObjects(false);

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_SHEETMUSIC);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }


    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_CHAPTER_TO_STT_CHAPTER)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkChapterToOldprintChapter(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_CHAPTER, OldPrintPlugin.MODEL_CHAPTER);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_CHAPTER);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_CHAPTER_TO_NDK_CHAPTER)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldprintChapterToNdkChapter(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, OldPrintPlugin.MODEL_CHAPTER, NdkPlugin.MODEL_CHAPTER);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), OldPrintPlugin.MODEL_CHAPTER);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_PICTURE_TO_STT_GRAPHIC)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkPictureToOldprintGraphic(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_PICTURE, OldPrintPlugin.MODEL_GRAPHICS);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_PICTURE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_GRAPHIC_TO_NDK_PICTURE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldprintGraphicToNdkPicture(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, OldPrintPlugin.MODEL_GRAPHICS, NdkPlugin.MODEL_PICTURE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), OldPrintPlugin.MODEL_GRAPHICS);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_CARTOGRAPHIC_TO_STT_CARTOGRAPHIC)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkCartographicToOldprintCartographic(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_CARTOGRAPHIC, OldPrintPlugin.MODEL_CARTOGRAPHIC);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_CARTOGRAPHIC);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_CARTOGRAPHIC_TO_NDK_CARTOGRAPHIC)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldprintCartographicToNdkCartographic(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, OldPrintPlugin.MODEL_CARTOGRAPHIC, NdkPlugin.MODEL_CARTOGRAPHIC);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), OldPrintPlugin.MODEL_CARTOGRAPHIC);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_MONOGRAPH_TO_STT_MONOGRAPH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographVolumeToOldprintMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_MONOGRAPHVOLUME, OldPrintPlugin.MODEL_MONOGRAPHVOLUME);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_MONOGRAPHVOLUME);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_MONOGRAPH_VOLUME_TO_STT_MONOGRAPH_UNIT)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldprintMonographVolumeToOldprintMonographUnit(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, OldPrintPlugin.MODEL_MONOGRAPHVOLUME, OldPrintPlugin.MODEL_MONOGRAPHUNIT);
            changeModels.findObjects(false);

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), OldPrintPlugin.MODEL_MONOGRAPHVOLUME);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_MONOGRAPH_UNIT_TO_STT_MONOGRAPH_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldprintMonographUnitToOldprintMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, OldPrintPlugin.MODEL_MONOGRAPHUNIT, OldPrintPlugin.MODEL_MONOGRAPHVOLUME);
            changeModels.findObjects(false);

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), OldPrintPlugin.MODEL_MONOGRAPHUNIT);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_MONOGRAPH_TO_NDK_MONOGRAPH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldPrintMonographVolumeToNdkMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, OldPrintPlugin.MODEL_MONOGRAPHVOLUME, NdkPlugin.MODEL_MONOGRAPHVOLUME);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), OldPrintPlugin.MODEL_MONOGRAPHVOLUME);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_SUPPLEMENT_TO_STT_SUPPLEMENT)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkMonographSupplementToOldPrintSupplement(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, OldPrintPlugin.MODEL_SUPPLEMENT);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_SUPPLEMENT_TO_NDK_SUPPLEMENT)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldPrintSupplementToNdkMonographSupplement(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, OldPrintPlugin.MODEL_SUPPLEMENT, NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), OldPrintPlugin.MODEL_SUPPLEMENT);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_MONOGRAPH_TO_STT_GRAPHIC)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldPrintMonographVolumeToOldprintGraphic(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, OldPrintPlugin.MODEL_MONOGRAPHVOLUME, OldPrintPlugin.MODEL_GRAPHICS);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), OldPrintPlugin.MODEL_MONOGRAPHVOLUME);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_GRAPHIC_TO_STT_MONOGRAPH_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldPrintGraphicToOldPrintMonographVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, OldPrintPlugin.MODEL_GRAPHICS, OldPrintPlugin.MODEL_MONOGRAPHVOLUME);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), OldPrintPlugin.MODEL_GRAPHICS);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_STT_MONOGRAPH_TO_STT_MUSICSHEET)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeOldPrintMonographVolumeToOldprintMusicSheet(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, OldPrintPlugin.MODEL_MONOGRAPHVOLUME, OldPrintPlugin.MODEL_SHEETMUSIC);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), OldPrintPlugin.MODEL_MONOGRAPHVOLUME);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_PERIODICAL_TO_NDK_EPERIODICAL)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkPeriodicalToNdkEPeriodical(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_PERIODICAL, NdkEbornPlugin.MODEL_EPERIODICAL);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_PERIODICAL);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_PERIODICAL_VOLUME_TO_NDK_EPERIODICAL_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkPeriodicalVolumeToNdkEPeriodicalVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_PERIODICALVOLUME, NdkEbornPlugin.MODEL_EPERIODICALVOLUME);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_PERIODICALVOLUME);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_PERIODICAL_ISSUE_TO_NDK_EPERIODICAL_ISSUE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkPeriodicalIssueToNdkEPeriodicalIssue(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_PERIODICALISSUE, NdkEbornPlugin.MODEL_EPERIODICALISSUE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_PERIODICALISSUE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_PERIODICAL_SUPPLEMENT_TO_NDK_EPERIODICAL_SUPPLEMENT)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkPeriodicalSupplementToNdkEPeriodicalSupplement(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_PERIODICALSUPPLEMENT, NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_PERIODICALSUPPLEMENT);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_ARTICLE_TO_NDK_EARTICLE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkArticleToNdkEArticle(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkPlugin.MODEL_ARTICLE, NdkEbornPlugin.MODEL_EARTICLE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkPlugin.MODEL_ARTICLE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_BDM_ARTICLE_TO_NDK_EARTICLE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeBdmArticleToNdkEArticle(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, BornDigitalModsPlugin.MODEL_ARTICLE, NdkEbornPlugin.MODEL_EARTICLE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), BornDigitalModsPlugin.MODEL_ARTICLE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_EPERIODICAL_TO_NDK_PERIODICAL)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkEPeriodicalToNdkPeriodical(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkEbornPlugin.MODEL_EPERIODICAL, NdkPlugin.MODEL_PERIODICAL);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkEbornPlugin.MODEL_EPERIODICAL);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_EPERIODICAL_VOLUME_TO_NDK_PERIODICAL_VOLUME)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkEPeriodicalVolumeToNdkPeriodicalVolume(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkEbornPlugin.MODEL_EPERIODICALVOLUME, NdkPlugin.MODEL_PERIODICALVOLUME);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkEbornPlugin.MODEL_EPERIODICALVOLUME);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_EPERIODICAL_ISSUE_TO_NDK_PERIODICAL_ISSUE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkEPeriodicalIssueToNdkPeriodicalIssue(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkEbornPlugin.MODEL_EPERIODICALISSUE, NdkPlugin.MODEL_PERIODICALISSUE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkEbornPlugin.MODEL_EPERIODICALISSUE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_EPERIODICAL_SUPPLEMENT_TO_NDK_PERIODICAL_SUPPLEMENT)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkEPeriodicalSupplementToNdkPeriodicalSupplement(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT, NdkPlugin.MODEL_PERIODICALSUPPLEMENT);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_EARTICLE_TO_NDK_ARTICLE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkEArticleToNdkArticle(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkEbornPlugin.MODEL_EARTICLE, NdkPlugin.MODEL_ARTICLE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkEbornPlugin.MODEL_EARTICLE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_NDK_EARTICLE_TO_BDM_ARTICLE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeNdkEArticleToBdmArticle(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_CHANGE_MODEL_FUNCTION);

        if (pids == null || pids.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_PID);
        }
        if (isLocked(pids)) {
            return returnValidationError(ERR_IS_LOCKED);
        }
        for (String pid : pids) {
            ChangeModels changeModels = new ChangeModels(appConfig, akubraConfiguration, pid, NdkEbornPlugin.MODEL_EARTICLE, BornDigitalModsPlugin.MODEL_ARTICLE);
            changeModels.findObjects();

            if (isLocked(changeModels.getPids())) {
                return returnValidationError(ERR_IS_LOCKED);
            }

            String parentPid = changeModels.findRootObject();
            ChangeModels.ChangeModelResult result = changeModels.changeModelsAndRepairMetadata(parentPid);
            if (result != null) {
                changeModels.changeModelBack(result.getPid(), NdkEbornPlugin.MODEL_EARTICLE);
                return returnFunctionError(ERR_CHANGING_MODEL_FAILED, result.getEx());
            }
        }
        return returnFunctionSuccess();
    }


    @PUT
    @Path(DigitalObjectResourceApi.REINDEX_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> reindex(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PARENT_PID) String parentPid,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId,
            @FormParam(ImportResourceApi.BATCHITEM_BATCHID) Integer batchId
    ) throws DigitalObjectException, IOException, FedoraClientException {
        Batch internalBatch;
        if (batchId != null && batchId > 0) {
            BatchParams params = new BatchParams(Collections.singletonList(batchId.toString()));
            internalBatch = BatchUtils.addNewBatch(this.importManager, Collections.singletonList("batchId:" + batchId.toString()), user, Batch.INTERNAL_REINDEX, Batch.State.INTERNAL_RUNNING, Batch.State.INTERNAL_FAILED, params);
        } else {
            BatchParams params = new BatchParams(Collections.singletonList(parentPid != null && !parentPid.isEmpty() ? parentPid : pid));
            internalBatch = BatchUtils.addNewBatch(this.importManager, Collections.singletonList(parentPid != null && !parentPid.isEmpty() ? parentPid : pid), user, Batch.INTERNAL_REINDEX, Batch.State.INTERNAL_RUNNING, Batch.State.INTERNAL_FAILED, params);
        }
        Locale locale = session.getLocale(httpHeaders);
        try {
            ReindexDigitalObjects reindexObjects = new ReindexDigitalObjects(appConfig, akubraConfiguration, user, pid, modelId);
            if (batchId != null) {
                Batch batch = importManager.get(batchId);
                List<BatchItemObject> objects = importManager.findLoadedObjects(batch);
                reindexObjects.reindexLocal(objects);
            } else {
                if (parentPid == null || parentPid.isEmpty()) {
                    IMetsElement parentElement = reindexObjects.getParentElement();
                    if (parentElement != null) {

                        if (isLocked(reindexObjects.getPids(parentElement))) {
                            BatchUtils.finishedWithError(this.importManager, internalBatch, internalBatch.getFolder(), returnLocalizedMessage(ERR_IS_LOCKED), Batch.State.INTERNAL_FAILED);
                            return returnValidationError(ERR_IS_LOCKED);
                        }
                        reindexObjects.reindex(parentElement);
                    } else {
                        BatchUtils.finishedWithError(this.importManager, internalBatch, internalBatch.getFolder(), returnLocalizedMessage(ERR_IN_GETTING_CHILDREN), Batch.State.INTERNAL_FAILED);
                        return returnValidationError(ERR_IN_GETTING_CHILDREN);
                    }
                } else {
                    if (isLocked(parentPid)) {
                        BatchUtils.finishedWithError(this.importManager, internalBatch, internalBatch.getFolder(), returnLocalizedMessage(ERR_IS_LOCKED), Batch.State.INTERNAL_FAILED);
                        return returnValidationError(ERR_IS_LOCKED);
                    }
                    reindexObjects.reindex(parentPid, locale);
                }
            }
            BatchUtils.finishedSuccessfully(this.importManager, internalBatch, internalBatch.getFolder(), null, Batch.State.INTERNAL_DONE);
            return returnFunctionSuccess();
        } catch (Exception ex) {
            BatchUtils.finishedWithError(this.importManager, internalBatch, internalBatch.getFolder(), BatchManager.toString(ex), Batch.State.INTERNAL_FAILED);
            throw ex;
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.UPDATE_ALL_OBJECTS_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> updateAllObjects(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId
    ) throws DigitalObjectException, IOException, FedoraClientException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_UPDATE_ALL_OBJECTS_FUNCTION);


        Locale locale = session.getLocale(httpHeaders);
        UpdateObjects updateObjects = new UpdateObjects(appConfig, akubraConfiguration, user, locale);
        List<SearchViewItem> objects = updateObjects.findAllObjects();
        //Map<String, Integer> map = updateObjects.countObjects(objects);
        updateObjects.setOrganization(objects, appConfig.getImportConfiguration().getDefaultProcessor());
        LOG.log(Level.INFO, "Update finished, updated " + updateObjects.getUpdatedObjects() + "/" + objects.size() + " object(s).");
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.UPDATE_NDK_ARTICLE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> updateNdkArticeObjects(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_UPDATE_MODEL_FUNCTION);

        Locale locale = session.getLocale(httpHeaders);
        UpdateObjects updateObjects = new UpdateObjects(appConfig, akubraConfiguration, user, locale);
        updateObjects.findObjects(pid, NdkPlugin.MODEL_ARTICLE);

        if (isLocked(updateObjects.getPids())) {
            return returnValidationError(ERR_IS_LOCKED);
        }

        updateObjects.repair(NdkPlugin.MODEL_ARTICLE);
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.UPDATE_NDK_PAGE)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> updateNdkPageObjects(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId
    ) throws DigitalObjectException {

        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_RUN_UPDATE_MODEL_FUNCTION);

        Locale locale = session.getLocale(httpHeaders);
        UpdateObjects updateObjects = new UpdateObjects(appConfig, akubraConfiguration, user, locale);
        updateObjects.findObjects(pid, NdkPlugin.MODEL_NDK_PAGE);

        if (isLocked(updateObjects.getPids())) {
            return returnValidationError(ERR_IS_LOCKED);
        }

        updateObjects.repair(NdkPlugin.MODEL_NDK_PAGE);
        return returnFunctionSuccess();
    }

    @POST
    @Path(DigitalObjectResourceApi.UPDATE_CATALOG_RECORD)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> updateCatalogRecord(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) List<String> pids,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CATALOGID) String catalogId
    ) throws DigitalObjectException, JSONException, IOException {
        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN, UserRole.PERMISSION_IMPORT_TO_CATALOG_FUNCTION);

        CatalogRecord catalogRecord = new CatalogRecord(appConfig, akubraConfiguration);

        BatchParams params = new BatchParams(pids);
        Batch batch = BatchUtils.addNewBatch(this.importManager, pids, user, Batch.INTERNAL_UPDATE_CATALOG_RECORDS, Batch.State.INTERNAL_RUNNING, Batch.State.INTERNAL_FAILED, params);

        StringBuilder builder = new StringBuilder();
        int count = 0;
        for (String pid : pids) {
            try {
                catalogRecord.update(catalogId, pid);
                count++;
            } catch (Throwable t) {
                builder.append(pid).append(" ").append(t.getMessage()).append("\n");
                t.printStackTrace();
            }
        }

        if (count == pids.size()) {
            BatchUtils.finishedSuccessfully(this.importManager, batch, batch.getFolder(), "Updatovano " + count + " z " + pids.size(), Batch.State.INTERNAL_DONE);
            return returnFunctionSuccess();
        } else {
            String message = "Updatovano " + count + " z " + pids.size() + "\n" + builder.toString();
            BatchUtils.finishedWithError(this.importManager, batch, batch.getFolder(), message, Batch.State.INTERNAL_FAILED);
            throw new IOException(message);
        }
    }

    @POST
    @Path(DigitalObjectResourceApi.CHANGE_OWNER_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<SearchViewItem> changeObjectOwner (
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_CHANGE_OWNER_OLD) String oldOwner,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_CHANGE_OWNER_NEW) String newOwner
    ) throws IOException, FedoraClientException, DigitalObjectException {
        Locale locale = session.getLocale(httpHeaders);
        checkPermission(session, user, UserRole.ROLE_SUPERADMIN, Permissions.ADMIN);

        UserProfile oldUser = null;
        UserProfile newUser = null;

        if (oldOwner == null || oldOwner.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_CHANGE_OWNER_OLD);
        } else {
            oldUser = UserUtil.getDefaultManger().find(oldOwner);
            if (oldUser == null) {
                return SmartGwtResponse.<SearchViewItem>asError()
                        .error(UserResourceApi.PATH, ServerMessages.get(locale).getFormattedMessage("UserResouce_Username_NotFound", oldOwner))
                        .build();
            }
        }

        if (newOwner == null || newOwner.isEmpty()) {
            return returnFunctionError(ERR_MISSING_PARAMETER, DigitalObjectResourceApi.DIGITALOBJECT_CHANGE_OWNER_NEW);
        } else {
            newUser = UserUtil.getDefaultManger().find(newOwner);
            if (newUser == null) {
                return SmartGwtResponse.<SearchViewItem>asError()
                        .error(UserResourceApi.PATH, ServerMessages.get(locale).getFormattedMessage("UserResouce_Username_NotFound", newOwner))
                        .build();
            }
        }
        if (newOwner.equals(oldOwner)) {
            return SmartGwtResponse.<SearchViewItem>asError()
                    .error(UserResourceApi.PATH, ServerMessages.get(locale).getFormattedMessage("UserResource_Both_username_are_same", newOwner))
                    .build();
        }

        BatchParams params = new BatchParams(Collections.singletonList(oldOwner + " --> " + newOwner));
        Batch batch = BatchUtils.addNewBatch(this.importManager, Collections.singletonList("Change object owner (" + oldOwner + " --> " + newOwner + ")."), user, Batch.INTERNAL_CHANGE_OBJECTS_OWNERS, Batch.State.INTERNAL_RUNNING, Batch.State.INTERNAL_FAILED, params);

        SearchView search = null;
        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            FedoraStorage remote = FedoraStorage.getInstance(appConfig);
            search = remote.getSearch(locale);
        } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            AkubraStorage akubra = AkubraStorage.getInstance(akubraConfiguration);
            search = akubra.getSearch(locale);
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }

        List<SearchViewItem> items = search.findByOwner(oldOwner);
        int updated = 0;

        try {
            for (SearchViewItem item : items) {
                ProArcObject object = findFedoraObject(item.getPid(), null);
                object.setOwner(newOwner);
                object.flush();
                updated++;
                if (updated % 50 == 0) {
                    LOG.info("Owners change for " + updated + " / " + items.size() + ".");
                }
            }
            LOG.info("Owners change for all objects (" + items.size() + ").");
            BatchUtils.finishedSuccessfully(this.importManager, batch, batch.getFolder(), null, Batch.State.INTERNAL_DONE);
            return returnFunctionSuccess();
        } catch (Throwable t) {
            BatchUtils.finishedWithError(this.importManager, batch, batch.getFolder(), t.getMessage(), Batch.State.INTERNAL_FAILED);
            throw t;
        }



    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class UrnNbnResult {

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_PID)
        private String pid;

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_MODEL)
        private String modelId;

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_LABEL)
        private String label;

        @XmlElement(name = DigitalObjectResourceApi.URNNBN_ITEM_URNNBN)
        private String urnnbn;

        @XmlElement(name = DigitalObjectResourceApi.URNNBN_ITEM_MESSAGE)
        private String message;

        @XmlElement(name = DigitalObjectResourceApi.URNNBN_ITEM_STATUSTYPE)
        private String type;

        @XmlElement(name = DigitalObjectResourceApi.URNNBN_ITEM_WARNING)
        private Boolean warning;

        @XmlElement(name = DigitalObjectResourceApi.URNNBN_ITEM_LOG)
        private String log;

        public UrnNbnResult() {
        }

        public UrnNbnResult(String pid, String urnnbn, SearchViewItem elm) {
            this.pid = pid;
            this.urnnbn = urnnbn;
            if (elm != null) {
                this.modelId = elm.getModel();
                this.label = elm.getLabel();
            }
        }

        public UrnNbnResult(String pid, String urnnbn, SearchViewItem elm, StatusEntry me) {
            this.pid = pid;
            this.urnnbn = urnnbn;
            if (me != null) {
                this.message = me.getMessage();
                this.type = me.getStatus().name();
            }
            if (elm != null) {
                this.modelId = elm.getModel();
                this.label = elm.getLabel();
            }
        }

        public UrnNbnResult(String pid, StatusEntry me, boolean warning, SearchViewItem elm) {
            this.pid = pid;
            this.message = me.getMessage();
            this.type = me.getStatus().name();
            this.warning = warning;
            this.urnnbn = me.getUrnNbn();
            if (elm != null) {
                this.modelId = elm.getModel();
                this.label = elm.getLabel();
            }
        }

    }

    private DigitalObjectHandler findHandler(String pid, Integer batchId) throws DigitalObjectNotFoundException {
        return findHandler(pid, batchId, true);
    }

    private DigitalObjectHandler findHandler(String pid, Batch batch) throws DigitalObjectNotFoundException {
        return findHandler(pid, batch, true);
    }

    private DigitalObjectHandler findHandler(String pid, Integer batchId, boolean readonly)
            throws DigitalObjectNotFoundException {

        Batch batch = null;
        if (batchId != null) {
            batch = importManager.get(batchId);
        }
        return findHandler(pid, batch, readonly);
    }

    private DigitalObjectHandler findHandler(String pid, Batch batch, boolean readonly)
            throws DigitalObjectNotFoundException {

        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        ProArcObject fobject = dom.find2(pid, batch, null);
        if (!readonly && fobject instanceof LocalObject) {
            ImportResourceV1.checkBatchState(batch);
        }
        return dom.createHandler(fobject);
    }

    @Deprecated
    private ProArcObject findFedoraObject(String pid, Integer batchId) throws IOException {
        return findFedoraObject(pid, batchId, true);
    }

    @Deprecated
    protected ProArcObject findFedoraObject(String pid, Integer batchId, boolean readonly) throws IOException {
        ProArcObject fobject;
        if (batchId != null) {
            Batch batch = importManager.get(batchId);
            if (batch == null) {
                throw RestException.plainNotFound(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID, String.valueOf(batchId));
            }
            if (!readonly) {
                ImportResourceV1.checkBatchState(batch);
            }
            if (pid == null || BatchManager.ROOT_ITEM_PID.equals(pid)) {
                fobject = importManager.getRootObject(batch);
            } else {
                BatchItemObject item = importManager.findBatchObject(batchId, pid);
                if (item == null) {
                    throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
                }
                fobject = new LocalStorage().load(pid, item.getFile());
            }
        } else {
            if (pid == null) {
                throw new NullPointerException("pid");
            }
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                fobject = FedoraStorage.getInstance(appConfig).find(pid);
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraStorage storage = AkubraStorage.getInstance(akubraConfiguration);
                fobject = storage.find(pid);
                SolrSearchView solrSearch = storage.getSearch();
                List<SearchViewItem> items = solrSearch.find(Collections.singletonList(pid));
                if (items != null && items.size() > 0) {
                    fobject.setModel(items.get(0).getModel());
                }
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
        }
        return fobject;
    }

    /**
     * Remove path from file name sent by client. It searches for platform path
     * delimiters.
     *
     * @param filepath file path
     * @return the file name
     */
    private static String getFilename(String filepath) {
        int slashIndex = filepath.lastIndexOf('/');
        int backslashIndex = filepath.lastIndexOf('\\');
        int index = Math.max(slashIndex, backslashIndex);
        if (index > 0) {
            filepath = filepath.substring(index);
        }
        return filepath;
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class DigitalObject {
        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_PID)
        private String pid;
        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_MODEL)
        private String model;

        public DigitalObject(String pid, String model) {
            this.pid = pid;
            this.model = model;
        }

        public DigitalObject() {
        }
    }

    private SmartGwtResponse<SearchViewItem> returnValidationError(String key) {
        SearchViewItem item = new SearchViewItem();
        item.setValidation(returnLocalizedMessage(key));
        return new SmartGwtResponse<SearchViewItem>(SmartGwtResponse.STATUS_SUCCESS, 0, 0, -1, Collections.singletonList(item));
    }

    private SmartGwtResponse<SearchViewItem> returnFunctionError(String key, DigitalObjectException ex) {
        return returnFunctionError(key, ex.getMessage());
    }

    private SmartGwtResponse<SearchViewItem> returnFunctionError(String key, String message) {
        SearchViewItem item = new SearchViewItem();
        item.setValidation(returnLocalizedMessage(key, message));
        return new SmartGwtResponse<SearchViewItem>(SmartGwtResponse.STATUS_SUCCESS, 0, 0, -1, Collections.singletonList(item));
    }

    private SmartGwtResponse<SearchViewItem> returnFunctionSuccess() {
        SearchViewItem item = new SearchViewItem();
        item.setStatus("OK");
        return new SmartGwtResponse<SearchViewItem>(SmartGwtResponse.STATUS_SUCCESS, 0, 0, 1, Collections.singletonList(item));
    }

    public String returnLocalizedMessage(String key, Object... arguments) {
        Locale locale = session.getLocale(httpHeaders);
        ServerMessages msgs = ServerMessages.get(locale);
        return msgs.getFormattedMessage(key, arguments);
    }

    public boolean isLocked(String pid) {
        if (pid == null) {
            return false;
        }
        List<String> pids = new ArrayList<>();
        pids.add(pid);
        return isLocked(pids);
    }

    public boolean isLocked(List<String> pids) {
        try {
            Locale locale = session.getLocale(httpHeaders);
            SearchView search = null;
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                FedoraStorage remote = FedoraStorage.getInstance(appConfig);
                search = remote.getSearch(locale);
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                search = akubraStorage.getSearch(locale);
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
            List<SearchViewItem> items = search.find(pids);
            for (SearchViewItem item : items) {
                if (item.isLocked() > 0) {
                    return true;
                }
            }
            return false;
        } catch (IOException | FedoraClientException e) {
            LOG.log(Level.SEVERE, e.getMessage());
            return true;
        }
    }

    private List<String> transform(Set<String> set) {
        List<String> pids = new ArrayList<>();
        for (String uuid : set) {
            pids.add(uuid);
        }
        return pids;
    }

    /**
     * The export result.
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class InternalExternalProcessResult {

        @XmlElement(name = DigitalObjectResourceApi.RESULT_ID)
        private Integer processId;

        @XmlElement(name = DigitalObjectResourceApi.RESULT_MSG)
        private String msg;

        public InternalExternalProcessResult() {
        }

        public InternalExternalProcessResult(Integer processId, String msg) {
            this.processId = processId;
            this.msg = msg;
        }

        public Integer getProcessId() {
            return processId;
        }

        public String getMsg() {
            return msg;
        }
    }
}
