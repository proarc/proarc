/*
 * Copyright (C) 2015 Jan Pokorsky
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

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectValidationException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectValidationException.ValidationResult;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.Mapping;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.workflow.WorkflowException;
import cz.cas.lib.proarc.common.workflow.WorkflowManager;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.JobFilter;
import cz.cas.lib.proarc.common.workflow.model.JobView;
import cz.cas.lib.proarc.common.workflow.model.Material;
import cz.cas.lib.proarc.common.workflow.model.MaterialFilter;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.MaterialView;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.model.TaskFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskParameterFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskParameterView;
import cz.cas.lib.proarc.common.workflow.model.TaskView;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.common.workflow.profile.JobDefinition;
import cz.cas.lib.proarc.common.workflow.profile.JobDefinitionView;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfileConsts;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.server.ServerMessages;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.WorkflowResourceApi;
import java.io.IOException;
import java.io.StringReader;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.transform.stream.StreamSource;

/**
 * It allows to manage workflow remotely.
 *
 * @author Jan Pokorsky
 */
@Path(WorkflowResourceApi.PATH)
public class WorkflowResource {

    private static final Logger LOG = Logger.getLogger(WorkflowResource.class.getName());

    private final SessionContext session;
    private final HttpHeaders httpHeaders;
    private final WorkflowManager workflowManager;
    private final WorkflowProfiles workflowProfiles;
    private final AppConfiguration appConfig;

    public WorkflowResource(
            @Context HttpHeaders httpHeaders,
            @Context HttpServletRequest httpRequest
    ) throws AppConfigurationException {
        this.session = SessionContext.from(httpRequest);
        this.httpHeaders = httpHeaders;
        this.workflowManager = WorkflowManager.getInstance();
        this.workflowProfiles = WorkflowProfiles.getInstance();
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
    }

    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<JobView> getJob(
            @QueryParam(WorkflowModelConsts.JOB_FILTER_ID) BigDecimal id,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_CREATED) List<String> created,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_LABEL) String label,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_MODIFIED) List<String> modified,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_PRIORITY) Integer priority,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_PROFILENAME) String profileName,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_STATE) Job.State state,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_OWNERID) BigDecimal userId,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_PARENTID) BigDecimal parentId,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_MATERIAL_BARCODE) String mBarcode,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_MATERIAL_DETAIL) String mDetail,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_MATERIAL_FIELD001) String mField001,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_MATERIAL_ISSUE) String mIssue,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_MATERIAL_SIGLA) String mSigla,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_MATERIAL_SIGNATURE) String mSignature,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_MATERIAL_VOLUME) String mVolume,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_MATERIAL_YEAR) String mYear,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_OFFSET) int startRow,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_SORTBY) String sortBy
    ) {
        int pageSize = 100;
        JobFilter filter = new JobFilter();
        filter.setLocale(session.getLocale(httpHeaders));
        filter.setMaxCount(pageSize);
        filter.setOffset(startRow);
        filter.setSortBy(sortBy);

        filter.setId(id);
        filter.setCreated(created);
        filter.setLabel(label);
        filter.setMaterialBarcode(mBarcode);
        filter.setMaterialDetail(mDetail);
        filter.setMaterialField001(mField001);
        filter.setMaterialIssue(mIssue);
        filter.setMaterialSigla(mSigla);
        filter.setMaterialSignature(mSignature);
        filter.setMaterialVolume(mVolume);
        filter.setMaterialYear(mYear);
        filter.setModified(modified);
        filter.setParentId(parentId);
        filter.setPriority(priority);
        filter.setProfileName(profileName);
        filter.setState(state);
        filter.setUserId(userId);
        try {
            List<JobView> jobs = workflowManager.findJob(filter);
            int resultSize = jobs.size();
            int endRow = startRow + resultSize;
            int total = (resultSize != pageSize) ? endRow : endRow + 1;
            return new SmartGwtResponse<JobView>(
                    SmartGwtResponse.STATUS_SUCCESS, startRow, endRow, total, jobs);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, null, ex);
            return SmartGwtResponse.asError(ex.getMessage());
        }
    }

    /**
     * Creates a new workflow job.
     * @param profileName profile name of the new job
     * @param metadata MODS
     * @param catalogId catalog ID
     * @param parentId ID of the parent job. If used the parameters metadata
     *      and catalogId are ignored. The new job is a subjob.
     * @return the job
     */
    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<JobView> addJob(
            @FormParam(WorkflowResourceApi.NEWJOB_PROFILE) String profileName,
            @FormParam(WorkflowResourceApi.NEWJOB_METADATA) String metadata,
            @FormParam(WorkflowResourceApi.NEWJOB_CATALOGID) String catalogId,
            @FormParam(WorkflowResourceApi.NEWJOB_PARENTID) BigDecimal parentId
    ) {
        metadata = "null".equals(metadata) ? null : metadata;
        catalogId = "null".equals(catalogId) ? null : catalogId;

        if (parentId != null) {
            return addSubjob(profileName, parentId);
        }

        CatalogConfiguration catalog = null;
        if (catalogId != null) {
            catalog = appConfig.getCatalogs().findConfiguration(catalogId);
        }

        WorkflowDefinition profiles = workflowProfiles.getProfiles();
        if (profiles == null) {
            return profileError();
        }
        JobDefinition profile = workflowProfiles.getProfile(profiles, profileName);
        if (profile == null) {
            return SmartGwtResponse.asError(WorkflowResourceApi.NEWJOB_PROFILE + " - invalid value! " + profileName);
        }
        try {
            Job job = workflowManager.addJob(profile, metadata, catalog, session.getUser());
            JobFilter filter = new JobFilter();
            filter.setLocale(session.getLocale(httpHeaders));
            filter.setId(job.getId());
            List<JobView> views = workflowManager.findJob(filter);
            return new SmartGwtResponse<>(views);
        } catch (WorkflowException ex) {
            return toError(ex, WorkflowResourceApi.NEWJOB_PROFILE + ":" + profileName
                    + ", " + WorkflowResourceApi.NEWJOB_CATALOGID + ":" + catalogId
                    + ", " + WorkflowResourceApi.NEWJOB_METADATA + ":\n" + metadata);
        }
    }

    private SmartGwtResponse<JobView> addSubjob(String profileName, BigDecimal parentId) {
        WorkflowDefinition profiles = workflowProfiles.getProfiles();
        if (profiles == null) {
            return profileError();
        }
        JobDefinition profile = workflowProfiles.getProfile(profiles, profileName);
        if (profile == null) {
            return SmartGwtResponse.asError(WorkflowResourceApi.NEWJOB_PROFILE + " - invalid value! " + profileName);
        }
        try {
            Job subjob = workflowManager.addSubjob(profile, parentId, session.getUser(), profiles);
            JobFilter filter = new JobFilter();
            filter.setLocale(session.getLocale(httpHeaders));
            filter.setId(subjob.getId());
            List<JobView> views = workflowManager.findJob(filter);
            return new SmartGwtResponse<>(views);
        } catch (WorkflowException ex) {
            return toError(ex, WorkflowResourceApi.NEWJOB_PROFILE + ":" + profileName
                    + ", " + WorkflowResourceApi.NEWJOB_PARENTID + ":" + parentId
                    );
        }
    }

    @PUT
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<JobView> updateJob(
            @FormParam(WorkflowModelConsts.JOB_ID) BigDecimal id,
            @FormParam(WorkflowModelConsts.JOB_LABEL) String label,
            @FormParam(WorkflowModelConsts.JOB_NOTE) String note,
            @FormParam(WorkflowModelConsts.JOB_FINANCED) String financed,
            @FormParam(WorkflowModelConsts.JOB_OWNERID) BigDecimal userId,
            @FormParam(WorkflowModelConsts.JOB_PARENTID) BigDecimal parentId,
            @FormParam(WorkflowModelConsts.JOB_PRIORITY) Integer priority,
            @FormParam(WorkflowModelConsts.JOB_STATE) Job.State state,
            @FormParam(WorkflowModelConsts.JOB_TIMESTAMP) long timestamp
    ) {
        if (id == null) {
            return SmartGwtResponse.asError("Missing job ID!");
        }
        WorkflowDefinition profiles = workflowProfiles.getProfiles();
        if (profiles == null) {
            return profileError();
        }
        Job job = new Job();
        job.setId(id);
        job.setFinanced(financed);
        job.setLabel(label);
        job.setNote(note);
        job.setOwnerId(userId);
        job.setParentId(parentId);
        job.setPriority(priority != null ? priority : 2);
        job.setState(state);
        job.setTimestamp(new Timestamp(timestamp));
        try {
            workflowManager.updateJob(job);

            JobFilter jobFilter = new JobFilter();
            jobFilter.setId(id);
            jobFilter.setLocale(session.getLocale(httpHeaders));
            List<JobView> result = workflowManager.findJob(jobFilter);
            return new SmartGwtResponse<JobView>(result);
        } catch (WorkflowException ex) {
            return toError(ex, null);
        }
    }

    @Path(WorkflowResourceApi.TASK_PATH)
    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<TaskView> getTask(
            @QueryParam(WorkflowModelConsts.TASK_FILTER_CREATED) List<String> created,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_ID) BigDecimal id,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_JOBID) BigDecimal jobId,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_JOBLABEL) String jobLabel,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_MODIFIED) List<String> modified,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_PRIORITY) List<Integer> priority,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_PROFILENAME) List<String> profileName,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_STATE) List<Task.State> state,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_OWNERID) List<BigDecimal> userId,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_OFFSET) int startRow,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_SORTBY) String sortBy
    ) {
        int pageSize = 100;
        TaskFilter filter = new TaskFilter();
        filter.setLocale(session.getLocale(httpHeaders));
        filter.setMaxCount(pageSize);
        filter.setOffset(startRow);
        filter.setSortBy(sortBy);

        filter.setCreated(created);
        filter.setId(id);
        filter.setJobId(jobId);
        filter.setJobLabel(jobLabel);
        filter.setModified(modified);
        filter.setPriority(priority);
        filter.setProfileName(profileName);
        filter.setState(state);
        filter.setUserId(userId);
        WorkflowDefinition workflow = workflowProfiles.getProfiles();
        if (workflow == null) {
            return profileError();
        }
        try {
            List<TaskView> tasks = workflowManager.tasks().findTask(filter, workflow);
            int resultSize = tasks.size();
            int endRow = startRow + resultSize;
            int total = (resultSize != pageSize) ? endRow : endRow + 1;
            return new SmartGwtResponse<TaskView>(
                    SmartGwtResponse.STATUS_SUCCESS, startRow, endRow, total, tasks);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, null, ex);
            return SmartGwtResponse.asError(ex.getMessage());
        }
    }

    @Path(WorkflowResourceApi.TASK_PATH)
    @POST
    @Consumes({MediaType.APPLICATION_FORM_URLENCODED})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<TaskView> addTask(
            @FormParam(WorkflowModelConsts.TASK_JOBID) BigDecimal jobId,
            @FormParam(WorkflowModelConsts.TASK_PROFILENAME) String taskName
    ) {
        if (jobId == null || taskName == null) {
            return SmartGwtResponse.asError("Invalid parameters!");
        }
        WorkflowDefinition workflow = workflowProfiles.getProfiles();
        if (workflow == null) {
            return profileError();
        }
        try {
            Task updatedTask = workflowManager.tasks().addTask(jobId, taskName, workflow, session.getUser());
            TaskFilter taskFilter = new TaskFilter();
            taskFilter.setId(updatedTask.getId());
            taskFilter.setLocale(session.getLocale(httpHeaders));
            List<TaskView> result = workflowManager.tasks().findTask(taskFilter, workflow);
            return new SmartGwtResponse<TaskView>(result);
        } catch (WorkflowException ex) {
            return toError(ex, "jobId: " + jobId + ", taskName: " + taskName);
        }
    }

    @Path(WorkflowResourceApi.TASK_PATH)
    @PUT
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<TaskView> updateTask(TaskUpdate task) {
        if (task == null) {
            return SmartGwtResponse.asError("No task!");
        }
        WorkflowDefinition workflow = workflowProfiles.getProfiles();
        if (workflow == null) {
            return profileError();
        }
        try {
            Task updatedTask = workflowManager.tasks().updateTask(task, task.params, workflow);
            TaskFilter taskFilter = new TaskFilter();
            taskFilter.setId(updatedTask.getId());
            taskFilter.setLocale(session.getLocale(httpHeaders));
            List<TaskView> result = workflowManager.tasks().findTask(taskFilter, workflow);
            return new SmartGwtResponse<TaskView>(result);
        } catch (WorkflowException ex) {
            return toError(ex, null);
        }
    }

    @XmlAccessorType(XmlAccessType.NONE)
    public static class TaskUpdate extends Task {
        @XmlElement(name = WorkflowModelConsts.TASK_PARAMETERS)
        public Map<String, Object> params;
    }

    @Path(WorkflowResourceApi.MATERIAL_PATH)
    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<MaterialView> getMaterial(
            @QueryParam(WorkflowModelConsts.MATERIALFILTER_ID) BigDecimal id,
            @QueryParam(WorkflowModelConsts.MATERIALFILTER_JOBID) BigDecimal jobId,
            @QueryParam(WorkflowModelConsts.MATERIALFILTER_TASKID) BigDecimal taskId,
            @QueryParam(WorkflowModelConsts.MATERIAL_TYPE) MaterialType materialType,
            @QueryParam(WorkflowModelConsts.MATERIALFILTER_OFFSET) int startRow,
            @QueryParam(WorkflowModelConsts.MATERIALFILTER_SORTBY) String sortBy
    ) {
        int pageSize = 100;
        MaterialFilter filter = new MaterialFilter();
        filter.setLocale(session.getLocale(httpHeaders));
        filter.setMaxCount(pageSize);
        filter.setOffset(startRow);
        filter.setSortBy(sortBy);
        filter.setType(materialType);
        filter.setId(id);
        filter.setJobId(jobId);
        filter.setTaskId(taskId);
        try {
            List<MaterialView> mvs = workflowManager.findMaterial(filter);
            int resultSize = mvs.size();
            int endRow = startRow + resultSize;
            int total = (resultSize != pageSize) ? endRow : endRow + 1;
            return new SmartGwtResponse<MaterialView>(
                    SmartGwtResponse.STATUS_SUCCESS, startRow, endRow, total, mvs);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, null, ex);
            return SmartGwtResponse.asError(ex.getMessage());
        }
    }

    @Path(WorkflowResourceApi.MATERIAL_PATH)
    @PUT
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<MaterialView> updateMaterial(
            MaterialView mv
    ) {
        if (mv == null || mv.getId() == null) {
            return SmartGwtResponse.asError("Invalid parameters!");
        }
        WorkflowDefinition workflow = workflowProfiles.getProfiles();
        if (workflow == null) {
            return profileError();
        }
        try {
            Material updateMaterial = workflowManager.updateMaterial(mv);
            MaterialFilter filter = new MaterialFilter();
            filter.setLocale(session.getLocale(httpHeaders));
            filter.setId(updateMaterial.getId());
            filter.setJobId(mv.getJobId());
            filter.setTaskId(mv.getTaskId());
            List<MaterialView> result = workflowManager.findMaterial(filter);
            return new SmartGwtResponse<MaterialView>(result);
        } catch (WorkflowException ex) {
            return toError(ex, null);
        }
    }

    /**
     * Gets subset of MODS properties in JSON.
     *
     * @param jobId workflow job id of requested digital object
     * @param editorId view defining subset of MODS properties
     */
    @Path(WorkflowResourceApi.MODS_PATH)
    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DescriptionMetadata<Object>> getDescriptionMetadata(
            @QueryParam(WorkflowModelConsts.PARAMETER_JOBID) BigDecimal jobId,
            @QueryParam(MetaModelDataSource.FIELD_EDITOR) String editorId,
            @QueryParam(MetaModelDataSource.FIELD_MODELOBJECT) String modelId
    ) throws DigitalObjectException {
        if (jobId == null) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, jobId.toString());
        }

        DigitalObjectHandler doHandler = findHandler(jobId, modelId);
        DescriptionMetadata<Object> metadata = doHandler.metadata().getMetadataAsJsonObject(editorId);
        return new SmartGwtResponse<DescriptionMetadata<Object>>(metadata);
    }

    @PUT
    @Path(WorkflowResourceApi.MODS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DescriptionMetadata<Object>> updateDescriptionMetadata(
            @FormParam(WorkflowModelConsts.PARAMETER_JOBID) BigDecimal jobId,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMJSONDATA) String jsonData,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMXMLDATA) String xmlData,
            @FormParam(MetaModelDataSource.FIELD_MODELOBJECT) String modelId,
            @DefaultValue("false")
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_IGNOREVALIDATION) boolean ignoreValidation
    ) throws DigitalObjectException {
        if (jobId == null) {
            throw RestException.plainNotFound(WorkflowModelConsts.PARAMETER_JOBID, jobId.toString());
        }
        LOG.fine(String.format("pid: %s, editor: %s, timestamp: %s, ignoreValidation: %s, json: %s, xml: %s",
                jobId, editorId, timestamp, ignoreValidation, jsonData, xmlData));
        final boolean isJsonData = xmlData == null;
        String data = isJsonData ? jsonData : xmlData;
        DigitalObjectHandler doHandler = findHandler(jobId, modelId);
        MetadataHandler<?> mHandler = doHandler.metadata();
        DescriptionMetadata<String> dMetadata = new DescriptionMetadata<String>();
        dMetadata.setEditor(editorId);
        dMetadata.setData(data);
        dMetadata.setTimestamp(timestamp);
        dMetadata.setIgnoreValidation(ignoreValidation);
        if (isJsonData) {
            mHandler.setMetadataAsJson(dMetadata, session.asFedoraLog());
        } else {
            mHandler.setMetadataAsXml(dMetadata, session.asFedoraLog());
        }
        doHandler.commit();
        return new SmartGwtResponse<DescriptionMetadata<Object>>(mHandler.getMetadataAsJsonObject(editorId));
    }

    private DigitalObjectHandler findHandler(BigDecimal jobId, String modelId) throws DigitalObjectNotFoundException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        FedoraObject fobject = dom.find(jobId, modelId, session.getLocale(httpHeaders));
        return dom.createHandler(fobject);
    }

    @Path(WorkflowResourceApi.PARAMETER_PATH)
    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<TaskParameterView> getParameter(
            @QueryParam(WorkflowModelConsts.PARAMETERPROFILE_TASKID) BigDecimal taskId
    ) {
        if (taskId == null) {
            return SmartGwtResponse.asError("taskId is required!");
        }
        int pageSize = 100;
        TaskParameterFilter filter = new TaskParameterFilter();
        filter.setLocale(session.getLocale(httpHeaders));
        filter.setMaxCount(pageSize);
        filter.setOffset(0);

        filter.setTaskId(taskId);
        try {
            List<TaskParameterView> params = workflowManager.findParameter(filter);
            return new SmartGwtResponse<TaskParameterView>(params);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, null, ex);
            return SmartGwtResponse.asError(ex.getMessage());
        }
    }

    /**
     * Gets workflow profiles defined with {@link WorkflowProfiles}
     * @param name a profile name filter
     * @param disabled an availability filter
     * @return the list of profiles
     */
    @Path(WorkflowResourceApi.PROFILE_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    @GET
    public SmartGwtResponse<JobDefinitionView> getProfiles(
            @QueryParam(WorkflowProfileConsts.NAME) String name,
            @QueryParam(WorkflowProfileConsts.DISABLED) Boolean disabled
    ) {
        WorkflowDefinition workflowDefinition = workflowProfiles.getProfiles();
        if (workflowDefinition == null) {
            return profileError();
        }
        String lang = session.getLocale(httpHeaders).getLanguage();
        ArrayList<JobDefinitionView> profiles = new ArrayList<JobDefinitionView>();
        for (JobDefinition job : workflowDefinition.getJobs()) {
            if ((name == null || name.equals(job.getName()))
                    && (disabled == null || disabled == job.isDisabled())) {
                Set<String> modelPids = MetaModelRepository.getInstance().find()
                        .stream().map(metaModel -> metaModel.getPid()).collect(Collectors.toSet());


                List<String> unknownModels = job.getModel().stream().map(metamodel -> metamodel.getPid())
                        .filter(p -> !modelPids.contains(p)).collect(Collectors.toList());

                if (!unknownModels.isEmpty()) {
                    return SmartGwtResponse.asError(WorkflowProfileConsts.MODEL_PID + " - invalid values! "
                            + String.join(", ", unknownModels));
                }

                profiles.add(new JobDefinitionView(job, lang));
            }
        }
        return new SmartGwtResponse<JobDefinitionView>(profiles);
    }

    private <T> SmartGwtResponse<T> profileError() {
        return toError(
                new WorkflowException("Invalid workflow.xml!")
                        .addInvalidXml(),
                null);
    }

    private <T> SmartGwtResponse<T> toError(WorkflowException ex, String log) {
        if (ex.getValidations().isEmpty()) {
            LOG.log(Level.SEVERE, log, ex);
            return SmartGwtResponse.asError(ex.getMessage());
        }
        StringBuilder sb = new StringBuilder();
        Locale locale = session.getLocale(httpHeaders);
        ServerMessages msgs = ServerMessages.get(locale);
        for (ValidationResult validation : ex.getValidations()) {
            String msg;
            try {
                msg = msgs.getFormattedMessage(validation.getBundleKey(), validation.getValues());
            } catch (MissingResourceException mrex) {
                LOG.log(Level.WARNING, validation.getBundleKey(), mrex);
                msg = validation.getBundleKey();
            }
            if (sb.length() > 0) {
                sb.append("<p>");
            }
            sb.append(msg);
            // log only unexpected errors
            if (WorkflowException.UNEXPECTED_ERROR_MSG_MSG.equals(validation.getBundleKey())) {
                LOG.log(Level.SEVERE, log, ex);
            }
        }
        return SmartGwtResponse.asError(sb.toString());
    }

}
