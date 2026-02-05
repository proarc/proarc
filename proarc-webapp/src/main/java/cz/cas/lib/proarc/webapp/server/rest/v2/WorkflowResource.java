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
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.storage.StringEditor;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.workflow.WorkflowException;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.JobView;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.MaterialView;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.model.TaskParameterView;
import cz.cas.lib.proarc.common.workflow.model.TaskView;
import cz.cas.lib.proarc.common.workflow.model.WorkflowModelConsts;
import cz.cas.lib.proarc.common.workflow.profile.JobDefinitionView;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfileConsts;
import cz.cas.lib.proarc.webapp.server.rest.SessionContext;
import cz.cas.lib.proarc.webapp.server.rest.ProArcResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.WorkflowResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.WorkflowResourceApi;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.DELETE;
import jakarta.ws.rs.DefaultValue;
import jakarta.ws.rs.FormParam;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.PUT;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_MISSING_PARAMETER;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_NO_PERMISSION;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.FIELD_MODELOBJECT;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_WF_CREATE_JOB;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.PERMISSION_FUNCTION_WF_DELETE_JOB;
import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.URL_API_VERSION_2;
import static cz.cas.lib.proarc.webapp.server.rest.UserPermission.hasPermission;

/**
 * It allows to manage workflow remotely.
 *
 * @author Lukas Sykora
 */
@Path(URL_API_VERSION_2 + "/" + WorkflowResourceApi.PATH)
public class WorkflowResource extends WorkflowResourceV1 {

    private static final Logger LOG = Logger.getLogger(WorkflowResource.class.getName());

    private final UserProfile user;
    private final SessionContext session;

    public WorkflowResource(
            @Context HttpHeaders httpHeaders,
            @Context HttpServletRequest httpRequest
    ) throws AppConfigurationException {
        super(httpHeaders, httpRequest);
        session = SessionContext.from(httpRequest);
        user = session.getUser();
    }

    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<JobView> getJob(
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
            @QueryParam(WorkflowModelConsts.JOB_FILTER_MATERIAL_EDITION) String mEdition,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_OFFSET) int startRow,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_FINANCED) String financed,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_SORTBY) String sortBy,
            @QueryParam(WorkflowModelConsts.JOB_TASK_NAME) String taskName,
            @QueryParam(WorkflowModelConsts.JOB_TASK_CHANGE_DATE) List<String> taskDate,
            @QueryParam(WorkflowModelConsts.JOB_TASK_CHANGE_USER) String taskUser,
            @QueryParam(WorkflowModelConsts.JOB_TASK_CHANGE_USERNAME) String taskUserName,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_DIGOBJ_PID) String pid,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_RAW_PATH) String rawPath,
            @QueryParam(WorkflowModelConsts.JOB_DEVICE_ID) String deviceId,
            @QueryParam(WorkflowModelConsts.JOB_NOTE) String note
    ) {
        try {
            return super.getJob(id, created, label, modified, priority, profileName, state, userId, parentId, mBarcode,
                    mDetail, mField001, mIssue, mSigla, mSignature, mVolume, mYear, mEdition, startRow, financed,
                    sortBy, taskName, taskDate, taskUser, taskUserName, pid, rawPath, deviceId, note);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<JobView> addJob(
            @FormParam(WorkflowResourceApi.NEWJOB_PROFILE) String profileName,
            @FormParam(WorkflowResourceApi.NEWJOB_MODEL) String model,
            @FormParam(WorkflowResourceApi.NEWJOB_METADATA) String metadata,
            @FormParam(WorkflowResourceApi.NEWJOB_CATALOGID) String catalogId,
            @FormParam(WorkflowResourceApi.NEWJOB_PARENTID) BigDecimal parentId,
            @FormParam(WorkflowResourceApi.NEWJOB_RDCZID) BigDecimal rdczId
    ) {
        if (!hasPermission(user, PERMISSION_FUNCTION_WF_CREATE_JOB)) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }

        try {
            return super.addJob(profileName, model, metadata, catalogId, parentId, rdczId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @PUT
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<JobView> updateJob(
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
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, WorkflowModelConsts.JOB_ID));
        }
        try {
            return super.updateJob(id, label, note, financed, userId, parentId, priority, state, timestamp);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @PUT
    @Path(WorkflowResourceApi.EDITOR_JOBS)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<JobView> updateJobs(
            @FormParam(WorkflowModelConsts.JOB_IDS) List<BigDecimal> ids,
            @FormParam(WorkflowModelConsts.JOB_FINANCED) String financed,
            @FormParam(WorkflowModelConsts.JOB_PRIORITY) Integer priority,
            @FormParam(WorkflowModelConsts.JOB_STATE) Job.State state,
            @FormParam(WorkflowModelConsts.JOB_NOTE) String note
    ) {
        if (ids == null) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, WorkflowModelConsts.JOB_IDS));
        }
        try {
            return super.updateJobs(ids, financed, priority, state, note);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @DELETE
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<JobView> deleteObject(
            @QueryParam(WorkflowModelConsts.JOB_FILTER_ID) List<BigDecimal> ids) {

        if (!hasPermission(user, PERMISSION_FUNCTION_WF_DELETE_JOB)) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }
        try {
            return super.deleteObject(ids);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @GET
    @Path(WorkflowResourceApi.TASK_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<TaskView> getTask(
            @QueryParam(WorkflowModelConsts.TASK_FILTER_CREATED) List<String> created,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_ID) BigDecimal id,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_JOBID) BigDecimal jobId,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_JOBLABEL) String jobLabel,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_MODIFIED) List<String> modified,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_PRIORITY) List<Integer> priority,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_PROFILENAME) List<String> profileName,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_STATE) List<Task.State> state,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_OWNERID) List<BigDecimal> userId,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_NOTE) String note,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_ORDER) BigInteger order,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_OFFSET) int startRow,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_SORTBY) String sortBy,
            @QueryParam(WorkflowModelConsts.MATERIAL_BARCODE) String barcode,
            @QueryParam(WorkflowModelConsts.MATERIAL_SIGNATURE) String signature
    ) {
        try {
            return super.getTask(created, id, jobId, jobLabel, modified, priority, profileName, state, userId, note, order, startRow, sortBy, barcode, signature);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @POST
    @Path(WorkflowResourceApi.TASK_PATH)
    @Consumes({MediaType.APPLICATION_FORM_URLENCODED})
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<TaskView> addTask(
            @FormParam(WorkflowModelConsts.TASK_JOBID) BigDecimal jobId,
            @FormParam(WorkflowModelConsts.TASK_PROFILENAME) String taskName
    ) {
        if (jobId == null) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, WorkflowModelConsts.JOB_ID));
        }
        if (taskName == null || taskName.isEmpty()) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, WorkflowModelConsts.TASK_PROFILENAME));
        }
        try {
            return super.addTask(jobId, taskName);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @PUT
    @Path(WorkflowResourceApi.TASK_PATH)
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<TaskView> updateTask(TaskUpdate task
    ) {
        if (task == null) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, "task"));
        }
        try {
            return super.updateTask(task);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @PUT
    @Path(WorkflowResourceApi.EDITOR_TASKS)
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<TaskView> updateTasks(TasksUpdate tasks) throws WorkflowException {
        if (tasks == null) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, "task"));
        }
        try {
            return super.updateTasks(tasks);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @GET
    @Path(WorkflowResourceApi.MATERIAL_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<MaterialView> getMaterial(
            @QueryParam(WorkflowModelConsts.MATERIALFILTER_ID) BigDecimal id,
            @QueryParam(WorkflowModelConsts.MATERIALFILTER_JOBID) BigDecimal jobId,
            @QueryParam(WorkflowModelConsts.MATERIALFILTER_TASKID) BigDecimal taskId,
            @QueryParam(WorkflowModelConsts.MATERIAL_TYPE) MaterialType materialType,
            @QueryParam(WorkflowModelConsts.MATERIALFILTER_OFFSET) int startRow,
            @QueryParam(WorkflowModelConsts.MATERIALFILTER_SORTBY) String sortBy
    ) {
        try {
            return super.getMaterial(id, jobId, taskId, materialType, startRow, sortBy);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @PUT
    @Path(WorkflowResourceApi.MATERIAL_PATH)
    @Consumes({MediaType.APPLICATION_JSON})
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<MaterialView> updateMaterial(
            MaterialView mv
    ) {
        if (mv == null || mv.getId() == null) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, "materialView"));
        }
        try {
            return super.updateMaterial(mv);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @GET
    @Path(WorkflowResourceApi.MODS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<DescriptionMetadata<Object>> getDescriptionMetadata(
            @QueryParam(WorkflowModelConsts.PARAMETER_JOBID) BigDecimal jobId,
            @QueryParam(DigitalObjectResourceApi.METAMODEL_MODSCUSTOMEDITORID_PARAM) String editorId,
            @QueryParam(FIELD_MODELOBJECT) String modelId
    ) {
        if (jobId == null) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, WorkflowModelConsts.JOB_ID));
        }
        try {
            return super.getDescriptionMetadata(jobId, editorId, modelId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @GET
    @Path(WorkflowResourceApi.MODS_PATH + '/' + WorkflowResourceApi.MODS_PLAIN_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public StringEditor.StringRecord getDescriptionMetadataTxt(
            @QueryParam(WorkflowModelConsts.PARAMETER_JOBID) BigDecimal jobId,
            @QueryParam(FIELD_MODELOBJECT) String modelId
    ) {
        try {
            return super.getDescriptionMetadataTxt(jobId, modelId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return new StringEditor.StringRecord(t);
        }
    }

    @PUT
    @Path(WorkflowResourceApi.MODS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<DescriptionMetadata<Object>> updateDescriptionMetadata(
            @FormParam(WorkflowModelConsts.PARAMETER_JOBID) BigDecimal jobId,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMJSONDATA) String jsonData,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMXMLDATA) String xmlData,
            @FormParam(FIELD_MODELOBJECT) String modelId,
            @DefaultValue("false")
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_IGNOREVALIDATION) boolean ignoreValidation,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_STANDARD) String standard
    ) {
        if (jobId == null) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, WorkflowModelConsts.JOB_ID));
        }
        try {
            return super.updateDescriptionMetadata(jobId, editorId, timestamp, jsonData, xmlData, modelId, ignoreValidation, standard);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @GET
    @Path(WorkflowResourceApi.PARAMETER_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<TaskParameterView> getParameter(
            @QueryParam(WorkflowModelConsts.PARAMETERPROFILE_TASKID) BigDecimal taskId
    ) {
        if (taskId == null) {
            return ProArcResponse.asError(returnLocalizedMessage(ERR_MISSING_PARAMETER, WorkflowModelConsts.PARAMETERPROFILE_TASKID));
        }
        try {
            return super.getParameter(taskId);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @GET
    @Path(WorkflowResourceApi.PROFILE_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<JobDefinitionView> getProfiles(
            @QueryParam(WorkflowProfileConsts.NAME) String name,
            @QueryParam(WorkflowProfileConsts.DISABLED) Boolean disabled
    ) {
        try {
            return super.getProfiles(name, disabled);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }
}
