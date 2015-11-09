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
import cz.cas.lib.proarc.common.workflow.WorkflowManager;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.JobFilter;
import cz.cas.lib.proarc.common.workflow.model.JobView;
import cz.cas.lib.proarc.common.workflow.model.MaterialFilter;
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
import cz.cas.lib.proarc.webapp.shared.rest.WorkflowResourceApi;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;

/**
 * I allows to manage workflow remotely.
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
            @QueryParam(WorkflowModelConsts.JOB_FILTER_PROFILENAME) String profileName,
            @QueryParam(WorkflowModelConsts.JOB_FILTER_OWNERID) BigDecimal userId,
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
        filter.setProfileName(profileName);
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
     * @param profileName profile name
     * @param metadata MODS
     * @param catalogId catalog ID
     * @return the job
     */
    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Job> addJob(
            @FormParam(WorkflowResourceApi.NEWJOB_PROFILE) String profileName,
            @FormParam(WorkflowResourceApi.NEWJOB_METADATA) String metadata,
            @FormParam(WorkflowResourceApi.NEWJOB_CATALOGID) String catalogId
    ) {
        if (metadata == null) {
            return SmartGwtResponse.asError(WorkflowResourceApi.NEWJOB_METADATA + " - missing value! ");
        }
        CatalogConfiguration catalog = appConfig.getCatalogs().findConfiguration(catalogId);
        if (catalog == null) {
            return SmartGwtResponse.asError(WorkflowResourceApi.NEWJOB_CATALOGID + " - invalid value! " + catalogId);
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
            return new SmartGwtResponse<Job>(job);
        } catch (Throwable ex) {
            LOG.log(Level.SEVERE,
                    WorkflowResourceApi.NEWJOB_PROFILE + ":" + profileName
                    + ", " + WorkflowResourceApi.NEWJOB_CATALOGID + ":" + catalogId
                    + ", " + WorkflowResourceApi.NEWJOB_METADATA + ":\n" + metadata,
                    ex);
            return SmartGwtResponse.asError(ex.getMessage());
        }
    }

    @Path(WorkflowResourceApi.TASK_PATH)
    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<TaskView> getTask(
            @QueryParam(WorkflowModelConsts.TASK_FILTER_ID) BigDecimal id,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_JOBID) BigDecimal jobId,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_PRIORITY) Integer priority,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_PROFILENAME) String profileName,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_STATE) Task.State state,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_OWNERID) BigDecimal userId,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_OFFSET) int startRow,
            @QueryParam(WorkflowModelConsts.TASK_FILTER_SORTBY) String sortBy
    ) {
        int pageSize = 100;
        TaskFilter filter = new TaskFilter();
        filter.setLocale(session.getLocale(httpHeaders));
        filter.setMaxCount(pageSize);
        filter.setOffset(startRow);
        filter.setSortBy(sortBy);

        filter.setId(id);
        filter.setJobId(jobId);
        filter.setPriority(priority);
        filter.setProfileName(profileName);
        filter.setState(state);
        filter.setUserId(userId);
        try {
            List<TaskView> tasks = workflowManager.findTask(filter);
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

    @Path(WorkflowResourceApi.MATERIAL_PATH)
    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<MaterialView> getMaterial(
            @QueryParam(WorkflowModelConsts.MATERIALFILTER_ID) BigDecimal id,
            @QueryParam(WorkflowModelConsts.MATERIALFILTER_JOBID) BigDecimal jobId,
            @QueryParam(WorkflowModelConsts.MATERIALFILTER_TASKID) BigDecimal taskId,
            @QueryParam(WorkflowModelConsts.MATERIALFILTER_OFFSET) int startRow,
            @QueryParam(WorkflowModelConsts.MATERIALFILTER_SORTBY) String sortBy
    ) {
        int pageSize = 100;
        MaterialFilter filter = new MaterialFilter();
        filter.setLocale(session.getLocale(httpHeaders));
        filter.setMaxCount(pageSize);
        filter.setOffset(startRow);
        filter.setSortBy(sortBy);

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
            @QueryParam(WorkflowProfileConsts.JOB_NAME_ATT) String name,
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
                JobDefinitionView p = new JobDefinitionView();
                p.setName(job.getName());
                p.setTitle(job.getTitle(lang, job.getName()));
                p.setHint(job.getHint(lang, null));
                p.setDisabled(job.isDisabled());
                profiles.add(p);
            }
        }
        return new SmartGwtResponse<JobDefinitionView>(profiles);
    }

    private static <T> SmartGwtResponse<T> profileError() {
        return SmartGwtResponse.asError("Invalid workflow.xml! Check server configuration.");
    }

}
