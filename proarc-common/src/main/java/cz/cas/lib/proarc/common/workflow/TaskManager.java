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
package cz.cas.lib.proarc.common.workflow;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.ConcurrentModificationException;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.dao.Transaction;
import cz.cas.lib.proarc.common.dao.WorkflowJobDao;
import cz.cas.lib.proarc.common.dao.WorkflowMaterialDao;
import cz.cas.lib.proarc.common.dao.WorkflowParameterDao;
import cz.cas.lib.proarc.common.dao.WorkflowTaskDao;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.JobFilter;
import cz.cas.lib.proarc.common.workflow.model.JobView;
import cz.cas.lib.proarc.common.workflow.model.Material;
import cz.cas.lib.proarc.common.workflow.model.MaterialFilter;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.MaterialView;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.model.Task.State;
import cz.cas.lib.proarc.common.workflow.model.TaskFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskParameter;
import cz.cas.lib.proarc.common.workflow.model.TaskView;
import cz.cas.lib.proarc.common.workflow.profile.BlockerDefinition;
import cz.cas.lib.proarc.common.workflow.profile.JobDefinition;
import cz.cas.lib.proarc.common.workflow.profile.MainBlockerDefinition;
import cz.cas.lib.proarc.common.workflow.profile.ParamDefinition;
import cz.cas.lib.proarc.common.workflow.profile.StepDefinition;
import cz.cas.lib.proarc.common.workflow.profile.TaskDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import java.math.BigDecimal;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

/**
 *
 * @author Jan Pokorsky
 */
public class TaskManager {

    private final DaoFactory daoFactory;
    private final WorkflowProfiles wp;
    private final WorkflowManager wmgr;

    public TaskManager(DaoFactory daoFactory, WorkflowProfiles wp, WorkflowManager wmgr) {
        this.daoFactory = daoFactory;
        this.wp = wp;
        this.wmgr = wmgr;
    }

    public List<TaskView> findTask(TaskFilter filter, WorkflowDefinition wd) {
        final boolean findJobTasks = filter.getJobId() != null && filter.getId() == null;
        if (findJobTasks) {
            filter.setMaxCount(1000); // enough to read all tasks of a single job
        }
        Transaction tx = daoFactory.createTransaction();
        WorkflowTaskDao taskDao = daoFactory.createWorkflowTaskDao();
        taskDao.setTransaction(tx);
        try {
            String lang = filter.getLocale().getLanguage();
            List<TaskView> tasks = taskDao.view(filter);
            for (TaskView task : tasks) {
                TaskDefinition taskProfile = wp.getTaskProfile(wd, task.getTypeRef());
                if (taskProfile != null) {
                    task.setProfileLabel(taskProfile.getTitle(lang, taskProfile.getName()));
                    task.setProfileHint(taskProfile.getHint(lang, null));
                } else {
                    task.setProfileLabel(task.getTypeRef());
                    task.setProfileHint("Unknown task XML ID: " + task.getTypeRef());
                }
            }
            if (findJobTasks && !tasks.isEmpty()) {
                WorkflowJobDao jobDao = daoFactory.createWorkflowJobDao();
                jobDao.setTransaction(tx);
                Job job = jobDao.find(filter.getJobId());
                if (job != null) {
                    JobDefinition jobDef = wp.getProfile(wd, job.getProfileName());
                    if (jobDef != null) {
                        tasks = sortJobTaskByBlockers(jobDef, tasks);
                    }
                }
            }
            return tasks;
        } finally {
            tx.close();
        }
    }

    public Task getTask(BigDecimal id) {
        Transaction tx = daoFactory.createTransaction();
        try {
            WorkflowTaskDao dao = daoFactory.createWorkflowTaskDao();
            dao.setTransaction(tx);
            return dao.find(id);
        } finally {
            tx.close();
        }
    }

    public void delete(BigDecimal jobId) {
        Transaction tx = daoFactory.createTransaction();
        WorkflowTaskDao taskDao = daoFactory.createWorkflowTaskDao();
        WorkflowMaterialDao materialDao = daoFactory.createWorkflowMaterialDao();
        WorkflowParameterDao parameterDao = daoFactory.createWorkflowParameterDao();
        taskDao.setTransaction(tx);
        materialDao.setTransaction(tx);
        parameterDao.setTransaction(tx);
        try {

            TaskFilter taskFilter = new TaskFilter();
            taskFilter.setJobId(jobId);
            List<TaskView> tasks = taskDao.view(taskFilter);
            for (TaskView task : tasks) {

                parameterDao.remove(task.getId());

                MaterialFilter materialFilter = new MaterialFilter();
                materialFilter.setTaskId(task.getId());
                List<MaterialView> materials = materialDao.view(materialFilter);

                for (MaterialView materialView : materials) {
                    materialDao.delete(materialView.getId());
                }
            }
            taskDao.delete(jobId);
            tx.commit();
        } finally {
            tx.close();
        }
    }

    private <T extends Task> List<T> sortJobTaskByBlockers(JobDefinition job, List<T> tasks) {
        ArrayList<T> sorted = new ArrayList<T>(tasks.size());
        for (String sortTaskName : job.getTaskNamesSortedByBlockers()) {
            for (Iterator<T> it = tasks.iterator(); it.hasNext();) {
                T task = it.next();
                if (sortTaskName.equals(task.getTypeRef())) {
                    sorted.add(task);
                    it.remove();
                }
            }
            if (tasks.isEmpty()) {
                break;
            }
        }
        // add tasks not found in the workflow definition
        sorted.addAll(tasks);
        return sorted;
    }

    public Task updateTask(
            Task task, Map<String, Object> paramMap, WorkflowDefinition workflow
    ) throws ConcurrentModificationException, WorkflowException {
        List<TaskParameter> params = null;
        if (paramMap != null) {
            params = new ArrayList<TaskParameter>();
            TaskDefinition taskDef = wp.getTaskProfile(workflow, task.getTypeRef());
            if (taskDef != null) {
                for (Entry<String, Object> entry : paramMap.entrySet()) {
                    String paramName = entry.getKey();
                    Object paramValue = entry.getValue();
                    if (paramValue == null) {
                        continue;
                    }
                    String paramStringValue = String.valueOf(paramValue);
                    ParamDefinition paramDef = wp.getParamProfile(taskDef, paramName);
                    TaskParameter param = new TaskParameter().addParamRef(paramName)
                            .addTaskId(task.getId());
                    if (paramDef != null) {
                        param.addValue(paramDef.getValueType(), paramStringValue);
                        params.add(param);
                    } else {
                        // ignore unknown params
                    }
                }
            }
        }
        return updateTask(task, params, workflow);
    }

    public Task updateTask(
            Task task, List<TaskParameter> params, WorkflowDefinition workflow
    ) throws ConcurrentModificationException, WorkflowException {
        if (task.getId() == null) {
            throw new IllegalArgumentException("Missing ID!");
        }
        Transaction tx = daoFactory.createTransaction();
        WorkflowJobDao jobDao = daoFactory.createWorkflowJobDao();
        WorkflowTaskDao taskDao = daoFactory.createWorkflowTaskDao();
        WorkflowParameterDao paramDao = daoFactory.createWorkflowParameterDao();
        jobDao.setTransaction(tx);
        taskDao.setTransaction(tx);
        paramDao.setTransaction(tx);
        try {
            Task old = taskDao.find(task.getId());
            if (old == null) {
                throw new WorkflowException("Task not found: " + task.getId()).addTaskNotFound(task.getId());
            }
            // keep read-only properties
            task.setCreated(old.getCreated());
            task.setJobId(old.getJobId());
            task.setTypeRef(old.getTypeRef());
            Job job = jobDao.find(task.getJobId());
            if (job == null) {
                throw new WorkflowException("Job not found: " + task.getJobId()).addJobNotFound(task.getJobId());
            }
            Job.State jobState = job.getState();
            if (jobState != Job.State.OPEN ) {
                throw new WorkflowException("Job already closed: " + jobState).addJobIsClosed();
            }
            WorkflowDefinition profiles = wp.getProfiles();
            JobDefinition jobDef = wp.getProfile(profiles, job.getProfileName());
            if (jobDef == null) {
                throw new WorkflowException("Job name definiton not found! " + job.getProfileName())
                        .addInvalidXmlId(job.getProfileName())
                        .addUnexpectedError();
            }
            // check task state
            TaskFilter taskFilter = new TaskFilter();
            taskFilter.setJobId(job.getId());
            List<TaskView> jobTasks = sortJobTaskByBlockers(jobDef, taskDao.view(taskFilter));
            updateTaskState(taskDao, getTaskStateUpdates(task.getState(), old.getState(), task, jobDef, jobTasks));
            updateJobState(jobTasks, job, jobDao);
            taskDao.update(task);
            if (params != null) {
                paramDao.remove(task.getId());
                paramDao.add(task.getId(), params);
            }
            tx.commit();

            if (task.getTypeRef().startsWith("task.scan") && State.FINISHED.equals(task.getState())) {
                updateFolderPath(job);
            }

            return task;
        } catch (ConcurrentModificationException t) {
            tx.rollback();
            throw t;
        } catch (WorkflowException t) {
            tx.rollback();
            throw t;
        } catch (Throwable t) {
            tx.rollback();
            throw new IllegalStateException("Cannot update task: " + task.getId().toString(), t);
        } finally {
            tx.close();
        }
    }

    private void updateFolderPath(Job job) throws WorkflowException {
        String barcode = null;
        MaterialFilter filter = new MaterialFilter();
        filter.setJobId(job.getId());
        filter.setType(MaterialType.PHYSICAL_DOCUMENT);
        filter.setLocale(new Locale("cs", "CZ"));

        List<MaterialView> materials = wmgr.findMaterial(filter);
        if (materials.size() != 1) {
            return;
        }
        MaterialView material = materials.get(0);
        if (material == null) {
            return;
        }
        barcode = material.getBarcode();
        if (barcode == null || barcode.isEmpty()) {
            return;
        }

        filter = new MaterialFilter();
        filter.setJobId(job.getId());
        filter.setType(MaterialType.FOLDER);
        filter.setLocale(new Locale("cs", "CZ"));

        materials = wmgr.findMaterial(filter);
        if (materials.size() != 1) {
            return;
        }
        material = materials.get(0);
        if (material == null) {
            return;
        }
        String path = material.getPath();
        if (path == null || path.isEmpty()) {
            return;
        }

        path = adjustPath(path, barcode);

        material.setPath(path);
        material.setLabel(path);
        wmgr.updateMaterial(material);
    }

    public static String adjustPath(String folderPath, String barcodeFolder) {
        boolean hasDotPrefix = folderPath.startsWith(".");
        Path path = Paths.get(folderPath).normalize();

        // Získáme název poslední složky
        String lastFolder = path.getFileName().toString();

        String pathValue = null;
        if (lastFolder.equals(barcodeFolder)) {
            // Pokud poslední složka odpovídá tmpFolder, vrať původní cestu (normalizovanou)
            pathValue = path.toString();
        } else {
            // Pokud ne, přidej tmpFolder na konec
            pathValue = path.resolve(barcodeFolder).toString();
        }

        if (hasDotPrefix) {
            pathValue = "./" + pathValue;
        }
        if (pathValue.contains("\\")) {
            pathValue = pathValue.replace("\\", "/");
        }
        return pathValue;
    }

    private void updateJobState(List<TaskView> jobTasks, Job job, WorkflowJobDao jobDao) throws ConcurrentModificationException {
        boolean allCanceled = true;
        boolean allClosed = true;
        for (TaskView jobTask : jobTasks) {
            if (jobTask.getState() != State.CANCELED) {
                allCanceled = false;
            }
            if (!jobTask.isClosed()) {
                allClosed = false;
            }
        }
        if (allClosed) {
            // check if all subjobs are closed
            JobFilter subjobFilter = new JobFilter();
            subjobFilter.setParentId(job.getId());
            List<JobView> subjobs = jobDao.view(subjobFilter);
            boolean areAllSubjobsClosed = subjobs.stream().allMatch(j -> j.isClosed());
            if (areAllSubjobsClosed) {
                job.setState(allCanceled ? Job.State.CANCELED : Job.State.FINISHED);
                jobDao.update(job);
            }
        }
    }

    private void updateTaskState(WorkflowTaskDao dao, List<Task> updates) {
        for (Task update : updates) {
            dao.update(update);
        }
    }

    private List<Task> getTaskStateUpdates(State newState, State oldState,
            Task t, JobDefinition job, List<? extends Task> sortedTasks
    ) throws WorkflowException {
        if (oldState == newState) {
            return Collections.emptyList();
        }
        if (oldState == State.WAITING) {
            if (newState == State.READY || newState == State.STARTED || newState == State.FINISHED) {
                if (isBlocked(getStepDefinition(job, t.getTypeRef()), sortedTasks)) {
                    throw new WorkflowException("Task is blocked by other tasks!").addTaskBlocked();
                }
            } else {
            }
        } else if (newState == State.WAITING) {
            throw new WorkflowException("The task cannot be waiting again!").addTaskCannotWaitAgain();
        } else if (newState == State.FINISHED) {
            if (isBlocked(getStepDefinition(job, t.getTypeRef()), sortedTasks)) {
                throw new WorkflowException("Task is blocked by other tasks!").addTaskBlocked();
            }
        }
        // set new state
        t.setState(newState);
        List<Task> updates = new ArrayList<Task>();
        // resolve waiters
        for (Task jobTask : sortedTasks) {
            if (t.getId().compareTo(jobTask.getId()) == 0) {
                jobTask.setState(newState);
                continue;
            }
            if (jobTask.getState() == State.WAITING)  {
                boolean blocked = isBlocked(getStepDefinition(job, jobTask.getTypeRef()), sortedTasks);
                if (!blocked) {
                    jobTask.setState(State.READY);
                    updates.add(jobTask);
                }
            }
        }
        return updates;
    }

    private boolean isBlocked(StepDefinition step, List<? extends Task> sortedTasks) {
        if (step != null) {
            for (BlockerDefinition blocker : step.getBlockers()) {
                if (isBlocker(blocker.getTask().getName(), sortedTasks)) {
                    return true;
                }
            }
            for (MainBlockerDefinition mainBlocker : step.getMainBlockers()) {
                if (isMainBlocker(step.getTask().getName(), sortedTasks)) {
                    return true;
                }
            }
        }
        return false;
    }

    private boolean isMainBlocker(String currentTask, List<? extends Task> tasks) {
        for (Task task : tasks) {
            if (currentTask.equals(task.getTypeRef())) {
                return false;
            }
            if (!task.isClosed()) {
                return true;
            }
        }
        return false;
    }

    private boolean isBlocker(String name, List<? extends Task> tasks) {
        for (Task task : tasks) {
            if (name.equals(task.getTypeRef()) && !task.isClosed()) {
                return true;
            }
        }
        return false;
    }

    public Task addTask(
            BigDecimal jobId, String taskName, WorkflowDefinition workflow, UserProfile defaultUser, AppConfiguration config
    ) throws WorkflowException {
        Map<String, UserProfile> users = wmgr.createUserMap();
        Transaction tx = daoFactory.createTransaction();
        WorkflowJobDao jobDao = daoFactory.createWorkflowJobDao();
        WorkflowTaskDao taskDao = daoFactory.createWorkflowTaskDao();
        WorkflowParameterDao paramDao = daoFactory.createWorkflowParameterDao();
        WorkflowMaterialDao materialDao = daoFactory.createWorkflowMaterialDao();
        jobDao.setTransaction(tx);
        taskDao.setTransaction(tx);
        paramDao.setTransaction(tx);
        materialDao.setTransaction(tx);
        Timestamp now = new Timestamp(System.currentTimeMillis());
        try {
            Job job = jobDao.find(jobId);
            if (job == null) {
                throw new WorkflowException("Job not found: " + jobId).addJobNotFound(jobId);
            }
            Job.State jobState = job.getState();
            if (jobState != Job.State.OPEN ) {
                throw new WorkflowException("Job already closed: " + jobState).addJobIsClosed();
            }
            JobDefinition jobDef = wp.getProfile(workflow, job.getProfileName());
            if (jobDef == null || jobDef.isDisabled()) {
                throw new WorkflowException("Job definition not found: " + job.getProfileName())
                        .addInvalidXmlId(job.getProfileName());
            }
            StepDefinition step = getStepDefinition(jobDef, taskName);

            TaskFilter taskFilter = new TaskFilter();
            taskFilter.setJobId(job.getId());
            List<TaskView> jobTasks = sortJobTaskByBlockers(jobDef, taskDao.view(taskFilter));
            boolean blockedTask = isBlocked(step, jobTasks);

            Task task = createTask(taskDao, now, job, step, users, defaultUser, blockedTask);
            wmgr.createTaskParams(paramDao, step, task);
            if (!step.getTask().getMaterialSetters().isEmpty()) {
                Map<String, Material> materialCache = new HashMap<String, Material>();
                MaterialFilter mFilter = new MaterialFilter();
                mFilter.setJobId(jobId);
                List<MaterialView> materials = materialDao.view(mFilter);
                for (MaterialView material : materials) {
                    materialCache.put(material.getName(), material);
                }
                wmgr.createMaterials(materialDao, step, task, materialCache, null, config);
            }
            tx.commit();
            return task;
        } catch (WorkflowException t) {
            tx.rollback();
            throw t;
        } catch (Throwable t) {
            tx.rollback();
            throw new WorkflowException("Cannot add task: " + taskName, t).addUnexpectedError();
        } finally {
            tx.close();
        }
    }

    private StepDefinition getStepDefinition(JobDefinition jobDef, String taskName) throws WorkflowException {
        for (StepDefinition step : jobDef.getSteps()) {
            if (step.getTask().getName().equals(taskName)) {
                if (step.getTask().isDisabled()) {
                    throw new WorkflowException("Task disabled: " + taskName).addTaskDisabled(taskName);
                }
                return step;
            }
        }
        throw new WorkflowException("Step definition not found: " + taskName
                + " for " + jobDef.getName())
                .addInvalidXmlId(taskName).addUnexpectedError();
    }

    private Task createTask(WorkflowTaskDao taskDao, Timestamp now, Job job,
            StepDefinition step, Map<String, UserProfile> users, UserProfile defaultUser,
            boolean blocked
    ) throws ConcurrentModificationException {

        Task task = taskDao.create().addCreated(now)
                .addJobId(job.getId())
                .addOwnerId(WorkflowManager.resolveUserId(step.getWorker(), users, defaultUser, false))
                .addPriority(job.getPriority())
                .setState(blocked? Task.State.WAITING : Task.State.READY)
                .addTimestamp(now)
                .addTypeRef(step.getTask().getName());
        taskDao.update(task);
        return task;
    }
}
