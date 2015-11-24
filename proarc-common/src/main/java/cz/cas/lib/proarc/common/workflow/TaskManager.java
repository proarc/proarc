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

import cz.cas.lib.proarc.common.dao.ConcurrentModificationException;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.dao.Transaction;
import cz.cas.lib.proarc.common.dao.WorkflowJobDao;
import cz.cas.lib.proarc.common.dao.WorkflowParameterDao;
import cz.cas.lib.proarc.common.dao.WorkflowTaskDao;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.model.TaskFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskParameter;
import cz.cas.lib.proarc.common.workflow.model.TaskView;
import cz.cas.lib.proarc.common.workflow.profile.JobDefinition;
import cz.cas.lib.proarc.common.workflow.profile.ParamDefinition;
import cz.cas.lib.proarc.common.workflow.profile.TaskDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

/**
 *
 * @author Jan Pokorsky
 */
public class TaskManager {

    private final DaoFactory daoFactory;
    private final WorkflowProfiles wp;

    public TaskManager(DaoFactory daoFactory, WorkflowProfiles wp) {
        this.daoFactory = daoFactory;
        this.wp = wp;
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
            List<TaskView> tasks = taskDao.view(filter);
            for (TaskView task : tasks) {
                TaskDefinition taskProfile = wp.getTaskProfile(wd, task.getTypeRef());
                if (taskProfile != null) {
                    task.setProfileLabel(taskProfile.getTitle(
                            filter.getLocale().getLanguage(),
                            taskProfile.getName()));
                } else {
                    task.setProfileLabel("Unknown task profile: " + task.getTypeRef());
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

    private List<TaskView> sortJobTaskByBlockers(JobDefinition job, List<TaskView> tasks) {
        ArrayList<TaskView> sorted = new ArrayList<TaskView>(tasks.size());
        for (String sortTaskName : job.getTaskNamesSortedByBlockers()) {
            for (Iterator<TaskView> it = tasks.iterator(); it.hasNext();) {
                TaskView task = it.next();
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

    public Task updateTask(Task task, Map<String, Object> paramMap, WorkflowDefinition workflow) throws ConcurrentModificationException {
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
        return updateTask(task, params);
    }

    public Task updateTask(Task task, List<TaskParameter> params) throws ConcurrentModificationException {
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
                throw new IllegalArgumentException("Task not found: " + task.getId());
            }
            // keep read-only properties
            task.setCreated(old.getCreated());
            task.setJobId(old.getJobId());
            task.setTypeRef(old.getTypeRef());
            Job job = jobDao.find(task.getJobId());
            if (job == null) {
                throw new IllegalArgumentException("Job not found: " + task.getJobId());
            }
            Job.State jobState = job.getState();
            if (jobState != Job.State.OPEN ) {
                throw new IllegalArgumentException("Job already closed: " + jobState);
            }
            // XXX check task state
            taskDao.update(task);
            if (params != null) {
                paramDao.remove(task.getId());
                paramDao.add(task.getId(), params);
            }
            tx.commit();
            return task;
        } catch (ConcurrentModificationException t) {
            tx.rollback();
            throw t;
        } catch (Throwable t) {
            tx.rollback();
            throw new IllegalStateException("Cannot update task: " + task.getId().toString(), t);
        } finally {
            tx.close();
        }
    }

}
