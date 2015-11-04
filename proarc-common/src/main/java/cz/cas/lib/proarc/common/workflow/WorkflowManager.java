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

import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.dao.ConcurrentModificationException;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.dao.Transaction;
import cz.cas.lib.proarc.common.dao.WorkflowJobDao;
import cz.cas.lib.proarc.common.dao.WorkflowMaterialDao;
import cz.cas.lib.proarc.common.dao.WorkflowParameterDao;
import cz.cas.lib.proarc.common.dao.WorkflowTaskDao;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.workflow.model.DigitalMaterial;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.Material;
import cz.cas.lib.proarc.common.workflow.model.FolderMaterial;
import cz.cas.lib.proarc.common.workflow.model.PhysicalMaterial;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.model.TaskParameter;
import cz.cas.lib.proarc.common.workflow.profile.JobDefinition;
import cz.cas.lib.proarc.common.workflow.profile.MaterialDefinition;
import cz.cas.lib.proarc.common.workflow.profile.SetMaterialDefinition;
import cz.cas.lib.proarc.common.workflow.profile.SetParamDefinition;
import cz.cas.lib.proarc.common.workflow.profile.StepDefinition;
import cz.cas.lib.proarc.common.workflow.profile.TaskDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkerDefinition;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public class WorkflowManager {

    private static WorkflowManager INSTANCE;
    private static final Logger LOG = Logger.getLogger(WorkflowManager.class.getName());
    private final DaoFactory daoFactory;
    private final UserManager userMgr;

    public static WorkflowManager getInstance() {
        return INSTANCE;
    }

    public static void setInstance(WorkflowManager instance) {
        INSTANCE = instance;
    }

    public WorkflowManager(DaoFactory daoFactory, UserManager users) {
        this.daoFactory = daoFactory;
        this.userMgr = users;
    }

    public Job addJob(JobDefinition jobProfile, String xml,
            CatalogConfiguration catalog, UserProfile defaultUser
    ) {
        Map<String, UserProfile> users = createUserMap();
        PhysicalMaterial physicalMaterial = new PhysicalMaterialBuilder()
                .setCatalog(catalog).setMetadata(xml)
                .build();
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
        String jobLabel = physicalMaterial.getLabel();

        try {
            Job job = createJob(jobDao, now, jobLabel, jobProfile, users, defaultUser);
            Map<String, Material> materialCache = new HashMap<String, Material>();

            for (StepDefinition step : jobProfile.getSteps()) {
                Task task = createTask(taskDao, now, job, step, users, defaultUser);
                createTaskParams(paramDao, step, task);
                createMaterials(materialDao, step, task, materialCache, physicalMaterial);
            }
            tx.commit();
            return job;
        } finally {
            tx.rollback();
        }
    }

    private Job createJob(WorkflowJobDao jobDao, Timestamp now, String jobLabel,
            JobDefinition jobProfile, Map<String, UserProfile> users, UserProfile defaultUser
    ) throws ConcurrentModificationException {

        Job job = jobDao.create().addCreated(now)
                .addLabel(jobLabel)
                .addOwnerId(resolveUserId(jobProfile.getWorker(), users, defaultUser, true))
                .addPriority(jobProfile.getPriority())
                .addProfileName(jobProfile.getName())
                .setState(Job.State.OPEN)
                .addTimestamp(now);
        jobDao.update(job);
        return job;
    }

    private Task createTask(WorkflowTaskDao taskDao, Timestamp now, Job job,
            StepDefinition step, Map<String, UserProfile> users, UserProfile defaultUser
    ) throws ConcurrentModificationException {

        Task task = taskDao.create().addCreated(now)
                .addJobId(job.getId())
                .addOwnerId(resolveUserId(step.getWorker(), users, defaultUser, false))
                .addPriority(job.getPriority())
                .setState(Task.State.WAITING)
                .addTimestamp(now)
                .addTypeRef(step.getTask().getName());
        taskDao.update(task);
        return task;
    }

    private List<TaskParameter> createTaskParams(WorkflowParameterDao paramDao,
            StepDefinition step, Task task) {

        ArrayList<TaskParameter> params = new ArrayList<TaskParameter>();
        for (SetParamDefinition param : step.getParamSetters()) {
            params.add(paramDao.create()
                    .addParamRef(param.getParam().getName())
                    .addTaskId(task.getId())
                    .addValue(param.getValue()));
        }
        paramDao.add(task.getId(), params);
        return params;
    }

    private void createMaterials(WorkflowMaterialDao dao, StepDefinition step,
            Task task, Map<String, Material> materialCache, PhysicalMaterial origin) {

        TaskDefinition taskProfile = step.getTask();
        for (SetMaterialDefinition setter : taskProfile.getMaterialSetters()) {
            MaterialDefinition mProfile = setter.getMaterial();
            String mName = mProfile.getName();
            // cache.get(mName)
            Material m = materialCache.get(mName);
            if (m == null) {
                // XXX add material type to profile
                if (mName.contains("folder")) {
                    m = new FolderMaterial();
                } else if (mName.contains("physical")) {
                    if (origin.getId() == null) {
                        m = origin;
                    } else {
                        m = new PhysicalMaterial();
                    }
                } else if (mName.contains("digital")) {
                    m = new DigitalMaterial();
                } else {
                    LOG.log(Level.WARNING, "Unknown material: {0}", mName);
                    break;
                }
                m.setName(mName);
                materialCache.put(mName, m);
                dao.update(m);
            }
            dao.addTaskReference(m, task, setter.getWay());
        }
    }

    private static BigDecimal resolveUserId(WorkerDefinition worker,
            Map<String, UserProfile> users, UserProfile defaultUser,
            boolean emptyUserAsDefault) {

        UserProfile up = resolveUser(worker, users, defaultUser, emptyUserAsDefault);
        return up != null ? new BigDecimal(up.getId()) : null;
    }

    private static UserProfile resolveUser(WorkerDefinition worker,
            Map<String, UserProfile> users, UserProfile defaultUser,
            boolean emptyUserAsDefault) {

        String username;
        if (worker != null && worker.getActual()) {
            username = defaultUser == null ? null: defaultUser.getUserName();
        } else {
            username = worker == null ? null : worker.getUsername();
        }
        UserProfile up = users.get(username);
        return up != null
                ? up
                : emptyUserAsDefault ? defaultUser : null;
    }

    private Map<String, UserProfile> createUserMap() {
        HashMap<String, UserProfile> map = new HashMap<String, UserProfile>();
        for (UserProfile up : userMgr.findAll()) {
            map.put(up.getUserName(), up);
        }
        return map;
    }

}
