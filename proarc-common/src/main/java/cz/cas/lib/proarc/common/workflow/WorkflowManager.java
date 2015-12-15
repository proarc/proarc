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
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.workflow.model.DigitalMaterial;
import cz.cas.lib.proarc.common.workflow.model.FolderMaterial;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.Job.State;
import cz.cas.lib.proarc.common.workflow.model.JobFilter;
import cz.cas.lib.proarc.common.workflow.model.JobView;
import cz.cas.lib.proarc.common.workflow.model.Material;
import cz.cas.lib.proarc.common.workflow.model.MaterialFilter;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.MaterialView;
import cz.cas.lib.proarc.common.workflow.model.PhysicalMaterial;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.model.TaskFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskParameter;
import cz.cas.lib.proarc.common.workflow.model.TaskParameterFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskParameterView;
import cz.cas.lib.proarc.common.workflow.model.TaskView;
import cz.cas.lib.proarc.common.workflow.profile.JobDefinition;
import cz.cas.lib.proarc.common.workflow.profile.MaterialDefinition;
import cz.cas.lib.proarc.common.workflow.profile.SetMaterialDefinition;
import cz.cas.lib.proarc.common.workflow.profile.SetParamDefinition;
import cz.cas.lib.proarc.common.workflow.profile.StepDefinition;
import cz.cas.lib.proarc.common.workflow.profile.TaskDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkerDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
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
    private final WorkflowProfiles wp;

    public WorkflowManager(WorkflowProfiles wp, DaoFactory daoFactory, UserManager users) {
        this.daoFactory = daoFactory;
        this.userMgr = users;
        this.wp = wp;
    }

    public Job getJob(BigDecimal id) {
        Transaction tx = daoFactory.createTransaction();
        WorkflowJobDao jobDao = daoFactory.createWorkflowJobDao();
        jobDao.setTransaction(tx);
        try {
            return jobDao.find(id);
        } finally {
            tx.close();
        }
    }

    public List<JobView> findJob(JobFilter filter) {
        WorkflowDefinition wd = wp.getProfiles();
        Transaction tx = daoFactory.createTransaction();
        WorkflowJobDao jobDao = daoFactory.createWorkflowJobDao();
        jobDao.setTransaction(tx);
        try {
            List<JobView> jobs = jobDao.view(filter);
            for (JobView job : jobs) {
                JobDefinition profile = wp.getProfile(wd, job.getProfileName());
                if (profile != null) {
                    job.setProfileLabel(profile.getTitle(filter.getLocale().getLanguage(), profile.getName()));
                } else {
                    job.setProfileLabel("Unknown job profile: " + job.getProfileName());
                }
            }
            return jobs;
        } finally {
            tx.close();
        }
    }

    public List<MaterialView> findMaterial(MaterialFilter filter) {
        WorkflowDefinition wd = wp.getProfiles();
        Transaction tx = daoFactory.createTransaction();
        WorkflowMaterialDao dao = daoFactory.createWorkflowMaterialDao();
        dao.setTransaction(tx);
        try {
            List<MaterialView> mats = dao.view(filter);
            for (MaterialView m : mats) {
                MaterialDefinition matProfile = wp.getMaterialProfile(wd, m.getName());
                if (matProfile != null) {
                    m.setProfileLabel(matProfile.getTitle(
                            filter.getLocale().getLanguage(),
                            matProfile.getName()));
                } else {
                    m.setProfileLabel("Unknown material profile: " + m.getName());
                }
            }
            return mats;
        } finally {
            tx.close();
        }
    }

    public Material updateMaterial(MaterialView view) throws ConcurrentModificationException, WorkflowException {
        Transaction tx = daoFactory.createTransaction();
        WorkflowMaterialDao dao = daoFactory.createWorkflowMaterialDao();
        WorkflowJobDao jobDao = daoFactory.createWorkflowJobDao();
        dao.setTransaction(tx);
        jobDao.setTransaction(tx);
        try {
            Material m = dao.find(view.getId());
            if (m == null) {
                throw new WorkflowException("Material not found: " + view.getId())
                        .addMaterialNotFound(view.getId());
            }
            if (m.getType() != view.getType()) {
                throw new WorkflowException("Material type mismatch: "
                        + view.getId() + ", " + m.getType() + "!=" + view.getType());
            }
            // check job state
            Job job = dao.findJob(m);
            if (m == null) {
                throw new WorkflowException("Job not found! Material: " + view.getId());
            }
            if (job.getState() != State.OPEN) {
                throw new WorkflowException("Job is closed!").addJobIsClosed();
            }
            m.setNote(view.getNote());
            if (m.getType() == MaterialType.FOLDER) {
                FolderMaterial fm = (FolderMaterial) m;
                fm.setPath(view.getPath());
                fm.setLabel(view.getPath());
            } else if (m.getType() == MaterialType.DIGITAL_OBJECT) {
                DigitalMaterial dm = (DigitalMaterial) m;
                String label = view.getPid();
                if (view.getPid() != null && !view.getPid().equals(dm.getPid())) {
                    List<Item> items = RemoteStorage.getInstance().getSearch().find(view.getPid());
                    if (!items.isEmpty()) {
                        label = items.get(0).getLabel();
                    }
                }
                dm.setPid(view.getPid());
                dm.setLabel(label);
            } else if (m.getType() == MaterialType.PHYSICAL_DOCUMENT) {
                String jobLabel = job.getLabel();
                PhysicalMaterial pm = (PhysicalMaterial) m;
                pm.setBarcode(view.getBarcode());
                pm.setField001(view.getField001());
                String newMetadata = view.getMetadata();
                String oldMetadata = pm.getMetadata();
                if (newMetadata == null ? oldMetadata != null : !newMetadata.equals(oldMetadata)) {
                    PhysicalMaterial t = new PhysicalMaterialBuilder().setMetadata(newMetadata).build();
                    pm.setMetadata(t.getMetadata());
                    pm.setLabel(t.getLabel());
                    jobLabel = pm.getLabel();
                }
                pm.setRdczId(view.getRdczId());
                pm.setSource(view.getSource());
                jobLabel = updateMaterialSignature(view.getSignature(), pm, jobLabel);
                if (jobLabel == null ? job.getLabel() != null : !jobLabel.equals(job.getLabel())) {
                    job.setLabel(jobLabel);
                    jobDao.update(job);
                }
            }
            dao.update(m);
            tx.commit();
            return m;
        } catch (ConcurrentModificationException t) {
            tx.rollback();
            throw t;
        } catch (WorkflowException t) {
            tx.rollback();
            throw t;
        } catch (Throwable t) {
            tx.rollback();
            throw new WorkflowException("Cannot update material: " + view.getId(), t).addUnexpectedError();
        } finally {
            tx.close();
        }
    }

    static String updateMaterialSignature(String newSignature, PhysicalMaterial update, String jobLabel) {
        String oldSignature = update.getSignature();
        if (newSignature == null ? oldSignature != null : !newSignature.equals(oldSignature)) {
            if (oldSignature != null && !oldSignature.isEmpty() && jobLabel.startsWith(oldSignature)) {
                jobLabel = jobLabel.replaceFirst(String.format("^%s[ ]*", oldSignature), "");
            }
            if (newSignature != null) {
                if ("?".equals(jobLabel) || jobLabel.isEmpty()) {
                    jobLabel = newSignature;
                } else {
                    jobLabel = newSignature + ' ' + jobLabel;
                }
            } else if (jobLabel.isEmpty()) {
                jobLabel = "?";
            }
        }
        update.setSignature(newSignature);
        return jobLabel;
    }

    public List<TaskParameterView> findParameter(TaskParameterFilter filter) {
        WorkflowDefinition wd = wp.getProfiles();
        Transaction tx = daoFactory.createTransaction();
        WorkflowParameterDao dao = daoFactory.createWorkflowParameterDao();
        dao.setTransaction(tx);
        try {
            List<TaskParameterView> params = dao.view(filter);
            Task task = null;
            if (params.isEmpty() && filter.getTaskId() != null) {
                WorkflowTaskDao taskDao = daoFactory.createWorkflowTaskDao();
                taskDao.setTransaction(tx);
                task = taskDao.find(filter.getTaskId());
            }
            return new FilterFindParameterQuery(wp).filter(params, filter, task, wd);
        } finally {
            tx.close();
        }
    }

    public Job updateJob(Job job) throws ConcurrentModificationException, WorkflowException {
        if (job.getId() == null) {
            throw new WorkflowException("Missing ID!");
        }
        Transaction tx = daoFactory.createTransaction();
        WorkflowJobDao jobDao = daoFactory.createWorkflowJobDao();
        WorkflowTaskDao taskDao = daoFactory.createWorkflowTaskDao();
        jobDao.setTransaction(tx);
        taskDao.setTransaction(tx);
        try {
            Job old = jobDao.find(job.getId());
            if (old == null) {
                throw new WorkflowException("Not found " + job.getId())
                        .addJobNotFound(job.getId());
            }
            // readonly properties
            job.setCreated(old.getCreated());
            job.setProfileName(old.getProfileName());
            jobDao.update(job);
            if (old.getState() != job.getState() && job.isClosed()) {
                // close all open tasks
                TaskFilter taskFilter = new TaskFilter();
                taskFilter.setJobId(job.getId());
                for (TaskView task : taskDao.view(taskFilter)) {
                    if (!task.isClosed()) {
                        task.setState(job.getState() == State.FINISHED
                                ? Task.State.FINISHED : Task.State.CANCELED);
                        taskDao.update(task);
                    }
                }
            }
            tx.commit();
            return job;
        } catch (ConcurrentModificationException t) {
            tx.rollback();
            throw t;
        } catch (WorkflowException ex) {
            tx.rollback();
            throw ex;
        } catch (Throwable t) {
            tx.rollback();
            throw new WorkflowException("Cannot update job: " + job.getId(), t)
                    .addUnexpectedError();
        } finally {
            tx.close();
        }
    }

    public TaskManager tasks() {
        return new TaskManager(daoFactory, wp, this);
    }

    public Job addJob(JobDefinition jobProfile, String xml,
            CatalogConfiguration catalog, UserProfile defaultUser
    ) throws WorkflowException {
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
                if (!step.isOptional()) {
                    Task task = createTask(taskDao, now, job, step, users, defaultUser);
                    createTaskParams(paramDao, step, task);
                    createMaterials(materialDao, step, task, materialCache, physicalMaterial);
                }
            }
            tx.commit();
            return job;
        } catch (WorkflowException ex) {
            tx.rollback();
            throw new WorkflowException("Cannot add job: " + jobProfile.getName(), ex)
                    .copy(ex);
        } catch (Throwable t) {
            tx.rollback();
            throw new WorkflowException("Cannot add job: " + jobProfile.getName(), t)
                    .addUnexpectedError();
        } finally {
            tx.close();
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
                .setState(step.getBlockers().isEmpty() ? Task.State.READY : Task.State.WAITING)
                .addTimestamp(now)
                .addTypeRef(step.getTask().getName());
        taskDao.update(task);
        return task;
    }

    List<TaskParameter> createTaskParams(WorkflowParameterDao paramDao,
            StepDefinition step, Task task) throws WorkflowException {

        ArrayList<TaskParameter> params = new ArrayList<TaskParameter>();
        for (SetParamDefinition setter : step.getParamSetters()) {
            params.add(paramDao.create()
                    .addParamRef(setter.getParam().getName())
                    .addTaskId(task.getId())
                    .addValue(setter.getParam().getValueType(), setter.getValue()));
        }
        paramDao.add(task.getId(), params);
        return params;
    }

    void createMaterials(WorkflowMaterialDao dao, StepDefinition step,
            Task task, Map<String, Material> materialCache, PhysicalMaterial origin) {

        TaskDefinition taskProfile = step.getTask();
        for (SetMaterialDefinition setter : taskProfile.getMaterialSetters()) {
            MaterialDefinition mProfile = setter.getMaterial();
            String mName = mProfile.getName();
            MaterialType mType = mProfile.getType();
            Material m = materialCache.get(mName);
            if (m == null) {
                if (mType == MaterialType.FOLDER) {
                    m = new FolderMaterial();
                } else if (mType == MaterialType.PHYSICAL_DOCUMENT) {
                    if (origin != null && origin.getId() == null) {
                        m = origin;
                    } else {
                        m = new PhysicalMaterial();
                    }
                } else if (mType == MaterialType.DIGITAL_OBJECT) {
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

    static BigDecimal resolveUserId(WorkerDefinition worker,
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

    Map<String, UserProfile> createUserMap() {
        HashMap<String, UserProfile> map = new HashMap<String, UserProfile>();
        for (UserProfile up : userMgr.findAll()) {
            map.put(up.getUserName(), up);
        }
        return map;
    }

}
