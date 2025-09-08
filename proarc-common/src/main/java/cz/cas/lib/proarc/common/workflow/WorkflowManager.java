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

import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.dao.ConcurrentModificationException;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.dao.Transaction;
import cz.cas.lib.proarc.common.dao.WorkflowJobDao;
import cz.cas.lib.proarc.common.dao.WorkflowMaterialDao;
import cz.cas.lib.proarc.common.dao.WorkflowParameterDao;
import cz.cas.lib.proarc.common.dao.WorkflowTaskDao;
import cz.cas.lib.proarc.common.device.Device;
import cz.cas.lib.proarc.common.device.DeviceRepository;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraTransaction;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
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
import cz.cas.lib.proarc.common.workflow.profile.BlockerDefinition;
import cz.cas.lib.proarc.common.workflow.profile.JobDefinition;
import cz.cas.lib.proarc.common.workflow.profile.MaterialDefinition;
import cz.cas.lib.proarc.common.workflow.profile.ModelDefinition;
import cz.cas.lib.proarc.common.workflow.profile.SetMaterialDefinition;
import cz.cas.lib.proarc.common.workflow.profile.SetParamDefinition;
import cz.cas.lib.proarc.common.workflow.profile.StepDefinition;
import cz.cas.lib.proarc.common.workflow.profile.TaskDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkerDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import cz.cas.lib.proarc.urnnbn.ResolverUtils;
import java.io.IOException;
import java.io.StringReader;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.stream.StreamSource;
import org.apache.commons.lang.StringUtils;

import static cz.cas.lib.proarc.common.sql.DbUtils.close;
import static cz.cas.lib.proarc.common.sql.DbUtils.commit;
import static cz.cas.lib.proarc.common.sql.DbUtils.rollback;

/**
 * @author Jan Pokorsky
 */
public class WorkflowManager {

    private static WorkflowManager INSTANCE;
    private final AppConfiguration appConfiguration;
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

    public WorkflowManager(WorkflowProfiles wp, DaoFactory daoFactory, UserManager users, AppConfiguration appConfiguration) {
        this.daoFactory = daoFactory;
        this.userMgr = users;
        this.wp = wp;
        this.appConfiguration = appConfiguration;
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
            String lang = filter.getLocale().getLanguage();
            List<JobView> jobs = jobDao.view(filter);
            for (JobView job : jobs) {
                job.setUserName(getUser(job.getOwnerId()));
                JobDefinition profile = wp.getProfile(wd, job.getProfileName());
                if (profile != null) {
                    job.setProfileLabel(profile.getTitle(lang, profile.getName()));
                    job.setProfileHint(profile.getHint(lang, null));
                    if ((job.getModel() == null || job.getModel().isEmpty()) && (profile.getModel() != null && profile.getModel().size() > 0)) {
                        job.setModel(profile.getModel().get(0).getName());
                    }
                } else {
                    job.setProfileLabel(job.getProfileName());
                    job.setProfileHint("Unknown job XML ID: " + job.getProfileName());
                }
                TaskDefinition taskProfile = wp.getTaskProfile(wd, job.getTaskName());
                if (taskProfile != null) {
                    job.setTaskLabel(taskProfile.getTitle(lang, taskProfile.getName()));
                    job.setTaskHint(taskProfile.getHint(lang, null));
                } else {
                    job.setTaskLabel(job.getTaskName());
                    job.setTaskHint("Unknown task XML ID: " + job.getTaskName());
                }
                job.setDevice(getDevice(jobDao.getDevice(job.getId())));
            }
            if (filter.getDeviceId() != null) {
                jobs = filterDeviceJobs(jobs, filter.getDeviceId());
            }
            return jobs;
        } finally {
            tx.close();
        }
    }

    private String getUser(BigDecimal ownerId) {
        UserManager manager = UserUtil.getDefaultManger();
        UserProfile profile = manager.find(ownerId.intValue());
        return profile.getUserName();
    }

    private List<JobView> filterDeviceJobs(List<JobView> jobs, String deviceId) {
        List<JobView> filteredJob = new ArrayList<>();
        for (JobView job : jobs) {
            if (deviceId.equals(job.getDeviceId())) {
                filteredJob.add(job);
            }
        }
        return filteredJob;
    }

    private Device getDevice(String deviceLabel) {
        if (deviceLabel == null || deviceLabel.isEmpty()) {
            return null;
        }
        DeviceRepository devRepo;
        Device retValDevice = null;
        try {
            if (Storage.FEDORA.equals(appConfiguration.getTypeOfStorage())) {
                devRepo = new DeviceRepository(FedoraStorage.getInstance(appConfiguration));
            } else if (Storage.AKUBRA.equals(appConfiguration.getTypeOfStorage())) {
                AkubraConfiguration akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfiguration.getConfigHome());
                devRepo = new DeviceRepository(AkubraStorage.getInstance(akubraConfiguration));
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfiguration.getTypeOfStorage());
            }
            List<Device> devices = devRepo.findAllDevices(appConfiguration, 0);
            for (Device device : devices) {
                if (deviceLabel.equals(device.getLabel())) {
                    retValDevice = device;
                    break;
                }
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        } finally {
            return retValDevice;
        }
    }

    public List<MaterialView> findMaterial(MaterialFilter filter) {
        Transaction tx = daoFactory.createTransaction();
        try {
            return findMaterial(filter, tx);
        } finally {
            tx.close();
        }
    }

    private List<MaterialView> findMaterial(MaterialFilter filter, Transaction tx) {
        WorkflowDefinition wd = wp.getProfiles();
        WorkflowMaterialDao dao = daoFactory.createWorkflowMaterialDao();
        dao.setTransaction(tx);
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
    }

    /**
     * Create a fedora object with metadata from physical material (connected to job) and its PID inserts to digital material in one transaction
     *
     * @param view physical material
     * @return digital material with PID
     */
    public MaterialView createDigitalMaterialFromPhysical(DigitalObjectManager.CreateHandler handler, MaterialView view, boolean createObject, boolean validation) throws ConcurrentModificationException, WorkflowException {
        Transaction tx = daoFactory.createTransaction();
        FedoraTransaction ftx = null;

        try {
            // fedora
            if (Storage.FEDORA.equals(appConfiguration.getTypeOfStorage())) {
                FedoraStorage fedoraStorage = FedoraStorage.getInstance();
                ftx = new FedoraTransaction(fedoraStorage);
                handler.setTransaction(ftx);
            }

            MaterialFilter filter = new MaterialFilter();
            filter.setLocale(Locale.ENGLISH);
            filter.setJobId(view.getJobId());
            filter.setType(MaterialType.DIGITAL_OBJECT);

            List<MaterialView> materials = findMaterial(filter, tx);

            if (materials.size() != 1) {
                throw new WorkflowException("Wrong number of digital materials").addUnexpectedError();
            }
            MaterialView digitalMaterial = materials.get(0);

            if (digitalMaterial.getPid() != null) {
                throw new WorkflowException("Already connected digital material").addUnexpectedError();
            }

            String mods = view.getMetadata();
            handler.setMetadataXml(mods);

            if (handler.getPid() == null && mods != null && !mods.isEmpty()) {
                String pid = FoxmlUtils.identifierAsPid(ResolverUtils.getIdentifier("uuid", ModsUtils.unmarshalModsType(new StreamSource(new StringReader(mods)))));
                if (pid == null || pid.isEmpty() || "uuid".equals(pid) || "uuid:".equals(pid)) {
                    pid = FoxmlUtils.createPid();
                }
                handler.setPid(pid);
            }
            SearchViewItem items = handler.createDigitalObject(createObject, validation);
            digitalMaterial.setPid(items.getPid());
            updateMaterial(digitalMaterial, tx);

            commit(tx, ftx);
        } catch (ConcurrentModificationException | WorkflowException e) {
            rollback(tx, ftx);
            throw e;
        } catch (Throwable t) {
            tx.rollback();
            throw new WorkflowException("Cannot update material: " + view.getId(), t).addUnexpectedError();
        } finally {
            close(tx, ftx);
        }

        return view;
    }

    public Material updateMaterial(MaterialView view) throws ConcurrentModificationException, WorkflowException {
        Transaction tx = daoFactory.createTransaction();
        try {
            Material material = updateMaterial(view, tx);
            tx.commit();
            return material;
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

    public Job getJobs(BigDecimal id) {
        Transaction tx = daoFactory.createTransaction();
        try {
            WorkflowMaterialDao dao = daoFactory.createWorkflowMaterialDao();
            dao.setTransaction(tx);
            Material m = dao.find(id);
            return dao.findJob(m);
        } finally {
            tx.close();
        }

    }

    private Material updateMaterial(MaterialView view, Transaction tx) throws ConcurrentModificationException, WorkflowException, IOException, FedoraClientException {
        WorkflowMaterialDao dao = daoFactory.createWorkflowMaterialDao();
        WorkflowJobDao jobDao = daoFactory.createWorkflowJobDao();
        dao.setTransaction(tx);
        jobDao.setTransaction(tx);
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
           /* if (view.getPid() != null && !view.getPid().equals(dm.getPid())) {
                List<Item> items = RemoteStorage.getInstance().getSearch().find(view.getPid());
                if (!items.isEmpty()) {
                    label = items.get(0).getLabel();
                }
            }*/
            dm.setPid(view.getPid());
            dm.setLabel(label);
            jobDao.update(job);
        } else if (m.getType() == MaterialType.PHYSICAL_DOCUMENT) {
            String jobLabel = job.getLabel();
            PhysicalMaterial pm = (PhysicalMaterial) m;
            pm.setBarcode(view.getBarcode());
            pm.setField001(view.getField001());
            String newMetadata = view.getMetadata();
            String oldMetadata = pm.getMetadata();

            pm.setRdczId(view.getRdczId());
            pm.setSource(view.getSource());
            pm.setSignature(view.getSignature());
            pm.setDetail(view.getDetail());
            pm.setIssue(view.getIssue());
            pm.setSigla(view.getSigla());
            pm.setVolume(view.getVolume());
            pm.setYear(view.getYear());
            pm.setEdition(view.getEdition());

            if (newMetadata == null ? oldMetadata != null : !newMetadata.equals(oldMetadata)) {
                WorkflowDefinition wd = WorkflowProfiles.getInstance().getProfiles();
                WorkflowProfiles wp = WorkflowProfiles.getInstance();
                JobDefinition jd = wp.getProfile(wd, job.getProfileName());
                List<ModelDefinition> models = jd.getModel();
                String model = null;
                if (models.size() > 0) {
                    model = models.get(0).getName();
                }
                PhysicalMaterial t = new PhysicalMaterialBuilder().setMetadata(newMetadata, model).build();
                pm.setMetadata(t.getMetadata());
                pm.setSignature(t.getSignature());
                pm.setSigla(t.getSigla());
                pm.setIssue(t.getIssue());
                pm.setVolume(t.getVolume());
                pm.setLabel(t.getLabel());
                pm.setYear(t.getYear());
                pm.setDetail(t.getDetail());
                pm.setBarcode(t.getBarcode());
                pm.setEdition(t.getEdition());
                jobLabel = t.getLabel();
            }
            if (jobLabel == null ? job.getLabel() != null : !jobLabel.equals(job.getLabel())) {
                job.setLabel(jobLabel);
                jobDao.update(job);
            }
        }
        dao.update(m);
        return m;
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

    public void deleteJob(JobFilter filter) throws WorkflowException {
        if (filter.getIds() == null && !filter.getIds().isEmpty()) {
            throw new WorkflowException("Missing ID!");
        }
        Transaction tx = daoFactory.createTransaction();
        WorkflowJobDao jobDao = daoFactory.createWorkflowJobDao();
        jobDao.setTransaction(tx);
        List<BigDecimal> todeleteJobIds = new ArrayList<>();
        try {
            for (BigDecimal id : filter.getIds()) {
                Job job = jobDao.find(id);
                if (job == null) {
                    throw new WorkflowException("Not found " + job.getId()).addJobNotFound(id);
                }
                JobFilter subjobFilter = new JobFilter();
                subjobFilter.setParentId(job.getId());
                List<JobView> subjobs = jobDao.view(subjobFilter);
                for (JobView subJob : subjobs) {
                    todeleteJobIds.add(subJob.getId());
                }
                todeleteJobIds.add(job.getId());

                for (BigDecimal jobId : todeleteJobIds) {
                    tasks().delete(jobId);
                    jobDao.delete(jobId);
                }
            }
            tx.commit();
        } catch (ConcurrentModificationException | WorkflowException t) {
            tx.rollback();
            throw t;
        } catch (Throwable t) {
            tx.rollback();
            throw new WorkflowException("Cannot delete jobs: " + filter.getIds().toString(), t).addUnexpectedError();
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

            if (old.getState() != job.getState()) {
                if (job.isClosed() && job.getParentId() == null) {
                    // check subjobs if any state is open
                    JobFilter subjobFilter = new JobFilter();
                    subjobFilter.setParentId(job.getId());
                    List<JobView> subjobs = jobDao.view(subjobFilter);
                    for (JobView subjob : subjobs) {
                        if (!subjob.isClosed()) {
                            throw new WorkflowException(
                                    String.format("Job ID:%s, open subjob ID:%s",
                                            job.getId(), subjob.getId())
                            ).addJobBlockedWithSubjob();
                        }
                    }
                    if (job.isFinished()) {
                        TaskFilter taskFilter = new TaskFilter();
                        taskFilter.setJobId(job.getId());
                        for (TaskView task : taskDao.view(taskFilter)) {
                            if (Task.State.READY.equals(task.getState()) || Task.State.STARTED.equals(task.getState()) || Task.State.WAITING.equals(task.getState())) {
                                throw new WorkflowException(
                                        String.format("Job ID:%s, open task ID:%s",
                                                job.getId(), task.getId())).addJobBlockedWithTask();
                            }
                        }
                    }
                }
            }

            // readonly properties
            job.setCreated(old.getCreated());
            job.setProfileName(old.getProfileName());
            job.setModel(old.getModel());
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
        } catch (ConcurrentModificationException | WorkflowException t) {
            tx.rollback();
            throw t;
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

    public Job addJob(JobDefinition jobProfile, String xml, String model,
                      CatalogConfiguration catalog, BigDecimal rdczId, UserProfile defaultUser, AppConfiguration appConfiguration
    ) throws WorkflowException {
        Map<String, UserProfile> users = createUserMap();
        if (model == null || model.isEmpty()) {
            List<ModelDefinition> models = jobProfile.getModel();
            if (models.size() > 0) {
                model = models.get(0).getName();
            }
        }
        PhysicalMaterial physicalMaterial = new PhysicalMaterialBuilder()
                .setCatalog(catalog).setMetadata(xml, model).setRdczId(rdczId)
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
        String jobLabel = StringUtils.defaultIfEmpty(physicalMaterial.getLabel(), "?");

        try {
            Job job = createJob(jobDao, now, jobLabel, jobProfile, model,null, users, defaultUser);
            Map<String, Material> materialCache = new HashMap<>();
            Integer order = 1;
            for (StepDefinition step : jobProfile.getSteps()) {
                if (!step.isOptional()) {
                    Task task = createTask(taskDao, now, job, jobProfile, step, users, defaultUser, order++);
                    createTaskParams(paramDao, step, task);
                    createMaterials(materialDao, step, task, materialCache, physicalMaterial, appConfiguration);
                }
            }
            tx.commit();
            return job;
        } catch (Throwable ex) {
            tx.rollback();
            WorkflowException nex = new WorkflowException("Cannot add job: " + jobProfile.getName(), ex);
            throw (ex instanceof WorkflowException) ? nex.copy((WorkflowException) ex) : nex.addUnexpectedError();
        } finally {
            tx.close();
        }
    }

    public Job addSubjob(JobDefinition jobProfile, BigDecimal parentId, String model,
                         UserProfile defaultUser, WorkflowDefinition profiles, AppConfiguration appConfiguration,
                         String xml, CatalogConfiguration catalog, BigDecimal rdeczId
    ) throws WorkflowException {
        Objects.requireNonNull(jobProfile, "jobProfile");
        Objects.requireNonNull(parentId, "parentId");
        Objects.requireNonNull(profiles, "profiles");
        Map<String, UserProfile> users = createUserMap();
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

        if (model == null || model.isEmpty()) {
            List<ModelDefinition> models = jobProfile.getModel();
            if (models.size() > 0) {
                model = models.get(0).getName();
            }
        }
        PhysicalMaterial physicalMaterial = new PhysicalMaterialBuilder()
                .setCatalog(catalog).setMetadata(xml, model).setRdczId(rdeczId)
                .build();

        try {
            Job parentJob = jobDao.find(parentId);
            if (parentJob == null) {
                throw new WorkflowException("The parent job not found! parent ID=" + parentId);
            } else if (parentJob.isClosed()) {
                throw new WorkflowException("The parent job not open! parent ID=" + parentId);
            }
            JobDefinition parentProfile = wp.getProfile(profiles, parentJob.getProfileName());
            if (parentProfile == null) {
                throw new WorkflowException("No job profile " + parentJob.getProfileName() + " for parent ID=" + parentId);
            }
            if (!parentProfile.getSubjobs().stream()
                    .anyMatch(p -> jobProfile.getName().equals(p.getJob().getName()))) {
                throw new WorkflowException(
                        String.format("The job '%s' does not permit '%s' as a subjob! Check your workflow.xml",
                                parentProfile.getName(), jobProfile.getName()));
            }
            // copy the parent's physical document
            MaterialFilter materialFilter = new MaterialFilter();
            materialFilter.setJobId(parentId);
            MaterialView mv = materialDao.view(materialFilter)
                    .stream().filter(m -> m.getType() == MaterialType.PHYSICAL_DOCUMENT)
                    .findFirst().orElse(null);
            String jobLabel = StringUtils.defaultIfEmpty(physicalMaterial.getLabel(), "?");
//            if (mv != null) {
//                physicalMaterial.setLabel(mv.getLabel());
//                jobLabel = physicalMaterial.getLabel();


                /* Issue #1254: bylo rozhodnotu, ze se nebudou kopirovat metadata do podzameru, schvaleno M. Nezbedovou dne 2020.12.11 */
                /*physicalMaterial.setBarcode(mv.getBarcode());
                physicalMaterial.setField001(mv.getField001());
                physicalMaterial.setDetail(mv.getDetail());

                physicalMaterial.setMetadata(mv.getMetadata());
                physicalMaterial.setName(mv.getName());
                physicalMaterial.setNote(mv.getNote());
                physicalMaterial.setRdczId(mv.getRdczId());
                physicalMaterial.setSigla(mv.getSigla());
                physicalMaterial.setSignature(mv.getSignature());
                physicalMaterial.setSource(mv.getSource());
                physicalMaterial.setVolume(mv.getVolume());
                physicalMaterial.setIssue(mv.getIssue());
                if (jobProfile.getModel().size() > 0 && NdkPlugin.MODEL_PERIODICALVOLUME.equals(jobProfile.getModel().get(0).getName())) {
                 physicalMaterial.setVolume(mv.getIssue());
                }
                physicalMaterial.setYear(mv.getYear());
//                physicalMaterial.setState(mv.getState());
                */
//            }

            Job job = createJob(jobDao, now, jobLabel, jobProfile, model, parentId, users, defaultUser);

            Map<String, Material> materialCache = new HashMap<>();
            Integer order = 1;
            for (StepDefinition step : jobProfile.getSteps()) {
                if (!step.isOptional()) {
                    Task task = createTask(taskDao, now, job, jobProfile, step, users, defaultUser, order++);
                    createTaskParams(paramDao, step, task);
                    createMaterials(materialDao, step, task, materialCache, physicalMaterial, appConfiguration);
                }
            }
            tx.commit();
            return job;
        } catch (Throwable ex) {
            tx.rollback();
            WorkflowException nex = new WorkflowException("Cannot add subjob: " + jobProfile.getName() + " to the parent job ID: " + parentId, ex);
            throw (ex instanceof WorkflowException) ? nex.copy((WorkflowException) ex) : nex.addUnexpectedError();
        } finally {
            tx.close();
        }
    }

    private Job createJob(WorkflowJobDao jobDao, Timestamp now, String jobLabel,
                          JobDefinition jobProfile, String model, BigDecimal parentId, Map<String, UserProfile> users, UserProfile defaultUser
    ) throws ConcurrentModificationException {

        Job job = jobDao.create().addCreated(now)
                .addLabel(jobLabel)
                .addOwnerId(resolveUserId(jobProfile.getWorker(), users, defaultUser, true))
                .addParentId(parentId)
                .addPriority(jobProfile.getPriority())
                .addProfileName(jobProfile.getName())
                .setState(Job.State.OPEN)
                .addModel(model)
                .addTimestamp(now);
        jobDao.update(job);
        return job;
    }

    private Task createTask(WorkflowTaskDao taskDao, Timestamp now,
                            Job job, JobDefinition jobProfile,
                            StepDefinition step,
                            Map<String, UserProfile> users, UserProfile defaultUser, Integer order
    ) throws ConcurrentModificationException {

        Task task = taskDao.create().addCreated(now)
                .addJobId(job.getId())
                .addOwnerId(resolveUserId(step.getWorker(), users, defaultUser, false))
                .addPriority(job.getPriority())
                .setState(isBlockedNewTask(step, jobProfile) ? Task.State.WAITING : Task.State.READY)
                .addTimestamp(now)
                .addTypeRef(step.getTask().getName())
                .addOrder(order);
        taskDao.update(task);
        return task;
    }

    /**
     * Checks whether there is a blocker declared as an non-optional step
     * of the newly created task step.
     * Do not use for DB tasks!
     */
    private static boolean isBlockedNewTask(StepDefinition newTaskStep, JobDefinition jobProfile) {
        for (BlockerDefinition blocker : newTaskStep.getBlockers()) {
            StepDefinition blockingStep = WorkflowProfiles.findStep(jobProfile, blocker.getTask().getName());
            if (blockingStep != null && !blockingStep.isOptional()) {
                return true;
            }
        }

        // rozsireni o novy blocker - ktery blokuje dokud vse pred nim neni ukonceno
        // neodkazuje se na urcite tasky, ale plati obecne pro vsechny pred nim
        if (!newTaskStep.getMainBlockers().isEmpty()) {
            return true;
        }

        return false;
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
                         Task task, Map<String, Material> materialCache, PhysicalMaterial origin, AppConfiguration config) {

        TaskDefinition taskProfile = step.getTask();
        for (SetMaterialDefinition setter : taskProfile.getMaterialSetters()) {
            MaterialDefinition mProfile = setter.getMaterial();
            String mName = mProfile.getName();
            MaterialType mType = mProfile.getType();
            Material m = materialCache.get(mName);
            if (m == null) {
                if (mType == MaterialType.FOLDER) {
                    m = new FolderMaterial();
                    setMaterialPath(m, mName, config);
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

    private void setMaterialPath(Material m, String mName, AppConfiguration config) {
        String label = "";
        switch (mName) {
            case "material.folder.rawScan":
                label = config.getWorkflowOptions().getRawScan();
                setPath(m, label);
                break;
            case "material.folder.masterCopy":
                label = config.getWorkflowOptions().getMasterCopy();
                setPath(m, label);
                break;
            case "material.folder.ocr":
                label = config.getWorkflowOptions().getOcr();
                setPath(m, label);
                break;
            case "material.folder.OcrAndProcessedImages":
                label = config.getWorkflowOptions().getOcrImage();
                setPath(m, label);
                break;
        }
    }

    private void setPath(Material m, String path) {
        ((FolderMaterial) m).setPath(path);
        m.setLabel(path);
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
            username = defaultUser == null ? null : defaultUser.getUserName();
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

    public List<BigDecimal> findMainJobId(List<JobView> jobs) {
        LinkedHashSet<BigDecimal> ids = new LinkedHashSet<>();
        for (JobView job : jobs) {
            if (job.getParentId() == null) {
                ids.add(job.getId());
            } else {
                ids.add(getParentJobId(getJob(job.getId())));
            }
        }
        return new ArrayList<>(ids);

    }

    private BigDecimal getParentJobId(Job job) {
        if (job.getParentId() == null) {
            return job.getId();
        }

        return getParentJobId(getJob(job.getParentId()));

    }
}
