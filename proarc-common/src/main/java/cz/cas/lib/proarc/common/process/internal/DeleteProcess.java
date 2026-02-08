package cz.cas.lib.proarc.common.process.internal;


import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.SearchViewQuery;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.PurgeAkubraObject;
import cz.cas.lib.proarc.common.storage.akubra.SolrUtils;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.fedora.PurgeFedoraObject;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.workflow.WorkflowActionHandler;
import cz.cas.lib.proarc.common.workflow.WorkflowException;
import cz.cas.lib.proarc.common.workflow.WorkflowManager;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.model.TaskFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskView;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.common.process.export.mets.MetsContext.buildAkubraContext;
import static cz.cas.lib.proarc.common.process.export.mets.MetsContext.buildFedoraContext;

public class DeleteProcess {

    private static final Logger LOG = Logger.getLogger(DeleteProcess.class.getName());

    private static AppConfiguration appConfig;
    private static AkubraConfiguration akubraConfiguration;

    private Locale locale;
    private UserProfile user;

    private List<String> pids;
    private Boolean purge;
    private Boolean restore;
    private Boolean hierarchy;

    public DeleteProcess(AppConfiguration appConfig, AkubraConfiguration akubraConfiguration, List<String> pids, Boolean purge, Boolean restore, boolean hierarchy, Locale locale, UserProfile user) {
        this.appConfig = appConfig;
        this.akubraConfiguration = akubraConfiguration;

        this.user = user;
        this.locale = locale;

        this.pids = pids;
        this.purge = purge;
        this.restore = restore;
        this.hierarchy = hierarchy;
    }

    public DeleteProcess(AppConfiguration appConfig, AkubraConfiguration akubraConfiguration, Boolean purge, Locale locale, UserProfile user) {
        this.appConfig = appConfig;
        this.akubraConfiguration = akubraConfiguration;

        this.user = user;
        this.locale = locale;

        this.pids = new ArrayList<>();
        this.purge = purge;
        this.restore = false;
        this.hierarchy = true;
    }

    public Result deleteObject() throws IOException, PurgeFedoraObject.PurgeException {
        Result result = new Result();

        for (String pid : pids) {
            try {
                setWorkflow("task.deletionPA", getIMetsElement(pid, false), this.locale, this.user);
            } catch (MetsExportException | DigitalObjectException | WorkflowException e) {
                if (e.getMessage() != null && e.getMessage().contains("low-level storage")) {
                    LOG.warning("Skiped setting task in workflow, " + e.getMessage() + " " + e.getStackTrace());
                } else if (e.getMessage() != null && e.getMessage().contains("Unable to get")) {
                    LOG.warning("Skiped setting task in workflow, " + e.getMessage() + " " + e.getStackTrace());
                } else {
                    e.printStackTrace();
                    result.setResult(e.getMessage());
                    return result;
                }
            }
        }

        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            FedoraStorage fedora = FedoraStorage.getInstance(appConfig);
            PurgeFedoraObject service = new PurgeFedoraObject(fedora);
            if (purge) { // opravneni na purge hlidano jiz v priprave procesu
                service.purge(pids, hierarchy, "Odstranění objektu");
            } else if (restore) {
                service.restore(pids, "Obnova objektu");
            } else {
                service.delete(pids, hierarchy, "Přidání příznaku smazáno");
            }
        } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            AkubraStorage akubra = AkubraStorage.getInstance(akubraConfiguration);
            PurgeAkubraObject service = new PurgeAkubraObject(akubra);
            if (purge) { // opravneni na purge hlidano jiz v priprave procesu
                service.purge(pids, hierarchy, "Odstranění objektu");
            } else if (restore) {
                service.restore(pids, "Obnova objektu");
            } else {
                service.delete(pids, hierarchy, "Přidání příznaku smazáno");
            }
        } else {
            result.setResult("Špatný druh uložiště. Mazání je podporována jen s AKUBROU nebo FEDOROU!");
        }
        return result;
    }

    private void setWorkflow(String type, IMetsElement root, Locale locale, UserProfile user) throws DigitalObjectException, WorkflowException {
        if (root != null) {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            DigitalObjectManager.CreateHandler handler = dom.create(root.getModel(), root.getOriginalPid(), null, user, null, "update workflow");
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
                editedTask.setOwnerId(new BigDecimal(user.getId()));
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

    public Result findAndDelete(String type) throws IOException, FedoraClientException, PurgeFedoraObject.PurgeException {
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

        int countDeleted = 0;
        Result result = new Result();

        while(true) {
            List<SearchViewItem> items = new ArrayList<>();
            switch (type.toLowerCase()) {
                case "orphan":
                    items = search.findAdvancedSearchItems(true, null, null, null, null, null, null, MetaModel.MODELS_LEAF, SolrUtils.PROPERTY_PARENTPID_NO_PARENT, "created", "desc", 0, 100);
                    break;
                case "deleted":
                    if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                        items = search.findQuery(new SearchViewQuery(), "deleted");
                        break;
                    } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                        items = search.findAdvancedSearchItems(false, null, null, null, null, null, null, MetaModel.MODELS_LEAF, SolrUtils.PROPERTY_PARENTPID_NO_PARENT, "created", "desc", 0, 100);
                        break;
                    } else {
                        throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
                    }
            }

            if (items.isEmpty()) {
                break;
            } else {
                List<String> pids = new LinkedList<>();
                for (SearchViewItem item : items) {
                    pids.add(item.getPid());
                }

                for (String pid : pids) {
                    try {
                        setWorkflow("task.deletionPA", getIMetsElement(pid, false), locale, user);
                    } catch (Exception e) {
                        if (e.getMessage() != null && e.getMessage().contains("low-level storage")) {
                            LOG.warning("Skiped setting task in workflow, " + e.getMessage() + " " + e.getStackTrace());
                        } else if (e.getMessage() != null && e.getMessage().contains("Unable to get")) {
                            LOG.warning("Skiped setting task in workflow, " + e.getMessage() + " " + e.getStackTrace());
                        } else {
                            e.printStackTrace();
                            result.setResult(e.getMessage());
                            return result;
                        }
                    }
                }

                if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                    FedoraStorage fedora = FedoraStorage.getInstance(appConfig);
                    PurgeFedoraObject service = new PurgeFedoraObject(fedora);
                    for (String pid : pids) {
                        service.purge(pid, true, "Odstranění objektu");
                        countDeleted++;
                    }
                } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                    AkubraStorage akubra = AkubraStorage.getInstance(akubraConfiguration);
                    PurgeAkubraObject service = new PurgeAkubraObject(akubra);
                    for (String pid : pids) {
                        service.purge(pid, true, "Odstranění objektu");
                        countDeleted++;
                    }
                } else {
                    throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
                }
            }
        }

        LOG.info(countDeleted + " objects deleted");
        return result;
    }

    public static class Result {

        private String result;

        public String getResult() {
            return result;
        }

        public void setResult(String result) {
            this.result = result;
        }

        public Boolean isStatusOk() {
            return result == null || result.isEmpty();
        }
    }
}
