/*
 * Copyright (C) 2021 Lukas Sykora
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
package cz.cas.lib.proarc.common.process.export.workflow;


import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.process.export.ExportUtils;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.process.export.mets.NdkExport;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.workflow.WorkflowException;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.model.TaskView;
import java.io.File;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;

/**
 * Get information from Workflow
 *
 * @author Lukáš Sýkora
 */
public class WorkflowExport {

    private final AppConfiguration appConfiguration;
    private final AkubraConfiguration akubraConfiguration;
    private Locale locale;
    private UserProfile user;

    public WorkflowExport(AppConfiguration appConfig, AkubraConfiguration akubraConfiguration, UserProfile user, Locale locale) {
        this.appConfiguration = appConfig;
        this.akubraConfiguration = akubraConfiguration;
        this.locale = locale;
        this.user = user;
    }


    public void export(File targetFolder, List<NdkExport.Result> results, String asFedoraLog) throws MetsExportException, DigitalObjectException, WorkflowException {
        targetFolder = getValidChildFolder(targetFolder);
        WorkflowExportFile wf = new WorkflowExportFile();
        for (NdkExport.Result result : results) {
            addJob(wf, getRoot(result.getPid(), targetFolder));
        }
        if (wf.getWorkflowJobExports().size() > 0) {
            ExportUtils.writeWorkflowResult(targetFolder, wf);
        }
    }

    private void addJob(WorkflowExportFile wf, IMetsElement root) throws DigitalObjectException, WorkflowException {
        if (root != null) {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            DigitalObjectManager.CreateHandler handler = dom.create(root.getModel(), root.getOriginalPid(), null, user, null, null);
            Job job = handler.getWfJob(root.getOriginalPid(), locale);
            if (job == null) {
                return;
            }
            WorkflowExportFile.WorkflowJobExport jobExport = new WorkflowExportFile.WorkflowJobExport();
            jobExport.setProfile(job.getProfileName());
            jobExport.setLabel(job.getLabel());
            jobExport.setUuid(root.getOriginalPid());
            jobExport.setModel(root.getModel().length() > 12 ? root.getModel().substring(12) : root.getModel());

            wf.getWorkflowJobExports().add(jobExport);

            List<TaskView> tasks = handler.getTask(job.getId(), locale);
            for (TaskView task : tasks) {
                WorkflowExportFile.WorkflowTaskExport taskExport = new WorkflowExportFile.WorkflowTaskExport();
                taskExport.setTaskId(task.getTypeRef());
                taskExport.setUser(task.getUserName());
                taskExport.setTaskName(task.getProfileLabel());
                taskExport.setState(transform(task.getStateAsString()));
                if (Task.State.CANCELED.name().equals(task.getStateAsString()) || Task.State.FINISHED.name().equals(task.getStateAsString())) {
                    taskExport.setTime(task.getTimestamp());
                }

                jobExport.getWorkflowTasks().add(taskExport);
            }
            if (root.getChildren() != null) {
                for (IMetsElement child : root.getChildren()) {
                    if (!child.getModel().contains("page")) {
                        addJob(wf, child);
                    }
                }
            }
        }
    }

    private String transform(String state) {
        LinkedHashMap<String, String> allTaskStates = new LinkedHashMap<String, String>() {{
            put(Task.State.WAITING.name(), "Čeká");
            put(Task.State.READY.name(), "Připraven");
            put(Task.State.STARTED.name(), "Probíhá");
            put(Task.State.FINISHED.name(), "Dokončen");
            put(Task.State.CANCELED.name(), "Zrušen");
        }};
        return allTaskStates.get(state);
    }

    private IMetsElement getRoot(String pid, File exportFolder) throws MetsExportException {
        MetsContext metsContext = null;
        ProArcObject object = null;

        try {
            if (Storage.FEDORA.equals(appConfiguration.getTypeOfStorage())) {
                FedoraStorage fedoraStorage = FedoraStorage.getInstance();
                object = fedoraStorage.find(pid);
                metsContext = MetsContext.buildFedoraContext(object, null, exportFolder, fedoraStorage, appConfiguration.getNdkExportOptions());
            } else if (Storage.AKUBRA.equals(appConfiguration.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                object = akubraStorage.find(pid);
                metsContext = MetsContext.buildAkubraContext(object, null, exportFolder, akubraStorage, appConfiguration.getNdkExportOptions());
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfiguration.getTypeOfStorage());
            }
        } catch (Exception ex) {
            throw new IllegalStateException(ex);
        }
        return getMetsElement(object, metsContext, true);
    }

    private MetsElement getMetsElement(ProArcObject fo, MetsContext metsContext, boolean hierarchy) throws MetsExportException {
        metsContext.resetContext();
        DigitalObject dobj = MetsUtils.readFoXML(fo.getPid(), metsContext);
        if (dobj == null) {
            throw new MetsExportException("Missing uuid");
        }
        return MetsElement.getElement(dobj, null, metsContext, hierarchy);
    }

    private File getValidChildFolder(File targetFolder) {
        if (targetFolder.isDirectory()) {
            for (File target : targetFolder.listFiles()) {
                if (target.isDirectory()) {
                    return target;
                }
            }
        }
        return targetFolder;
    }
}
