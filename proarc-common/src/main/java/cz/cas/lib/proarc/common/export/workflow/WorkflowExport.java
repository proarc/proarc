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
package cz.cas.lib.proarc.common.export.workflow;


import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.export.ExportUtils;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.NdkExport;
import cz.cas.lib.proarc.common.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
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

    private AppConfiguration appConfiguration;
    private Locale locale;
    private UserProfile user;

    public WorkflowExport(AppConfiguration appConfig, UserProfile user, Locale locale) {
        this.appConfiguration = appConfig;
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
        RemoteStorage rstorage = RemoteStorage.getInstance();
        RemoteStorage.RemoteObject fo = rstorage.find(pid);
        MetsContext mc = buildContext(rstorage, fo, null, exportFolder);
        return getMetsElement(fo, mc, true);
    }

    private MetsElement getMetsElement(RemoteStorage.RemoteObject fo, MetsContext dc, boolean hierarchy) throws MetsExportException {
        dc.resetContext();
        DigitalObject dobj = MetsUtils.readFoXML(fo.getPid(), fo.getClient());
        if (dobj == null) {
            throw new MetsExportException("Missing uuid");
        }
        return MetsElement.getElement(dobj, null, dc, hierarchy);
    }

    private MetsContext buildContext(RemoteStorage rstorage, RemoteStorage.RemoteObject fo, String packageId, File targetFolder) {
        MetsContext mc = new MetsContext();
        mc.setFedoraClient(fo.getClient());
        mc.setRemoteStorage(rstorage);
        mc.setPackageID(packageId);
        mc.setOutputPath(targetFolder.getAbsolutePath());
        mc.setAllowNonCompleteStreams(false);
        mc.setAllowMissingURNNBN(false);
        mc.setConfig(appConfiguration.getNdkExportOptions());
        return mc;
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
