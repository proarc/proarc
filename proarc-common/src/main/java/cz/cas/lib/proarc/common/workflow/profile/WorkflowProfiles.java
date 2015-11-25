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
package cz.cas.lib.proarc.common.workflow.profile;

import cz.cas.lib.proarc.common.i18n.BundleValue;
import cz.cas.lib.proarc.common.i18n.BundleValueMap;
import cz.cas.lib.proarc.common.object.ValueMap;
import cz.cas.lib.proarc.common.workflow.profile.ValueMapDefinition.ValueMapItemDefinition;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.UnmarshalException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.ValidationEvent;
import javax.xml.bind.util.ValidationEventCollector;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import org.apache.commons.io.FileUtils;
import org.xml.sax.SAXException;

/**
 * Reads actual configuration of workflow profiles.
 *
 * @author Jan Pokorsky
 */
public class WorkflowProfiles {

    private static final Logger LOG = Logger.getLogger(WorkflowProfiles.class.getName());
    private static WorkflowProfiles INSTANCE;

    private final File file;
    private long lastModified;
    private WorkflowDefinition profiles;

    public static WorkflowProfiles getInstance() {
        return INSTANCE;
    }

    public static void setInstance(WorkflowProfiles wp) {
        INSTANCE = wp;
    }

    /**
     * Creates a new workflow definition file if not exists.
     * @param target
     * @throws IOException error
     */
    public static void copyDefaultFile(File target) throws IOException {
        if (target.exists()) {
            return ;
        }
        FileUtils.copyURLToFile(WorkflowProfiles.class.getResource("workflow.xml"), target);
    }

    public WorkflowProfiles(File file) {
        if (file == null) {
            throw new NullPointerException();
        }
        this.file = file;
    }

    /**
     * Gets actual profiles or {@code null} if in case of an error.
     */
    public synchronized WorkflowDefinition getProfiles() {
        try {
            read();
            return profiles;
        } catch (JAXBException ex) {
            LOG.log(Level.SEVERE, file.toString(), ex);
            return null;
        }
    }

    public JobDefinition getProfile(WorkflowDefinition workflow, String name) {
        for (JobDefinition job : workflow.getJobs()) {
            if (job.getName().equals(name)) {
                return job;
            }
        }
        return null;
    }

    public TaskDefinition getTaskProfile(WorkflowDefinition workflow, String taskName) {
        for (TaskDefinition task : workflow.getTasks()) {
            if (task.getName().equals(taskName)) {
                return task;
            }
        }
        return null;
    }

    public ParamDefinition getParamProfile(TaskDefinition task, String paramName) {
        for (ParamDefinition paramDef : task.getParams()) {
            if (paramDef.getName().equals(paramName)) {
                return paramDef;
            }
        }
        return null;
    }

    public MaterialDefinition getMaterialProfile(WorkflowDefinition workflow, String materialName) {
        for (MaterialDefinition md : workflow.getMaterials()) {
            if (md.getName().equals(materialName)) {
                return md;
            }
        }
        return null;
    }

    public List<ValueMap> getValueMap(ValueMap.Context ctx) {
        WorkflowDefinition wd = getProfiles();
        if (wd == null) {
            return Collections.emptyList();
        }
        ArrayList<ValueMap> result = new ArrayList<ValueMap>();
        for (ValueMapDefinition vmd : wd.getValueMaps()) {
            if (vmd.getSource() == ValueMapSource.INTERNAL) {
                String id = vmd.getId();
                List<ValueMapItemDefinition> items = vmd.getItems();
                ArrayList<BundleValue> bitems = new ArrayList<BundleValue>();
                for (ValueMapItemDefinition vmitem : items) {
                    String key = vmitem.getKey() != null ? vmitem.getKey() : vmitem.getValue();
                    bitems.add(new BundleValue(key, vmitem.getValue()));
                }

                BundleValueMap bundleValueMap = new BundleValueMap();
                bundleValueMap.setMapId(id);
                bundleValueMap.setValues(bitems);
                result.add(bundleValueMap);
            }
        }
        result.add(getAllTaskValueMap(wd, ctx));
        return result;
    }

    private ValueMap<WorkflowItemView> getAllTaskValueMap(WorkflowDefinition wd, ValueMap.Context ctx) {
        String lang = ctx.getLocale().getLanguage();
        ArrayList<WorkflowItemView> taskList = new ArrayList<WorkflowItemView>(wd.getTasks().size());
        for (TaskDefinition task : wd.getTasks()) {
            taskList.add(new WorkflowItemView(task, lang));
        }
        return new ValueMap<WorkflowItemView>(
                WorkflowProfileConsts.WORKFLOWITEMVIEW_TASKS_VALUEMAP, taskList);
    }

    /**
     * Gets a list of sorted job's task names. The order of tasks is driven by
     * the position of their step declaration in XML and by their blockers.
     */
    public List<String> getSortedTaskNames(JobDefinition job) {
        List<String> sortedTasks = new ArrayList<String>(job.getSteps().size());
        // tasks sorted by step declaration; taskName->blockers
        Map<String, Set<String>> taskDeps = new LinkedHashMap<String, Set<String>>();
        for (StepDefinition step : job.getSteps()) {
            String stepTaskName = step.getTask().getName();
            Set<String> blockers = taskDeps.get(stepTaskName);
            if (blockers == null) {
                blockers = new HashSet<String>();
                taskDeps.put(stepTaskName, blockers);
            } else {
                throw new IllegalStateException("A duplicate step declaration: "
                        + job.getName() + ", " + stepTaskName);
            }
            for (BlockerDefinition blocker : step.getBlockers()) {
                if (stepTaskName.equals(blocker.getTask().getName())) {
                    // short cycle, ignore
                    continue;
                }
                blockers.add(blocker.getTask().getName());
            }
        }
        while (!taskDeps.isEmpty()) {
            List<String> shakedOffDeps = shakeOffUnblockedTasks(taskDeps);
            if (shakedOffDeps.isEmpty() && !taskDeps.isEmpty()) {
                throw new IllegalStateException("There must be a cycle: "
                        + job.getName() + ", " + taskDeps);
            }
            sortedTasks.addAll(shakedOffDeps);
        }
        return sortedTasks;
    }

    /**
     * Removes unblocked tasks from the map and returns their ordered names.
     */
    private List<String> shakeOffUnblockedTasks(Map<String, Set<String>> taskDeps) {
        List<String> sortedTasks = new ArrayList<String>();
        for (Entry<String, Set<String>> taskEntry : taskDeps.entrySet()) {
            String taskName = taskEntry.getKey();
            Set<String> blockers = taskEntry.getValue();
            for (Iterator<String> it = blockers.iterator(); it.hasNext();) {
                if (!taskDeps.containsKey(it.next())) {
                    it.remove();
                }
            }
            if (blockers.isEmpty()) {
                sortedTasks.add(taskName);
            }
        }
        // remove resolved tasks
        for (String sortedTask : sortedTasks) {
            taskDeps.remove(sortedTask);
        }
        return sortedTasks;
    }

    private synchronized void setProfiles(WorkflowDefinition profiles, long time) {
        if (time > lastModified) {
            this.profiles = profiles;
            this.lastModified = time;
        }
    }

    private void read() throws JAXBException {
        long currentTime = file.lastModified();
        if (currentTime == lastModified) {
            return ;
        }
        Unmarshaller unmarshaller = getUnmarshaller();
        ValidationEventCollector errors = (ValidationEventCollector) unmarshaller.getEventHandler();
        WorkflowDefinition wf = null;
        try {
            wf = (WorkflowDefinition) unmarshaller.unmarshal(file);
            wf = errors.hasEvents() ? null : wf;
            readCaches(wf);
        } catch (UnmarshalException ex) {
            if (!errors.hasEvents()) {
                throw ex;
            }
        } finally {
            setProfiles(wf, currentTime);
        }
        if (errors.hasEvents()) {
            StringBuilder err = new StringBuilder();
            for (ValidationEvent event : errors.getEvents()) {
                err.append(event).append('\n');
            }
            throw new JAXBException(err.toString());
        }
    }

    private void readCaches(WorkflowDefinition wf) {
        if (wf == null) {
            return ;
        }
        for (JobDefinition job : wf.getJobs()) {
            job.setTaskNamesSortedByBlockers(Collections.unmodifiableList(getSortedTaskNames(job)));
        }
    }

    private Unmarshaller getUnmarshaller() throws JAXBException {
        JAXBContext jctx = JAXBContext.newInstance(WorkflowDefinition.class);
        Unmarshaller unmarshaller = jctx.createUnmarshaller();
        SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        URL schemaUrl = WorkflowDefinition.class.getResource("workflow.xsd");
        Schema schema = null;
        try {
            schema = sf.newSchema(new StreamSource(schemaUrl.toExternalForm()));
        } catch (SAXException ex) {
            throw new JAXBException("Missing schema workflow.xsd!", ex);
        }
        unmarshaller.setSchema(schema);
        ValidationEventCollector errors = new ValidationEventCollector() {

            @Override
            public boolean handleEvent(ValidationEvent event) {
                super.handleEvent(event);
                return true;
            }

        };
        unmarshaller.setEventHandler(errors);
        return unmarshaller;
    }

}
