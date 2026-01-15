/*
 * Copyright (C) 2017 Martin Rumanek
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

import cz.cas.lib.proarc.common.workflow.model.MaterialFilter;
import cz.cas.lib.proarc.common.workflow.model.MaterialView;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.profile.ActionDefinition;
import cz.cas.lib.proarc.common.workflow.profile.TaskDefinition;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowDefinition;
import org.apache.commons.beanutils.BeanUtils;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

/**
 * Workflow action handler
 * <p>
 * Running actions after task changes
 *
 * @author Martin Rumanek
 */
public class WorkflowActionHandler {
    private WorkflowDefinition workflow;
    private final WorkflowManager workflowManager = WorkflowManager.getInstance();
    private final Locale locale;

    /**
     *
     * @param workflow loaded from workflow.xml
     * @param locale   locale
     */
    public WorkflowActionHandler(WorkflowDefinition workflow, Locale locale) {
        this.workflow = workflow;
        this.locale = locale;
    }

    /**
     * Run command after task change. Task is defined in workflow.xml
     *
     * @param task changed task
     * @throws WorkflowException probably bad workflow profile
     * @throws IOException       error when executing the command
     * @see @linktourl{https://github.com/proarc/proarc/wiki/RDflow}
     */
    public void runAction(Task task) throws WorkflowException, IOException {

        TaskDefinition taskFromProfile = workflow.getTasks().stream().filter(taskDef -> taskDef.getName().equals(task.getTypeRef()))
                .findFirst().orElse(null);

        if (taskFromProfile != null) {
            for (ActionDefinition actionDefinition : taskFromProfile.getActions()) {
                MaterialFilter filter = new MaterialFilter();
                filter.setLocale(locale);
                filter.setTaskId(task.getId());
                for (MaterialView material : workflowManager.findMaterial(filter)) {
                    List<String> command = new ArrayList<>(Collections.singletonList(actionDefinition.getCommand()));
                    for (String arg : actionDefinition.getArgs()) {
                        try {
                            command.add(BeanUtils.getProperty(material, arg));
                        } catch (IllegalAccessException e) {
                            throw new WorkflowException("caller does not have access to the property accessor method");
                        } catch (InvocationTargetException e) {
                            throw new WorkflowException("the property accessor method throws an exception");
                        } catch (NoSuchMethodException e) {
                            throw new WorkflowException("an accessor method for this property cannot be found");
                        }
                    }
                    new ProcessBuilder(command).start();
                }
            }
        }

    }
}
