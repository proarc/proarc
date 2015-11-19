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

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.model.TaskParameterFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskParameterView;
import cz.cas.lib.proarc.common.workflow.profile.DisplayType;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import java.io.File;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import org.apache.commons.io.FileUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.Rule;

/**
 *
 * @author Jan Pokorsky
 */
public class FilterFindParameterQueryTest {

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder(true);
    private WorkflowProfiles wp;

    @Before
    public void setUp() throws Exception {
        File xmlWorkflow = temp.newFile("workflowTest.xml");
        FileUtils.copyURLToFile(WorkflowManagerTest.class.getResource("WorkflowManagerAddProfile.xml"), xmlWorkflow);
        WorkflowProfiles.setInstance(new WorkflowProfiles(xmlWorkflow));
        wp = WorkflowProfiles.getInstance();
    }

    @Test
    public void testFilterNoDbParam() {
        List<TaskParameterView> params = Collections.emptyList();
        TaskParameterFilter filter = new TaskParameterFilter();
        filter.setTaskId(BigDecimal.TEN);
        filter.setLocale(Locale.ENGLISH);
        Task task = new Task();
        task.setTypeRef("task.id1");
        task.setId(filter.getTaskId());
        List<TaskParameterView> result = new FilterFindParameterQuery(wp).filter(params, filter, task, wp.getProfiles());
        assertEquals(2, result.size());
        assertEquals("param.id1", result.get(0).getParamRef());
        assertEquals(DisplayType.TEXT, result.get(0).getDisplayType());
        assertEquals(task.getTypeRef(), result.get(0).getTaskProfileName());
        assertEquals(filter.getTaskId(), result.get(0).getTaskId());
    }

    @Test
    public void testFilter() {
        String taskProfileName = "task.id1";
        TaskParameterView dbParam2 = new TaskParameterView();
        dbParam2.setJobId(BigDecimal.ZERO);
        dbParam2.setParamRef("param.id2");
        dbParam2.setTaskId(BigDecimal.TEN);
        dbParam2.setTaskProfileName(taskProfileName);
        dbParam2.addValueString("testVal");
        List<TaskParameterView> params = Arrays.asList(dbParam2);
        TaskParameterFilter filter = new TaskParameterFilter();
        filter.setTaskId(BigDecimal.TEN);
        filter.setLocale(Locale.ENGLISH);
        List<TaskParameterView> result = new FilterFindParameterQuery(wp)
                .filter(params, filter, null, wp.getProfiles());
        assertEquals(2, result.size());
        assertEquals("param.id1", result.get(0).getParamRef());
        assertEquals(DisplayType.TEXT, result.get(0).getDisplayType());
        assertEquals(taskProfileName, result.get(0).getTaskProfileName());
        assertEquals(filter.getTaskId(), result.get(0).getTaskId());

        assertEquals("param.id2", result.get(1).getParamRef());
        assertEquals("testVal", result.get(1).getValue());
        assertEquals(DisplayType.TEXT, result.get(1).getDisplayType());
        assertEquals(taskProfileName, result.get(1).getTaskProfileName());
        assertEquals(filter.getTaskId(), result.get(1).getTaskId());
    }

}
