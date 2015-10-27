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

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.workflow.profile.ValueMapDefinition.ValueMapItemDefinition;
import cz.cas.lib.proarc.common.workflow.profile.ValueMapDefinition.ValueMapSource;
import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.xml.bind.JAXB;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.SchemaOutputResolver;
import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamResult;
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
public class WorkflowProfilesTest {

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder(true);

    public WorkflowProfilesTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testReadXmlFile() throws Exception {
        String xml = "<?xml version='1.0' encoding='UTF-8' standalone='yes'?>\n"
                + "<workflow xmlns='http://proarc.lib.cas.cz/xml/common/workflow/v1'>\n"
                + "    <job name='ndk' priority='1'>\n"
                + "        <worker>worker</worker>\n"
                + "        <step taskRef='task.id1'>\n"
                + "            <worker>step1Worker</worker>\n"
                + "            <setParam paramRef='param.id1'>param.id1.value</setParam>\n"
                + "        </step>"
                + "        <title>defaultTitle</title>\n"
                + "        <title lang='cs'>csTitle</title>\n"
                + "    </job>\n"

                + "    <materialType name='material0'/>\n"
                + "    <materialType name='material1'/>\n"

                + "    <task name='task.id1'>\n"
                + "        <param name='param.id1' required='true' datasource='workflow.valuemap.colors'/>\n"
                + "        <param name='param.id2' required='true' datasource='proarc.devices'/>\n"
                + "        <material type='material0' way='input'/>\n"
                + "        <title lang='cs'>Úkol 1</title>\n"
                + "        <title lang='en'>Task 1</title>\n"
                + "    </task>"

                + "    <valuemap name='workflow.valuemap.colors'>\n"
                + "      <value>barevně</value>\n"
                + "      <value>v šedi</value>\n"
                + "    </valuemap>\n"
                + "    <valuemap name='workflow.valuemap.colorsWithKeys' >\n"
                + "      <value key='colorful'>barevně</value>\n"
                + "      <value key='grey'>v šedi</value>\n"
                + "    </valuemap>\n"
                + "    <valuemap name='proarc.devices' source='proarc'/>\n"
                + "</workflow>"
                ;
        File xmlFile = new File(temp.getRoot(), "workflow.xml");
        FileUtils.write(xmlFile, xml);
        WorkflowProfiles profiles = new WorkflowProfiles(xmlFile);
        WorkflowDefinition wf = profiles.getProfiles();
        assertNotNull(wf);

        List<JobDefinition> jobs = wf.getJobs();
        assertFalse(jobs.isEmpty());
        JobDefinition job0 = jobs.get(0);
        assertEquals("ndk", job0.getName());
        assertEquals("worker", job0.getWorker());
        assertEquals(1, job0.getPriority());
        assertEquals("csTitle", job0.getTitles().get("cs"));
        assertEquals("defaultTitle", job0.getTitles().get(null));
        assertEquals("step1Worker", job0.getSteps().get(0).getWorker());

        List<SetParamDefinition> paramSetters = job0.getSteps().get(0).getParamSetters();
        assertFalse(paramSetters.isEmpty());
        assertEquals("param.id1", paramSetters.get(0).getParam().getName());
        assertEquals("param.id1.value", paramSetters.get(0).getValue());

        List<MaterialTypeDefinition> materials = wf.getMaterials();
        assertFalse(materials.isEmpty());
        assertEquals("material0", materials.get(0).getName());

        List<TaskDefinition> tasks = wf.getTasks();
        assertFalse(tasks.isEmpty());
        assertEquals(job0.getSteps().get(0).getTask(), tasks.get(0));
        assertEquals("task.id1", tasks.get(0).getName());

        assertEquals("workflow.valuemap.colors", wf.getValueMaps().get(0).getId());
        assertEquals(ValueMapSource.INTERNAL, wf.getValueMaps().get(0).getSource());
        assertEquals("v šedi", wf.getValueMaps().get(0).getItems().get(1).getValue());
        assertNull(wf.getValueMaps().get(0).getItems().get(1).getKey());
    }

    @Test
    public void testCopyDefaultFile() throws Exception {
        File wfFile = new File(temp.getRoot(), "wf.xml");
        WorkflowProfiles.copyDefaultFile(wfFile);
        assertTrue(wfFile.exists());
        assertTrue(wfFile.length() > 0);
        WorkflowProfiles wp = new WorkflowProfiles(wfFile);
        assertNotNull(wp.getProfiles());
    }

//    @Test
    public void testCreateSchema() throws Exception {
        JAXBContext jctx = JAXBContext.newInstance(WorkflowDefinition.class);
        final Map<String, StreamResult> schemas = new HashMap<String, StreamResult>();
        jctx.generateSchema(new SchemaOutputResolver() {

            @Override
            public Result createOutput(String namespaceUri, String suggestedFileName) throws IOException {
                StreamResult sr = new StreamResult(new StringWriter());
                sr.setSystemId(namespaceUri);
                schemas.put(namespaceUri, sr);
                return sr;
            }
        });
        System.out.println(schemas.get(WorkflowProfileConsts.NS_WORKFLOW_V1).getWriter().toString());
    }

    @Test
    public void testWriteXml() {
        WorkflowDefinition wf = new WorkflowDefinition();

        MaterialTypeDefinition material1 = new MaterialTypeDefinition().setName("material1");
        material1.getTitles().put("cs", "csMaterialTitle");
        material1.getHints().put("cs", "csMaterialDescription");

        TaskDefinition task1 = new TaskDefinition().setName("task.id1");
        task1.getTitles().put("cs", "Úkol 1");
        task1.getTitles().put("en", "Task 1");
        task1.getMaterials().add(new MaterialDefinition().setType(material1).setWay("input"));

        ParamDefinition param1 = new ParamDefinition().setName("param.id1").setRequired(true);
        task1.getParams().add(param1);

        JobDefinition job = new JobDefinition()
                .setName("job0")
                .setWorker("worker")
                .setPriority(3)
                .setDisabled(true);
        job.getTitles().put("cs", "csTitle");
        job.getTitles().put("en", "enTitle");
        job.getTitles().put(null, "defaultTitle");
        StepDefinition step1 = new StepDefinition().setTask(task1).setWorker("operator2");
        step1.getParamSetters().add(new SetParamDefinition().setParam(param1).setValue("param1.value"));
        job.getSteps().add(step1);
        wf.getJobs().add(job);

        wf.getMaterials().add(material1);

        wf.getTasks().add(task1);

        ValueMapDefinition vmap = new ValueMapDefinition().setId("workflow.valuemap.id1");
        vmap.getItems().add(new ValueMapItemDefinition().setKey("grey").setValue("v šedi"));
        wf.getValueMaps().add(vmap);

        JAXB.marshal(wf, System.out);
    }

}
