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
package cz.cas.lib.proarc.common.dao.empiredb;

import cz.cas.lib.proarc.common.workflow.model.DigitalMaterial;
import cz.cas.lib.proarc.common.workflow.model.FolderMaterial;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.Material;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.MaterialFilter;
import cz.cas.lib.proarc.common.workflow.model.MaterialView;
import cz.cas.lib.proarc.common.workflow.model.PhysicalMaterial;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.profile.Way;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;
import org.dbunit.dataset.CompositeDataSet;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.ReplacementDataSet;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class EmpireWorkflowMaterialDaoTest {

    private DbUnitSupport support;
    private ProarcDatabase schema;
    private EmpireDaoFactory daos;
    private SqlTransaction tx;
    private EmpireWorkflowTaskDao daoTask;
    private Timestamp dbTimestamp;
    private EmpireWorkflowMaterialDao dao;

    public EmpireWorkflowMaterialDaoTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
        support = new DbUnitSupport();
        schema = support.getEmireCfg().getSchema();
        daos = new EmpireDaoFactory(support.getEmireCfg());
        daos.init();
        tx = daos.createTransaction();
        dao = daos.createWorkflowMaterialDao();
        dao.setTransaction(tx);
        daoTask = daos.createWorkflowTaskDao();
        daoTask.setTransaction(tx);
    }

    @After
    public void tearDown() {
        if (tx != null) {
            tx.close();
        }
    }

    private IDataSet database(IDataSet... ds) throws Exception {
        ReplacementDataSet rds = new ReplacementDataSet(new CompositeDataSet(ds));
        rds.addReplacementObject("{$user.home}", "relative/path/");
        dbTimestamp = new Timestamp(System.currentTimeMillis());
        rds.addReplacementObject("{$now}", dbTimestamp);
        return rds;
    }

    @Test
    public void testCreateFolder() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_task.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        FolderMaterial m = dao.create(MaterialType.FOLDER);
        m.setName("material.folder");
        m.setNote("mnote");
        dao.update(m);
        assertNotNull(m.getId());
        assertEquals(MaterialType.FOLDER, m.getType());

        FolderMaterial result = dao.find(m.getId());
        assertNotNull(result);
        assertMaterial(m, result);

        m.setLabel("/tmp");
        m.setPath("file:///tmp");
        dao.update(m);
        result = dao.find(m.getId());
        assertMaterial(m, result);
    }

    @Test
    public void testCreateDigital() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_task.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        DigitalMaterial m = dao.create(MaterialType.DIGITAL_OBJECT);
        assertEquals(MaterialType.DIGITAL_OBJECT, m.getType());
        m.setName("material.digital");
        m.setNote("dnote");
        dao.update(m);
        assertNotNull(m.getId());
        assertNotNull(m.getType());

        DigitalMaterial result = dao.find(m.getId());
        assertMaterial(m, result);

        m.setPid("uuid:ebfd7bf2-169d-476e-a230-0cc39f01764c");
        dao.update(m);

        result = dao.find(m.getId());
        assertMaterial(m, result);
        assertEquals("uuid:ebfd7bf2-169d-476e-a230-0cc39f01764c", result.getPid());
    }

    @Test
    public void testCreatePhysical() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_task.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        PhysicalMaterial m = dao.create(MaterialType.PHYSICAL_DOCUMENT);
        assertEquals(MaterialType.PHYSICAL_DOCUMENT, m.getType());
        m.setLabel("Label");
        m.setName("material.physical");
        m.setNote("pnote");

        dao.update(m);
        assertNotNull(m.getId());
        assertNotNull(m.getType());

        PhysicalMaterial result = dao.find(m.getId());
        assertMaterial(m, result);

        m.setBarcode("1234");
        m.setDetail("detail");
        m.setField001("field001");
        m.setIssue("12");
        m.setMetadata("<xml/>");
        m.setRdczId("12345");
        m.setSource("http://something.somewhere");
        m.setSigla("ABA123");
        m.setVolume("13");
        m.setYear("1998-1999");
        dao.update(m);

        result = dao.find(m.getId());
        assertMaterial(m, result);
        assertEquals("1234", result.getBarcode());
        assertEquals("detail", result.getDetail());
        assertEquals("field001", result.getField001());
        assertEquals("12", result.getIssue());
        assertEquals("<xml/>", result.getMetadata());
        assertEquals("12345", result.getRdczId());
        assertEquals("http://something.somewhere", result.getSource());
        assertEquals("ABA123", result.getSigla());
        assertEquals("13", result.getVolume());
        assertEquals("1998-1999", result.getYear());
    }

    @Test
    public void testView() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_task.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_material.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        // view a task's digital material
        MaterialFilter filter = new MaterialFilter();
        filter.setTaskId(BigDecimal.ONE);
        filter.setId(new BigDecimal(2));
        List<MaterialView> ms = dao.view(filter);
        assertEquals(1, ms.size());
        assertEquals(filter.getId(), ms.get(0).getId());
        assertNull(ms.get(0).getJobId());
        assertEquals("Dig Obj", ms.get(0).getLabel());
        assertEquals("material.digital", ms.get(0).getName());
        assertEquals("note d1", ms.get(0).getNote());
        assertEquals("uuid:ebfd7bf2-169d-476e-a230-0cc39f01764c", ms.get(0).getPid());
        assertNull(ms.get(0).getPath());
        assertEquals(filter.getTaskId(), ms.get(0).getTaskId());
        assertEquals(MaterialType.DIGITAL_OBJECT, ms.get(0).getType());
        assertEquals(Way.INPUT, ms.get(0).getWay());

        // view a task's physical material
        filter = new MaterialFilter();
        filter.setTaskId(BigDecimal.ONE);
        filter.setId(new BigDecimal(3));
        ms = dao.view(filter);
        assertEquals(1, ms.size());
        assertEquals(filter.getId(), ms.get(0).getId());
        assertNull(ms.get(0).getJobId());
        assertEquals("Phys Doc", ms.get(0).getLabel());
        assertEquals("material.physical", ms.get(0).getName());
        assertEquals("note p1", ms.get(0).getNote());
        assertEquals("1234", ms.get(0).getBarcode());
        assertEquals("001", ms.get(0).getField001());
        assertEquals("Metadata", ms.get(0).getMetadata());
        assertEquals("3", ms.get(0).getRdczId());
        assertEquals("sig123", ms.get(0).getSignature());
        assertEquals("http://catalog", ms.get(0).getSource());
        assertEquals("detail", ms.get(0).getDetail());
        assertEquals("11", ms.get(0).getIssue());
        assertEquals("ABA123", ms.get(0).getSigla());
        assertEquals("13", ms.get(0).getVolume());
        assertEquals("1998-1999", ms.get(0).getYear());
        assertNull(ms.get(0).getPid());
        assertNull(ms.get(0).getPath());
        assertEquals(filter.getTaskId(), ms.get(0).getTaskId());
        assertEquals(MaterialType.PHYSICAL_DOCUMENT, ms.get(0).getType());
        assertEquals(Way.INPUT, ms.get(0).getWay());

        // view job's materials
        filter = new MaterialFilter();
        filter.setJobId(BigDecimal.ONE);
        filter.setSortBy(schema.tableWorkflowMaterial.id.getBeanPropertyName());
        ms = dao.view(filter);
        assertEquals(3, ms.size());
        assertEquals(BigDecimal.ONE, ms.get(0).getId());
        assertEquals(BigDecimal.ONE, ms.get(0).getJobId());
        assertEquals("/folder1", ms.get(0).getLabel());
        assertEquals("material.folder", ms.get(0).getName());
        assertEquals("note m1", ms.get(0).getNote());
        assertEquals("file:///folder1", ms.get(0).getPath());
        assertNull(ms.get(0).getTaskId());
        assertEquals(MaterialType.FOLDER, ms.get(0).getType());
        assertNull(ms.get(0).getWay());

        // view job's  single material
        filter = new MaterialFilter();
        filter.setJobId(BigDecimal.ONE);
        filter.setId(BigDecimal.ONE);
        filter.setSortBy(schema.tableWorkflowMaterial.id.getBeanPropertyName());
        ms = dao.view(filter);
        assertEquals(1, ms.size());
        assertEquals(BigDecimal.ONE, ms.get(0).getId());
        assertEquals(BigDecimal.ONE, ms.get(0).getJobId());
        assertEquals("/folder1", ms.get(0).getLabel());
        assertEquals("material.folder", ms.get(0).getName());
        assertEquals("note m1", ms.get(0).getNote());
        assertEquals("file:///folder1", ms.get(0).getPath());
        assertNull(ms.get(0).getTaskId());
        assertEquals(MaterialType.FOLDER, ms.get(0).getType());
        assertNull(ms.get(0).getWay());
    }

    @Test
    public void testAddTaskReference() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_task.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_material.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        Material m1 = dao.find(BigDecimal.ONE);
        Task t1 = daoTask.find(BigDecimal.ONE);
        dao.addTaskReference(m1, t1, Way.INPUT);
        tx.commit();

        MaterialFilter filter = new MaterialFilter();
        filter.setId(m1.getId());
        filter.setTaskId(t1.getId());
        filter.setSortBy(schema.tableWorkflowMaterialInTask.way.getBeanPropertyName());
        List<MaterialView> result = dao.view(filter);
        assertEquals(2, result.size());
        assertEquals(BigDecimal.ONE, result.get(0).getId());
        assertEquals(Way.INPUT, result.get(0).getWay());
        assertEquals(BigDecimal.ONE, result.get(1).getId());
        assertEquals(Way.OUTPUT, result.get(1).getWay());
    }

    @Test
    public void testFindJob() throws Exception {
        IDataSet db = database(
                support.loadFlatXmlDataStream(getClass(), "user.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_job.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_task.xml"),
                support.loadFlatXmlDataStream(getClass(), "wf_material.xml")
                );
        support.cleanInsert(support.getConnection(tx), db);
        tx.commit();

        Material m = new Material();
        m.setId(BigDecimal.ONE);
        Job job = dao.findJob(m);
        assertNotNull(job);
        assertEquals(BigDecimal.ONE, job.getId());
    }

    private void assertMaterial(Material exp, Material result) {
        assertNotNull(result);
        assertEquals(exp.getId(), result.getId());
        assertEquals(exp.getLabel(), result.getLabel());
        assertEquals(exp.getName(), result.getName());
        assertEquals(exp.getNote(), result.getNote());
        assertEquals(exp.getType(), result.getType());
    }

}
