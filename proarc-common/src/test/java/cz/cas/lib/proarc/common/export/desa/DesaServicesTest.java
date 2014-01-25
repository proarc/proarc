/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.export.desa;

import cz.cas.lib.proarc.common.export.desa.DesaServices.DesaConfiguration;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import java.util.Arrays;
import java.util.Map;
import org.apache.commons.configuration.BaseConfiguration;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class DesaServicesTest {

    private BaseConfiguration conf;
    private DesaServices desaServices;

    public DesaServicesTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
        conf = new BaseConfiguration();
        conf.setProperty(DesaServices.PROPERTY_DESASERVICES, "ds1, dsNulls");

        String prefix = DesaServices.PREFIX_DESA + '.' + "ds1" + '.';
        conf.setProperty(prefix + DesaConfiguration.PROPERTY_USER, "ds1user");
        conf.setProperty(prefix + DesaConfiguration.PROPERTY_PASSWD, "ds1passwd");
        conf.setProperty(prefix + DesaConfiguration.PROPERTY_PRODUCER, "ds1producer");
        conf.setProperty(prefix + DesaConfiguration.PROPERTY_OPERATOR, "ds1operator");
        conf.setProperty(prefix + DesaConfiguration.PROPERTY_EXPORTMODELS, "model:id1, model:id2");
        conf.setProperty(prefix + DesaConfiguration.PROPERTY_RESTAPI, "https://SERVER/dea-frontend/rest/sipsubmission");
        conf.setProperty(prefix + DesaConfiguration.PROPERTY_WEBSERVICE, "https://SERVER/dea-frontend/ws/SIPSubmissionService");

        prefix = DesaServices.PREFIX_DESA + '.' + "dsNulls" + '.';
        conf.setProperty(prefix + DesaConfiguration.PROPERTY_USER, null);
        conf.setProperty(prefix + DesaConfiguration.PROPERTY_PASSWD, "");
        conf.setProperty(prefix + DesaConfiguration.PROPERTY_EXPORTMODELS, null);

        prefix = DesaServices.PREFIX_DESA + '.' + "dsNotActive" + '.';
        conf.setProperty(prefix + DesaConfiguration.PROPERTY_USER, "NA");
        desaServices = new DesaServices(conf);
    }

    @Test
    public void testFindConfiguration_MetaModel() {
        MetaModel model = new MetaModel("model:id1", true, true, null, "", "", null, null);
        DesaConfiguration result = desaServices.findConfiguration(model);
        assertNotNull(result);
        assertEquals("ds1", result.getServiceId());
    }

    @Test
    public void testFindConfiguration_MetaModel_Unknown() {
        MetaModel model = new MetaModel("model:unknown", true, true, null, "", "", null, null);
        DesaConfiguration result = desaServices.findConfiguration(model);
        assertNull(result);
    }

    @Test
    public void testFindConfiguration_String() {
        DesaConfiguration ds1 = desaServices.findConfiguration("ds1");
        assertNotNull(ds1);
        assertEquals("ds1", ds1.getServiceId());
        assertEquals(Arrays.asList("model:id1", "model:id2"), ds1.getExportModels());
        Map<String, String> tc = ds1.toTransporterConfig();
        assertEquals("ds1user", tc.get("desa.user"));
        assertEquals("ds1passwd", tc.get("desa.password"));
        assertEquals("ds1producer", tc.get("desa.producer"));
        assertEquals("ds1operator", tc.get("desa.operator"));
        assertEquals(ds1.toString(), "https://SERVER/dea-frontend/rest/sipsubmission", tc.get("desa.restapi"));
        assertEquals("https://SERVER/dea-frontend/ws/SIPSubmissionService", tc.get("desa.webservice"));
        assertEquals("true", tc.get("desa.rest"));
    }

    @Test
    public void testFindConfiguration_String2() {
        DesaConfiguration ds = desaServices.findConfiguration("dsNulls");
        assertNotNull(ds);
        assertEquals("dsNulls", ds.getServiceId());
        assertEquals(Arrays.asList(), ds.getExportModels());
        Map<String, String> tc = ds.toTransporterConfig();
        assertEquals(null, tc.get("desa.user"));
        assertEquals("", tc.get("desa.password"));
        assertEquals("true", tc.get("desa.rest"));
    }

    @Test
    public void testFindConfiguration_String3() {
        DesaConfiguration ds = desaServices.findConfiguration("dsNotActive");
        assertNull(ds);
    }

}