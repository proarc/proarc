/*
 * Copyright (C) 2012 Jan Pokorsky
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.server.config;

import cz.incad.pas.editor.server.CustomTemporaryFolder;
import java.io.File;
import java.io.FileOutputStream;
import java.util.HashMap;
import java.util.Properties;
import org.apache.commons.configuration.Configuration;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class PasConfigurationTest {

    private static final String EXPECTED_DEFAULT_VALUE = "defaultValue";
    private static final String TEST_PROPERTY_NAME = "cz.incad.pas.editor.server.config.testProperty";
    private static final String TEST_DEFAULT_PROPERTY_NAME = "cz.incad.pas.editor.server.config.testDefaultProperty";

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder();
    private PasConfigurationFactory factory;

    public PasConfigurationTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() {
        factory = PasConfigurationFactory.getInstance();
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testGetConfigHome() throws Exception {
        final File confHome = temp.getRoot();
        assertNotNull(confHome);
        PasConfiguration config = factory.create(new HashMap<String, String>() {{
            put(PasConfiguration.CONFIG_FOLDER, confHome.toString());
        }});
        assertNotNull(config);
        assertEquals(confHome, config.getConfigHome());
        assertNull(config.getConfiguration().getString(TEST_PROPERTY_NAME));
        // test internal property
        assertEquals(confHome.toString(), config.getConfiguration().getString(PasConfiguration.PROPERTY_CONFIG_HOME));
    }

    @Test
    public void testGetAllUserHome() throws Exception {
        final File confHome = temp.getRoot();
        assertNotNull(confHome);
        PasConfiguration config = factory.create(new HashMap<String, String>() {{
            put(PasConfiguration.CONFIG_FOLDER, confHome.toString());
        }});
        assertNotNull(config);
        File expectedUserFolder = new File(confHome, "users");
        assertEquals(expectedUserFolder, config.getDefaultUsersHome());
        assertTrue(expectedUserFolder.exists());
        assertTrue(expectedUserFolder.isDirectory());
        assertTrue(expectedUserFolder.canRead());
        assertTrue(expectedUserFolder.canWrite());
    }

    @Test
    public void testReadProperty() throws Exception {
        final File confHome = temp.newFolder(PasConfiguration.CONFIG_FOLDER_NAME);

        // init paseditor.cfg
        Properties props = new Properties();
        final String expectedPropValue = "test";
        props.put(TEST_PROPERTY_NAME, expectedPropValue);
        final File configFile = new File(confHome, PasConfiguration.CONFIG_FILE_NAME);
        FileOutputStream propsOut = new FileOutputStream(configFile);
        props.store(propsOut, null);
        propsOut.close();
        assertTrue(configFile.exists());

        PasConfiguration pconfig = factory.create(new HashMap<String, String>() {{
            put(PasConfiguration.CONFIG_FOLDER, confHome.toString());
        }});

        Configuration config = pconfig.getConfiguration();
        assertEquals(expectedPropValue, config.getString(TEST_PROPERTY_NAME));
        assertEquals(EXPECTED_DEFAULT_VALUE, config.getString(TEST_DEFAULT_PROPERTY_NAME));

        // test reload (like servlet reload)
        final String expectedReloadValue = "reload";
        props.put(TEST_PROPERTY_NAME, expectedReloadValue);
        propsOut = new FileOutputStream(configFile);
        props.store(propsOut, null);
        propsOut.close();
        assertTrue(configFile.exists());

        pconfig = factory.create(new HashMap<String, String>() {{
            put(PasConfiguration.CONFIG_FOLDER, confHome.toString());
        }});

        config = pconfig.getConfiguration();
        assertEquals(expectedReloadValue, config.getString(TEST_PROPERTY_NAME));
        assertEquals(EXPECTED_DEFAULT_VALUE, config.getString(TEST_DEFAULT_PROPERTY_NAME));
    }

    @Test
    public void testOverrideProperty() throws Exception {
        final File confHome = temp.newFolder(PasConfiguration.CONFIG_FOLDER_NAME);

        // init paseditor.cfg
        Properties props = new Properties();
        final String expPropValue = "overriddenValue";
        props.put(TEST_DEFAULT_PROPERTY_NAME, expPropValue);
        
        final File configFile = new File(confHome, PasConfiguration.CONFIG_FILE_NAME);
        FileOutputStream propsOut = new FileOutputStream(configFile);
        props.store(propsOut, null);
        propsOut.close();
        assertTrue(configFile.exists());

        PasConfiguration pconfig = factory.create(new HashMap<String, String>() {{
            put(PasConfiguration.CONFIG_FOLDER, confHome.toString());
        }});

        Configuration config = pconfig.getConfiguration();
        assertEquals(expPropValue, config.getString(TEST_DEFAULT_PROPERTY_NAME));
    }
}
