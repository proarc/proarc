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
package cz.cas.lib.proarc.common.config;

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.imports.ImportProfile;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.HashMap;
import java.util.List;
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
public class AppConfigurationTest {

    private static final String EXPECTED_DEFAULT_VALUE = "defaultValue";
    private static final String TEST_PROPERTY_NAME = "cz.cas.lib.proarc.common.config.testProperty";
    private static final String TEST_DEFAULT_PROPERTY_NAME = "cz.cas.lib.proarc.common.config.testDefaultProperty";

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder();
    private AppConfigurationFactory factory;
    private File confHome;
    private File proarcCfg;

    public AppConfigurationTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() throws Exception {
        factory = AppConfigurationFactory.getInstance();
        confHome = temp.newFolder(AppConfiguration.DEFAULT_APP_HOME_NAME);
        assertNotNull(confHome);
        proarcCfg = new File(confHome, AppConfiguration.CONFIG_FILE_NAME);
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testGetConfigHome() throws Exception {
        AppConfiguration config = factory.create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, confHome.toString());
        }});
        assertNotNull(config);
        assertEquals(confHome, config.getConfigHome());
        assertNull(config.getConfiguration().getString(TEST_PROPERTY_NAME));
        // test internal property
        assertEquals(confHome.toString(), config.getConfiguration().getString(AppConfiguration.PROPERTY_APP_HOME));
    }

    @Test
    public void testGetAllUserHome() throws Exception {
        AppConfiguration config = factory.create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, confHome.toString());
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
        // init proarc.cfg
        final String expectedPropValue = "test-čŇů"; // test UTF-8
        Properties props = new Properties();
        props.put(TEST_PROPERTY_NAME, expectedPropValue);
        createConfigFile(props, proarcCfg);

        AppConfiguration pconfig = factory.create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, confHome.toString());
        }});

        Configuration config = pconfig.getConfiguration();
        assertEquals(expectedPropValue, config.getString(TEST_PROPERTY_NAME));
        assertEquals(EXPECTED_DEFAULT_VALUE, config.getString(TEST_DEFAULT_PROPERTY_NAME));

        // test reload (like servlet reload)
        final String expectedReloadValue = "reload";
        props.put(TEST_PROPERTY_NAME, expectedReloadValue);
        OutputStreamWriter propsOut = new OutputStreamWriter(new FileOutputStream(proarcCfg), "UTF-8");
        // FileChangedReloadingStrategy waits 5s to reload changes so give it a chance
        Thread.sleep(5000);
        props.store(propsOut, null);
        propsOut.close();
        assertTrue(proarcCfg.exists());

        AppConfiguration pconfigNew = factory.create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, confHome.toString());
        }});

        Configuration configNew = pconfigNew.getConfiguration();
        assertEquals(expectedReloadValue, configNew.getString(TEST_PROPERTY_NAME));
        assertEquals(EXPECTED_DEFAULT_VALUE, configNew.getString(TEST_DEFAULT_PROPERTY_NAME));

        // test FileChangedReloadingStrategy
        assertEquals(expectedReloadValue, config.getString(TEST_PROPERTY_NAME));
        assertEquals(EXPECTED_DEFAULT_VALUE, config.getString(TEST_DEFAULT_PROPERTY_NAME));
    }

    @Test
    public void testOverrideProperty() throws Exception {
        // init proarc.cfg
        Properties props = new Properties();
        final String expPropValue = "overriddenValue";
        props.put(TEST_DEFAULT_PROPERTY_NAME, expPropValue);

        createConfigFile(props, proarcCfg);

        AppConfiguration pconfig = factory.create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, confHome.toString());
        }});

        Configuration config = pconfig.getConfiguration();
        assertEquals(expPropValue, config.getString(TEST_DEFAULT_PROPERTY_NAME));
    }

    @Test
    public void testGetProfile() throws Exception {
        // init proarc.cfg
        final String expProfileValue = "profileValue";
        final String expDefaultProfileValue = "defaultProfileValue";
        final String importProfileGroup = ImportProfile.PROFILES;

        createConfigFile(new Properties() {{
                put(importProfileGroup, "profile.ndk");
                put("profile.ndk.label", "NDK label");
                put("profile.ndk.description", "NDK description");
                put("profile.ndk.file", "ndk.cfg");
                put(ImportProfile.PLAIN_OCR_CHARSET, expDefaultProfileValue);
            }},
            new File(confHome, AppConfiguration.CONFIG_FILE_NAME));

        File ndkProfileFile = createConfigFile(new Properties() {{
                put(TEST_PROPERTY_NAME, expProfileValue);
                put(ImportProfile.PLAIN_OCR_CHARSET, expProfileValue);
            }},
            new File(confHome, "ndk.cfg"));

        AppConfiguration config = factory.create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, confHome.toString());
        }});

        List<ConfigurationProfile> profiles = config.getProfiles().getProfiles(importProfileGroup);
        assertNotNull(profiles);
        assertEquals("profile.ndk", profiles.get(0).getId());
        assertEquals("NDK label", profiles.get(0).getLabel());
        assertEquals("NDK description", profiles.get(0).getDescription());
        assertEquals(ndkProfileFile, profiles.get(0).getFile());

        assertEquals(expProfileValue, config.getImportConfiguration(profiles.get(0)).getPlainOcrCharset());
        assertEquals(expDefaultProfileValue, config.getImportConfiguration().getPlainOcrCharset());
    }

    @Test
    public void testGetSoundRecordingProfile() throws Exception {
        // init proarc.cfg
        final String expDefaultProfileValue = "defaultProfileValue";
        final String importProfileGroup = ImportProfile.PROFILES;

        createConfigFile(new Properties() {{
                             put(importProfileGroup, "profile.soundrecording_import");
                             put("profile.soundrecording_import.label", "NDK SoundRecording label");
                             put("profile.soundrecording_import.description", "NDK SoundRecording description");
                             put(ImportProfile.PLAIN_OCR_CHARSET, expDefaultProfileValue);
                         }},
                new File(confHome, AppConfiguration.CONFIG_FILE_NAME));

        AppConfiguration config = factory.create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, confHome.toString());
        }});

        List<ConfigurationProfile> profiles = config.getProfiles().getProfiles(importProfileGroup);
        assertNotNull(profiles);
        assertEquals("profile.soundrecording_import", profiles.get(0).getId());
        assertEquals("NDK SoundRecording label", profiles.get(0).getLabel());
        assertEquals("NDK SoundRecording description", profiles.get(0).getDescription());
        assertEquals(expDefaultProfileValue, config.getImportConfiguration().getPlainOcrCharset());
    }

    private File createConfigFile(Properties props, File configFile) throws IOException {
        FileOutputStream propsOut = new FileOutputStream(configFile);
        props.store(propsOut, null);
        propsOut.close();
        assertTrue(configFile.exists());
        return configFile;
    }

}
