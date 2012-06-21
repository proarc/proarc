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
package cz.incad.pas.editor.server.user;

import cz.incad.pas.editor.server.CustomTemporaryFolder;
import cz.incad.pas.editor.server.config.PasConfiguration;
import cz.incad.pas.editor.server.config.PasConfigurationFactory;
import java.io.File;
import java.util.Collection;
import java.util.HashMap;
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
public class UserManagerMemoryImplTest {

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder();
    private PasConfiguration config;

    public UserManagerMemoryImplTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() throws Exception {
        config = PasConfigurationFactory.getInstance().create(new HashMap<String, String>() {{
            put(PasConfiguration.PROPERTY_APP_HOME, temp.getRoot().getPath());
        }});
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testGetInstance() throws Exception {
        UserManager mgr = UserManagerMemoryImpl.getInstance(config);
        assertNotNull(mgr);
        Collection<UserProfile> all = mgr.findAll();
        assertEquals(1, all.size());
        UserProfile admin = all.iterator().next();
        assertEquals("admin", admin.getUserName());
        assertEquals(Integer.valueOf(1), admin.getId());
        assertEquals(new File(config.getDefaultUsersHome(), "admin").toURI(), admin.getUserHomeUri().normalize());
    }

    @Test
    public void testFindAll() {
        UserManager mgr = UserManagerMemoryImpl.getInstance(config);
        assertNotNull(mgr);
        Collection<UserProfile> all = mgr.findAll();
        assertEquals(1, all.size());
    }

    @Test
    public void testFindUserName() {
        String userName = "admin";
        UserManager mgr = UserManagerMemoryImpl.getInstance(config);
        assertNotNull(mgr);
        UserProfile result = mgr.find(userName);
        assertEquals(userName, result.getUserName());

        try {
            UserProfile up = mgr.find("unknown");
            fail("exception expected, profile: " + up);
        } catch (IllegalArgumentException ex) {
        }
    }

    @Test
    public void testFindUserId() {
        int userId = 1;
        UserManager mgr = UserManagerMemoryImpl.getInstance(config);
        UserProfile result = mgr.find(userId);
        assertEquals(Integer.valueOf(userId), result.getId());
        assertEquals("admin", result.getUserName());

        try {
            UserProfile up = mgr.find(4);
            fail("exception expected, profile: " + up);
        } catch (IllegalArgumentException ex) {
        }
    }

    @Test
    public void testAdd() throws Exception {
        String userName = "datel";
        String displayName = "Datel Urputný";
        String userHomePath = null;
        UserManagerMemoryImpl instance = (UserManagerMemoryImpl) UserManagerMemoryImpl.getInstance(config);
        instance.add(userName, displayName, userHomePath);

        assertEquals(2, instance.findAll().size());
        UserProfile up = instance.find(userName);
        assertEquals(userName, up.getUserName());
        assertEquals(displayName, up.getDisplayName());
        assertEquals(new File(config.getDefaultUsersHome().getAbsolutePath() + "/" + userName + "/import").toURI(), up.getImportFolder());
    }

    @Test
    public void testAddExistingHome() throws Exception {
        String userName = "datel";
        String displayName = "Datel Urputný";
        File userHome = temp.newFolder(userName);
        String userHomePath = userHome.getPath();
        UserManagerMemoryImpl instance = (UserManagerMemoryImpl) UserManagerMemoryImpl.getInstance(config);
        instance.add(userName, displayName, userHomePath);

        assertEquals(2, instance.findAll().size());
        UserProfile up = instance.find(userName);
        assertEquals(userName, up.getUserName());
        assertEquals(displayName, up.getDisplayName());
        assertEquals(new File(userHomePath + '/' + UserUtil.IMPORT_FOLDER_NAME).toURI(), up.getImportFolder());
    }

    @Test
    public void testAddEmptyHomeAndUserNameClashingWithExistingFolder() throws Exception {
        String userName = "datel";
        String displayName = "Datel Urputný";
        File userHome = new File(config.getDefaultUsersHome(), userName);
        userHome.mkdirs();
        String userHomePath = null;
        UserManagerMemoryImpl instance = (UserManagerMemoryImpl) UserManagerMemoryImpl.getInstance(config);
        instance.add(userName, displayName, userHomePath);

        assertEquals(2, instance.findAll().size());
        UserProfile up = instance.find(userName);
        assertEquals(userName, up.getUserName());
        assertEquals(displayName, up.getDisplayName());
        assertEquals(new File(config.getDefaultUsersHome().getAbsolutePath() + "/" + userName + "_1/" + UserUtil.IMPORT_FOLDER_NAME)
                .toURI(), up.getImportFolder());
    }

    @Test
    public void testAddAlreadyUsedHome() throws Exception {
        String userName = "datel";
        String displayName = "Datel Urputný";
        String userHomePath = config.getDefaultUsersHome().getAbsolutePath() + "/admin";
        UserManagerMemoryImpl instance = (UserManagerMemoryImpl) UserManagerMemoryImpl.getInstance(config);

        try {
            UserProfile up = instance.add(userName, displayName, userHomePath);
            fail("exception expected, userHome: " + up.getUserHomeUri());
        } catch (Exception ex) {
        }
    }

    @Test
    public void testAddInvalidUserName() throws Exception {
        String userName = "Datel";
        String displayName = "Datel Urputný";
        String userHomePath = null;
        UserManagerMemoryImpl instance = (UserManagerMemoryImpl) UserManagerMemoryImpl.getInstance(config);
        try {
            instance.add(userName, displayName, userHomePath);
            fail("exception expected, userName: " + userName);
        } catch (Exception ex) {
        }
    }

    @Test
    public void testExistingUserName() throws Exception {
        String userName = "admin";
        String displayName = "Datel Urputný";
        String userHomePath = null;
        UserManagerMemoryImpl instance = (UserManagerMemoryImpl) UserManagerMemoryImpl.getInstance(config);
        try {
            instance.add(userName, displayName, userHomePath);
            fail("exception expected, userName: " + userName);
        } catch (Exception ex) {
        }
    }
}
