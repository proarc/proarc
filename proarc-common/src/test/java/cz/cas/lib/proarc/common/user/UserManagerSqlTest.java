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
package cz.cas.lib.proarc.common.user;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.empiredb.DbUnitSupport;
import cz.cas.lib.proarc.common.dao.empiredb.EmpireDaoFactory;
import cz.cas.lib.proarc.common.dao.empiredb.EmpireUserDaoTest;
import java.io.File;
import java.sql.Connection;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import javax.sql.DataSource;
import org.dbunit.database.IDatabaseConnection;
import org.dbunit.dataset.CompositeDataSet;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.ReplacementDataSet;
import org.easymock.EasyMock;
import org.easymock.IAnswer;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Integration tests.
 *
 * @author Jan Pokorsky
 */
public class UserManagerSqlTest {

    @TempDir
    static File tempDir;

    private AppConfiguration configuration;
    private DbUnitSupport db;
    private UserManagerSql manager;

    public UserManagerSqlTest() {
    }

    @BeforeAll
    public static void setUpClass() {
    }

    @AfterAll
    public static void tearDownClass() {
    }

    @BeforeEach
    public void setUp() throws Exception {
        // fedora init
        File appHome = new File(tempDir, "proarc-home");
        appHome.mkdirs();
        new File(appHome, AppConfiguration.CONFIG_FILE_NAME).createNewFile();
        Map<String, String> testConfig = new HashMap<String, String>();
        testConfig.put(AppConfiguration.PROPERTY_APP_HOME, appHome.getAbsolutePath());
        configuration = AppConfigurationFactory.getInstance().create(testConfig);

        // rdbms init
        db = new DbUnitSupport();
        EmpireDaoFactory daos = new EmpireDaoFactory(db.getEmireCfg());
        daos.init();
        DataSource dataSource = EasyMock.createMock(DataSource.class);
        EasyMock.expect(dataSource.getConnection()).andAnswer(new IAnswer<Connection>() {

            @Override
            public Connection answer() throws Throwable {
                return db.getEmireCfg().getConnection();
            }
        }).anyTimes();
        IDataSet database = database(
                // XXX related fedora objects do not exist!
                db.loadFlatXmlDataStream(EmpireUserDaoTest.class, "user.xml")
//                db.loadFlatXmlDataStream(getClass(), "group.xml")
        );
        final IDatabaseConnection con = db.getConnection();
        try {
            db.cleanInsert(con, database);
            db.initSequences(con.getConnection(), 10, db.getEmireCfg().getSchema().tableUser.id.getSequenceName());
            db.initSequences(con.getConnection(), 10, db.getEmireCfg().getSchema().tableUserGroup.id.getSequenceName());
            con.getConnection().commit();
        } finally {
            con.getConnection().close();
        }

        EasyMock.replay(dataSource);
        manager = new UserManagerSql(dataSource, configuration, daos);
    }

    @AfterEach
    public void tearDown() {
    }

    private IDataSet database(IDataSet... ds) throws Exception {
        ReplacementDataSet rds = new ReplacementDataSet(new CompositeDataSet(ds));
        rds.addReplacementObject("{$user.home}", "relative/path/");
        rds.addReplacementObject("{$now}", new Timestamp(System.currentTimeMillis()));
        return rds;
    }

    @Test
    public void keycloakCreatesPasswordlessUserAndLinksByEmail() {
        configuration.getAuthenticators().setProperty("authenticators", "keycloak");
        UserProfile user = UserProfile.create("keycloakuser", null, "Keycloak");
        user.setEmail("new@example.org");
        manager.add(user, Collections.emptyList(), "test", "add Keycloak user");

        assertNotNull(manager.find(user.getId()));
        assertNull(user.getUserPassword());
        assertNull(user.getRemoteName());
        assertNull(user.getRemoteType());
        UserProfile linked = manager.resolveKeycloakIdentity("https://sso.example.org/realms/proarc", "new-subject", "new@example.org");
        assertNotNull(linked);
        assertEquals(user.getId(), linked.getId());
    }

    @Test
    public void keycloakIgnoresSubmittedPasswordsOnCreateAndUpdate() {
        configuration.getAuthenticators().setProperty("authenticators", "keycloak");
        UserProfile user = UserProfile.create("keycloakpassword", "123456", "Keycloak");
        manager.add(user, Collections.emptyList(), "test", "add Keycloak user");
        assertNull(manager.authenticate("keycloakpassword", "123456"));

        user.setUserPassword("654321");
        manager.update(user, "test", "update Keycloak user");
        assertNull(manager.authenticate("keycloakpassword", "654321"));
    }

    @Test
    public void localUsersStillRequireValidPasswords() {
        configuration.getAuthenticators().setProperty("authenticators", "proarc");
        for (String password : Arrays.asList(null, "", "12345")) {
            UserProfile user = UserProfile.create("localuser", password, "Local");
            assertThrows(IllegalArgumentException.class,
                    () -> manager.add(user, Collections.emptyList(), "test", "add local user"));
        }
    }

    @Test
    public void testAddUser() {
        String userName = "datel";
        String passwd = "123456";

        // proarc user
        UserProfile user = UserProfile.create(userName, passwd, "Datel");
        user.setEmail("email@somewhere");
        manager.add(user, Collections.<Group>emptyList(), "test", "add user");
        assertNotNull(user.getId());
        assertNull(user.getUserPassword());

        // set permissions
        manager.setPermissions(user.getUserGroup(), Permissions.ADMIN);

        // authentication
        UserProfile authenticate = manager.authenticate(userName, passwd);
        assertNotNull(authenticate);
        assertEquals(userName, authenticate.getUserName());
        assertNull(authenticate.getUserPassword());

        // invalid authentication
        authenticate = manager.authenticate(userName, "invalid");
        assertNull(authenticate);
    }

    @Test
    public void keycloakLinksResolvesAndRelinksAfterReset() {
        UserProfile local = manager.find(1);
        local.setEmail("Person@Example.org");
        manager.update(local, "test", "prepare link");
        UserProfile linked = manager.resolveKeycloakIdentity("https://issuer", "subject", "person@example.org");
        assertNotNull(linked);
        assertEquals(local.getId(), linked.getId());
        assertEquals("subject", manager.find(1).getRemoteName());
        assertEquals("https://issuer", manager.find(1).getRemoteType());
        assertNotNull(manager.resolveKeycloakIdentity("https://issuer", "subject", null));
        assertNull(manager.resolveKeycloakIdentity("https://issuer", "other", "person@example.org"));
        linked.setRemoteName("");
        linked.setRemoteType("");
        manager.update(linked, "test", "reset link");
        assertNotNull(manager.resolveKeycloakIdentity("https://issuer", "other", "person@example.org"));
    }

    @Test
    public void keycloakRefusesDuplicateEmailsAndPartialIdentity() {
        UserProfile first = manager.find(1);
        first.setEmail("same@example.org");
        manager.update(first, "test", "email");
        UserProfile second = manager.find(2);
        second.setEmail("SAME@example.org");
        manager.update(second, "test", "duplicate email");
        assertNull(manager.resolveKeycloakIdentity("https://issuer", "subject", "same@example.org"));
        second.setEmail("different@example.org");
        manager.update(second, "test", "remove duplicate");
        first.setRemoteType("https://issuer");
        manager.update(first, "test", "partial identity");
        assertNull(manager.resolveKeycloakIdentity("https://issuer", "subject", "same@example.org"));
    }

    @Test
    public void concurrentKeycloakLinksCannotAssignOneIdentityToTwoAccounts() throws Exception {
        UserProfile first = manager.find(1);
        first.setEmail("first@example.org");
        manager.update(first, "test", "email");
        UserProfile second = manager.find(2);
        second.setEmail("second@example.org");
        manager.update(second, "test", "email");
        java.util.concurrent.CountDownLatch start = new java.util.concurrent.CountDownLatch(1);
        try (java.util.concurrent.ExecutorService executor = java.util.concurrent.Executors.newFixedThreadPool(2)) {
            java.util.concurrent.Callable<UserProfile> linkFirst = () -> concurrentLink(start, "first@example.org");
            java.util.concurrent.Callable<UserProfile> linkSecond = () -> concurrentLink(start, "second@example.org");
            java.util.concurrent.Future<UserProfile> a = executor.submit(linkFirst);
            java.util.concurrent.Future<UserProfile> b = executor.submit(linkSecond);
            start.countDown();
            UserProfile resultA = a.get(10, java.util.concurrent.TimeUnit.SECONDS);
            UserProfile resultB = b.get(10, java.util.concurrent.TimeUnit.SECONDS);
            org.junit.jupiter.api.Assertions.assertTrue(resultA != null || resultB != null);
            if (resultA != null && resultB != null) {
                assertEquals(resultA.getId(), resultB.getId());
            }
        }
        long linked = manager.findAll().stream().filter(u -> "subject".equals(u.getRemoteName())
                && "https://issuer".equals(u.getRemoteType())).count();
        assertEquals(1, linked);
    }

    private UserProfile concurrentLink(java.util.concurrent.CountDownLatch start, String email) throws Exception {
        start.await();
        try {
            return manager.resolveKeycloakIdentity("https://issuer", "subject", email);
        } catch (IllegalStateException conflict) {
            // PostgreSQL may abort a competing serializable transaction. Never authenticate it.
            return null;
        }
    }

    @Test
    public void testAddRemoteUser() {
        String remoteType = "desa";
        String remoteGroupName = "producer";
        String remoteUserName = "desa_user";
        Group group = manager.findRemoteGroup(remoteGroupName, remoteType);
        if (group == null) {
            group = Group.createRemote(
                    UserUtil.toUserName("desa", remoteGroupName),
                    remoteGroupName, remoteGroupName, remoteType);
            manager.addGroup(group, Arrays.asList(Permissions.REPO_SEARCH_GROUPOWNER),
                    "testGroup", "add remote group");
        }
        assertNotNull(group.getId());
        assertEquals(remoteGroupName, group.getRemoteName());
        assertEquals(remoteType, group.getRemoteType());

        UserProfile user = manager.find(remoteUserName, remoteType);
        if (user == null) {
            user = UserProfile.createRemote(remoteUserName, remoteType, "Datel");
            user.setEmail("email@somewhere");
            user.setUserName(UserUtil.toUserName("desa", remoteUserName));
            user.setDefaultGroup(group.getId());
            manager.add(user, Arrays.asList(group), "test2", "add remote user");
        }
        assertNotNull(user.getId());
        assertEquals(remoteUserName, user.getRemoteName());
        assertEquals(remoteType, user.getRemoteType());
        assertEquals(group.getId(), user.getDefaultGroup());
        assertNull(user.getUserPassword());
    }

}
