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
import cz.cas.lib.proarc.common.storage.FedoraTestSupport;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import java.io.File;
import java.sql.Connection;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Collections;
import javax.sql.DataSource;
import org.apache.empire.db.DBContext;
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

/**
 * Integration tests.
 *
 * @author Jan Pokorsky
 */
public class UserManagerSqlTest {

    @TempDir
    File tempDir;

    private AppConfiguration configuration;
    private DbUnitSupport db;
    private FedoraTestSupport fedora;
    private FedoraStorage fedoraStorage;
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
        fedora = new FedoraTestSupport();
        fedora.cleanUp();
        fedoraStorage = fedora.getRemoteStorage();
        configuration = AppConfigurationFactory.getInstance().defaultInstance();

        // rdbms init
        db = new DbUnitSupport();
        EmpireDaoFactory daos = new EmpireDaoFactory(db.getEmireCfg());
        daos.init();
        DataSource dataSource = EasyMock.createMock(DataSource.class);
        EasyMock.expect(dataSource.getConnection()).andAnswer(new IAnswer<Connection>() {

            @Override
            public Connection answer() throws Throwable {
                return db.getEmireCfg().getContext().getConnection();
            }
        }).anyTimes();
        IDataSet database = database(
                // XXX related fedora objects do not exist!
                db.loadFlatXmlDataStream(EmpireUserDaoTest.class, "user.xml")
//                db.loadFlatXmlDataStream(getClass(), "group.xml")
        );
        final DBContext context = db.getEmireCfg().getContext();
        try {
            db.cleanInsert(context, database);
            db.initSequences(context.getConnection(), 10, (String) db.getEmireCfg().getSchema().tableUser.id.getDefaultValue());
            db.initSequences(context.getConnection(), 10, (String) db.getEmireCfg().getSchema().tableUserGroup.id.getDefaultValue());
            context.discard();
            ;
        } finally {
            context.discard();
        }

        EasyMock.replay(dataSource);
        manager = new UserManagerSql(configuration, dataSource, tempDir, fedoraStorage, daos);
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
    public void testAddUser() {
        String userName = "datel";
        String passwd = "123456";

        // proarc user
        UserProfile user = UserProfile.create(userName, passwd, "Datel");
        user.setEmail("email@somewhere");
        manager.add(user, Collections.<Group>emptyList(), fedora.getTestUser(), "add user");
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
                    fedora.getTestUser(), "add remote group");
        }
        UserProfile user = manager.find(remoteUserName, remoteType);
        if (user == null) {
            user = UserProfile.createRemote(remoteUserName, remoteType, "Datel");
            user.setEmail("email@somewhere");
            user.setUserName(UserUtil.toUserName("desa", remoteUserName));
            user.setDefaultGroup(group.getId());
            manager.add(user, Arrays.asList(group), fedora.getTestUser(), "add remote user");
        }
    }

}
