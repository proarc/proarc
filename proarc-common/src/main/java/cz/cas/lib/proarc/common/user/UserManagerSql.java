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
package cz.cas.lib.proarc.common.user;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.dao.GroupDao;
import cz.cas.lib.proarc.common.dao.Transaction;
import cz.cas.lib.proarc.common.dao.UserDao;
import cz.cas.lib.proarc.common.dao.empiredb.EmpireDaoFactory;
import cz.cas.lib.proarc.common.dao.empiredb.SqlTransaction;
import cz.cas.lib.proarc.common.fedora.FedoraTransaction;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.Storage;
import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;
import javax.sql.DataSource;
import org.apache.commons.io.FileUtils;

import static cz.cas.lib.proarc.common.sql.DbUtils.close;
import static cz.cas.lib.proarc.common.sql.DbUtils.commit;
import static cz.cas.lib.proarc.common.sql.DbUtils.rollback;

/**
 * Manages user stuff in RDBMS and the Fedora storage.
 *
 * @author Jan Pokorsky
 */
final class UserManagerSql implements UserManager {
    
    private static final Logger LOG = Logger.getLogger(UserManagerSql.class.getName());
    private final DataSource source;
    private final AppConfiguration appConfig;
    private final File defaultHome;
    private final GroupSqlStorage groupStorage;
    private final PermissionSqlStorage permissionStorage;
    private final RemoteStorage remoteStorage;
    private final DaoFactory daos;

    public UserManagerSql(DataSource source, AppConfiguration config, DaoFactory daos) throws IOException {
        this.appConfig = config;
        this.source = source;
        this.defaultHome = config.getDefaultUsersHome();
        groupStorage = new GroupSqlStorage(source);
        permissionStorage = new PermissionSqlStorage(source);
        if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
            this.remoteStorage = RemoteStorage.getInstance(config);
        } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
            this.remoteStorage = null;
        } else{
            throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
        }
        this.daos = daos;
    }

    public UserManagerSql(AppConfiguration configuration, DataSource source, File defaultHome, RemoteStorage remoteStorage, EmpireDaoFactory daos) {
        this.appConfig = configuration;
        this.source = source;
        this.defaultHome = defaultHome;
        this.groupStorage = new GroupSqlStorage(source);
        this.permissionStorage = new PermissionSqlStorage(source);
        this.remoteStorage = remoteStorage;
        this.daos = daos;
    }

    @Override
    public UserProfile authenticate(String userName, String passwd) {
        if (userName == null || passwd == null) {
            return null;
        }
        String digist = UserUtil.getDigist(passwd);
        Transaction tx = daos.createTransaction();
        UserDao users = daos.createUser();
        users.setTransaction(tx);
        try {
            List<UserProfile> result = users.find(userName, digist, null, null, null);
            return result.isEmpty() ? null : filter(result.get(0));
        } finally {
            tx.close();
        }
    }

    @Override
    public UserProfile find(String userName) throws IllegalArgumentException {
        if (userName == null) {
            return null;
        }
        Transaction tx = daos.createTransaction();
        UserDao users = daos.createUser();
        users.setTransaction(tx);
        try {
            List<UserProfile> result = users.find(userName, null, null, null, null);
            return result.isEmpty() ? null : filter(result.get(0));
        } finally {
            tx.close();
        }
    }

    @Override
    public UserProfile find(String remoteName, String remoteType) {
        if (remoteName == null || remoteType == null) {
            return null;
        }
        Transaction tx = daos.createTransaction();
        UserDao users = daos.createUser();
        users.setTransaction(tx);
        try {
            List<UserProfile> result = users.find(null, null, remoteName, remoteType, null);
            return result.isEmpty() ? null : filter(result.get(0));
        } finally {
            tx.close();
        }
    }

    @Override
    public UserProfile find(int userId) throws IllegalArgumentException {
        Transaction tx = daos.createTransaction();
        UserDao users = daos.createUser();
        users.setTransaction(tx);
        try {
            return filter(users.find(userId));
        } finally {
            tx.close();
        }
    }

    @Override
    public List<UserProfile> findAll() {
        Transaction tx = daos.createTransaction();
        UserDao users = daos.createUser();
        users.setTransaction(tx);
        try {
            return filter(users.find(null, null, null, null, null));
        } finally {
            tx.close();
        }
    }

    @Override
    public List<UserProfile> findMyOrganization(String organization) {
        Transaction tx = daos.createTransaction();
        UserDao users = daos.createUser();
        users.setTransaction(tx);
        try {
            return filter(users.find(null, null, null, null, organization));
        } finally {
            tx.close();
        }
    }

    @Override
    public UserProfile add(UserProfile profile, List<Group> groups, String owner, String log) {
        if (profile == null) {
            throw new NullPointerException();
        }
        profile.validateAsNew();
        File userHome = null;
        Transaction tx = daos.createTransaction();
        UserDao userDao = daos.createUser();
        GroupDao groupDao = daos.createUserGroup();
        userDao.setTransaction(tx);
        groupDao.setTransaction(tx);
        FedoraTransaction ftx = null;

        try {

            Group userGroup = createUserGroup(profile);
            final List<Group> membership = new ArrayList<Group>(groups.size() + 1);
            membership.addAll(groups);
            membership.add(userGroup);

            // fedora
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                ftx = new FedoraTransaction(remoteStorage);
                FedoraUserDao fedoraUsers = new FedoraUserDao();
                fedoraUsers.setTransaction(ftx);
                fedoraUsers.add(profile, owner, log);
                fedoraUsers.setMembership(profile, membership, log);

                FedoraGroupDao fedoraGroups = new FedoraGroupDao();
                fedoraGroups.setTransaction(ftx);
                fedoraGroups.addNewGroup(userGroup, owner, log);
            }

            // rdbms
            groupDao.update(userGroup);

            userHome = UserUtil.createUserHome(profile.getUserName(), null, defaultHome);
            profile.setUserHome(UserUtil.toUri(userHome));
            profile.setUserGroup(userGroup.getId());
            userDao.update(profile);
            // sql membership
            groupStorage.addMembership(((SqlTransaction) tx).getConnection(), profile.getId(), membership);

            // filesystem
            UserUtil.createUserSubfolders(userHome);
            userHome = null;
            commit(tx, ftx);
            return filter(profile);
        } catch (Throwable ex) {
            rollback(tx, ftx);
            throw new IllegalStateException(filter(profile).toString(), ex);
        } finally {
            if (userHome != null) {
                FileUtils.deleteQuietly(userHome);
            }
            close(tx, ftx);
        }
    }

    @Override
    public void update(UserProfile profile, String owner, String log) {
        Transaction tx = daos.createTransaction();
        UserDao users = daos.createUser();
        users.setTransaction(tx);
        try {
            if (profile.getUserPassword() != null) {
                profile.setUserPasswordDigest(UserUtil.getDigist(profile.getUserPassword()));
            }
            users.update(profile);
            filter(profile);
            tx.commit();
        } catch (Throwable ex) {
            tx.rollback();
            throw new IllegalStateException(String.valueOf(filter(profile)), ex);
        } finally {
            tx.close();
        }
    }

    @Override
    public Group addGroup(Group group, List<Permission> permissions, String owner, String log) {
        if (!UserUtil.isValidGroupPid(group.getName())) {
            throw new IllegalArgumentException("groupName: " + group.getName());
        }
        Transaction tx = daos.createTransaction();
        GroupDao groupDao = daos.createUserGroup();
        groupDao.setTransaction(tx);
        FedoraTransaction ftx = null;
        try {
            // fedora
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                ftx = new FedoraTransaction(remoteStorage);
                FedoraGroupDao fedoraGroups = new FedoraGroupDao();
                fedoraGroups.setTransaction(ftx);
                fedoraGroups.addGroup(group, owner, log);
            }
            groupDao.update(group);
            if (!permissions.isEmpty()) {
                permissionStorage.set(((SqlTransaction) tx).getConnection(), group.getId(),
                        permissions.toArray(new Permission[permissions.size()]));
            }
            commit(tx, ftx);
            return group;
        } catch (Throwable ex) {
            rollback(tx, ftx);
            throw new IllegalStateException(String.valueOf(group), ex);
        } finally {
            close(tx, ftx);
        }
    }

    @Override
    public List<Group> findGroups() {
        Transaction tx = daos.createTransaction();
        GroupDao groupDao = daos.createUserGroup();
        groupDao.setTransaction(tx);
        try {
            return groupDao.find(null, null, null, null);
        } finally {
            tx.close();
        }
    }

    @Override
    public Group findGroup(int groupId) {
        Transaction tx = daos.createTransaction();
        GroupDao groupDao = daos.createUserGroup();
        groupDao.setTransaction(tx);
        try {
            return groupDao.find(groupId);
        } finally {
            tx.close();
        }
    }

    @Override
    public Group findRemoteGroup(String remoteName, String remoteType) {
        Transaction tx = daos.createTransaction();
        GroupDao groupDao = daos.createUserGroup();
        groupDao.setTransaction(tx);
        try {
            List<Group> groups = groupDao.find(null, null, remoteName, remoteType);
            return groups.isEmpty() ? null : groups.get(0);
        } finally {
            tx.close();
        }
    }

    @Override
    public void setUserGroups(UserProfile user, List<Group> groups, String owner, String log) {
        try {
            Connection c = source.getConnection();
            boolean rollback = true;
            FedoraTransaction ftx = null;
            try {
                c.setAutoCommit(false);
                groupStorage.removeMembership(c, user.getId());
                if (!groups.isEmpty()) {
                    groupStorage.addMembership(c, user.getId(), groups);
                }
                if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                    ftx = new FedoraTransaction(remoteStorage);
                    FedoraUserDao fedoraUsers = new FedoraUserDao();
                    fedoraUsers.setTransaction(ftx);
                    fedoraUsers.setMembership(user, groups, log);
                    ftx.commit();
                }
                c.commit();
                rollback = false;
            } finally {
                close(ftx);
                close(c, rollback);
            }
        } catch (Exception ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public List<Group> findUserGroups(int userId) {
        try {
            Connection c = getConnection();
            try {
                c.setAutoCommit(true);
                return groupStorage.findUserGroups(c, userId);
            } finally {
                close(c);
            }
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public Set<Permission> findUserPermissions(int userId) {
        try {
            Connection c = getConnection();
            try {
                c.setAutoCommit(true);
                return permissionStorage.find(c, userId);
            } finally {
                close(c);
            }
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public String findUserRole(int userId) {
        Transaction tx = daos.createTransaction();
        UserDao users = daos.createUser();
        users.setTransaction(tx);
        try {
            UserProfile user = filter(users.find(userId));
            return user.getRole();
        } finally {
            tx.close();
        }
    }

    @Override
    public void setPermissions(int groupId, Permission... permissions) {
        if (permissions == null || permissions.length == 0) {
            throw new IllegalStateException("permissions");
        }
        try {
            Connection c = source.getConnection();
            boolean rollback = true;
            try {
                c.setAutoCommit(false);
                permissionStorage.set(c, groupId, permissions);
                c.commit();
                rollback = false;
            } finally {
                close(c, rollback);
            }
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public void removePermissions(int groupId) {
        try {
            Connection c = source.getConnection();
            boolean rollback = true;
            try {
                c.setAutoCommit(false);
                permissionStorage.remove(c, groupId);
                c.commit();
                rollback = false;
            } finally {
                close(c, rollback);
            }
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
        }
    }

    private static Group createUserGroup(UserProfile user) {
        return Group.create(user.getUserName(), null);
    }

    private static UserProfile filter(UserProfile user) {
        if (user != null) {
            user.setUserPassword(null);
            user.setUserPasswordDigest(null);
        }
        return user;
    }

    private static List<UserProfile> filter(List<UserProfile> users) {
        for (UserProfile user : users) {
            filter(user);
        }
        return users;
    }

    private Connection getConnection() throws SQLException {
        return source.getConnection();
    }

}
