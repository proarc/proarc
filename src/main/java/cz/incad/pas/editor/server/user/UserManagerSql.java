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

import cz.incad.pas.editor.server.sql.DbUtils;
import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;
import javax.sql.DataSource;

/**
 * XXX delete/disable will require to set proarc_users.status="DELETED|DISABLED" + remove tomcat_roles(username, proarc)
 *
 * @author Jan Pokorsky
 */
final class UserManagerSql implements UserManager {
    
    private static final Logger LOG = Logger.getLogger(UserManagerSql.class.getName());
    private static final String USERID = "userid";
    private static final String USERNAME = "username";
    private static final String USERPASS = "userpass";
    private static final String FORENAME = "forename";
    private static final String SURNAME = "surname";
    private static final String EMAIL = "email";
    private static final String STATUS = "status";
    private static final String CREATED = "created";
    private static final String HOME = "home";
    private final DataSource source;
    private final File defaultHome;
    private final GroupSqlStorage groupStorage;
    private final PermissionSqlStorage permissionStorage;

    public UserManagerSql(DataSource source, File defaultHome) {
        this.source = source;
        this.defaultHome = defaultHome;
        groupStorage = new GroupSqlStorage(source);
        permissionStorage = new PermissionSqlStorage(source);
    }

    @Override
    public UserProfile find(String userName) throws IllegalArgumentException {
        try {
            Connection c = getConnection();
            try {
                c.setAutoCommit(true);
                return find(c, userName);
            } finally {
                DbUtils.close(c);
            }
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public UserProfile find(int userId) throws IllegalArgumentException {
        try {
            Connection c = getConnection();
            try {
                c.setAutoCommit(true);
                return find(c, userId);
            } finally {
                DbUtils.close(c);
            }
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public List<UserProfile> findAll() {
        try {
            Connection c = getConnection();
            try {
                c.setAutoCommit(true);
                return findAll(c);
            } finally {
                DbUtils.close(c);
            }
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public UserProfile add(UserProfile profile) {
        if (profile == null) {
            throw new NullPointerException();
        }
        profile.validateAsNew();
        String userName = profile.getUserName();
        String userHomePath = profile.getUserHome();
        File allUsersHome = defaultHome;
        File toRollback = null;
        File userHome;
        try {
            if (userHomePath == null) {
                toRollback = userHome = UserUtil.createUserHome(userName, userHomePath, allUsersHome);
            } else {
                toRollback = userHome = new File(userHomePath);
                if (!userHome.mkdir()) { // already exists
                    toRollback = null;
                    if (!userHome.isDirectory()) {
                        throw new IOException(String.format("Not a folder: '%s'!", userHome));
                    }
                }
            }
            profile.setUserHome(UserUtil.toUri(userHome));
        } catch (Throwable ex) {
            if (toRollback != null) {
                toRollback.delete();
            }
            throw new IllegalStateException(profile.toString(), ex);
        }

        try {
            Connection c = getConnection();
            c.setAutoCommit(false);
            profile = add(c, profile);
            UserUtil.createUserSubfolders(userHome);
            toRollback = null;
            return profile;
        } catch (SQLException ex) {
            throw new IllegalStateException(profile.toString(), ex);
        } finally {
            if (toRollback != null) {
                toRollback.delete();
            }
        }
    }

    @Override
    public void update(UserProfile profile) {
        try {
            Connection c = getConnection();
            boolean rollback = true;
            try {
                c.setAutoCommit(false);
                if (profile.getUserPassword() != null) {
                    profile.setUserPasswordDigest(UserUtil.getDigist(profile.getUserPassword()));
                    updateTomcatUser(c, profile);
                    profile.setUserPassword(null);
                    profile.setUserPasswordDigest(null);
                }
                updateProarcUser(c, profile);
                c.commit();
                rollback = false;
            } finally {
                DbUtils.close(c, rollback);
            }
        } catch (SQLException ex) {
            throw new IllegalStateException(profile.toString(), ex);
        }
    }

    @Override
    public Group addGroup(Group group) {
        return groupStorage.add(group);
    }

    @Override
    public List<Group> findGroups() {
        return groupStorage.find();
    }

    @Override
    public Group findGroup(int groupId) {
        return groupStorage.find(groupId);
    }

    @Override
    public void setUserGroups(int userId, Group... groups) {
        try {
            Connection c = source.getConnection();
            boolean rollback = true;
            try {
                c.setAutoCommit(false);
                groupStorage.removeMembership(c, userId);
                if (groups != null && groups.length > 0) {
                    groupStorage.addMembership(c, userId, groups);
                }
                c.commit();
                rollback = false;
            } finally {
                DbUtils.close(c, rollback);
            }
        } catch (SQLException ex) {
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
                DbUtils.close(c);
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
                DbUtils.close(c);
            }
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
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
                DbUtils.close(c, rollback);
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
                DbUtils.close(c, rollback);
            }
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
        }
    }

    private void updateTomcatUser(Connection c, UserProfile profile) throws SQLException {
        String query = "UPDATE tomcat_users SET userpass=? WHERE username=?";
        PreparedStatement stmt = c.prepareStatement(query);
        try {
            stmt.setString(1, profile.getUserPasswordDigest());
            stmt.setString(2, profile.getUserName());
            int result = stmt.executeUpdate();
            if (result != 1) {
                throw new IllegalArgumentException("User not found: " + profile.getUserName());
            }
        } finally {
            DbUtils.close(stmt);
        }
    }

    private void updateProarcUser(Connection c, UserProfile profile) throws SQLException {
        String query = "UPDATE proarc_users SET email=?, forename=?, surname=?, home=? WHERE userid=?";
        PreparedStatement stmt = c.prepareStatement(query);
        try {
            int column = 1;
            stmt.setString(column++, profile.getEmail());
            stmt.setString(column++, profile.getForename());
            stmt.setString(column++, profile.getSurname());
            stmt.setString(column++, profile.getUserHome());
            stmt.setInt(column++, profile.getId());
            int result = stmt.executeUpdate();
            if (result != 1) {
                throw new IllegalArgumentException("User not found: " + profile.getId());
            }
        } finally {
            DbUtils.close(stmt);
        }
    }

    UserProfile add(Connection c, UserProfile profile) {
        boolean rollback = true;
        try {
            addTomcatUser(c, profile);
            addTomcatRole(c, profile);
            profile = addProarcUser(c, profile);
            c.commit();
            rollback = false;
            return profile;
        } catch (SQLException ex) {
            // XXX check for unique constraint violation ex.getMessage()
            throw new IllegalStateException(profile.toString(), ex);
        } finally {
            DbUtils.close(c, rollback);
        }
    }

    private void addTomcatUser(Connection c, UserProfile profile) throws SQLException {
        String query = "INSERT INTO tomcat_users (username, userpass) VALUES (?, ?)";
        PreparedStatement stmt = c.prepareStatement(query);
        try {
            stmt.setString(1, profile.getUserName());
            stmt.setString(2, profile.getUserPasswordDigest());
            stmt.executeUpdate();
        } finally {
            stmt.close();
        }
    }

    private void addTomcatRole(Connection c, UserProfile profile) throws SQLException {
        String query = "INSERT INTO tomcat_roles (username, rolename) VALUES (?, 'proarc')";
        PreparedStatement stmt = c.prepareStatement(query);
        try {
            stmt.setString(1, profile.getUserName());
            stmt.executeUpdate();
        } finally {
            stmt.close();
        }
    }

    private UserProfile addProarcUser(Connection c, UserProfile profile) throws SQLException {
        String query = "INSERT INTO proarc_users (email, forename, surname, home, username) VALUES (?, ?, ?, ?, ?)";
        PreparedStatement stmt = c.prepareStatement(query, Statement.RETURN_GENERATED_KEYS);
        try {
            int column = 1;
            stmt.setString(column++, profile.getEmail());
            stmt.setString(column++, profile.getForename());
            stmt.setString(column++, profile.getSurname());
            stmt.setString(column++, profile.getUserHome());
            stmt.setString(column++, profile.getUserName());
            stmt.executeUpdate();
            ResultSet rs = stmt.getGeneratedKeys();
            try {
                if (rs.next()) {
                    profile.setId(rs.getInt(USERID));
                    profile.setCreated(rs.getDate(CREATED));
                    profile.setUserPassword(null);
                    profile.setUserPasswordDigest(null);
                    return profile;
                } else {
                    throw new SQLException("no autogenerated field");
                }
            } finally {
                DbUtils.close(rs);
            }
        } finally {
            DbUtils.close(stmt);
        }
    }

    UserProfile find(Connection c, int userId) throws SQLException {
        String query = "SELECT * FROM proarc_users WHERE userid=?";
        PreparedStatement stmt = c.prepareStatement(query);
        try {
            stmt.setInt(1, userId);
            ResultSet rs = stmt.executeQuery();
            try {
                UserProfile up = null;
                if (rs.next()) {
                    up = readUser(rs);
                }
                return up;
            } finally {
                DbUtils.close(rs);
            }
        } finally {
            DbUtils.close(stmt);
        }
    }

    UserProfile find(Connection c, String userName) throws SQLException {
        String query = "SELECT * FROM proarc_users WHERE username=?";
        PreparedStatement stmt = c.prepareStatement(query);
        try {
            stmt.setString(1, userName);
            ResultSet rs = stmt.executeQuery();
            try {
                UserProfile up = null;
                if (rs.next()) {
                    up = readUser(rs);
                }
                return up;
            } finally {
                DbUtils.close(rs);
            }
        } finally {
            DbUtils.close(stmt);
        }
    }

    List<UserProfile> findAll(Connection c) throws SQLException {
        String query = "SELECT * FROM proarc_users ORDER BY surname";
        PreparedStatement stmt = c.prepareStatement(query);
        try {
            ResultSet rs = stmt.executeQuery();
            LinkedList<UserProfile> profiles = new LinkedList<UserProfile>();
            try {
                while (rs.next()) {
                    profiles.add(readUser(rs));
                }
                return profiles;
            } finally {
                DbUtils.close(rs);
            }
        } finally {
            DbUtils.close(stmt);
        }
    }

    private UserProfile readUser(ResultSet rs) throws SQLException {
        // ignore password
        UserProfile up = new UserProfile();
        up.setCreated(rs.getDate(CREATED));
        up.setEmail(rs.getString(EMAIL));
        up.setForename(rs.getString(FORENAME));
        up.setId(rs.getInt(USERID));
        up.setSurname(rs.getString(SURNAME));
        String home = rs.getString(HOME);
        up.setUserHome(home);
        up.setUserName(rs.getString(USERNAME));
        return up;
    }

    private Connection getConnection() throws SQLException {
        return source.getConnection();
    }

}
