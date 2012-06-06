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
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;
import javax.sql.DataSource;

/**
 *
 * @author Jan Pokorsky
 */
final class PermissionSqlStorage {

    private static final Logger LOG = Logger.getLogger(PermissionSqlStorage.class.getName());
    private final DataSource source;

    public PermissionSqlStorage(DataSource source) {
        this.source = source;
    }

    Set<Permission> find(Connection c, int userId) throws SQLException {
        String query = "SELECT permissionid FROM proarc_group_members gm, proarc_group_permissions gp WHERE gm.userid=? and gp.groupid=gm.groupid";
        PreparedStatement stmt = c.prepareStatement(query);
        try {
            stmt.setInt(1, userId);
            ResultSet rs = stmt.executeQuery();
            try {
                HashSet<Permission> permissions = new HashSet<Permission>();
                while (rs.next()) {
                    String actionId = rs.getString("permissionid");
                    Permission permission = new Permission();
                    permission.setId(actionId);
                    permissions.add(permission);
                }
                return permissions;
            } finally {
                DbUtils.close(rs);
            }
        } finally {
            DbUtils.close(stmt);
        }
    }

    void set(Connection c, int groupId, Permission... permissions) throws SQLException {
        remove(c, groupId);
        add(c, groupId, permissions);
    }

    void add(Connection c, int groupId, Permission... permissions) throws SQLException {
        String query = "INSERT INTO proarc_group_permissions (permissionid, groupid, objectid) VALUES (?, ?, 'SYSTEM')";
        PreparedStatement stmt = c.prepareStatement(query);
        try {
            for (Permission permission : permissions) {
                stmt.setString(1, permission.getId());
                stmt.setInt(2, groupId);
                stmt.addBatch();
            }
            stmt.executeBatch();
        } finally {
            DbUtils.close(stmt);
        }
    }

    void remove(Connection c, int groupId) throws SQLException {
        String query = "DELETE FROM proarc_group_permissions WHERE groupid=?";
        PreparedStatement stmt = c.prepareStatement(query);
        try {
            stmt.setInt(1, groupId);
            stmt.executeUpdate();
        } finally {
            DbUtils.close(stmt);
        }
    }

}
