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

import cz.cas.lib.proarc.common.sql.DbUtils;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;
import javax.sql.DataSource;

/**
 *
 * @author Jan Pokorsky
 */
final class GroupSqlStorage {

    private static final Logger LOG = Logger.getLogger(GroupSqlStorage.class.getName());
    private static final String GROUPID = "groupid";
    private static final String NAME = "name";
    private final DataSource source;

    public GroupSqlStorage(DataSource source) {
        this.source = source;
    }

    List<Group> findUserGroups(Connection c, int userId) throws SQLException {
        String query = "SELECT g.groupid groupid, g.name \"name\" FROM proarc_group_members m, proarc_groups g"
                + " WHERE m.userid=? AND m.groupid=g.groupid ORDER BY g.name";
        PreparedStatement stmt = c.prepareStatement(query);
        try {
            stmt.setInt(1, userId);
            ResultSet rs = stmt.executeQuery();
            try {
                LinkedList<Group> groups = new LinkedList<Group>();
                while (rs.next()) {
                    Group group = readGroup(rs);
                    groups.add(group);
                }
                return groups;
            } finally {
                DbUtils.close(rs);
            }
        } finally {
            DbUtils.close(stmt);
        }
    }

    void addMembership(Connection c, int userId, List<Group> groups) throws SQLException {
        String query = "INSERT INTO proarc_group_members (groupid, userid) VALUES (?, ?)";
        PreparedStatement stmt = c.prepareStatement(query);
        try {
            for (Group group : groups) {
                stmt.setInt(1, group.getId());
                stmt.setInt(2, userId);
                stmt.addBatch();
            }
            stmt.executeBatch();
        } finally {
            DbUtils.close(stmt);
        }
    }

    void removeMembership(Connection c, int userId) throws SQLException {
        String query = "DELETE FROM proarc_group_members WHERE userid=?";
        PreparedStatement stmt = c.prepareStatement(query);
        try {
            stmt.setInt(1, userId);
            stmt.executeUpdate();
        } finally {
            DbUtils.close(stmt);
        }
    }

    private Group readGroup(ResultSet rs) throws SQLException {
        Group group = new Group();
        group.setId(rs.getInt(GROUPID));
        group.setName(rs.getString(NAME));
        return group;
    }
}
