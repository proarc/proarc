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
package cz.cas.lib.proarc.common.dao.empiredb;

import cz.cas.lib.proarc.common.dao.GroupDao;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.UserGroupTable;
import cz.cas.lib.proarc.common.user.Group;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import org.apache.empire.data.bean.BeanResult;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBContext;
import org.apache.empire.db.DBReader;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.exceptions.RecordNotFoundException;

/**
 * Manages user groups stored in RDBMS.
 *
 * @author Jan Pokorsky
 */
public final class EmpireGroupDao extends EmpireDao implements GroupDao {

    private final UserGroupTable table;

    public EmpireGroupDao(ProarcDatabase db) {
        super(db);
        table = db.tableUserGroup;
    }

    @Override
    public Group create() {
        return new Group();
    }

    @Override
    public void delete(Integer groupId) {
        if (groupId == null) {
            throw new IllegalArgumentException("Unsupported missing groupId!");
        }

        deleteConnectiogTableGroupUser(groupId);

        DBCommand cmd = db.createCommand();
        cmd.where(db.tableUserGroup.id.is(groupId));

        DBContext context = getContext();
        context.executeDelete(db.tableUserGroup, cmd);
    }

    private void deleteConnectiogTableGroupUser(Integer groupId) {
        if (groupId == null) {
            throw new IllegalArgumentException("Unsupported missing groupId!");
        }

        DBCommand cmd = db.createCommand();
        cmd.where(db.tableGroupMember.groupid.is(groupId));

        DBContext context = getContext();
        context.executeDelete(db.tableGroupMember, cmd);
    }

    @Override
    public void update(Group group) {
        DBContext contet = getContext();
        DBRecord record = new DBRecord(contet, table);

        try {
            if (group.getId() == null) {
                record.create();
                final Timestamp now = new Timestamp(System.currentTimeMillis());
                if (group.getCreated() == null) {
                    group.setCreated(now);
                }
                group.setTimestamp(now);
            } else {
                record.read(table.id.is(group.getId()));
            }
            record.setBeanProperties(group);

            record.update();
        } finally {
            record.close();
        }

        DBCommand cmd = db.createCommand();
        cmd.select(table.getColumns());
        cmd.where(table.id.is(group.getId()));

        getBeanProperties(cmd, 1);
    }

    @Override
    public Group find(int id) {

        DBCommand cmd = db.createCommand();
        cmd.select(table.getColumns());
        cmd.where(table.id.is(id));

        return getBeanProperties(cmd, 1);
    }

    @Override
    public List<Group> find(Integer id, String grpName, String grpRemoteName, String grpRemoteType) {
        BeanResult<Group> beans = new BeanResult<Group>(Group.class, table);
        DBCommand cmd = beans.getCommand();
        if (id != null) {
            cmd.where(table.id.is(id));
        }
        if (grpName != null) {
            cmd.where(table.groupname.is(grpName));
        }
        if (grpRemoteName != null) {
            cmd.where(table.remoteName.is(grpRemoteName));
        }
        if (grpRemoteType != null) {
            cmd.where(table.remoteType.is(grpRemoteType));
        }
        beans.fetch(getContext());
        return Collections.unmodifiableList(beans);
    }

    private Group getBeanProperties(DBCommand cmd, int limit) {
        DBContext context = getContext();
        DBReader reader = new DBReader(context);
        try {
            reader.open(cmd);
            List<Group> groups = reader.getBeanList(Group.class, limit);
            if (!groups.isEmpty()) {
                return groups.getFirst();
            } else {
                return null;
            }
        } catch (RecordNotFoundException ex) {
            return null;
        } finally {
            reader.close();
        }
    }

}
