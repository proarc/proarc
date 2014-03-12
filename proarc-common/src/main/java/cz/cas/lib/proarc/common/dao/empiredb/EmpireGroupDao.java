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

import cz.cas.lib.proarc.common.dao.ConcurrentModificationException;
import cz.cas.lib.proarc.common.dao.GroupDao;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.UserGroupTable;
import cz.cas.lib.proarc.common.user.Group;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import org.apache.empire.data.bean.BeanResult;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.exceptions.RecordNotFoundException;
import org.apache.empire.db.exceptions.RecordUpdateInvalidException;

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
    public void update(Group group) {
        DBRecord dbr = new DBRecord();
        try {
            if (group.getId() == null) {
                dbr.create(table);
                final Timestamp now = new Timestamp(System.currentTimeMillis());
                if (group.getCreated() == null) {
                    group.setCreated(now);
                }
                group.setTimestamp(now);
            } else {
                dbr.read(table, new Object[] {group.getId()}, getConnection());
            }
            dbr.setBeanValues(group);
            try {
                dbr.update(getConnection());
            } catch (RecordUpdateInvalidException ex) {
                throw new ConcurrentModificationException(ex);
            }
            dbr.getBeanProperties(group);
        } finally {
            dbr.close();
        }
    }

    @Override
    public Group find(int id) {
        DBRecord dbr = new DBRecord();
        try {
            dbr.read(table, id, getConnection());
            Group group = new Group();
            dbr.getBeanProperties(group);
            return group;
        } catch (RecordNotFoundException ex) {
            return null;
        } finally {
            dbr.close();
        }
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
        beans.fetch(getConnection());
        return Collections.unmodifiableList(beans);
    }

}
