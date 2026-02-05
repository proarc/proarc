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

import cz.cas.lib.proarc.common.dao.UserDao;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.UserTable;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.apache.empire.data.Column;
import org.apache.empire.data.bean.BeanResult;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBContext;
import org.apache.empire.db.DBReader;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.exceptions.RecordNotFoundException;

/**
 * Manages users stored in RDBMS.
 *
 * @author Jan Pokorsky
 */
public class EmpireUserDao extends EmpireDao implements UserDao {

    private final UserTable table;

    public EmpireUserDao(ProarcDatabase db) {
        super(db);
        table = db.tableUser;
    }

    @Override
    public UserProfile create() {
        return new UserProfile();
    }

    @Override
    public void delete(Integer userId) {
        if (userId == null) {
            throw new IllegalArgumentException("Unsupported missing userId!");
        }

        deleteConnectiogTableGroupUser(userId);

        DBCommand cmd = db.createCommand();
        cmd.where(db.tableUser.id.is(userId));

        DBContext context = getContext();
        context.executeDelete(db.tableUser, cmd);
    }

    private void deleteConnectiogTableGroupUser(Integer userId) {
        if (userId == null) {
            throw new IllegalArgumentException("Unsupported missing userId!");
        }

        DBCommand cmd = db.createCommand();
        cmd.where(db.tableGroupMember.userid.is(userId));

        DBContext context = getContext();
        context.executeDelete(db.tableGroupMember, cmd);
    }

    @Override
    public void update(UserProfile user) {
        DBContext context = getContext();
        DBRecord record = new DBRecord(context, table);

        try {
            if (user.getId() == null) {
                record.create();
                Timestamp now = new Timestamp(System.currentTimeMillis());
                user.setTimestamp(now);
                if (user.getCreated() == null) {
                    user.setCreated(now);
                }
                record.setBeanProperties(user);
            } else {
                record.read(table.id.is(user.getId()));
                Collection<Column> ignore = user.getUserPasswordDigest() == null ? Arrays.<Column>asList(table.passwd) : null;
                record.setBeanProperties(user, ignore);
            }

            record.update();
        } finally {
            record.close();
        }

        DBCommand cmd = db.createCommand();
        cmd.select(table.getColumns());
        cmd.where(table.id.is(user.getId()));

        getBeanProperties(cmd, 1);
    }

    @Override
    public UserProfile find(int userId) {
        DBCommand cmd = db.createCommand();
        cmd.select(table.getColumns());
        cmd.where(table.id.is(userId));

        return getBeanProperties(cmd, 1);
    }

    @Override
    public List<UserProfile> find(String userName, String passwd, String remoteName, String remoteType, String organization) {
        BeanResult<UserProfile> beans = new BeanResult<UserProfile>(UserProfile.class, table);
        DBCommand cmd = beans.getCommand();
        if (userName != null) {
            cmd.where(table.username.is(userName));
        }
        if (passwd != null) {
            cmd.where(table.passwd.is(passwd));
        }
        if (remoteName != null) {
            cmd.where(table.remoteName.is(remoteName));
        }
        if (remoteType != null) {
            cmd.where(table.remoteType.is(remoteType));
        }
        if (organization != null) {
            cmd.where(table.organization.is(organization));
        }
        cmd.orderBy(table.username);
        beans.fetch(getContext());
        return Collections.unmodifiableList(beans);
    }

    private UserProfile getBeanProperties(DBCommand cmd, int limit) {
        DBContext context = getContext();
        DBReader reader = new DBReader(context);
        try {
            reader.open(cmd);
            List<UserProfile> userProfiles = reader.getBeanList(UserProfile.class, limit);
            if (!userProfiles.isEmpty()) {
                return userProfiles.getFirst();
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
