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
import java.util.Collections;
import java.util.List;
import org.apache.empire.data.bean.BeanResult;
import org.apache.empire.db.DBCommand;
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
    public void update(UserProfile user) {
        DBRecord dbr = new DBRecord();
        try {
            if (user.getId() == null) {
                dbr.create(table);
                Timestamp now = new Timestamp(System.currentTimeMillis());
                if (user.getCreated() == null) {
                    user.setCreated(now);
                }
                user.setTimestamp(now);
                dbr.setValue(table.timestamp, now);
            } else {
                dbr.init(table, new Object[] {user.getId()}, false);
            }
            dbr.setBeanValues(user);
            dbr.update(getConnection());
            dbr.getBeanProperties(user);
        } finally {
            dbr.close();
        }
    }

    @Override
    public UserProfile find(int userId) {
        DBRecord r = new DBRecord();
        try {
            r.read(table, userId, getConnection());
            UserProfile user = new UserProfile();
            r.getBeanProperties(user);
            return user;
        } catch (RecordNotFoundException ex) {
            return null;
        } finally {
            r.close();
        }
    }

    @Override
    public List<UserProfile> find(String userName, String passwd, String remoteName, String remoteType) {
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
        cmd.orderBy(table.surname);
        beans.fetch(getConnection());
        return Collections.unmodifiableList(beans);
    }

}
