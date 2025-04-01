/*
 * Copyright (C) 2015 Lukas Sykora
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
import cz.cas.lib.proarc.common.dao.UserSettingDao;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.UserSettingTable;
import cz.cas.lib.proarc.common.user.UserSetting;
import java.util.Collections;
import java.util.List;
import org.apache.empire.data.bean.BeanResult;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.exceptions.RecordNotFoundException;
import org.apache.empire.db.exceptions.RecordUpdateInvalidException;

/**
 * Manages user setting stored in RDBMS.
 *
 * @author Lukas Sykora
 */
public final class EmpireUserSettingDao extends EmpireDao implements UserSettingDao {

    private final UserSettingTable table;

    public EmpireUserSettingDao(ProarcDatabase db) {
        super(db);
        table = db.tableUserSetting;
    }

    @Override
    public UserSetting create() {
        return new UserSetting();
    }

    @Override
    public void update(UserSetting userSetting) {
        DBRecord dbr = new DBRecord();
        try {
            if (userSetting.getId() == null) {
                dbr.create(table);
            } else {
                dbr.read(table, userSetting.getId(), getConnection());
            }
            dbr.setModified(table.userSetting, true);
            dbr.setBeanValues(userSetting);
            try {
                dbr.update(getConnection());
            } catch (RecordUpdateInvalidException ex) {
                throw new ConcurrentModificationException(ex);
            }
            dbr.getBeanProperties(userSetting);
        } finally {
            dbr.close();
        }
    }

    @Override
    public UserSetting find(int id) {
        DBRecord dbr = new DBRecord();
        try {
            dbr.read(table, id, getConnection());
            UserSetting userSetting = new UserSetting();
            dbr.getBeanProperties(userSetting);
            return userSetting;
        } catch (RecordNotFoundException ex) {
            return null;
        } finally {
            dbr.close();
        }
    }

    @Override
    public List<UserSetting> findByUserId(int userId) {
        BeanResult<UserSetting> beans = new BeanResult<>(UserSetting.class, table);
        DBCommand cmd = beans.getCommand();
        cmd.where(table.userId.is(userId));
        cmd.orderBy(table.id);
        beans.fetch(getConnection());
        return Collections.unmodifiableList(beans);
    }
}
