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

import cz.cas.lib.proarc.common.dao.UserSettingDao;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.UserSettingTable;
import cz.cas.lib.proarc.common.user.UserSetting;
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
        DBContext context = getContext();
        DBRecord record = new DBRecord(context, table);

        try {
            if (userSetting.getId() == null) {
                record.create();
                Timestamp now = new Timestamp(System.currentTimeMillis());
                userSetting.setTimestamp(now);
            } else {
                record.read(table.id.is(userSetting.getId()));
            }
            record.setBeanProperties(userSetting);

            record.update();
        } finally {
            record.close();
        }

        DBCommand cmd = db.createCommand();
        cmd.select(table.getColumns());
        cmd.where(table.id.is(userSetting.getId()));

        getBeanProperties(cmd, 1);
    }

    @Override
    public UserSetting find(int id) {
        DBCommand cmd = db.createCommand();
        cmd.select(table.getColumns());
        cmd.where(table.id.is(id));

        return getBeanProperties(cmd, 1);
    }

    @Override
    public List<UserSetting> findByUserId(int userId) {
        BeanResult<UserSetting> beans = new BeanResult<>(UserSetting.class, table);
        DBCommand cmd = beans.getCommand();
        cmd.where(table.userId.is(userId));
        cmd.orderBy(table.id);
        beans.fetch(getContext());
        return Collections.unmodifiableList(beans);
    }

    private UserSetting getBeanProperties(DBCommand cmd, int limit) {
        DBContext context = getContext();
        DBReader reader = new DBReader(context);
        try {
            reader.open(cmd);
            List<UserSetting> userSettings = reader.getBeanList(UserSetting.class, limit);
            if (!userSettings.isEmpty()) {
                return userSettings.getFirst();
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
