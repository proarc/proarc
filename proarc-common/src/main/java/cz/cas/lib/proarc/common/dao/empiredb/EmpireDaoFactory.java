/*
 * Copyright (C) 2013 Jan Pokorsky
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

import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.dao.GroupDao;
import cz.cas.lib.proarc.common.dao.UserDao;
import cz.cas.lib.proarc.common.dao.UserSettingDao;
import java.sql.Connection;
import java.sql.SQLException;

/**
 *
 * @author Jan Pokorsky
 */
public class EmpireDaoFactory implements DaoFactory {

    private final ProarcDatabase db;
    private final EmpireConfiguration conf;

    public EmpireDaoFactory(EmpireConfiguration conf) {
        this.db = conf.getSchema();
        this.conf = conf;
        
    }

    public ProarcDatabase getDb() {
        return db;
    }

    @Override
    public void init() {
        try {
            db.init(conf);
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public SqlTransaction createTransaction() {
        try {
            Connection c = conf.getConnection();
            c.setAutoCommit(false);
            return new SqlTransaction(c);
        } catch (SQLException ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public EmpireBatchDao createBatch() {
        return new EmpireBatchDao(db);
    }

    @Override
    public EmpireBatchItemDao createBatchItem() {
        return new EmpireBatchItemDao(db);
    }

    @Override
    public UserDao createUser() {
        return new EmpireUserDao(db);
    }

    @Override
    public GroupDao createUserGroup() {
        return new EmpireGroupDao(db);
    }

    @Override
    public UserSettingDao createUserSettingDao() {
        return new EmpireUserSettingDao(db);
    }

    @Override
    public EmpireWorkflowJobDao createWorkflowJobDao() {
        return new EmpireWorkflowJobDao(db);
    }

    @Override
    public EmpireWorkflowTaskDao createWorkflowTaskDao() {
        return new EmpireWorkflowTaskDao(db);
    }

    @Override
    public EmpireWorkflowParameterDao createWorkflowParameterDao() {
        return new EmpireWorkflowParameterDao(db);
    }

    @Override
    public EmpireWorkflowMaterialDao createWorkflowMaterialDao() {
        return new EmpireWorkflowMaterialDao(db);
    }
    
}
