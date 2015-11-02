/*
 * Copyright (C) 2015 Jan Pokorsky
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

import cz.cas.lib.proarc.common.dao.WorkflowParameterDao;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.WorkflowParameterTable;
import cz.cas.lib.proarc.common.workflow.model.TaskParameter;
import java.math.BigDecimal;
import java.sql.Connection;
import java.util.Collections;
import java.util.List;
import org.apache.empire.data.bean.BeanResult;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBRecord;

/**
 *
 * @author Jan Pokorsky
 */
public class EmpireWorkflowParameterDao extends EmpireDao implements WorkflowParameterDao {

    private final WorkflowParameterTable tableParams;

    public EmpireWorkflowParameterDao(ProarcDatabase db) {
        super(db);
        tableParams = db.tableWorkflowParameter;
    }

    @Override
    public TaskParameter create() {
        return new TaskParameter();
    }

    @Override
    public void add(BigDecimal taskId, List<TaskParameter> params) {
        Connection c = getConnection();
        DBRecord r = new DBRecord();
        for (TaskParameter param : params) {
            r.create(tableParams);
            r.setBeanValues(param);
            r.update(c);
        }
    }

    @Override
    public void remove(BigDecimal taskId) {
        Connection c = getConnection();
        DBCommand cmd = db.createCommand();
        cmd.where(tableParams.taskId.is(taskId));
        db.executeDelete(tableParams, cmd, c);
    }

    @Override
    public List<TaskParameter> find(BigDecimal taskId) {
        BeanResult<TaskParameter> result = new BeanResult<TaskParameter>(TaskParameter.class, tableParams);
        DBCommand cmd = result.getCommand();
        cmd.where(tableParams.taskId.is(taskId));
        result.fetch(getConnection());
        return Collections.unmodifiableList(result);
    }

}
