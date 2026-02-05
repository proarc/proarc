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
import cz.cas.lib.proarc.common.workflow.model.TaskParameterFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskParameterView;
import java.math.BigDecimal;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.empire.data.bean.BeanResult;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBContext;
import org.apache.empire.db.DBReader;
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
        if (taskId == null) {
            throw new IllegalArgumentException("Unsupported missing taskId!");
        }

        DBContext context = getContext();
        DBRecord record = new DBRecord(context, tableParams);
        try {
            for (TaskParameter param : params) {
                param.setTaskId(taskId);
                record.create();
                record.setBeanProperties(param);
                record.update();
            }
        } finally {
            record.close();
        }
    }

    @Override
    public void remove(BigDecimal taskId) {
        if (taskId == null) {
            throw new IllegalArgumentException("Unsupported missing taskId!");
        }

        DBCommand cmd = db.createCommand();
        cmd.where(tableParams.taskId.is(taskId));

        DBContext context = getContext();
        context.executeDelete(tableParams, cmd);
    }

    @Override
    public List<TaskParameter> find(BigDecimal taskId) {
        if (taskId == null) {
            throw new IllegalArgumentException("Unsupported missing taskId!");
        }
        BeanResult<TaskParameter> result = new BeanResult<TaskParameter>(TaskParameter.class, tableParams);
        DBCommand cmd = result.getCommand();
        cmd.where(tableParams.taskId.is(taskId));
        result.fetch(getContext());
        return Collections.unmodifiableList(result);
    }

    public Map<String, String> findAsMap(BigDecimal taskId) {
        HashMap<String, String> m = new HashMap<String, String>();
        for (TaskParameter param : find(taskId)) {
            m.put(param.getParamRef(), param.getValue());
        }
        return m;
    }

    @Override
    public List<TaskParameterView> view(TaskParameterFilter filter) {
        BeanResult<TaskParameterView> result = new BeanResult<TaskParameterView>(
                TaskParameterView.class, tableParams);
        DBCommand cmd = result.getCommand();
        cmd.select(tableParams.getColumns());
        cmd.select(db.tableWorkflowTask.jobId, db.tableWorkflowTask.typeRef.as("TASK_PROFILE_NAME"));
        cmd.join(tableParams.taskId, db.tableWorkflowTask.id);

        if (filter.getTaskId() != null) {
            cmd.where(tableParams.taskId.is(filter.getTaskId()));
        }
        if (filter.getProfileName() != null) {
            cmd.where(tableParams.paramRef.is(filter.getProfileName()));
        }
        if (filter.getJobId() != null) {
            cmd.where(db.tableWorkflowTask.jobId.is(filter.getJobId()));
        }

        DBContext context = getContext();
        DBReader reader = new DBReader(context);
        try {
            reader.open(cmd);
            if (!reader.skipRows(filter.getOffset())) {
                return Collections.emptyList();
            }
            List<TaskParameterView> viewItems = reader.getBeanList(TaskParameterView.class, filter.getMaxCount());
            return viewItems;
        } finally {
            reader.close();
        }
    }

}
