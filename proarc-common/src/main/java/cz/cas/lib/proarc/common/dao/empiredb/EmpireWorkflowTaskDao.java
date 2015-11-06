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

import cz.cas.lib.proarc.common.dao.ConcurrentModificationException;
import cz.cas.lib.proarc.common.dao.WorkflowTaskDao;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.WorkflowTaskTable;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.model.TaskFilter;
import cz.cas.lib.proarc.common.workflow.model.TaskView;
import java.math.BigDecimal;
import java.sql.Connection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBJoinType;
import org.apache.empire.db.DBReader;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.DBRecordData;
import org.apache.empire.db.exceptions.RecordNotFoundException;
import org.apache.empire.db.exceptions.RecordUpdateInvalidException;

/**
 *
 * @author Jan Pokorsky
 */
public class EmpireWorkflowTaskDao extends EmpireDao implements WorkflowTaskDao {

    private final WorkflowTaskTable tableTask;

    public EmpireWorkflowTaskDao(ProarcDatabase db) {
        super(db);
        tableTask = db.tableWorkflowTask;
    }

    @Override
    public Task create() {
        return new Task();
    }

    @Override
    public void update(Task task) throws ConcurrentModificationException {
        Connection c = getConnection();
        DBRecord r = new DBRecord();
        if (task.getId() == null) {
            r.create(tableTask);
        } else {
            r.read(tableTask, task.getId(), c);
        }
        r.setBeanValues(task);
        try {
            r.update(c);
        } catch (RecordUpdateInvalidException ex) {
            throw new ConcurrentModificationException(ex);
        }
        r.getBeanProperties(task);
    }

    @Override
    public Task find(BigDecimal id) {
        DBRecord record = new DBRecord();
        try {
            record.read(tableTask, id, getConnection());
            Task task = new Task();
            record.getBeanProperties(task);
            return task;
        } catch (RecordNotFoundException ex) {
            return null;
        } finally {
            record.close();
        }
    }

    @Override
    public List<TaskView> view(TaskFilter filter) {
        DBCommand cmd = db.createCommand();
        cmd.select(tableTask.getColumns());
        cmd.select(db.tableWorkflowJob.label.as("JOB_LABEL"));
        cmd.select(db.tableUser.username);
        cmd.join(tableTask.jobId, db.tableWorkflowJob.id);
        cmd.join(tableTask.ownerId, db.tableUser.id, DBJoinType.LEFT);

        if (filter.getId() != null) {
            cmd.where(tableTask.id.is(filter.getId()));
        }
        if (filter.getJobId() != null) {
            cmd.where(tableTask.jobId.is(filter.getJobId()));
        }
        if (filter.getPriority() != null) {
            cmd.where(tableTask.priority.is(filter.getPriority()));
        }
        if (filter.getProfileName() != null) {
            cmd.where(tableTask.typeRef.is(filter.getProfileName()));
        }
        if (filter.getState() != null) {
            cmd.where(tableTask.state.is(filter.getState().name()));
        }
        if (filter.getUserId() != null) {
            cmd.where(tableTask.ownerId.is(filter.getUserId()));
        }
        EmpireUtils.addOrderBy(cmd, filter.getSortBy(), tableTask.timestamp, false);

        DBReader reader = new DBReader();
        try {
            reader.open(cmd, getConnection());
            if (!reader.skipRows(filter.getOffset())) {
                return Collections.emptyList();
            }
            ArrayList<TaskView> viewItems = new ArrayList<TaskView>(filter.getMaxCount());
            for (Iterator<DBRecordData> it = reader.iterator(filter.getMaxCount()); it.hasNext();) {
                DBRecordData rec = it.next();
                TaskView view = new TaskView();
                rec.getBeanProperties(view);
                viewItems.add(view);
            }
            return viewItems;
        } finally {
            reader.close();
        }
    }

}
