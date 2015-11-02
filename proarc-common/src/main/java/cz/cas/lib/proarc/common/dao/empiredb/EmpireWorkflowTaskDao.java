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
import java.math.BigDecimal;
import java.sql.Connection;
import org.apache.empire.db.DBRecord;
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

}
