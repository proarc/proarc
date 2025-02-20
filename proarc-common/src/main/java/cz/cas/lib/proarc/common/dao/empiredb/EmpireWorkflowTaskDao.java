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
import org.apache.empire.db.DBColumnExpr;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBJoinType;
import org.apache.empire.db.DBQuery;
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
            r.setModified(tableTask.state, true);
            r.update(c);
        } catch (RecordUpdateInvalidException ex) {
            throw new ConcurrentModificationException(ex);
        }
        r.getBeanProperties(task);
    }

    @Override
    public void delete(BigDecimal jobId) {
        if (jobId == null) {
            throw new IllegalArgumentException("Unsupported missing jobId!");
        }
        Connection c = getConnection();
        DBCommand cmd = db.createCommand();
        cmd.where(db.tableWorkflowTask.jobId.is(jobId));
        db.executeDelete(db.tableWorkflowTask, cmd, c);
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

        cmd.select(db.tableWorkflowPhysicalDoc.barcode);
        cmd.select(db.tableWorkflowPhysicalDoc.signature);

        DBCommand pmatCmd = db.createCommand();
        final DBColumnExpr pmatMaterialId = db.tableWorkflowMaterial.id.min().as(db.tableWorkflowMaterial.id);
        pmatCmd.select(pmatMaterialId);
        pmatCmd.select(db.tableWorkflowTask.jobId);
        pmatCmd.join(db.tableWorkflowMaterial.id, db.tableWorkflowMaterialInTask.materialId);
        pmatCmd.join(db.tableWorkflowTask.id, db.tableWorkflowMaterialInTask.taskId);
        pmatCmd.where(db.tableWorkflowMaterial.type.is("PHYSICAL_DOCUMENT"));
        pmatCmd.groupBy(db.tableWorkflowTask.jobId);
        DBQuery pmatQuery = new DBQuery(pmatCmd);

        DBQuery.DBQueryColumn wmId = pmatQuery.findQueryColumn(pmatMaterialId);
        DBQuery.DBQueryColumn wjId = pmatQuery.findQueryColumn(db.tableWorkflowTask.jobId);
        cmd.join(db.tableWorkflowJob.id, wjId, DBJoinType.LEFT);
        // empire db reverse the joint type so that is why RIGHT instead of LEFT
        cmd.join(db.tableWorkflowPhysicalDoc.materialId, wmId, DBJoinType.RIGHT);

        if (filter.getIds() != null) {
            cmd.where(tableTask.id.in(filter.getIds()));
        } else if (filter.getId() != null) {
            cmd.where(tableTask.id.is(filter.getId()));
        }
        if (filter.getJobId() != null) {
            cmd.where(tableTask.jobId.is(filter.getJobId()));
        }
        if (filter.getJobLabel() != null) {
            String pattern = filter.getJobLabel().trim().replace("%", "\\%");
            if (!pattern.isEmpty()) {
                cmd.where(db.tableWorkflowJob.label.likeUpper('%' + pattern + '%'));
            }
        }
        if (!filter.getPriority().isEmpty()) {
            cmd.where(tableTask.priority.in(filter.getPriority()));
        }
        if (!filter.getProfileName().isEmpty()) {
            cmd.where(tableTask.typeRef.in(filter.getProfileName()));
        }
        if (!filter.getState().isEmpty()) {
            cmd.where(tableTask.state.in(filter.getStateAsString()));
        }
        if (!filter.getUserId().isEmpty()) {
            cmd.where(tableTask.ownerId.in(filter.getUserId()));
        }
        if (filter.getBarcode() != null) {
            cmd.where(db.tableWorkflowPhysicalDoc.barcode.is(filter.getBarcode()));
        }
        if (filter.getOrder() != null) {
            cmd.where(tableTask.order.in(filter.getOrder()));
        }
        if (filter.getNote() != null) {
            cmd.where(tableTask.note.is(filter.getNote()));
        }
        if (filter.getSignature() != null) {
            cmd.where(db.tableWorkflowPhysicalDoc.signature.is(filter.getSignature()));
        }
        EmpireUtils.addWhereDate(cmd, tableTask.created, filter.getCreated());
        EmpireUtils.addWhereDate(cmd, tableTask.timestamp, filter.getModified());
        EmpireUtils.addOrderBy(cmd, filter.getSortBy(), tableTask.order, false);

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
