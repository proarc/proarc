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
import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import org.apache.empire.db.DBColumnExpr;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBContext;
import org.apache.empire.db.DBJoinType;
import org.apache.empire.db.DBQuery;
import org.apache.empire.db.DBQueryColumn;
import org.apache.empire.db.DBReader;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.exceptions.RecordNotFoundException;

/**
 *
 * @author Jan Pokorsky
 */
public class EmpireWorkflowTaskDao extends EmpireDao implements WorkflowTaskDao {

    private final WorkflowTaskTable table;

    public EmpireWorkflowTaskDao(ProarcDatabase db) {
        super(db);
        table = db.tableWorkflowTask;
    }

    @Override
    public Task create() {
        return new Task();
    }

    @Override
    public void update(Task task) throws ConcurrentModificationException {
        DBContext context = getContext();
        DBRecord record = new DBRecord(context, table);

        try {
            if (task.getId() == null) {
                record.create();
                Timestamp now = new Timestamp(System.currentTimeMillis());
                task.setTimestamp(now);
            } else {
                record.read(table.id.is(task.getId()));
            }
            record.setBeanProperties(task);

            record.update();
        } finally {
            record.close();
        }

        DBCommand cmd = db.createCommand();
        cmd.select(table.getColumns());
        cmd.where(table.id.is(task.getId()));

        getBeanProperties(cmd, 1);
    }

    @Override
    public void delete(BigDecimal jobId) {
        if (jobId == null) {
            throw new IllegalArgumentException("Unsupported missing jobId!");
        }

        DBCommand cmd = db.createCommand();
        cmd.where(db.tableWorkflowTask.jobId.is(jobId));

        DBContext context = getContext();
        context.executeDelete(db.tableWorkflowTask, cmd);
    }

    @Override
    public Task find(BigDecimal id) {
        DBCommand cmd = db.createCommand();
        cmd.select(table.getColumns());
        cmd.where(table.id.is(id));
        return getBeanProperties(cmd, 1);
    }

    @Override
    public List<TaskView> view(TaskFilter filter) {
        DBCommand cmd = db.createCommand();
        cmd.select(table.getColumns());
        cmd.select(db.tableWorkflowJob.label.as("JOB_LABEL"));
        cmd.select(db.tableUser.username);
        cmd.join(table.jobId, db.tableWorkflowJob.id);
        cmd.join(table.ownerId, db.tableUser.id, DBJoinType.LEFT);

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

        DBQueryColumn wmId = pmatQuery.findColumn(pmatMaterialId);
        DBQueryColumn wjId = pmatQuery.findColumn(db.tableWorkflowTask.jobId);
        cmd.join(db.tableWorkflowJob.id, wjId, DBJoinType.LEFT);
        // empire db reverse the joint type so that is why RIGHT instead of LEFT
        cmd.join(db.tableWorkflowPhysicalDoc.materialId, wmId, DBJoinType.RIGHT);

        if (filter.getIds() != null) {
            cmd.where(table.id.in(filter.getIds()));
        } else if (filter.getId() != null) {
            cmd.where(table.id.is(filter.getId()));
        }
        if (filter.getJobId() != null) {
            cmd.where(table.jobId.is(filter.getJobId()));
        }
        if (filter.getJobLabel() != null) {
            String pattern = filter.getJobLabel().trim().replace("%", "\\%");
            if (!pattern.isEmpty()) {
                cmd.where(db.tableWorkflowJob.label.likeUpper('%' + pattern + '%'));
            }
        }
        if (!filter.getPriority().isEmpty()) {
            cmd.where(table.priority.in(filter.getPriority()));
        }
        if (!filter.getProfileName().isEmpty()) {
            cmd.where(table.typeRef.in(filter.getProfileName()));
        }
        if (!filter.getState().isEmpty()) {
            cmd.where(table.state.in(filter.getStateAsString()));
        }
        if (!filter.getUserId().isEmpty()) {
            cmd.where(table.ownerId.in(filter.getUserId()));
        }
        if (filter.getBarcode() != null) {
            cmd.where(db.tableWorkflowPhysicalDoc.barcode.is(filter.getBarcode()));
        }
        if (filter.getOrder() != null) {
            cmd.where(table.order.in(filter.getOrder()));
        }
        if (filter.getNote() != null) {
            cmd.where(table.note.like(filter.getNote()));
        }
        if (filter.getSignature() != null) {
            cmd.where(db.tableWorkflowPhysicalDoc.signature.is(filter.getSignature()));
        }
        EmpireUtils.addWhereDate(cmd, table.created, filter.getCreated());
        EmpireUtils.addWhereDate(cmd, table.timestamp, filter.getModified());
        EmpireUtils.addOrderBy(cmd, filter.getSortBy(), table.order, false);

        DBContext context = getContext();
        DBReader reader = new DBReader(context);
        try {
            reader.open(cmd);
            if (!reader.skipRows(filter.getOffset())) {
                return Collections.emptyList();
            }
            List<TaskView> taskViews = reader.getBeanList(TaskView.class, filter.getMaxCount());
            return taskViews;
        } finally {
            reader.close();
        }
    }

    private Task getBeanProperties(DBCommand cmd, int limit) {
        DBContext context = getContext();
        DBReader reader = new DBReader(context);
        try {
            reader.open(cmd);
            List<Task> tasks = reader.getBeanList(Task.class, limit);
            if (!tasks.isEmpty()) {
                return tasks.getFirst();
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
