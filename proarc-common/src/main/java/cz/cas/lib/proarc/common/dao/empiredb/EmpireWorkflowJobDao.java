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
import cz.cas.lib.proarc.common.dao.WorkflowJobDao;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.WorkflowJobTable;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.JobFilter;
import cz.cas.lib.proarc.common.workflow.model.JobView;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
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
public class EmpireWorkflowJobDao extends EmpireDao implements WorkflowJobDao {

    private final WorkflowJobTable tableJob;

    public EmpireWorkflowJobDao(ProarcDatabase db) {
        super(db);
        tableJob = db.tableWorkflowJob;
    }

    @Override
    public Job create() {
        return new Job();
    }

    @Override
    public void update(Job job) throws ConcurrentModificationException {
        Connection c = getConnection();
        DBRecord r = new DBRecord();
        if (job.getId() == null) {
            r.create(tableJob);
        } else {
            r.read(tableJob, job.getId(), c);
        }
        r.setBeanValues(job);
        try {
            // this column is "always changed", because we need update timestamp of job (optimistic transactions on materials)
            r.setModified(tableJob.label, true);
            r.update(c);
        } catch (RecordUpdateInvalidException ex) {
            throw new ConcurrentModificationException(ex);
        }
        r.getBeanProperties(job);
    }

    @Override
    public Job find(BigDecimal id) {
        DBRecord record = new DBRecord();
        try {
            record.read(tableJob, id, getConnection());
            Job job = new Job();
            record.getBeanProperties(job);
            return job;
        } catch (RecordNotFoundException ex) {
            return null;
        } finally {
            record.close();
        }
    }

    @Override
    public List<JobView> view(JobFilter filter) {
        DBCommand cmd = db.createCommand();
        cmd.select(tableJob.getColumns());
        //cmd.select(db.tableUser.username);
        final ProarcDatabase.WorkflowPhysicalDocTable tpd = db.tableWorkflowPhysicalDoc;
        cmd.select(tpd.barcode, tpd.detail, tpd.field001, tpd.issue, tpd.sigla, tpd.signature, tpd.volume, tpd.year, tpd.edition);
        //cmd.join(tableJob.ownerId, db.tableUser.id, DBJoinType.LEFT);

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
        cmd.join(tableJob.id, wjId, DBJoinType.LEFT);
        // empire db reverse the joint type so that is why RIGHT instead of LEFT
        cmd.join(tpd.materialId, wmId, DBJoinType.RIGHT);

        final ProarcDatabase.WorkflowDigObjTable tdo = db.tableWorkflowDigObj;
        cmd.select(tdo.pid);

        DBCommand digObjCmd = db.createCommand();
        final DBColumnExpr digObjMaterialId = db.tableWorkflowMaterial.id.min().as(db.tableWorkflowMaterial.id);
        digObjCmd.select(digObjMaterialId);
        digObjCmd.select(db.tableWorkflowTask.jobId);
        digObjCmd.join(db.tableWorkflowMaterial.id, db.tableWorkflowMaterialInTask.materialId);
        digObjCmd.join(db.tableWorkflowTask.id, db.tableWorkflowMaterialInTask.taskId);
        digObjCmd.where(db.tableWorkflowMaterial.type.is("DIGITAL_OBJECT"));
        digObjCmd.groupBy(db.tableWorkflowTask.jobId);
        DBQuery digObjQuery = new DBQuery(digObjCmd);

        DBQuery.DBQueryColumn doId = digObjQuery.findQueryColumn(digObjMaterialId);
        DBQuery.DBQueryColumn djId = digObjQuery.findQueryColumn(db.tableWorkflowTask.jobId);
        cmd.join(tableJob.id, djId, DBJoinType.LEFT);
        cmd.join(tdo.materialId, doId, DBJoinType.RIGHT);

        final ProarcDatabase.WorkflowFolderTable twfR = db.tableWorkflowFolder;
        final DBColumnExpr rawPath = twfR.path.as("raw_Path");
        cmd.select(rawPath);

        DBCommand rawCmd = db.createCommand();
        final DBColumnExpr rawMaterialId = db.tableWorkflowMaterial.id.min().as(db.tableWorkflowMaterial.id);
        rawCmd.select(rawMaterialId);
        rawCmd.select(db.tableWorkflowTask.jobId);
        rawCmd.join(db.tableWorkflowMaterial.id, db.tableWorkflowMaterialInTask.materialId);
        rawCmd.join(db.tableWorkflowTask.id, db.tableWorkflowMaterialInTask.taskId);
        rawCmd.where(db.tableWorkflowMaterial.name.is("material.folder.rawScan"));
        rawCmd.groupBy(db.tableWorkflowTask.jobId);
        DBQuery rawQuery = new DBQuery(rawCmd);

        DBQuery.DBQueryColumn rawId = rawQuery.findQueryColumn(rawMaterialId);
        DBQuery.DBQueryColumn rawjId = rawQuery.findQueryColumn(db.tableWorkflowTask.jobId);
        cmd.join(tableJob.id, rawjId, DBJoinType.LEFT);
        cmd.join(twfR.materialId, rawId, DBJoinType.RIGHT);


        final ProarcDatabase.WorkflowTaskTable twTt = db.tableWorkflowTask;
        final DBColumnExpr taskName = twTt.typeRef.as("task_Name");
        final DBColumnExpr taskDate = twTt.timestamp.as("task_Date");
        final DBColumnExpr taskUserId = twTt.ownerId.as("task_User");
        cmd.select(taskName, taskDate, taskUserId);
        DBCommand taskCmd = db.createCommand();
        final DBColumnExpr taskExpression = db.tableWorkflowTask.id.max().as(db.tableWorkflowTask.id);
        taskCmd.select(taskExpression);
        taskCmd.select(db.tableWorkflowTask.jobId);
//        taskCmd.join(db.tableWorkflowTask.jobId, db.tableWorkflowJob.id);
        taskCmd.where(db.tableWorkflowTask.state.is("FINISHED"));
        taskCmd.groupBy(db.tableWorkflowTask.jobId);
        DBQuery taskQuery = new DBQuery(taskCmd);
        DBQuery.DBQueryColumn taskJobId = taskQuery.findQueryColumn(db.tableWorkflowTask.jobId);
        DBQuery.DBQueryColumn taskId = taskQuery.findQueryColumn(taskExpression);
        cmd.join(tableJob.id, taskJobId, DBJoinType.LEFT);
        cmd.join(twTt.id, taskId, DBJoinType.RIGHT);


        final ProarcDatabase.UserTable tUsers = db.tableUser;
        final DBColumnExpr taskUserName = tUsers.username.as("task_Username");
        cmd.select(taskUserName);
        cmd.join(taskUserId, db.tableUser.id, DBJoinType.LEFT);

        if (filter.getIds() != null) {
            EmpireUtils.addWhereIsIn(cmd, tableJob.id, filter.getIds());
        } else {
            EmpireUtils.addWhereIs(cmd, tableJob.id, filter.getId());
        }
        EmpireUtils.addWhereLikeIgnoreCase(cmd, tableJob.label, filter.getLabel());
        EmpireUtils.addWhereLike(cmd, tableJob.financed, filter.getFinanced());
        EmpireUtils.addWhereLike(cmd, tpd.barcode, filter.getMaterialBarcode());
        EmpireUtils.addWhereLike(cmd, tpd.detail, filter.getMaterialDetail());
        EmpireUtils.addWhereLike(cmd, tpd.field001, filter.getMaterialField001());
        EmpireUtils.addWhereLike(cmd, tpd.issue, filter.getMaterialIssue());
        EmpireUtils.addWhereLike(cmd, tpd.sigla, filter.getMaterialSigla());
        EmpireUtils.addWhereLikeIgnoreCase(cmd, tpd.signature, filter.getMaterialSignature());
        EmpireUtils.addWhereLike(cmd, tpd.volume, filter.getMaterialVolume());
        EmpireUtils.addWhereLike(cmd, tpd.year, filter.getMaterialYear());
        EmpireUtils.addWhereLike(cmd, tpd.edition, filter.getMaterialEdition());
        EmpireUtils.addWhereIs(cmd, tableJob.parentId, filter.getParentId());
        EmpireUtils.addWhereIs(cmd, tableJob.profileName, filter.getProfileName());
        EmpireUtils.addWhereIs(cmd, tableJob.state, filter.getState() == null ? null : filter.getState().name());
        EmpireUtils.addWhereIs(cmd, tableJob.ownerId, filter.getUserId());
        EmpireUtils.addWhereIs(cmd, tableJob.priority, filter.getPriority());
        EmpireUtils.addWhereLike(cmd, taskName, filter.getTaskName());
        EmpireUtils.addWhereDate(cmd, db.tableWorkflowJob.timestamp, filter.getTaskDate());
        EmpireUtils.addWhereIs(cmd, taskUserId, filter.getTaskUser());
        EmpireUtils.addWhereLike(cmd, rawPath, filter.getRawPath());
        EmpireUtils.addWhereLike(cmd, tdo.pid, filter.getPid());

        EmpireUtils.addWhereDate(cmd, tableJob.created, filter.getCreated());
        EmpireUtils.addWhereDate(cmd, tableJob.timestamp, filter.getModified());

        EmpireUtils.addOrderBy(cmd, filter.getSortBy(), tableJob.timestamp, true);

        DBReader reader = new DBReader();
        try {
            reader.open(cmd, getConnection());
            if (!reader.skipRows(filter.getOffset())) {
                return Collections.emptyList();
            }
            ArrayList<JobView> viewItems = new ArrayList<JobView>(filter.getMaxCount());
            for (Iterator<DBRecordData> it = reader.iterator(filter.getMaxCount()); it.hasNext();) {
                DBRecordData rec = it.next();
                JobView view = new JobView();
                rec.getBeanProperties(view);
                viewItems.add(view);
            }
            return viewItems;
        } finally {
            reader.close();
        }
    }

    @Override
    public String getDevice(BigDecimal jobId) {
        String device = "";
        try {
            Connection connection = getConnection();
            PreparedStatement SCANNER = connection.prepareStatement("select p1.value_string as device from proarc_wf_job j1 left join proarc_wf_task t1 on j1.id = t1.job_id left join proarc_wf_parameter p1 on t1.id = p1.task_id where t1.type_ref='task.scan' and p1.param_ref='param.scan.scannerNew' and j1.id = " + String.valueOf(jobId));
            final ResultSet resultSet = SCANNER.executeQuery();
            while(resultSet.next()) {
                device = resultSet.getString("device");
            }
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            return device;
        }
    }

    @Override
    public void delete(BigDecimal jobId) {
        if (jobId == null) {
            throw new IllegalArgumentException("Unsupported missing jobId!");
        }
        Connection c = getConnection();
        DBCommand cmd = db.createCommand();
        cmd.where(db.tableWorkflowJob.id.is(jobId));
        db.executeDelete(db.tableWorkflowJob, cmd, c);
    }
}
