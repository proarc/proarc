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

import cz.cas.lib.proarc.common.dao.WorkflowMaterialDao;
import cz.cas.lib.proarc.common.workflow.model.DigitalMaterial;
import cz.cas.lib.proarc.common.workflow.model.FolderMaterial;
import cz.cas.lib.proarc.common.workflow.model.Job;
import cz.cas.lib.proarc.common.workflow.model.Material;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.MaterialFilter;
import cz.cas.lib.proarc.common.workflow.model.MaterialView;
import cz.cas.lib.proarc.common.workflow.model.PhysicalMaterial;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.profile.Way;
import java.math.BigDecimal;
import java.sql.Connection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import org.apache.empire.data.DataType;
import org.apache.empire.db.DBColumn;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBJoinType;
import org.apache.empire.db.DBQuery;
import org.apache.empire.db.DBReader;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.DBRecordData;
import org.apache.empire.db.DBTable;
import org.apache.empire.db.exceptions.RecordNotFoundException;
import org.apache.empire.db.expr.compare.DBCompareExpr;

/**
 *
 * @author Jan Pokorsky
 */
public class EmpireWorkflowMaterialDao extends EmpireDao implements WorkflowMaterialDao {

    public EmpireWorkflowMaterialDao(ProarcDatabase db) {
        super(db);
    }

    @Override
    public <T extends Material> T create(MaterialType type) {
        Material m = type == MaterialType.FOLDER ? new FolderMaterial()
                : type == MaterialType.DIGITAL_OBJECT ? new DigitalMaterial()
                : type == MaterialType.PHYSICAL_DOCUMENT ? new PhysicalMaterial()
                : null;
        return (T) m;
    }

    @Override
    public <T extends Material> T find(BigDecimal id) {
        DBRecord record = new DBRecord();
        try {
            record.read(db.tableWorkflowMaterial, id, getConnection());
            MaterialType type = MaterialType.valueOf(record.getString(db.tableWorkflowMaterial.type));
            Material m = create(type);
            record.getBeanProperties(m);
            return (T) fetchCustom(m);
        } catch (RecordNotFoundException ex) {
            return null;
        } finally {
            record.close();
        }
    }

    @Override
    public Job findJob(Material m) {
        DBCommand cmd = db.createCommand();
        cmd.select(db.tableWorkflowJob.getColumns());
        cmd.selectDistinct();
        cmd.join(db.tableWorkflowMaterialInTask.taskId, db.tableWorkflowTask.id);
        cmd.join(db.tableWorkflowTask.jobId, db.tableWorkflowJob.id);
        cmd.where(db.tableWorkflowMaterialInTask.materialId.is(m.getId()));
        DBReader reader = new DBReader();
        try {
            reader.open(cmd, getConnection());
            for (Iterator<DBRecordData> it = reader.iterator(1); it.hasNext();) {
                DBRecordData rec = it.next();
                Job job = new Job();
                rec.getBeanProperties(job);
                return job;
            }
            return null;
        } finally {
            reader.close();
        }
    }

    private <T extends Material> T fetchCustom(Material m) {
        DBTable t = getMaterialTable(m.getType());
        DBRecord record = new DBRecord();
        try {
            record.read(t, m.getId(), getConnection());
            record.getBeanProperties(m);
            return (T) m;
        } catch (RecordNotFoundException ex) {
            return null;
        } finally {
            record.close();
        }
    }

    @Override
    public List<MaterialView> view(MaterialFilter filter) {
        DBCommand cmd = db.createCommand();
        cmd.select(db.tableWorkflowMaterial.getColumns());
        cmd.select(db.tableWorkflowFolder.path);
        cmd.select(db.tableWorkflowDigObj.pid);
        List<DBColumn> physicalSelections = new ArrayList<DBColumn>(db.tableWorkflowPhysicalDoc.getColumns());
        physicalSelections.remove(db.tableWorkflowPhysicalDoc.materialId);
        cmd.select(physicalSelections);

        cmd.join(db.tableWorkflowMaterial.id, db.tableWorkflowFolder.materialId, DBJoinType.LEFT);
        cmd.join(db.tableWorkflowMaterial.id, db.tableWorkflowDigObj.materialId, DBJoinType.LEFT);
        cmd.join(db.tableWorkflowMaterial.id, db.tableWorkflowPhysicalDoc.materialId, DBJoinType.LEFT);

        if (filter.getId() != null) {
            cmd.where(db.tableWorkflowMaterial.id.is(filter.getId()));
        }
        if (filter.getPid() != null) {
            cmd.where(db.tableWorkflowDigObj.pid.is(filter.getPid()));
        }
        if (filter.getType() != null) {
            cmd.addWhereConstraints(Collections.singletonList(db.tableWorkflowMaterial.type.is(filter.getType())));
        }
        if (filter.getTaskId() != null) {
            cmd.select(db.tableWorkflowMaterialInTask.taskId);
            cmd.select(db.tableWorkflowMaterialInTask.way);
            cmd.join(db.tableWorkflowMaterial.id, db.tableWorkflowMaterialInTask.materialId);
            cmd.addWhereConstraints(Collections.<DBCompareExpr>singletonList(
                    db.tableWorkflowMaterialInTask.taskId.is(filter.getTaskId())
            ));
        } else if (filter.getJobId() != null) {
            DBCommand subcmd = db.createCommand();
            subcmd.select(db.tableWorkflowMaterialInTask.materialId);
            subcmd.selectDistinct();
            subcmd.join(db.tableWorkflowMaterialInTask.taskId, db.tableWorkflowTask.id);
            subcmd.where(db.tableWorkflowTask.jobId.is(filter.getJobId()));
            DBQuery materialIds = new DBQuery(subcmd);

            cmd.addWhereConstraints(Collections.<DBCompareExpr>singletonList(
                    db.tableWorkflowMaterial.id.in(materialIds.findQueryColumn(db.tableWorkflowMaterialInTask.materialId))
            ));
            cmd.select(db.getValueExpr(filter.getJobId(), DataType.DECIMAL).as(db.tableWorkflowTask.jobId));
        } else {
            cmd.select(db.tableWorkflowMaterialInTask.taskId);
            cmd.select(db.tableWorkflowMaterialInTask.way);
            cmd.join(db.tableWorkflowMaterial.id, db.tableWorkflowMaterialInTask.materialId);
        }

        EmpireUtils.addOrderBy(cmd, filter.getSortBy(), db.tableWorkflowMaterial.id, false);

        DBReader reader = new DBReader();
        try {
            reader.open(cmd, getConnection());
            if (!reader.skipRows(filter.getOffset())) {
                return Collections.emptyList();
            }
            ArrayList<MaterialView> viewItems = new ArrayList<MaterialView>(filter.getMaxCount());
            for (Iterator<DBRecordData> it = reader.iterator(filter.getMaxCount()); it.hasNext();) {
                DBRecordData rec = it.next();
                MaterialView view = new MaterialView();
                rec.getBeanProperties(view);
                viewItems.add(view);
            }
            return viewItems;
        } finally {
            reader.close();
        }
    }

    @Override
    public void update(Material m) {
        Connection c = getConnection();
        DBRecord r = new DBRecord();
        boolean isNew = m.getId() == null;
        if (isNew) {
            r.create(db.tableWorkflowMaterial);
        } else {
            r.read(db.tableWorkflowMaterial, m.getId(), c);
        }
        r.setBeanValues(m);
        r.update(c);
        r.getBeanProperties(m);
        updateCustom(m, isNew, c);
    }

    void updateCustom(Material m, boolean isNew, Connection c) {
        DBTable t = getMaterialTable(m.getType());
        DBRecord r = new DBRecord();
        if (isNew) {
            r.create(t);
        } else {
            r.read(t, m.getId(), c);
        }
        r.setBeanValues(m);
        r.update(c);
    }

    @Override
    public void delete(BigDecimal materialId) {
        if (materialId == null) {
            throw new IllegalArgumentException("Unsupported missing materialId!");
        }
        deleteMaterialFolder(materialId);
        deleteMaterialDigitalObject(materialId);
        deleteMaterialPhysicalDocument(materialId);
        deleteConnectingTableTaskMaterial(materialId);

        Connection c = getConnection();
        DBCommand cmd = db.createCommand();
        cmd.where(db.tableWorkflowMaterial.id.is(materialId));
        db.executeDelete(db.tableWorkflowMaterial, cmd, c);
    }

    public void deleteMaterialFolder(BigDecimal materialId) {
        if (materialId == null) {
            throw new IllegalArgumentException("Unsupported missing materialId!");
        }
        Connection c = getConnection();
        DBCommand cmd = db.createCommand();
        cmd.where(db.tableWorkflowFolder.materialId.is(materialId));
        db.executeDelete(db.tableWorkflowFolder, cmd, c);
    }

    public void deleteMaterialDigitalObject(BigDecimal materialId) {
        if (materialId == null) {
            throw new IllegalArgumentException("Unsupported missing materialId!");
        }
        Connection c = getConnection();
        DBCommand cmd = db.createCommand();
        cmd.where(db.tableWorkflowDigObj.materialId.is(materialId));
        db.executeDelete(db.tableWorkflowDigObj, cmd, c);
    }

    public void deleteMaterialPhysicalDocument(BigDecimal materialId) {
        if (materialId == null) {
            throw new IllegalArgumentException("Unsupported missing materialId!");
        }
        Connection c = getConnection();
        DBCommand cmd = db.createCommand();
        cmd.where(db.tableWorkflowPhysicalDoc.materialId.is(materialId));
        db.executeDelete(db.tableWorkflowPhysicalDoc, cmd, c);
    }

    public void deleteConnectingTableTaskMaterial(BigDecimal materialId) {
        if (materialId == null) {
            throw new IllegalArgumentException("Unsupported missing materialId!");
        }
        Connection c = getConnection();
        DBCommand cmd = db.createCommand();
        cmd.where(db.tableWorkflowMaterialInTask.materialId.is(materialId));
        db.executeDelete(db.tableWorkflowMaterialInTask, cmd, c);
    }

    @Override
    public void addTaskReference(Material m, Task t, Way way) {
        DBRecord r = new DBRecord();
        r.create(db.tableWorkflowMaterialInTask);
        r.setValue(db.tableWorkflowMaterialInTask.materialId, m.getId());
        r.setValue(db.tableWorkflowMaterialInTask.taskId, t.getId());
        r.setValue(db.tableWorkflowMaterialInTask.way, way.name());
        r.update(getConnection());
    }

    DBTable getMaterialTable(MaterialType type) {
        return type == MaterialType.FOLDER ? db.tableWorkflowFolder
                : type == MaterialType.DIGITAL_OBJECT ? db.tableWorkflowDigObj
                : type == MaterialType.PHYSICAL_DOCUMENT ? db.tableWorkflowPhysicalDoc
                : null;
    }

}
