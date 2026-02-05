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
import cz.cas.lib.proarc.common.workflow.model.MaterialFilter;
import cz.cas.lib.proarc.common.workflow.model.MaterialType;
import cz.cas.lib.proarc.common.workflow.model.MaterialView;
import cz.cas.lib.proarc.common.workflow.model.PhysicalMaterial;
import cz.cas.lib.proarc.common.workflow.model.Task;
import cz.cas.lib.proarc.common.workflow.profile.Way;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.apache.empire.data.DataType;
import org.apache.empire.db.DBColumn;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBContext;
import org.apache.empire.db.DBJoinType;
import org.apache.empire.db.DBQuery;
import org.apache.empire.db.DBReader;
import org.apache.empire.db.DBRecord;
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
        DBCommand cmd = db.createCommand();
        cmd.select(db.tableWorkflowMaterial.getColumns());
        cmd.where(db.tableWorkflowMaterial.id.is(id));
        Material material = getMaterialBeanProperties(cmd, 1);

        return (T) fetchCustom(material);
    }

    @Override
    public Job findJob(Material m) {
        DBCommand cmd = db.createCommand();
        cmd.select(db.tableWorkflowJob.getColumns());
        cmd.selectDistinct();
        cmd.join(db.tableWorkflowMaterialInTask.taskId, db.tableWorkflowTask.id);
        cmd.join(db.tableWorkflowTask.jobId, db.tableWorkflowJob.id);
        cmd.where(db.tableWorkflowMaterialInTask.materialId.is(m.getId()));

        return getJobBeanProperties(cmd, 1);
    }

    private <T extends Material> T fetchCustom(Material m) {
        return getBeanProperties(m, 1);
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
                    db.tableWorkflowMaterial.id.in(materialIds.findColumn(db.tableWorkflowMaterialInTask.materialId))
            ));
            cmd.select(db.getValueExpr(filter.getJobId(), DataType.DECIMAL).as(db.tableWorkflowTask.jobId));
        } else {
            cmd.select(db.tableWorkflowMaterialInTask.taskId);
            cmd.select(db.tableWorkflowMaterialInTask.way);
            cmd.join(db.tableWorkflowMaterial.id, db.tableWorkflowMaterialInTask.materialId);
        }

        EmpireUtils.addOrderBy(cmd, filter.getSortBy(), db.tableWorkflowMaterial.id, false);

        DBContext context = getContext();
        DBReader reader = new DBReader(context);
        try {
            reader.open(cmd);
            if (!reader.skipRows(filter.getOffset())) {
                return Collections.emptyList();
            }
            List<MaterialView> materials = reader.getBeanList(MaterialView.class, filter.getMaxCount());
            return materials;
        } finally {
            reader.close();
        }
    }

    @Override
    public void update(Material m) {

        DBContext context = getContext();
        DBRecord record = new DBRecord(context, db.tableWorkflowMaterial);

        boolean isNew = m.getId() == null;

        try {
            if (isNew) {
                record.create();
            } else {
                record.read(db.tableWorkflowMaterial.id.is(m.getId()));
            }
            record.setBeanProperties(m);
            record.update();
        } finally {
            record.close();
        }

        DBCommand cmd = db.createCommand();
        cmd.select(db.tableWorkflowMaterial.getColumns());
        cmd.where(db.tableWorkflowMaterial.id.is(m.getId()));

        m = getMaterialBeanProperties(cmd, 1);
        updateCustom(m, isNew);
    }

    void updateCustom(Material m, boolean isNew) {
        DBTable t = getMaterialTable(m.getType());

        DBContext context = getContext();
        DBRecord r = new DBRecord(context, t);
        try {
            if (isNew) {
                r.create();
            } else {
                if (t instanceof ProarcDatabase.WorkflowFolderTable) {
                    r.read(db.tableWorkflowFolder.materialId.is(m.getId()));
                } else if (t instanceof ProarcDatabase.WorkflowDigObjTable) {
                    r.read(db.tableWorkflowDigObj.materialId.is(m.getId()));
                } else if (t instanceof ProarcDatabase.WorkflowPhysicalDocTable) {
                    r.read(db.tableWorkflowPhysicalDoc.materialId.is(m.getId()));
                }
            }
            r.setBeanProperties(m);
            r.update();
        } finally {
            r.close();
        }
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

        DBCommand cmd = db.createCommand();
        cmd.where(db.tableWorkflowMaterial.id.is(materialId));

        DBContext context = getContext();
        context.executeDelete(db.tableWorkflowMaterial, cmd);
    }

    public void deleteMaterialFolder(BigDecimal materialId) {
        if (materialId == null) {
            throw new IllegalArgumentException("Unsupported missing materialId!");
        }

        DBCommand cmd = db.createCommand();
        cmd.where(db.tableWorkflowFolder.materialId.is(materialId));

        DBContext context = getContext();
        context.executeDelete(db.tableWorkflowFolder, cmd);
    }

    public void deleteMaterialDigitalObject(BigDecimal materialId) {
        if (materialId == null) {
            throw new IllegalArgumentException("Unsupported missing materialId!");
        }

        DBCommand cmd = db.createCommand();
        cmd.where(db.tableWorkflowDigObj.materialId.is(materialId));

        DBContext context = getContext();
        context.executeDelete(db.tableWorkflowDigObj, cmd);
    }

    public void deleteMaterialPhysicalDocument(BigDecimal materialId) {
        if (materialId == null) {
            throw new IllegalArgumentException("Unsupported missing materialId!");
        }

        DBCommand cmd = db.createCommand();
        cmd.where(db.tableWorkflowPhysicalDoc.materialId.is(materialId));

        DBContext context = getContext();
        context.executeDelete(db.tableWorkflowPhysicalDoc, cmd);
    }

    public void deleteConnectingTableTaskMaterial(BigDecimal materialId) {
        if (materialId == null) {
            throw new IllegalArgumentException("Unsupported missing materialId!");
        }

        DBCommand cmd = db.createCommand();
        cmd.where(db.tableWorkflowMaterialInTask.materialId.is(materialId));
        DBContext context = getContext();
        context.executeDelete(db.tableWorkflowMaterialInTask, cmd);
    }

    @Override
    public void addTaskReference(Material m, Task t, Way way) {
        DBContext context = getContext();
        DBRecord r = new DBRecord(context, db.tableWorkflowMaterialInTask);
        try {
            r.create();
            r.set(db.tableWorkflowMaterialInTask.materialId, m.getId());
            r.set(db.tableWorkflowMaterialInTask.taskId, t.getId());
            r.set(db.tableWorkflowMaterialInTask.way, way.name());
            r.update();
        } finally {
            r.close();
        }
    }

    DBTable getMaterialTable(MaterialType type) {
        return type == MaterialType.FOLDER ? db.tableWorkflowFolder
                : type == MaterialType.DIGITAL_OBJECT ? db.tableWorkflowDigObj
                : type == MaterialType.PHYSICAL_DOCUMENT ? db.tableWorkflowPhysicalDoc
                : null;
    }

    private Material getMaterialBeanProperties(DBCommand cmd, int limit) {
        DBContext context = getContext();
        DBReader reader = new DBReader(context);
        try {
            reader.open(cmd);
            List<Material> materials = reader.getBeanList(Material.class, limit);
            if (!materials.isEmpty()) {
                return materials.getFirst();
            } else {
                return null;
            }
        } catch (RecordNotFoundException ex) {
            return null;
        } finally {
            reader.close();
        }
    }

    private Job getJobBeanProperties(DBCommand cmd, int limit) {
        DBContext context = getContext();
        DBReader reader = new DBReader(context);
        try {
            reader.open(cmd);
            List<Job> jobs = reader.getBeanList(Job.class, limit);
            if (!jobs.isEmpty()) {
                return jobs.getFirst();
            } else {
                return null;
            }
        } catch (RecordNotFoundException ex) {
            return null;
        } finally {
            reader.close();
        }
    }

    private <T extends Material> T getBeanProperties(Material m, int limit) {
        DBContext context = getContext();
        DBReader reader = new DBReader(context);
        try {
            if (m.getType() == MaterialType.FOLDER) {

                DBCommand cmd = db.createCommand();
                cmd.select(db.tableWorkflowFolder.getColumns());
                cmd.where(db.tableWorkflowFolder.materialId.is(m.getId()));
                reader.open(cmd);

                List<FolderMaterial> materials = reader.getBeanList(FolderMaterial.class, limit);
                if (!materials.isEmpty()) {
                    return (T) materials.getFirst();
                } else {
                    return null;
                }
            } else if (m.getType() == MaterialType.DIGITAL_OBJECT) {

                DBCommand cmd = db.createCommand();
                cmd.select(db.tableWorkflowDigObj.getColumns());
                cmd.where(db.tableWorkflowDigObj.materialId.is(m.getId()));
                reader.open(cmd);

                List<DigitalMaterial> materials = reader.getBeanList(DigitalMaterial.class, limit);
                if (!materials.isEmpty()) {
                    return (T) materials.getFirst();
                } else {
                    return null;
                }
            } else if (m.getType() == MaterialType.PHYSICAL_DOCUMENT) {

                DBCommand cmd = db.createCommand();
                cmd.select(db.tableWorkflowPhysicalDoc.getColumns());
                cmd.where(db.tableWorkflowPhysicalDoc.materialId.is(m.getId()));
                reader.open(cmd);

                List<PhysicalMaterial> materials = reader.getBeanList(PhysicalMaterial.class, limit);
                if (!materials.isEmpty()) {
                    return (T) materials.getFirst();
                } else {
                    return null;
                }
            }
            return null;
        } catch (RecordNotFoundException ex) {
            return null;
        } finally {
            reader.close();
        }
    }

}
