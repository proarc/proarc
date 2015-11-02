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
import cz.cas.lib.proarc.common.workflow.model.Material;
import cz.cas.lib.proarc.common.workflow.model.Material.Type;
import cz.cas.lib.proarc.common.workflow.model.Task;
import java.sql.Connection;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.DBTable;

/**
 *
 * @author Jan Pokorsky
 */
public class EmpireWorkflowMaterialDao extends EmpireDao implements WorkflowMaterialDao {

    public EmpireWorkflowMaterialDao(ProarcDatabase db) {
        super(db);
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
        DBTable t = m.getType() == Type.FOLDER ? db.tableWorkflowFolder
                : m.getType() == Type.DIGITAL_OBJECT ? db.tableWorkflowDigObj
                : m.getType() == Type.PHYSICAL_DOCUMENT ? db.tableWorkflowPhysicalDoc
                : null;
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
    public void addTaskReference(Material m, Task t, String way) {
        DBRecord r = new DBRecord();
        r.create(db.tableWorkflowMaterialInTask);
        r.setValue(db.tableWorkflowMaterialInTask.materialId, m.getId());
        r.setValue(db.tableWorkflowMaterialInTask.taskId, t.getId());
        r.setValue(db.tableWorkflowMaterialInTask.way, way);
        r.update(getConnection());
    }

}
