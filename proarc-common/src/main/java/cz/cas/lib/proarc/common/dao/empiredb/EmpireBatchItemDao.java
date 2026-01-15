/*
 * Copyright (C) 2013 Jan Pokorsky
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

import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dao.BatchItemDao;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.BatchItemTable;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import org.apache.empire.data.bean.BeanResult;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBContext;
import org.apache.empire.db.DBReader;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.exceptions.RecordNotFoundException;

/**
 *
 * @author Jan Pokorsky
 */
public class EmpireBatchItemDao extends EmpireDao implements BatchItemDao {

    private final BatchItemTable table;

    public EmpireBatchItemDao(ProarcDatabase db) {
        super(db);
        this.table = db.tableBatchItem;
    }

    @Override
    public BatchItem create() {
        return new BatchItem();
    }

    @Override
    public void update(BatchItem item) {
        DBContext context = getContext();
        DBRecord record = new DBRecord(context, table);

        try {
            if (item.getId() == null) {
                record.create();
                Timestamp now = new Timestamp(System.currentTimeMillis());
                item.setTimestamp(now);
            } else {
                record.read(table.id.is(item.getId()));
            }

            record.setBeanProperties(item);
            record.update();
        } finally {
            record.close();
        }

        DBCommand cmd = db.createCommand();
        cmd.select(table.getColumns());
        cmd.where(table.id.is(item.getId()));

        getBeanProperties(cmd, 1);
    }

    @Override
    public BatchItem find(int id) {

        DBCommand cmd = db.createCommand();
        cmd.select(table.getColumns());
        cmd.where(table.id.is(id));

        return getBeanProperties(cmd, 1);
    }

    @Override
    public List<BatchItem> find(int batchId, String pid, String dsId, String file, String state, String type) {
        BeanResult<BatchItem> result = new BeanResult<BatchItem>(BatchItem.class, table);
        DBCommand cmd = result.getCommand();
        cmd.where(table.batchId.is(batchId));
        if (pid != null) {
            cmd.where(table.pid.is(pid));
        }

        if (dsId != null) {
            cmd.where(table.dsId.is(dsId));
        }
        if (file != null) {
            cmd.where(table.file.is(file));
        }
        if (state != null) {
            cmd.where(table.state.is(state));
        }
        if (type != null) {
            cmd.where(table.type.is(type));
        }
        cmd.orderBy(table.timestamp);
        result.fetch(getContext());
        return Collections.unmodifiableList(result);
    }

    @Override
    public void removeItems(int batchId) {
        DBCommand cmd = db.createCommand();
        cmd.where(table.batchId.is(batchId));

        DBContext context = getContext();
        context.executeDelete(table, cmd);
    }

    @Override
    public void removeItem(int batchId, String pid) {
        DBCommand cmd = db.createCommand();
        cmd.where(table.batchId.is(batchId));
        cmd.where(table.pid.is(pid));

        DBContext context = getContext();
        context.executeDelete(table, cmd);
    }

    private BatchItem getBeanProperties(DBCommand cmd, int limit) {
        DBContext context = getContext();
        DBReader reader = new DBReader(context);
        try {
            reader.open(cmd);
            List<BatchItem> batchitems = reader.getBeanList(BatchItem.class, limit);
            if (!batchitems.isEmpty()) {
                return batchitems.getFirst();
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
