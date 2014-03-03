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

import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.Batch.State;
import cz.cas.lib.proarc.common.dao.BatchDao;
import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dao.BatchView;
import cz.cas.lib.proarc.common.dao.ConcurrentModificationException;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.BatchItemTable;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.BatchTable;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.UserTable;
import java.sql.Connection;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;
import org.apache.empire.data.bean.BeanResult;
import org.apache.empire.db.DBColumn;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBReader;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.DBRecordData;
import org.apache.empire.db.DBTable;
import org.apache.empire.db.exceptions.RecordNotFoundException;
import org.apache.empire.db.exceptions.RecordUpdateInvalidException;
import org.apache.empire.db.expr.compare.DBCompareExpr;

/**
 *
 * @author Jan Pokorsky
 */
public class EmpireBatchDao extends EmpireDao implements BatchDao {

    private static final Logger LOG = Logger.getLogger(EmpireBatchDao.class.getName());
    private final BatchTable table;
    private int pageSize = 100;

    public EmpireBatchDao(ProarcDatabase db) {
        super(db);
        table = db.tableBatch;
    }

    @Override
    public Batch create() {
        return new Batch();
    }

    @Override
    public void update(Batch batch) throws ConcurrentModificationException {
        DBRecord record = new DBRecord();
        Connection c = getConnection();
        if (batch.getId() == null) {
            record.create(table);
            Timestamp now = new Timestamp(System.currentTimeMillis());
            batch.setTimestamp(now);
        } else {
            record.read(table, batch.getId(), c);
        }
        record.setBeanValues(batch);
        try {
            record.update(c);
        } catch (RecordUpdateInvalidException ex) {
            throw new ConcurrentModificationException(ex);
        }
        getBeanProperties(record, batch);
    }

    @Override
    public Batch find(int batchId) {
        DBRecord record = new DBRecord();
        try {
            record.read(table, batchId, getConnection());
            return getBeanProperties(record);
        } catch (RecordNotFoundException ex) {
            return null;
        } finally {
            record.close();
        }
    }

    @Override
    public Batch findForPid(String pid) {
        BatchItemTable tableItem = db.tableBatchItem;
        DBCommand cmd = db.createCommand();
        cmd.select(table.getColumns());
        cmd.join(table.id, tableItem.batchId);
        cmd.where(tableItem.pid.is(pid));
        cmd.where(tableItem.type.is(BatchItem.Type.OBJECT));
        cmd.where(tableItem.dsId.is(null));
        DBReader reader = new DBReader();
        try {
            reader.open(cmd, getConnection());
            if (reader.moveNext()) {
                return getBeanProperties(reader);
            } else {
                return null;
            }
        } finally {
            reader.close();
        }
    }

    @Override
    public List<Batch> findLoadingBatches() {
        BeanResult<Batch> result = new BeanResult<Batch>(Batch.class, table);
        result.getCommand().where(table.state.is(State.LOADING));
        result.fetch(getConnection());
        return Collections.unmodifiableList(result);
    }

    private Batch getBeanProperties(DBRecordData record) {
        return getBeanProperties(record, null);
    }

    private Batch getBeanProperties(DBRecordData record, Batch instance) {
        Batch batch = instance != null ? instance : new Batch();
        record.getBeanProperties(batch);
        return batch;
    }

    @Override
    public List<BatchView> view(Integer userId, Integer batchId, State state, int offset) {
        return view(userId, batchId,
                state == null ? null : EnumSet.of(state),
                null, null, offset, pageSize, table.create.getName());
    }

    @Override
    public List<BatchView> view(Integer userId, Integer batchId, Set<State> state,
            Timestamp from, Timestamp to, int offset, int maxCount, String sortBy) {

        UserTable ut = db.tableUser;
        DBCommand cmd = db.createCommand();
        cmd.select(table.id, table.state, table.userId, table.folder, table.title,
                table.create, table.parentPid, table.timestamp, table.log);
        cmd.select(ut.username);
        cmd.join(table.userId, ut.id);
        if (userId != null) {
            cmd.where(ut.id.is(userId));
        }
        if (batchId != null) {
            cmd.where(table.id.is(batchId));
        }
        if (state != null && !state.isEmpty()) {
            cmd.where(table.state.in(state));
        }
        if (from != null) {
            cmd.addWhereConstraints(Collections.<DBCompareExpr>singletonList(table.create.isMoreOrEqual(from)));
        }
        if (to != null) {
            cmd.addWhereConstraints(Collections.<DBCompareExpr>singletonList(table.create.isLessOrEqual(to)));
        }
        DBColumn sortByCol = getSortColumn(table, sortBy);
        boolean descending;
        if (sortByCol != null) {
            descending = isDescendingSort(sortBy);
        } else {
            sortByCol = table.create;
            descending = true;
        }
        cmd.orderBy(sortByCol, descending);
        DBReader reader = new DBReader();
        try {
            reader.open(cmd, getConnection());
            if (!reader.skipRows(offset)) {
                return Collections.emptyList();
            }
            ArrayList<BatchView> viewItems = new ArrayList<BatchView>(maxCount);
            for (Iterator<DBRecordData> it = reader.iterator(maxCount); it.hasNext();) {
                DBRecordData rec = it.next();
                BatchView view = new BatchView();
                rec.getBeanProperties(view);
                viewItems.add(view);
            }
            return viewItems;
        } finally {
            reader.close();
        }
    }

    private static boolean isDescendingSort(String beanPropertyName) {
        return beanPropertyName.charAt(0) == '-';
    }

    private static DBColumn getSortColumn(DBTable table, String beanPropertyName) {
        if (beanPropertyName == null) {
            return null;
        }
        return findColumn(table, beanPropertyName.charAt(0) == '-' ? beanPropertyName.substring(1) : beanPropertyName);
    }

    private static DBColumn findColumn(DBTable table, String beanPropertyName) {
        for (DBColumn col : table.getColumns()) {
            if (beanPropertyName.equals(col.getBeanPropertyName())) {
                return col;
            }
        }
        return null;
    }

}
