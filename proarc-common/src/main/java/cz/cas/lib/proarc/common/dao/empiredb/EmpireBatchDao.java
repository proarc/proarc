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

import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.Batch.State;
import cz.cas.lib.proarc.common.dao.BatchDao;
import cz.cas.lib.proarc.common.dao.BatchItem;
import cz.cas.lib.proarc.common.dao.BatchView;
import cz.cas.lib.proarc.common.dao.BatchViewFilter;
import cz.cas.lib.proarc.common.dao.ConcurrentModificationException;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.BatchItemTable;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.BatchTable;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.UserTable;
import org.apache.empire.data.bean.BeanResult;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBReader;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.DBRecordData;
import org.apache.empire.db.exceptions.RecordNotFoundException;
import org.apache.empire.db.exceptions.RecordUpdateInvalidException;
import org.apache.empire.db.expr.compare.DBCompareExpr;
import java.sql.Connection;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

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

        return view(new BatchViewFilter().setUserId(userId).setBatchId(batchId)
                .setState(state).setCreatedFrom(from).setCreatedTo(to)
                .setOffset(offset).setMaxCount(maxCount).setSortBy(sortBy));
    }

    @Override
    public List<BatchView> view(BatchViewFilter filter) {
        if (filter == null) {
            throw new NullPointerException();
        }

        UserTable ut = db.tableUser;
        DBCommand cmd = db.createCommand();
        cmd.select(table.id, table.state, table.userId, table.folder, table.title,
                table.create, table.parentPid, table.timestamp, table.log, table.profileId);
        cmd.select(ut.username);
        cmd.join(table.userId, ut.id);
        if (filter.getUserId() != null) {
            cmd.where(ut.id.in(new Integer[]{1, filter.getUserId()}));
            //cmd.where(ut.id.is(filter.getUserId()));
        }
        if (filter.getBatchId() != null) {
            cmd.where(table.id.is(filter.getBatchId()));
        }
        if (filter.getState() != null && !filter.getState().isEmpty()) {
            cmd.where(table.state.in(filter.getState()));
        }
        if (filter.getCreatedFrom() != null) {
            cmd.addWhereConstraints(Collections.<DBCompareExpr>singletonList(
                    table.create.isMoreOrEqual(filter.getCreatedFrom())));
        }
        if (filter.getCreatedTo() != null) {
            cmd.addWhereConstraints(Collections.<DBCompareExpr>singletonList(
                    table.create.isLessOrEqual(filter.getCreatedTo())));
        }
        if (filter.getModifiedFrom() != null) {
            cmd.addWhereConstraints(Collections.<DBCompareExpr>singletonList(
                    table.timestamp.isMoreOrEqual(filter.getModifiedFrom())));
        }
        if (filter.getModifiedTo() != null) {
            cmd.addWhereConstraints(Collections.<DBCompareExpr>singletonList(
                    table.timestamp.isLessOrEqual(filter.getModifiedTo())));
        }
        String filePattern = filter.getFilePattern();
        if (filePattern != null) {
            BatchItemTable bitems = db.tableBatchItem;
            cmd.selectDistinct();
            cmd.join(table.id, bitems.batchId);
            cmd.where(table.folder.like('%' + filePattern + '%')
                    .or(bitems.type.is(BatchItem.Type.FILE).and(bitems.file.like('%' + filePattern + '%')))
            );
        }
        String profileId = filter.getProfile();
        if (profileId != null) {
            cmd.where(table.profileId.in(profileId));
        }
        EmpireUtils.addOrderBy(cmd, filter.getSortBy(), table.create, true);
        DBReader reader = new DBReader();
        try {
            reader.open(cmd, getConnection());
            if (!reader.skipRows(filter.getOffset())) {
                return Collections.emptyList();
            }
            ArrayList<BatchView> viewItems = new ArrayList<BatchView>(filter.getMaxCount());
            for (Iterator<DBRecordData> it = reader.iterator(filter.getMaxCount()); it.hasNext();) {
                DBRecordData rec = it.next();
                BatchView view = new BatchView();
                rec.getBeanProperties(view);
                if (view.getProfileId() == null) {
                    view.setProfileId(ConfigurationProfile.DEFAULT);
                }
                viewItems.add(view);
            }
            return viewItems;
        } finally {
            reader.close();
        }
    }

}
