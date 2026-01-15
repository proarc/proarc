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
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.BatchItemTable;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.BatchTable;
import cz.cas.lib.proarc.common.dao.empiredb.ProarcDatabase.UserTable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;
import org.apache.empire.data.bean.BeanResult;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBContext;
import org.apache.empire.db.DBReader;
import org.apache.empire.db.DBRecord;
import org.apache.empire.db.exceptions.RecordNotFoundException;
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
    public void update(Batch batch) {
        DBContext context = getContext();
        DBRecord record = new DBRecord(context, table);

        try {
            if (batch.getId() == null) {
                record.create();
                Timestamp now = new Timestamp(System.currentTimeMillis());
                batch.setTimestamp(now);
            } else {
                record.read(table.id.is(batch.getId()));
            }
            record.setBeanProperties(batch);

            record.update();
        } finally {
            record.close();
        }

        DBCommand cmd = db.createCommand();
        cmd.select(table.getColumns());
        cmd.where(table.id.is(batch.getId()));

        getBeanProperties(cmd, 1);
    }

    @Override
    public Batch find(int batchId) {
        DBCommand cmd = db.createCommand();
        cmd.select(table.getColumns());
        cmd.where(table.id.is(batchId));
        return getBeanProperties(cmd, 1);
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

        return getBeanProperties(cmd, 1);
    }

    @Override
    public List<Batch> findLoadingBatches() {
        BeanResult<Batch> result = new BeanResult<Batch>(Batch.class, table);
        result.getCommand().where(table.state.is(State.LOADING));
        result.fetch(getContext());
        return Collections.unmodifiableList(result);
    }

    @Override
    public List<Batch> findWaitingImportBatches() {
        BeanResult<Batch> result = new BeanResult<Batch>(Batch.class, table);
        result.getCommand().where(table.state.is(State.IMPORT_PLANNED));
        result.fetch(getContext());
        return Collections.unmodifiableList(result);
    }

    @Override
    public List<Batch> findExportingBatches() {
        BeanResult<Batch> result = new BeanResult<Batch>(Batch.class, table);
        result.getCommand().where(table.state.is(State.EXPORTING));
        result.fetch(getContext());
        return Collections.unmodifiableList(result);
    }

    @Override
    public List<Batch> findWaitingExportBatches() {
        BeanResult<Batch> result = new BeanResult<Batch>(Batch.class, table);
        result.getCommand().where(table.state.is(State.EXPORT_PLANNED));
        result.fetch(getContext());
        return Collections.unmodifiableList(result);
    }

    @Override
    public List<Batch> findUploadingBatches() {
        BeanResult<Batch> result = new BeanResult<Batch>(Batch.class, table);
        result.getCommand().where(table.state.is(State.UPLOADING));
        result.fetch(getContext());
        return Collections.unmodifiableList(result);
    }

    @Override
    public List<Batch> findWaitingInternalBatches() {
        BeanResult<Batch> result = new BeanResult<Batch>(Batch.class, table);
        result.getCommand().where(table.state.is(State.INTERNAL_PLANNED));
        result.fetch(getContext());
        return Collections.unmodifiableList(result);
    }

    @Override
    public List<Batch> findIntenalRunningBatches() {
        BeanResult<Batch> result = new BeanResult<Batch>(Batch.class, table);
        result.getCommand().where(table.state.is(State.INTERNAL_RUNNING));
        result.fetch(getContext());
        return Collections.unmodifiableList(result);
    }

    @Override
    public List<Batch> findBatch(String pid, String processProfile, State state) {
        BeanResult<Batch> result = new BeanResult<Batch>(Batch.class, table);
        result.getCommand().where(table.state.is(state));
        result.getCommand().where(table.profileId.is(processProfile));
        result.getCommand().where(table.title.is(pid));
        result.getCommand().orderBy(table.create, true);
        result.fetch(getContext());
        return Collections.unmodifiableList(result);
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

        return view(new BatchViewFilter().setUserId(userId).setBatchIds(Collections.singletonList(batchId))
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
                table.create, table.parentPid, table.timestamp, table.log, table.profileId, table.estimateItemNumber, table.priority, table.updated, table.itemUpdated, table.nightOnly);
        cmd.select(ut.username);
        cmd.join(table.userId, ut.id);
        if (filter.getCreatorId() != null) {
            cmd.where(ut.id.is(filter.getCreatorId()));
        }
        if (filter.getBatchIds() != null && !filter.getBatchIds().isEmpty()) {
            List<Integer> batchIds = new ArrayList<>();
            for (Integer batchId : filter.getBatchIds()) {
                if (batchId != null && batchId > 0) {
                    batchIds.add(batchId);
                }
            }
            if (!batchIds.isEmpty()) {
                cmd.where(table.id.in(batchIds));
            }
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
        if (filter.getUpdatedFrom() != null) {
            cmd.addWhereConstraints(Collections.<DBCompareExpr>singletonList(
                    table.updated.isMoreOrEqual(filter.getUpdatedFrom())));
        }
        if (filter.getUpdatedTo() != null) {
            cmd.addWhereConstraints(Collections.<DBCompareExpr>singletonList(
                    table.updated.isLessOrEqual(filter.getUpdatedTo())));
        }
        if (filter.getItemUpdatedFrom() != null) {
            cmd.addWhereConstraints(Collections.<DBCompareExpr>singletonList(
                    table.itemUpdated.isMoreOrEqual(filter.getItemUpdatedFrom())));
        }
        if (filter.getItemUpdatedTo() != null) {
            cmd.addWhereConstraints(Collections.<DBCompareExpr>singletonList(
                    table.itemUpdated.isLessOrEqual(filter.getItemUpdatedTo())));
        }
        if (filter.getModifiedFrom() != null) {
            cmd.addWhereConstraints(Collections.<DBCompareExpr>singletonList(
                    table.timestamp.isMoreOrEqual(filter.getModifiedFrom())));
        }
        if (filter.getModifiedTo() != null) {
            cmd.addWhereConstraints(Collections.<DBCompareExpr>singletonList(
                    table.timestamp.isLessOrEqual(filter.getModifiedTo())));
        }
        if (filter.getPriority() != null) {
            if (Batch.PRIORITY_MEDIUM.equals(filter.getPriority())) {
                cmd.where(table.priority.is(filter.getPriority()).or(table.priority.is(null)));
            } else {
                cmd.where(table.priority.is(filter.getPriority()));
            }
        }
        String filePattern = filter.getFilePattern();
        if (filePattern != null) {
            BatchItemTable bitems = db.tableBatchItem;
            cmd.selectDistinct();
            if (filePattern.startsWith("uuid:")) {
                cmd.where(table.title.likeUpper('%' + filePattern.toUpperCase() + '%'));
            } else {
                cmd.join(table.id, bitems.batchId);
                cmd.where(table.title.likeUpper('%' + filePattern.toUpperCase() + '%')
                        .or(bitems.type.is(BatchItem.Type.FILE).and(bitems.file.likeUpper('%' + filePattern.toUpperCase() + '%')))
                );
            }
        }
        String profileId = filter.getProfile();
        if (profileId != null) {
            cmd.where(table.profileId.in(profileId));
        }
        EmpireUtils.addOrderBy(cmd, filter.getSortBy(), table.create, true);

        DBContext context = getContext();
        DBReader reader = new DBReader(context);
        try {
            reader.open(cmd);
            if (!reader.skipRows(filter.getOffset())) {
                return Collections.emptyList();
            }
            List<BatchView> viewItems = reader.getBeanList(BatchView.class, filter.getMaxCount());
            for (BatchView view : viewItems) {
                if (view.getProfileId() == null) {
                    view.setProfileId(ConfigurationProfile.DEFAULT);
                }
            }
            return viewItems;
        } finally {
            reader.close();
        }
    }

    @Override
    public void removeBatch(int batchId) {
        DBCommand cmd = db.createCommand();
        cmd.where(table.id.is(batchId));

        DBContext context = getContext();
        context.executeDelete(table, cmd);
    }

    private Batch getBeanProperties(DBCommand cmd, int limit) {
        DBContext context = getContext();
        DBReader reader = new DBReader(context);
        try {
            reader.open(cmd);
            List<Batch> batches = reader.getBeanList(Batch.class, limit);
            if (!batches.isEmpty()) {
                return batches.getFirst();
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
