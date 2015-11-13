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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import org.apache.empire.db.DBCommand;
import org.apache.empire.db.DBJoinType;
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
        cmd.select(db.tableUser.username);
        cmd.join(tableJob.ownerId, db.tableUser.id, DBJoinType.LEFT);

        if (filter.getId() != null) {
            cmd.where(tableJob.id.is(filter.getId()));
        }
        if (filter.getLabel() != null) {
            String pattern = filter.getLabel().trim().replace("%", "\\%");
            if (!pattern.isEmpty()) {
                cmd.where(tableJob.label.like('%' + pattern + '%'));
            }
        }
        if (filter.getProfileName() != null) {
            cmd.where(tableJob.profileName.is(filter.getProfileName()));
        }
        if (filter.getState() != null) {
            cmd.where(tableJob.state.is(filter.getState().name()));
        }

        if (filter.getUserId() != null) {
            cmd.where(tableJob.ownerId.is(filter.getUserId()));
        }

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

}
