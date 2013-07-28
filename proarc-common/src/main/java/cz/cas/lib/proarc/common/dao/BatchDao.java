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
package cz.cas.lib.proarc.common.dao;

import cz.cas.lib.proarc.common.dao.Batch.State;
import java.sql.Timestamp;
import java.util.List;
import java.util.Set;

/**
 * DAO for {@link Batch}.
 *
 * @author Jan Pokorsky
 */
public interface BatchDao extends Dao {

    Batch create();

    /**
     * @return batch or {@code null}
     */
    Batch find(int batchId);

    Batch findForPid(String pid);

    void update(Batch batch) throws ConcurrentModificationException;

    List<BatchView> view(Integer userId, Integer batchId, State state, int offset);

    List<BatchView> view(Integer userId, Integer batchId, Set<State> state,
            Timestamp from, Timestamp to, int offset, int maxCount, String sortBy);

    List<Batch> findLoadingBatches();

}
