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

import java.util.List;

/**
 * DAO for {@link BatchItem}
 *
 * @author Jan Pokorsky
 */
public interface BatchItemDao extends Dao {

    BatchItem create();

    BatchItem find(int id);

    List<BatchItem> find(int batchId, String pid, String dsId, String file, String state, String type);

    void removeItems(int batchId);

    void update(BatchItem item);

}
