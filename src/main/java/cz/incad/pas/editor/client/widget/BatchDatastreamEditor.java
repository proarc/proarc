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
package cz.incad.pas.editor.client.widget;

import com.smartgwt.client.data.Record;

/**
 * The data stream editor to edit a batch of digital objects.
 *
 * <p>Recommended usage is to provide this interface in {@link #getCapability}.
 *
 * @author Jan Pokorsky
 */
public interface BatchDatastreamEditor extends DatastreamEditor {

    /**
     * Starts editing.
     *
     * @param items record must contain at least PID and model
     * @param batchId optional import batch containing items
     */
    void edit(Record[] items, String batchId);
}
