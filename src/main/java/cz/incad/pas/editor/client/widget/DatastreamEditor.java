/*
 * Copyright (C) 2012 Jan Pokorsky
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

import com.smartgwt.client.widgets.Canvas;
import cz.incad.pas.editor.client.ds.MetaModelDataSource.MetaModelRecord;

/**
 * Digital object's data stream editor.
 *
 * @author Jan Pokorsky
 */
public interface DatastreamEditor {

    public void edit(String pid, String batchId, MetaModelRecord model);

    public <T> T getCapability(Class<T> clazz);

    public Canvas[] getToolbarItems();
    
    public Canvas getUI();

}
