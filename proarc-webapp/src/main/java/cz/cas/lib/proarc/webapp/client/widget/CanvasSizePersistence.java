/*
 * Copyright (C) 2017 Martin Rumanek
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
package cz.cas.lib.proarc.webapp.client.widget;

import com.smartgwt.client.util.Offline;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;

/**
 *
 * @author Martin Rumanek
 */
public class CanvasSizePersistence {

    private final String dbPrefix;

    public CanvasSizePersistence(String dbPrefix, Canvas canvas) {
        this.dbPrefix = dbPrefix;
        canvas.addResizedHandler(new ResizedHandler() {
            @Override
            public void onResized(ResizedEvent resizedEvent) {
                Offline.put(dbPrefix + ".width", canvas.getWidth());
                Offline.put(dbPrefix + ".height", canvas.getHeight());
            }
        });
    }

    public Integer getWidth() {
        try {
            return Integer.valueOf((String) Offline.get(dbPrefix + ".width"));
        } catch (NumberFormatException e) {
            return null;
        }
    }

    public Integer getHeight() {
        try {
            return Integer.valueOf((String) Offline.get(dbPrefix + ".height"));
        } catch (NumberFormatException e) {
            return null;
        }
    }

}
