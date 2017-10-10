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

/**
 *
 * @author Martin Rumanek
 */
public class CanvasSizePersistence {

    public static final String WIDTH = ".width";
    public static final String HEIGHT = ".height";

    private final String dbPrefix;

    public CanvasSizePersistence(String dbPrefix, Canvas canvas) {
        this.dbPrefix = dbPrefix;
        canvas.addResizedHandler(resizedEvent -> {
            Offline.put(dbPrefix + WIDTH, canvas.getWidth());
            Offline.put(dbPrefix + HEIGHT, canvas.getHeight());
        });
    }

    public Integer getWidth() {
        try {
            return Integer.valueOf((String) Offline.get(dbPrefix + WIDTH));
        } catch (NumberFormatException e) {
            return null;
        }
    }

    public Integer getHeight() {
        try {
            return Integer.valueOf((String) Offline.get(dbPrefix + HEIGHT));
        } catch (NumberFormatException e) {
            return null;
        }
    }

}
