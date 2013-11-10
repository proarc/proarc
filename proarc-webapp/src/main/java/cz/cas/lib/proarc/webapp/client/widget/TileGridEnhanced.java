/*
 * Copyright (C) 2011 Jan Pokorsky
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.client.widget;

import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.Timer;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.widgets.grid.events.HasSelectionUpdatedHandlers;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.tile.TileGrid;
import com.smartgwt.client.widgets.tile.events.SelectionChangedEvent;
import com.smartgwt.client.widgets.tile.events.SelectionChangedHandler;

/**
 * Enhances TileGrid with SelectionUpdatedHandler ability. Only user input is
 * fired as events.
 *
 * @author Jan Pokorsky
 */
public class TileGridEnhanced extends TileGrid implements HasSelectionUpdatedHandlers {

    private SelectionTimer selectionTimer;
    private boolean selectionInProgress;

    public TileGridEnhanced(JavaScriptObject jsObj) {
        super(jsObj);
    }

    public TileGridEnhanced() {
    }

    @Override
    public HandlerRegistration addSelectionUpdatedHandler(SelectionUpdatedHandler handler) {
        if (getHandlerCount(SelectionUpdatedEvent.getType()) == 0) {
            selectionTimer = new SelectionTimer();
            addSelectionChangedHandler(selectionTimer);
        }
        return doAddHandler(handler, SelectionUpdatedEvent.getType());
    }

    @Override
    public void selectAllRecords() {
        selectionInProgress = true;
        try {
            super.selectAllRecords();
        } finally {
            selectionInProgress = false;
        }
    }

    @Override
    public void selectRecord(Record record) {
        selectionInProgress = true;
        try {
            super.selectRecord(record);
        } finally {
            selectionInProgress = false;
        }
    }

    @Override
    public void selectRecord(int record) {
        selectionInProgress = true;
        try {
            super.selectRecord(record);
        } finally {
            selectionInProgress = false;
        }
    }

    @Override
    public void selectRecords(Record[] records) {
        selectionInProgress = true;
        try {
            super.selectRecords(records);
        } finally {
            selectionInProgress = false;
        }
    }

    @Override
    public void selectRecords(int[] records) {
        selectionInProgress = true;
        try {
            super.selectRecords(records);
        } finally {
            selectionInProgress = false;
        }
    }

    @Override
    public void deselectAllRecords() {
        selectionInProgress = true;
        try {
            super.deselectAllRecords();
        } finally {
            selectionInProgress = false;
        }
    }

    @Override
    public void deselectRecord(Record record) {
        selectionInProgress = true;
        try {
            super.deselectRecord(record);
        } finally {
            selectionInProgress = false;
        }
    }

    @Override
    public void deselectRecord(int record) {
        selectionInProgress = true;
        try {
            super.deselectRecord(record);
        } finally {
            selectionInProgress = false;
        }
    }

    @Override
    public void deselectRecords(Record[] records) {
        selectionInProgress = true;
        try {
            super.deselectRecords(records);
        } finally {
            selectionInProgress = false;
        }
    }

    @Override
    public void deselectRecords(int[] records) {
        selectionInProgress = true;
        try {
            super.deselectRecords(records);
        } finally {
            selectionInProgress = false;
        }
    }



    private final class SelectionTimer extends Timer implements SelectionChangedHandler {

        @Override
        public void run() {
            SelectionUpdatedEvent.fire(TileGridEnhanced.this, null);
        }

        @Override
        public void onSelectionChanged(SelectionChangedEvent event) {
            if (!selectionInProgress) {
                schedule(100);
            }
        }

    }

}
