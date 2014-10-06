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
package cz.cas.lib.proarc.webapp.client.widget;

import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.event.shared.HandlerRegistration;
import com.smartgwt.client.widgets.form.events.HasSubmitValuesHandlers;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import cz.cas.lib.proarc.webapp.client.event.EditorLoadHandler;
import cz.cas.lib.proarc.webapp.client.event.EditorLoadEvent;
import cz.cas.lib.proarc.webapp.client.event.HasEditorLoadHandlers;

/**
 * Basic editor implementation.
 *
 * @author Jan Pokorsky
 */
public abstract class AbstractDatastreamEditor implements DatastreamEditor, HasEditorLoadHandlers,
        HasSubmitValuesHandlers {

    private HandlerManager handlerManager;

    @Override
    public HandlerRegistration addEditorLoadHandler(EditorLoadHandler handler) {
        return ensureHandlers().addHandler(EditorLoadEvent.TYPE, handler);
    }

    @Override
    public HandlerRegistration addSubmitValuesHandler(SubmitValuesHandler handler) {
        return ensureHandlers().addHandler(SubmitValuesEvent.getType(), handler);
    }

    @Override
    public void fireEvent(GwtEvent<?> event) {
        if (handlerManager != null) {
            handlerManager.fireEvent(event);
        }
    }

    protected HandlerManager ensureHandlers() {
        return handlerManager == null ? handlerManager = createHandlerManager()
                : handlerManager;
    }

    private HandlerManager createHandlerManager() {
        return new HandlerManager(this);
    }

}
