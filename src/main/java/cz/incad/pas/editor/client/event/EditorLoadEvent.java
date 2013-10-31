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
package cz.incad.pas.editor.client.event;

import com.google.gwt.event.shared.GwtEvent;

/**
 * Event notifying editor load status.
 *
 * @author Jan Pokorsky
 */
public final class EditorLoadEvent extends GwtEvent<EditorLoadHandler> {

    public static final Type<EditorLoadHandler> TYPE = new Type<EditorLoadHandler>();
    private final boolean loadFailed;

    public EditorLoadEvent(boolean loadFailed) {
        this.loadFailed = loadFailed;
    }

    @Override
    public Type<EditorLoadHandler> getAssociatedType() {
        return TYPE;
    }

    public boolean isLoadFailed() {
        return loadFailed;
    }

    @Override
    protected void dispatch(EditorLoadHandler handler) {
        handler.onEditorLoad(this);
    }

}
