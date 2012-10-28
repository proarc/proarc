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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.client.action;

import cz.incad.pas.editor.client.ClientMessages;

/**
 * The refresh action invokes {@link Refreshable} on the source object.
 *
 * @author Jan Pokorsky
 */
public final class RefreshAction extends AbstractAction {

    public RefreshAction(ClientMessages i18n) {
        super(i18n.RefreshAction_Title(), "[SKIN]/actions/refresh.png", i18n.RefreshAction_Hint());
    }

    @Override
    public void performAction(ActionEvent event) {
        Object source = event.getSource();
        if (source instanceof Refreshable) {
            Refreshable r = (Refreshable) source;
            r.refresh();
        }
    }

    public interface Refreshable {

        void refresh();

    }

}
