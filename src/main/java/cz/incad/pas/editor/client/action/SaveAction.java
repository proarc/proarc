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
package cz.incad.pas.editor.client.action;

import cz.incad.pas.editor.client.ClientMessages;

/**
 * Helper class providing description for subclasses that saves some content.
 *
 * @author Jan Pokorsky
 */
public abstract class SaveAction extends AbstractAction {

    public SaveAction(ClientMessages i18n) {
        super(i18n.SaveAction_Title(),
                "[SKIN]/actions/save.png",
                i18n.SaveAction_Hint());
    }

}
