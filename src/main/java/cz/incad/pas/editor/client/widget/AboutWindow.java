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

import com.smartgwt.client.widgets.Dialog;
import cz.incad.pas.editor.client.ClientMessages;

/**
 * Informations about the application.
 *
 * @author Jan Pokorsky
 */
public final class AboutWindow {

    private static AboutWindow INSTANCE;

    private final Dialog dialog;

    public static AboutWindow getInstance(ClientMessages i18n) {
        if (INSTANCE == null) {
            INSTANCE = new AboutWindow(i18n);
        }
        return INSTANCE;
    }

    private AboutWindow(ClientMessages i18n) {
        this.dialog = init(i18n);
    }

    public void show() {
        dialog.show();
        dialog.focus();
    }

    private Dialog init(ClientMessages i18n) {
        String rev = String.valueOf(i18n.proarc_build_revision());
        rev = rev.length() < 9 ? rev : rev.substring(0, 9);
        String msg = i18n.AboutWindow_Msg(
                i18n.proarc_version(),
                i18n.proarc_build_timestamp(), rev);

        Dialog d = new Dialog();
        d.setTitle(i18n.AboutWindow_Title());
        d.setMessage(msg);
        d.setButtons(Dialog.OK);
        d.setIsModal(Boolean.TRUE);
        d.setDismissOnOutsideClick(Boolean.TRUE);
        d.setAutoFocus(Boolean.TRUE);
        return d;
    }

}
