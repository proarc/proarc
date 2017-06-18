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
package cz.cas.lib.proarc.webapp.server;

import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler;
import java.text.MessageFormat;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.ResourceBundle.Control;

/**
 * Helper to get server localized messages.
 *
 * @author Jan Pokorsky
 */
public final class ServerMessages {

    private static final Control CONTROL = Control.getNoFallbackControl(Control.FORMAT_PROPERTIES);

    public static ServerMessages get(Locale l) {
        return new ServerMessages(l);
    }

    static String getMessage(Locale l, String key) throws MissingResourceException {
        return getBundle(l).getString(key);
    }

    static String getFormattedMessage(Locale l, String key, Object... arguments) throws MissingResourceException {
        return MessageFormat.format(getMessage(l, key), arguments);
    }

    static ResourceBundle getBundle(Locale l) {
        return ResourceBundle.getBundle(ServerMessages.class.getName(), l, CONTROL);
    }

    private final Locale l;

    private ServerMessages(Locale l) {
        this.l = l;
    }

    public String getFormattedMessage(String key, Object... arguments) throws MissingResourceException {
        return getFormattedMessage(l, key, arguments);
    }

    public String DeviceResource_Delete_InUse_Msg() {
        return getMessage(l, "DeviceResource_Delete_InUse_Msg");
    }

    public String Err_Ndk_Remove_UrnNbn(String urnNbn) {
        return getFormattedMessage(l, NdkMetadataHandler.ERR_NDK_REMOVE_URNNBN, urnNbn);
    }

    public String Err_Ndk_Change_Mods_With_UrnNbn(String urnNbn) {
        return getFormattedMessage(l, NdkMetadataHandler.ERR_NDK_CHANGE_MODS_WITH_URNNBN, urnNbn);
    }

    public String Err_Ndk_Change_Mods_With_Members() {
        return getMessage(l, NdkMetadataHandler.ERR_NDK_CHANGE_MODS_WITH_MEMBERS);
    }

    public String Err_Ndk_Doi_Duplicity(String doi) {
        return getFormattedMessage(l, NdkMetadataHandler.ERR_NDK_DOI_DUPLICITY, doi);
    }

    public String ImportResource_BatchLoadingFailed_Msg() {
        return getMessage(l, "ImportResource_BatchLoadingFailed_Msg");
    }

}
