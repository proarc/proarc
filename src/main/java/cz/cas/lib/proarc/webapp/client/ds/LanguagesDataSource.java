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
package cz.cas.lib.proarc.webapp.client.ds;

import com.google.gwt.i18n.client.LocaleInfo;
import com.smartgwt.client.data.Criteria;
import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.incad.pas.editor.shared.rest.LocalizationResourceApi;

/**
 * ISO 639-2 languages support.
 *
 * @author Jan Pokorsky
 * @see <a href="http://www.loc.gov/standards/iso639-2/php/English_list.php>">ISO 639-2</a>
 */
public final class LanguagesDataSource {

    public static final String FIELD_CODE = LocalizationResourceApi.ITEM_KEY;
    public static final String FIELD_VALUE = LocalizationResourceApi.ITEM_VALUE;

    public static LocalizationDataSource getInstance() {
        return LocalizationDataSource.getInstance();
    }

    public static String activeLocale() {
        String locale = LocaleInfo.getCurrentLocale().getLocaleName();
        return locale == null || "default".equals(locale)
                ? "en" : locale;
    }

    public static Criteria languageCriteria() {
        return LocalizationDataSource.asCriteria(BundleName.LANGUAGES_ISO639_2);
    }

}
