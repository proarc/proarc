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
package cz.incad.pas.editor.client.ds;

import com.google.gwt.i18n.client.LocaleInfo;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.FieldType;

/**
 * ISO 639-2 languages support.
 *
 * @author Jan Pokorsky
 * @see <a href="http://www.loc.gov/standards/iso639-2/php/English_list.php>">ISO 639-2</a>
 */
public final class LanguagesDataSource extends DataSource {

    public static final String ID = "LanguagesDataSource";

    public static final String FIELD_CODE = "code";
    public static final String FIELD_LOCALE = "locale";
    public static final String FIELD_VALUE = "value";

    public LanguagesDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setRecordXPath("item");
        setDataURL("ds/mods/languagues.json");
        setClientOnly(true);

        DataSourceField code = new DataSourceField(FIELD_CODE, FieldType.TEXT);
        DataSourceField locale = new DataSourceField(FIELD_LOCALE, FieldType.TEXT);
        DataSourceField value = new DataSourceField(FIELD_VALUE, FieldType.TEXT);
        setFields(code, locale, value);
    }

    public static LanguagesDataSource getInstance() {
        LanguagesDataSource ds = (LanguagesDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new LanguagesDataSource();
        return ds;
    }

    public static String activeLocale() {
        String locale = LocaleInfo.getCurrentLocale().getLocaleName();
        return locale == null || "default".equals(locale)
                ? "en" : locale;
    }

    public static Criteria activeLocaleAsCriteria() {
        return new Criteria(FIELD_LOCALE, activeLocale());
    }

}
