/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.shared.form;

import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlValue;

/**
 * A text value for given {@link java.util.Locale locale}.
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(value = XmlAccessType.FIELD)
public class Localized {

    @XmlAttribute
    private String locale;
    @XmlValue
    private String value;

    public Localized() {
    }

    public Localized(String locale, String value) {
        this.locale = locale;
        this.value = value;
    }

    public String getLocale() {
        return locale == null ? "cs" : locale;
    }

    public void setLocale(String locale) {
        this.locale = locale;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    static String getElement(List<Localized> elms, String locale) {
        for (Localized elm : elms) {
            if (locale == null ? elm.getLocale() == null : locale.equals(elm.getLocale())) {
                return elm.getValue();
            }
        }
        return elms.isEmpty() ? null : elms.get(0).getValue();
    }

}
