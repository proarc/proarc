/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.workflow.profile;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlValue;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(value = XmlAccessType.FIELD)
public class I18NString {

    @XmlAttribute(name = WorkflowProfileConsts.I18N_LANG_ATT)
    private String lang;

    @XmlValue
    private String value;

    public String getLang() {
        return lang;
    }

    public I18NString setLang(String lang) {
        this.lang = lang;
        return this;
    }

    public String getValue() {
        return value;
    }

    public I18NString setValue(String value) {
        this.value = value;
        return this;
    }

    public static Map<String, String> toMap(List<I18NString> list) {
        return toMap(list, new LinkedHashMap<String, String>());
    }

    public static Map<String, String> toMap(List<I18NString> list, Map<String, String> m) {if (list == null || list.isEmpty()) {
            return m;
        }
        for (I18NString s : list) {
            m.put(s.getLang(), s.getValue());
        }
        return m;
    }

    public static List<I18NString> fromMap(Map<String, String> m) {
        if (m.isEmpty()) {
            return null;
        }
        ArrayList<I18NString> result = new ArrayList<I18NString>();
        for (Entry<String, String> entry : m.entrySet()) {
            result.add(new I18NString().setLang(entry.getKey()).setValue(entry.getValue()));
        }
        return result;
    }

}
