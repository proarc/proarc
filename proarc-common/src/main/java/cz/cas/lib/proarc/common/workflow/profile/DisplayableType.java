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

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlID;
import jakarta.xml.bind.annotation.XmlTransient;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 *
 * @author Jan Pokorsky
 */
// @XmlTransient helps include XML elements in subclasses without creating a XML abstract type.
@XmlTransient
public abstract class DisplayableType<T extends DisplayableType> implements IDisplayableType<T> {

    @XmlAttribute(name = WorkflowProfileConsts.NAME, required = true)
    @XmlID
    private String name;

    @XmlAttribute(name = WorkflowProfileConsts.DISABLED, required = false)
    private Boolean disabled;

    @XmlTransient
    private Map<String, String> titles;

    @XmlTransient
    private Map<String, String> hints;

    @XmlElement(name = WorkflowProfileConsts.HINT_EL)
    public List<I18NString> hints_new;

    @XmlElement(name = WorkflowProfileConsts.TITLE_EL)
    public List<I18NString> titles_new;

    @Override
    public String getName() {
        return name;
    }

    public T setName(String name) {
        this.name = name;
        return (T) this;
    }

    @Override
    public boolean isDisabled() {
        return disabled != null && disabled;
    }

    public T setDisabled(Boolean disabled) {
        this.disabled = disabled;
        return (T) this;
    }

    @Override
    public String getTitle(String lang, String defaultValue) {
        return getI18n(getTitles(), lang, defaultValue);
    }

    public Map<String, String> getTitles() {
        if (titles == null) {
            titles = I18NString.toMap(getTitlesNew());
        }
        if (titles == null) {
            titles = new LinkedHashMap<String, String>();
        }
        return titles;
    }

    public List<I18NString> getTitlesNew() {
        return titles_new;
    }

    @Override
    public String getHint(String lang, String defaultValue) {
        return getI18n(getHints(), lang, defaultValue);
    }

    public Map<String, String> getHints() {
        if (hints == null) {
            hints = I18NString.toMap(getHintsNew());
        }
        if (hints == null) {
            hints = new LinkedHashMap<String, String>();
        }
        return hints;
    }

    public List<I18NString> getHintsNew() {
        return hints_new;
    }

    private static String getI18n(Map<String, String> vals, String lang, String defaultValue) {
        String i18n = vals.get(lang);
        if (i18n == null) {
            Iterator<String> i18ns = vals.values().iterator();
            if (i18ns.hasNext()) {
                i18n = i18ns.next();
            }
        }
        return i18n != null ? i18n : defaultValue;
    }

    @XmlElement(name = WorkflowProfileConsts.TITLE_EL)
    private void setITitles(List<I18NString> list) {
        this.titles = I18NString.toMap(list);
    }

    private List<I18NString> getITitles() {
        return I18NString.fromMap(getTitles());
    }

    @XmlElement(name = WorkflowProfileConsts.HINT_EL)
    private void setIHints(List<I18NString> list) {
        this.hints = I18NString.toMap(list);
    }

    private List<I18NString> getIHints() {
        return I18NString.fromMap(getHints());
    }

}
