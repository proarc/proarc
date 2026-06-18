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
package cz.cas.lib.proarc.common.mods.custom;

import cz.cas.lib.proarc.common.mods.custom.ArrayMapper.ArrayItem;
import cz.cas.lib.proarc.mods.CodeOrText;
import cz.cas.lib.proarc.mods.LanguageDefinition;
import cz.cas.lib.proarc.mods.LanguageTermDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.ObjectFactory;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import java.util.ArrayList;
import java.util.List;

/**
 * Example:<br/>{@code
 * <language objectPart="summary">
 * <languageTerm type= "code" authority="iso639-2b">spa</languageTerm>
 * </language>
 *
 * @author Jan Pokorsky
 * @see <a href='http://www.loc.gov/standards/iso639-2/ISO-639-2_utf-8.txt'>ISO-639-2 CSV list</a>
 * @see <a href='http://www.loc.gov/standards/iso639-2/php/code_list.php'>ISO-639-2 list</a>
 * @see <a href='http://www.loc.gov/standards/mods/userguide/language.html#languageterm'>MODS user guide</a>
 */
final class LanguageMapper {

    private final ArrayMapper<LanguageDefinition, LanguageItem> arrayMapper =
            new ArrayMapper<>(new LanguageItemMapper());

    public List<LanguageItem> map(ModsDefinition mods) {
        List<LanguageItem> all = arrayMapper.map(mods.getLanguage());
        return filter(all, false);
    }

    public ModsDefinition map(ModsDefinition mods, List<LanguageItem> updates) {
        updates = MapperUtils.noNull(updates);
        List<LanguageDefinition> oldies = mods.getLanguage();
        List<LanguageItem> oldItems = arrayMapper.map(oldies);
        List<LanguageItem> ignored = filter(oldItems, true);
        updates = MapperUtils.mergeList(updates, ignored);
        List<LanguageDefinition> news = arrayMapper.map(updates, oldies);
        oldies.clear();
        oldies.addAll(news);
        return mods;
    }

    private static List<LanguageItem> filter(List<LanguageItem> items, boolean ignored) {
        ArrayList<LanguageItem> result = new ArrayList<>(items.size());
        for (LanguageItem item : items) {
            if (ignored == true && item.ignore || ignored == false && ignored == item.ignore) {
                result.add(item);
            }
        }
        return result;
    }

    private static final class LanguageItemMapper implements ArrayMapper.ItemMapper<LanguageDefinition, LanguageItem> {

        private final ObjectFactory factory = new ObjectFactory();

        @Override
        public LanguageItem map(LanguageDefinition source) {
            LanguageItem result = new LanguageItem();
            LanguageTermDefinition term = getLanguageTerm(source, false);
            if (term != null) {
                result.setValue(term.getValue());
            } else {
                result.ignore = true;
            }
            return result;
        }

        @Override
        public LanguageDefinition map(LanguageItem item, LanguageDefinition origin) {
            if (item.ignore) {
                return origin;
            }
            if (origin == null) {
                origin = factory.createLanguageDefinition();
            }
            LanguageTermDefinition term = getLanguageTerm(origin, true);
            term.setValue(item.getValue());
            return origin;
        }

        private LanguageTermDefinition getLanguageTerm(LanguageDefinition source, boolean create) {
            LanguageTermDefinition result = null;
            for (LanguageTermDefinition lterm : source.getLanguageTerm()) {
                String authority = lterm.getAuthority();
                CodeOrText type = lterm.getType();
                if (type == CodeOrText.CODE && "iso639-2b".equals(authority)) {
                    result = lterm;
                    break;
                }
            }
            if (create && result == null) {
                result = factory.createLanguageTermDefinition();
                result.setType(CodeOrText.CODE);
                result.setAuthority("iso639-2b");
                source.getLanguageTerm().add(result);
            }
            return result;
        }

    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class LanguageItem implements ArrayItem {

        private Integer index;
        @XmlElement(name = ModsConstants.FIELD_LANGUAGE_CODE)
        private String value;
        private boolean ignore;

        public LanguageItem() {
        }

        public LanguageItem(Integer index, String value) {
            this.index = index;
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }

        @Override
        public Integer getArrayIndex() {
            return index;
        }

        @Override
        public void setArrayIndex(Integer index) {
            this.index = index;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final LanguageItem other = (LanguageItem) obj;
            if (this.index != other.index && (this.index == null || !this.index.equals(other.index))) {
                return false;
            }
            if ((this.value == null) ? (other.value != null) : !this.value.equals(other.value)) {
                return false;
            }
            return true;
        }

        @Override
        public String toString() {
            return String.format("LanguageItem{index: %s, value: %s}", index, value);
        }

    }

}
