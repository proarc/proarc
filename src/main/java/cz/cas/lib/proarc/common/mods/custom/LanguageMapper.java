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
import cz.fi.muni.xkremser.editor.server.mods.CodeOrText;
import cz.fi.muni.xkremser.editor.server.mods.LanguageType;
import cz.fi.muni.xkremser.editor.server.mods.LanguageType.LanguageTerm;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.ObjectFactory;
import cz.incad.pas.editor.client.ds.ModsCustomDataSource;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * Example:<br/>{@code
     <language objectPart="summary">
         <languageTerm type= "code" authority="iso639-2b">spa</languageTerm>
     </language>
 *
 * @see <a href='http://www.loc.gov/standards/iso639-2/ISO-639-2_utf-8.txt'>ISO-639-2 CSV list</a>
 * @see <a href='http://www.loc.gov/standards/iso639-2/php/code_list.php'>ISO-639-2 list</a>
 * @see <a href='http://www.loc.gov/standards/mods/userguide/language.html#languageterm'>MODS user guide</a>
 *
 * @author Jan Pokorsky
 */
final class LanguageMapper {

    private final ArrayMapper<LanguageType, LanguageItem> arrayMapper =
            new ArrayMapper<LanguageType, LanguageItem>(new LanguageItemMapper());

    public List<LanguageItem> map(ModsType mods) {
        List<LanguageItem> all = arrayMapper.map(MapperUtils.find(mods.getModsGroup(), LanguageType.class));
        return filter(all, false);
    }

    public ModsType map(ModsType mods, List<LanguageItem> updates) {
        updates = MapperUtils.noNull(updates);
        List<LanguageType> oldies = MapperUtils.find(mods.getModsGroup(), LanguageType.class);
        List<LanguageItem> oldItems = arrayMapper.map(oldies);
        List<LanguageItem> ignored = filter(oldItems, true);
        updates = MapperUtils.mergeList(updates, ignored);
        List<LanguageType> news = arrayMapper.map(updates, oldies);
        MapperUtils.update(mods.getModsGroup(), news, LanguageType.class);
        return mods;
    }

    private static List<LanguageItem> filter(List<LanguageItem> items, boolean ignored) {
        ArrayList<LanguageItem> result = new ArrayList<LanguageItem>(items.size());
        for (LanguageItem item : items) {
            if (ignored == true && item.ignore || ignored == false && ignored == item.ignore) {
                result.add(item);
            }
        }
        return result;
    }

    private static final class LanguageItemMapper implements ArrayMapper.ItemMapper<LanguageType, LanguageItem> {

        private final ObjectFactory factory = new ObjectFactory();

        @Override
        public LanguageItem map(LanguageType source) {
            LanguageItem result = new LanguageItem();
            LanguageTerm term = getLanguageTerm(source, false);
            if (term != null) {
                result.setValue(term.getValue());
            } else {
                result.ignore = true;
            }
            return result;
        }

        @Override
        public LanguageType map(LanguageItem item, LanguageType origin) {
            if (item.ignore) {
                return origin;
            }
            if (origin == null) {
                origin = factory.createLanguageType();
            }
            LanguageTerm term = getLanguageTerm(origin, true);
            term.setValue(item.getValue());
            return origin;
        }

        private LanguageTerm getLanguageTerm(LanguageType source, boolean create) {
            LanguageTerm result = null;
            for (LanguageTerm lterm : source.getLanguageTerm()) {
                String authority = lterm.getAuthority();
                CodeOrText type = lterm.getType();
                if (type == CodeOrText.CODE && "iso639-2b".equals(authority)) {
                    result = lterm;
                    break;
                }
            }
            if (create && result == null) {
                result = factory.createLanguageTypeLanguageTerm();
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
        @XmlElement(name = ModsCustomDataSource.FIELD_LANGUAGE_CODE)
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
