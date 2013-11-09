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
import cz.cas.lib.proarc.common.mods.custom.NameMapper.NameItem.NameRole;
import cz.fi.muni.xkremser.editor.server.mods.CodeOrText;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.NamePartType;
import cz.fi.muni.xkremser.editor.server.mods.NameType;
import cz.fi.muni.xkremser.editor.server.mods.NameTypeAttribute;
import cz.fi.muni.xkremser.editor.server.mods.ObjectFactory;
import cz.fi.muni.xkremser.editor.server.mods.RoleType;
import cz.fi.muni.xkremser.editor.server.mods.RoleType.RoleTerm;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.namespace.QName;

/**
 * Usage:
 * <pre>
 * {@code
        // read
        List<NameItem> all = map(mods);
        List<NameItem> authors = filter(all, true, NameRole.AUTHOR);
        List<NameItem> contributors = filter(all, true, NameRole.CONTRIBUTOR);
        // write
        all = map(mods);
        List<NameItem> others = filter(all, false, NameRole.AUTHOR, NameRole.CONTRIBUTOR);
        List<NameItem> news = MapperUtils.mergeList(cast(authors, NameRole.AUTHOR),
                cast(contributors, NameRole.CONTRIBUTOR), others);
        map(mods, news);
 * }
 * </pre>
 *
 * {@code mods/name/namePart[@type={family,given}]}
 * {@code mods/name/role/roleTerm/[@type={CODE,TEXT}]}, see
 * authors:[{family:'', given:''}]
 * XXX names can be of type personal, corporate, conference, family
 *
 * @author Jan Pokorsky
 */
final class NameMapper {
    
    ArrayMapper<NameType, NameItem> nameMap = new ArrayMapper<NameType, NameItem>(new NameItemMapper());

    public List<NameItem> map(ModsType mods) {
        List<NameType> names = MapperUtils.find(mods.getModsGroup(), NameType.class);
        return nameMap.map(names);
    }

    public ModsType map(ModsType mods, List<NameItem> authors, List<NameItem> contributors) {
        List<NameItem> oldItems = map(mods);
        List<NameItem> others = filter(oldItems, true, NameRole.OTHER);
        List<NameItem> names = MapperUtils.mergeList(
                cast(MapperUtils.noNull(authors), NameRole.AUTHOR),
                cast(MapperUtils.noNull(contributors), NameRole.CONTRIBUTOR),
                others);
        return map(mods, names);
    }

    public ModsType map(ModsType mods, List<NameItem> names) {
        List<NameType> oldies = MapperUtils.find(mods.getModsGroup(), NameType.class);
        List<NameType> news = nameMap.map(MapperUtils.noNull(names), oldies);
        MapperUtils.update(mods.getModsGroup(), news, NameType.class);
        return mods;
    }

    public static List<NameItem> filter(List<NameItem> names, boolean include, NameRole first, NameRole... rest) {
        return filter(names, EnumSet.of(first, rest), include);
    }

    public static List<NameItem> filter(List<NameItem> names, Set<NameRole> filter, boolean include) {
        List<NameItem> result = new ArrayList<NameItem>();

        for (NameItem name : names) {
            boolean contains = filter.contains(name.getRole());
            if ((include == true && include == contains) || (include == false && include == contains)) {
                result.add(name);
            }
        }
        return result;
    }

    /**
     * Ensures that items belongs to role.
     */
    public static List<NameItem> cast(List<NameItem> items, NameRole role) {
        for (NameItem item : items) {
            item.setRole(role);
        }
        return items;
    }

    private static final class NameItemMapper implements ArrayMapper.ItemMapper<NameType, NameItem> {

        private static final String TYPE_FAMILY = "family";
        private static final String TYPE_GIVEN = "given";

        private final ObjectFactory factory = new ObjectFactory();

        @Override
        public NameItem map(NameType source) {
            NameItem result = new NameItem();
            int familyCount = 0;
            int givenCount = 0;

            for (JAXBElement<?> nameSubelm : source.getNamePartOrDisplayFormOrAffiliation()) {
                QName qname = nameSubelm.getName();

                if (ObjectFactory._NameTypeRole_QNAME.equals(qname)) {
                    if (result.getRole() != null && result.getRole() != NameRole.OTHER) {
                        continue;
                    }
                    RoleType role = (RoleType) nameSubelm.getValue();
                    result.setRole(NameRole.fromDom(role));

                } else if ((familyCount == 0 || givenCount == 0) && ObjectFactory._NameTypeNamePart_QNAME.equals(qname)) {
                    NamePartType namePart = (NamePartType) nameSubelm.getValue();
                    String type = namePart.getType();
                    String partValue = namePart.getValue();
                    if (familyCount == 0 && TYPE_FAMILY.equals(type)) {
                        result.setFamily(partValue);
                        ++familyCount;
                    } else if (givenCount == 0 && TYPE_GIVEN.equals(type)) {
                        result.setGiven(partValue);
                        ++givenCount;
                    }
                }
            }
            return result;
        }

        @Override
        public NameType map(final NameItem item, final NameType origin) {
            if (item.getRole() == NameRole.OTHER) {
                return origin;
            }
            int familyCount = 0;
            int givenCount = 0;
            NameType result = origin != null ? origin : new NameType();

            for (JAXBElement<?> nameSubelm : result.getNamePartOrDisplayFormOrAffiliation()) {
                QName qname = nameSubelm.getName();
                if (ObjectFactory._NameTypeNamePart_QNAME.equals(qname)) {
                    NamePartType namePart = (NamePartType) nameSubelm.getValue();
                    String type = namePart.getType();
                    if (familyCount == 0 && TYPE_FAMILY.equals(type)) {
                        namePart.setValue(item.getFamily());
                        ++familyCount;
                    } else if (givenCount == 0 && TYPE_GIVEN.equals(type)) {
                        namePart.setValue(item.getGiven());
                        ++givenCount;
                    }
                }
                if (familyCount > 0 && givenCount > 0) {
                    break;
                }
            }
            if (familyCount == 0 && item.getFamily() != null) {
                createPartName(result, TYPE_FAMILY, item.getFamily());
            }

            if (givenCount == 0 && item.getGiven() != null) {
                createPartName(result, TYPE_GIVEN, item.getGiven());
            }

            if (origin == null) { // new item
                result.setAtType(NameTypeAttribute.PERSONAL);
                createRole(result, item.getRole());
            }
            return result;
        }

        private void createPartName(NameType nameType, String type, String name) {
            NamePartType namePart = factory.createNamePartType();
            namePart.setType(type);
            namePart.setValue(name);
            nameType.getNamePartOrDisplayFormOrAffiliation().add(factory.createNameTypeNamePart(namePart));
        }

        private void createRole(NameType name, NameRole role) {
            RoleType roleType = factory.createRoleType();
            RoleTerm roleTerm = factory.createRoleTypeRoleTerm();
            roleTerm.setType(CodeOrText.CODE);
            roleTerm.setValue(role.getCode());
            roleType.getRoleTerm().add(roleTerm);
            roleTerm = factory.createRoleTypeRoleTerm();
            roleTerm.setType(CodeOrText.TEXT);
            roleTerm.setValue(role.getText());
            roleType.getRoleTerm().add(roleTerm);
            name.getNamePartOrDisplayFormOrAffiliation().add(factory.createNameTypeRole(roleType));
        }
        
    }

    @javax.xml.bind.annotation.XmlAccessorType(XmlAccessType.FIELD)
    public static class NameItem implements ArrayItem {

        private Integer index;
        @XmlElement(name = ModsConstants.FIELD_NAME_FAMILY)
        private String family;
        @XmlElement(name = ModsConstants.FIELD_NAME_GIVEN)
        private String given;
        private transient NameRole role;

        public enum NameRole {
            AUTHOR("cre", "Author"), CONTRIBUTOR("ctb", "Contributor"), OTHER("undefined", "undefined");
            private String code;
            private String text;

            private NameRole(String code, String text) {
                this.code = code;
                this.text = text;
            }

            public String getCode() {
                return code;
            }

            public String getText() {
                return text;
            }

            public static NameRole fromCode(String code) {
                for (NameRole role : values()) {
                    if (role.getCode().equals(code)) {
                        return role;
                    }
                }
                return OTHER;
            }

            public static NameRole fromText(String text) {
                for (NameRole role : values()) {
                    if (role.getText().equals(text)) {
                        return role;
                    }
                }
                return OTHER;
            }

            private static NameRole fromDom(RoleType... roles) {
                NameRole result = NameRole.OTHER;
                for (RoleType role : roles) {
                    for (RoleTerm roleTerm : role.getRoleTerm()) {
                        switch (roleTerm.getType()) {
                            case CODE: return fromCode(roleTerm.getValue());
                            case TEXT: return fromText(roleTerm.getValue());
                        }
                    }
                }
                return result;
            }

        }

        public NameItem(Integer index, String family, String given, NameRole role) {
            this.index = index;
            this.family = MapperUtils.normalize(family);
            this.given = MapperUtils.normalize(given);
            this.role = role;
        }

        public NameItem(String family, String given, NameRole role) {
            this(null, family, given, role);
        }

        public NameItem() {
        }

        @Override
        public Integer getArrayIndex() {
            return index;
        }

        @Override
        public void setArrayIndex(Integer index) {
            this.index = index;
        }

        public String getFamily() {
            return family;
        }

        public void setFamily(String family) {
            this.family = MapperUtils.normalize(family);
        }

        public String getGiven() {
            return given;
        }

        public void setGiven(String given) {
            this.given = MapperUtils.normalize(given);
        }

        public NameRole getRole() {
            return role;
        }

        public void setRole(NameRole role) {
            this.role = role;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final NameItem other = (NameItem) obj;
            if ((this.family == null) ? (other.family != null) : !this.family.equals(other.family)) {
                return false;
            }
            if ((this.given == null) ? (other.given != null) : !this.given.equals(other.given)) {
                return false;
            }
            if (this.role != other.role) {
                return false;
            }
            return true;
        }

        @Override
        public String toString() {
            return String.format("NameValue{family=%s, given=%s, role=%s, index=%s}", family, given, role, index);
        }

    }

}
