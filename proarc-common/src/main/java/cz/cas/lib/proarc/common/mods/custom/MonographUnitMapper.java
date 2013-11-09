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

import cz.cas.lib.proarc.common.mods.custom.IdentifierMapper.IdentifierItem;
import cz.cas.lib.proarc.common.mods.custom.Mapping.Mapper;
import cz.cas.lib.proarc.common.mods.custom.MonographUnitMapper.MonographUnit;
import cz.fi.muni.xkremser.editor.server.mods.DetailType;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.ObjectFactory;
import cz.fi.muni.xkremser.editor.server.mods.PartType;
import cz.fi.muni.xkremser.editor.server.mods.UnstructuredText;
import java.util.List;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Handles MonographUnit number and note.
 *
 * @author Jan Pokorsky
 */
final class MonographUnitMapper implements Mapper<MonographUnit> {

    @Override
    public MonographUnit map(ModsType mods) {
        NodeLookup nlookup = new NodeLookup(mods);
        MonographUnit result = new MonographUnit();
        // identifiers
        IdentifierMapper identifierMap = new IdentifierMapper();
        result.setIdentifiers(identifierMap.map(mods));
        // unit number
        JAXBElement<String> number = nlookup.getNumber(false);
        result.setNumber(number == null ? null : number.getValue());
        // note
        UnstructuredText note = nlookup.getNote(false);
        result.setNote(note == null ? null : note.getValue());
        return result;
    }

    @Override
    public ModsType map(ModsType mods, MonographUnit munit) {
        NodeLookup nlookup = new NodeLookup(mods);
        // identifiers
        IdentifierMapper identifierMap = new IdentifierMapper();
        identifierMap.map(mods, munit.getIdentifiers());
        // unit number
        if (munit.getNumber() != null) {
            nlookup.getNumber(true).setValue(munit.getNumber());
        } else {
            JAXBElement<String> number = nlookup.getNumber(false);
            if (number != null) {
                number.setValue(null);
            }
        }
        // note
        if (munit.getNote() != null) {
            nlookup.getNote(true).setValue(munit.getNote());
        } else {
            UnstructuredText note = nlookup.getNote(false);
            if (note != null) {
                note.setValue(null);
            }
        }
        new OriginInfoMapper().map(mods, null, null, OriginInfoMapper.ISSUANCE_MONOGRAPHIC);
        new TypeOfResourceMapper().map(mods, TypeOfResourceMapper.Type.TEXT);
        return mods;
    }

    private static final class NodeLookup {

        private static final String ATTR_VOLUME = "Volume";
        private final ObjectFactory factory = new ObjectFactory();
        private ModsType mods;
        private PartType part;
        private DetailType detail;
        private JAXBElement<String> number;
        private UnstructuredText note;

        public NodeLookup(ModsType mods) {
            this.mods = mods;
        }

        public JAXBElement<String> getNumber(boolean create) {
            if (number == null) {
                if (getDetail(create) != null) {
                    number = MapperUtils.findFirst(detail.getNumberOrCaptionOrTitle(),
                            MapperUtils.<String>jaxbElementSelector(ObjectFactory._DetailTypeNumber_QNAME));
                }
            }
            if (number == null && create) {
                number = factory.createDetailTypeNumber(null);
                detail.getNumberOrCaptionOrTitle().add(number);
            }
            return number;
        }

        public UnstructuredText getNote(boolean create) {
            if (note == null) {
                if (getPart(create) != null) {
                    note = MapperUtils.findFirst(part.getDetailOrExtentOrDate(), UnstructuredText.class);
                }
            }
            if (note == null && create) {
                note = factory.createUnstructuredText();
                part.getDetailOrExtentOrDate().add(note);
            }
            return note;
        }

        public PartType getPart(boolean create) {
            if (part == null) {
                part = MapperUtils.findFirst(MapperUtils.find(mods.getModsGroup(), PartType.class), new MapperUtils.Selector<PartType>(){

                    @Override
                    public boolean select(PartType item) {
                        return ATTR_VOLUME.equals(item.getType());
                    }
                });
            }
            if (part == null && create) {
                part = factory.createPartType();
                part.setType(ATTR_VOLUME);
                MapperUtils.add(mods, part);
            }
            return part;
        }

        public DetailType getDetail(boolean create) {
            if (detail == null) {
                if (getPart(create) != null) {
                    detail = MapperUtils.findFirst(part.getDetailOrExtentOrDate(), DetailType.class);
                }
            }
            if (detail == null && create) {
                detail = factory.createDetailType();
                MapperUtils.add(part, detail);
            }
            return detail;
        }
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public static final class MonographUnit {

        @XmlElement(name = ModsConstants.FIELD_IDENTIFIERS)
        private List<IdentifierItem> identifiers;
        @XmlElement(name = ModsConstants.FIELD_MONOGRAPHUNIT_NUMBER)
        private String number;
        @XmlElement(name = ModsConstants.FIELD_NOTE)
        private String note;

        public MonographUnit() {
        }

        public List<IdentifierItem> getIdentifiers() {
            return identifiers;
        }

        public void setIdentifiers(List<IdentifierItem> identifiers) {
            this.identifiers = identifiers;
        }

        public String getNote() {
            return note;
        }

        public void setNote(String note) {
            this.note = note;
        }

        public String getNumber() {
            return number;
        }

        public void setNumber(String number) {
            this.number = number;
        }

    }
}
