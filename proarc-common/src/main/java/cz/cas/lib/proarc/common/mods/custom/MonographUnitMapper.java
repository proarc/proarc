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
import cz.cas.lib.proarc.mods.DetailDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.ObjectFactory;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.Text;
import java.util.List;
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
    public MonographUnit map(ModsDefinition mods) {
        NodeLookup nlookup = new NodeLookup(mods);
        MonographUnit result = new MonographUnit();
        // identifiers
        IdentifierMapper identifierMap = new IdentifierMapper();
        result.setIdentifiers(identifierMap.map(mods));
        // unit number
        StringPlusLanguage number = nlookup.getNumber(false);
        result.setNumber(number == null ? null : number.getValue());
        // note
        Text note = nlookup.getNote(false);
        result.setNote(note == null ? null : note.getValue());
        return result;
    }

    @Override
    public ModsDefinition map(ModsDefinition mods, MonographUnit munit) {
        NodeLookup nlookup = new NodeLookup(mods);
        // identifiers
        IdentifierMapper identifierMap = new IdentifierMapper();
        identifierMap.map(mods, munit.getIdentifiers());
        // unit number
        if (munit.getNumber() != null) {
            nlookup.getNumber(true).setValue(munit.getNumber());
        } else {
            StringPlusLanguage number = nlookup.getNumber(false);
            if (number != null) {
                number.setValue(null);
            }
        }
        // note
        if (munit.getNote() != null) {
            nlookup.getNote(true).setValue(munit.getNote());
        } else {
            Text note = nlookup.getNote(false);
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
        private ModsDefinition mods;
        private PartDefinition part;
        private DetailDefinition detail;
        private StringPlusLanguage number;
        private Text note;

        public NodeLookup(ModsDefinition mods) {
            this.mods = mods;
        }

        public StringPlusLanguage getNumber(boolean create) {
            if (number == null) {
                if (getDetail(create) != null) {
                    number = detail.getNumber().stream().findFirst().orElse(null);
                }
            }
            if (number == null && create) {
                number = factory.createStringPlusLanguage();
                detail.getNumber().add(number);
            }
            return number;
        }

        public Text getNote(boolean create) {
            if (note == null) {
                if (getPart(create) != null) {
                    note = part.getText().stream().findFirst().orElse(null);
                }
            }
            if (note == null && create) {
                note = factory.createText();
                part.getText().add(note);
            }
            return note;
        }

        public PartDefinition getPart(boolean create) {
            if (part == null) {
                part = mods.getPart().stream()
                        .filter(part -> ATTR_VOLUME.equals(part.getType()))
                        .findFirst().orElse(null);
            }
            if (part == null && create) {
                part = factory.createPartDefinition();
                part.setType(ATTR_VOLUME);
                mods.getPart().add(part);
            }
            return part;
        }

        public DetailDefinition getDetail(boolean create) {
            if (detail == null) {
                if (getPart(create) != null) {
                    detail = part.getDetail().stream().findFirst().orElse(null);
                }
            }
            if (detail == null && create) {
                detail = factory.createDetailDefinition();
                part.getDetail().add(detail);
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
