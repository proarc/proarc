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
import cz.cas.lib.proarc.common.mods.custom.PeriodicalVolumeMapper.PeriodicalVolume;
import cz.cas.lib.proarc.mods.DateDefinition;
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
 *
 * @author Jan Pokorsky
 */
final class PeriodicalVolumeMapper implements Mapper<PeriodicalVolume>{

    private final IdentifierMapper identMap = new IdentifierMapper();

    @Override
    public PeriodicalVolume map(ModsDefinition mods) {
        NodeLookup nlookup = new NodeLookup(mods);
        PeriodicalVolume volume = new PeriodicalVolume();
        volume.setIdentifiers(identMap.map(mods));
        if (nlookup.getPart(false) != null) {
            DateDefinition year = nlookup.getDate(false);
            volume.setYear(year == null ? null : year.getValue());

            volume.setVolumeNumber(MapperUtils.toString(nlookup.getNumber(false)));

            Text note = nlookup.getNote(false);
            volume.setNote(note == null ? null : note.getValue());
        }
        return volume;
    }

    @Override
    public ModsDefinition map(ModsDefinition mods, PeriodicalVolume volume) {
        NodeLookup nlookup = new NodeLookup(mods);
        identMap.map(mods, volume.getIdentifiers());
        updateNumber(volume, nlookup);
        updateYear(volume, nlookup);
        updateNote(volume, nlookup);
        new TypeOfResourceMapper().map(mods, TypeOfResourceMapper.Type.TEXT);
        return mods;
    }

    private static void updateNumber(PeriodicalVolume volume, NodeLookup nlookup) {
        if (volume.getVolumeNumber() != null) {
            nlookup.getNumber(true).setValue(volume.getVolumeNumber());
        } else {
            StringPlusLanguage number = nlookup.getNumber(false);
            if (number != null) {
                number.setValue(null);
            }
        }
    }

    private static void updateYear(PeriodicalVolume volume, NodeLookup nlookup) {
        if (volume.getYear() != null) {
            nlookup.getDate(true).setValue(volume.getYear());
        } else {
            DateDefinition date = nlookup.getDate(false);
            if (date != null) {
                date.setValue(null);
            }
        }
    }

    private static void updateNote(PeriodicalVolume volume, NodeLookup nlookup) {
        if (volume.getNote() != null) {
            nlookup.getNote(true).setValue(volume.getNote());
        } else {
            Text note = nlookup.getNote(false);
            if (note != null) {
                note.setValue(null);
            }
        }
    }

    private static final class NodeLookup {

        private final ObjectFactory factory = new ObjectFactory();
        private ModsDefinition mods;
        private PartDefinition part;
        private DetailDefinition detail;
        private DateDefinition date;
        private StringPlusLanguage number;
        private Text note;

        public NodeLookup(ModsDefinition mods) {
            this.mods = mods;
        }

        public DateDefinition getDate(boolean create) {
            if (date == null) {
                if (getPart(create) != null) {
                    date = part.getDate().stream().findFirst().orElse(null);
                }
            }
            if (date == null && create) {
                date = factory.createDateDefinition();
                part.getDate().add(date);
            }
            return date;
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
                part = mods.getPart().stream().findFirst().orElse(null);
            }
            if (part == null && create) {
                part = factory.createPartDefinition();
                mods.getPart().add(part);
            }
            return part;
        }

        public DetailDefinition getDetail(boolean create) {
            if (detail == null) {
                if (getPart(create) != null) {
                    detail = part.getDetail().stream()
                            .filter(d -> "volume".equals(d.getType()))
                            .findFirst().orElse(null);
                }
            }
            if (detail == null && create) {
                detail = factory.createDetailDefinition();
                detail.setType("volume");
                part.getDetail().add(detail);
            }
            return detail;
        }
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public static final class PeriodicalVolume {
        
        @XmlElement(name = ModsConstants.FIELD_IDENTIFIERS)
        private List<IdentifierItem> identifiers;
        @XmlElement(name = ModsConstants.FIELD_PER_VOLUME_YEAR)
        private String year;
        @XmlElement(name = ModsConstants.FIELD_PER_VOLUME_NUMBER)
        private String volumeNumber;
        @XmlElement(name = ModsConstants.FIELD_NOTE)
        private String note;

        public PeriodicalVolume() {
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

        public String getVolumeNumber() {
            return volumeNumber;
        }

        public void setVolumeNumber(String volumeNumber) {
            this.volumeNumber = volumeNumber;
        }

        public String getYear() {
            return year;
        }

        public void setYear(String year) {
            this.year = year;
        }
    }

}
