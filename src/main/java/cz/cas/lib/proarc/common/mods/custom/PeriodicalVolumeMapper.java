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
import cz.fi.muni.xkremser.editor.server.mods.BaseDateType;
import cz.fi.muni.xkremser.editor.server.mods.DetailType;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.ObjectFactory;
import cz.fi.muni.xkremser.editor.server.mods.PartType;
import cz.fi.muni.xkremser.editor.server.mods.UnstructuredText;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource;
import java.util.List;
import javax.xml.bind.JAXBElement;
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
    public PeriodicalVolume map(ModsType mods) {
        NodeLookup nlookup = new NodeLookup(mods);
        PeriodicalVolume volume = new PeriodicalVolume();
        volume.setIdentifiers(identMap.map(mods));
        if (nlookup.getPart(false) != null) {
            BaseDateType year = nlookup.getDate(false);
            volume.setYear(year == null ? null : year.getValue());

            volume.setVolumeNumber(MapperUtils.toString(nlookup.getNumber(false)));

            UnstructuredText note = nlookup.getNote(false);
            volume.setNote(note == null ? null : note.getValue());
        }
        return volume;
    }

    @Override
    public ModsType map(ModsType mods, PeriodicalVolume volume) {
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
            JAXBElement<String> number = nlookup.getNumber(false);
            if (number != null) {
                number.setValue(null);
            }
        }
    }

    private static void updateYear(PeriodicalVolume volume, NodeLookup nlookup) {
        if (volume.getYear() != null) {
            nlookup.getDate(true).setValue(volume.getYear());
        } else {
            BaseDateType date = nlookup.getDate(false);
            if (date != null) {
                date.setValue(null);
            }
        }
    }

    private static void updateNote(PeriodicalVolume volume, NodeLookup nlookup) {
        if (volume.getNote() != null) {
            nlookup.getNote(true).setValue(volume.getNote());
        } else {
            UnstructuredText note = nlookup.getNote(false);
            if (note != null) {
                note.setValue(null);
            }
        }
    }

    private static final class NodeLookup {

        private final ObjectFactory factory = new ObjectFactory();
        private ModsType mods;
        private PartType part;
        private DetailType detail;
        private BaseDateType date;
        private JAXBElement<String> number;
        private UnstructuredText note;

        public NodeLookup(ModsType mods) {
            this.mods = mods;
        }

        public BaseDateType getDate(boolean create) {
            if (date == null) {
                if (getPart(create) != null) {
                    date = MapperUtils.findFirst(part.getDetailOrExtentOrDate(), BaseDateType.class);
                }
            }
            if (date == null && create) {
                date = factory.createBaseDateType();
                MapperUtils.add(part, date);
            }
            return date;
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
                part = MapperUtils.findFirst(mods.getModsGroup(), PartType.class);
            }
            if (part == null && create) {
                part = factory.createPartType();
                MapperUtils.add(mods, part);
            }
            return part;
        }

        public DetailType getDetail(boolean create) {
            if (detail == null) {
                if (getPart(create) != null) {
                    detail = MapperUtils.findFirst(
                            MapperUtils.find(part.getDetailOrExtentOrDate(), DetailType.class),
                            MapperUtils.detailSelector("volume"));
                }
            }
            if (detail == null && create) {
                detail = factory.createDetailType();
                detail.setType("volume");
                MapperUtils.add(part, detail);
            }
            return detail;
        }
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public static final class PeriodicalVolume {
        
        @XmlElement(name = ModsCustomDataSource.FIELD_IDENTIFIERS)
        private List<IdentifierItem> identifiers;
        @XmlElement(name = ModsCustomDataSource.FIELD_PER_VOLUME_YEAR)
        private String year;
        @XmlElement(name = ModsCustomDataSource.FIELD_PER_VOLUME_NUMBER)
        private String volumeNumber;
        @XmlElement(name = ModsCustomDataSource.FIELD_NOTE)
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
