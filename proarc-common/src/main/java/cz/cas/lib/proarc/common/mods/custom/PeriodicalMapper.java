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

import cz.cas.lib.proarc.common.mods.custom.ArrayMapper.StringItem;
import cz.cas.lib.proarc.common.mods.custom.ClassificationMapper.ClassificationPair;
import cz.cas.lib.proarc.common.mods.custom.IdentifierMapper.IdentifierItem;
import cz.cas.lib.proarc.common.mods.custom.LanguageMapper.LanguageItem;
import cz.cas.lib.proarc.common.mods.custom.NameMapper.NameItem;
import cz.cas.lib.proarc.common.mods.custom.OriginInfoMapper.OriginInfoItem;
import cz.cas.lib.proarc.common.mods.custom.OriginInfoMapper.PublisherItem;
import cz.cas.lib.proarc.common.mods.custom.PeriodicalMapper.Periodical;
import cz.cas.lib.proarc.common.mods.custom.PhysicalDescriptionMapper.ExtentPair;
import cz.fi.muni.xkremser.editor.server.mods.LocationType;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.NoteType;
import cz.fi.muni.xkremser.editor.server.mods.ObjectFactory;
import cz.fi.muni.xkremser.editor.server.mods.PhysicalLocationType;
import cz.fi.muni.xkremser.editor.server.mods.RecordInfoType;
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
final class PeriodicalMapper implements Mapping.Mapper<Periodical> {

    private final OriginInfoMapper originInfoMap = new OriginInfoMapper();

    @Override
    public Periodical map(ModsType mods) {
        NodeLookup nlookup = new NodeLookup(mods);
        Periodical result = new Periodical();
        // identifiers
        IdentifierMapper identMap = new IdentifierMapper();
        result.setIdentifiers(identMap.map(mods));
        // sigla + shelf locators
        LocationType location = nlookup.getLocation(false);
        if (location != null) {
            PhysicalLocationType physicalLocation = nlookup.getPhysicalLocation(false);
            result.setSigla(physicalLocation == null ? null : physicalLocation.getValue());
            result.setShelfLocators(ArrayMapper.toStringItemList(location.getShelfLocator()));
        }
        // periodicity + publishers + printers
        List<OriginInfoItem> origins = originInfoMap.map(mods);
        result.setPublishers(OriginInfoMapper.filter(origins, true, PublisherItem.Role.PUBLISHER));
        result.setPrinters(OriginInfoMapper.filter(origins, true, PublisherItem.Role.PRINTER));
        result.setPeriodicities(ArrayMapper.toStringItemList(OriginInfoMapper.getFreqencies(origins)));
        // titles
        TitleInfoMapper titleMap = new TitleInfoMapper(mods);
        result.setTitles(ArrayMapper.toStringItemList(titleMap.getTitles()));
        result.setSubtitles(ArrayMapper.toStringItemList(titleMap.getSubtitles()));
        result.setAlternativeTitles(ArrayMapper.toStringItemList(titleMap.getAlternativeTitles()));
        result.setKeyTitles(ArrayMapper.toStringItemList(titleMap.getKeyTitles()));
        // authors + contributors
        NameMapper nameMap = new NameMapper();
        List<NameItem> allNames = nameMap.map(mods);
        result.setAuthors(NameMapper.filter(allNames, true, NameItem.NameRole.AUTHOR));
        result.setContributors(NameMapper.filter(allNames, true, NameItem.NameRole.CONTRIBUTOR));
        // languages
        LanguageMapper languageMap = new LanguageMapper();
        result.setLanguages(languageMap.map(mods));
        // classifications
        ClassificationMapper classificationMap = new ClassificationMapper();
        result.setClassifications(classificationMap.mapPairs(mods));
        // keywords
        SubjectMapper subjectMap = new SubjectMapper(mods);
        result.setKeywords(ArrayMapper.toStringItemList(subjectMap.getKeywords()));
        // physicalDescriptions
        PhysicalDescriptionMapper physicalDescriptionMap = new PhysicalDescriptionMapper();
        result.setPhysicalDescriptions(PhysicalDescriptionMapper.toPairs(physicalDescriptionMap.map(mods)));
        // recordOrigin
        JAXBElement<String> recordOrigin = nlookup.getRecordOrigin(false);
        result.setRecordOrigin(recordOrigin == null ? null : recordOrigin.getValue());
        // note
        NoteType note = nlookup.getNote(false);
        result.setNote(note == null ? null : note.getValue());
        return result;
    }

    @Override
    public ModsType map(ModsType mods, Periodical periodical) {
        NodeLookup nlookup = new NodeLookup(mods);
        // identifiers
        IdentifierMapper identMap = new IdentifierMapper();
        identMap.map(mods, periodical.getIdentifiers());
        // sigla + shelf locators
        if (periodical.getSigla() != null) {
            PhysicalLocationType physicalLocation = nlookup.getPhysicalLocation(true);
            physicalLocation.setValue(periodical.getSigla());
        } else {
            PhysicalLocationType physicalLocation = nlookup.getPhysicalLocation(false);
            if (physicalLocation != null) {
                physicalLocation.setValue(periodical.getSigla());
            }
        }
        if (MapperUtils.isEmpty(periodical.getShelfLocators())) {
            LocationType location = nlookup.getLocation(false);
            if (location != null && !location.getShelfLocator().isEmpty()) {
                location.getShelfLocator().clear();
            }
        } else {
            LocationType location = nlookup.getLocation(true);
            List<String> shelfLocators = location.getShelfLocator();
            shelfLocators.clear();
            shelfLocators.addAll(ArrayMapper.toStringList(periodical.getShelfLocators()));
        }
        // periodicity + publishers + printers
        originInfoMap.map(mods, periodical.getPublishers(), periodical.getPrinters(),
                ArrayMapper.toStringList(periodical.getPeriodicities()), OriginInfoMapper.ISSUANCE_CONTINUING);
        // titles
        TitleInfoMapper titleMap = new TitleInfoMapper(mods);
        titleMap.setTitles(ArrayMapper.toStringList(periodical.getTitles()),
                ArrayMapper.toStringList(periodical.getSubtitles()));
        titleMap.setKeyTitles(ArrayMapper.toStringList(periodical.getKeyTitles()));
        titleMap.setAlternativeTitles(ArrayMapper.toStringList(periodical.getAlternativeTitles()));
        // authors + contributors
        NameMapper nameMap = new NameMapper();
        nameMap.map(mods, periodical.getAuthors(), periodical.getContributors());
        // languages
        LanguageMapper languageMap = new LanguageMapper();
        languageMap.map(mods, periodical.getLanguages());
        // classifications
        ClassificationMapper classificationMap = new ClassificationMapper();
        classificationMap.mapPairs(mods, periodical.getClassifications());
        // keywords
        SubjectMapper subjectMap = new SubjectMapper(mods);
        subjectMap.setKeywords(ArrayMapper.toStringList(periodical.getKeywords()));
        // physicalDescriptions
        PhysicalDescriptionMapper physicalDescriptionMap = new PhysicalDescriptionMapper();
        physicalDescriptionMap.mapPairs(mods, periodical.getPhysicalDescriptions());
        // recordOrigin
        if (periodical.getRecordOrigin() != null) {
            nlookup.getRecordOrigin(true).setValue(periodical.getRecordOrigin());
        } else {
            JAXBElement<String> recordOrigin = nlookup.getRecordOrigin(false);
            if (recordOrigin != null) {
                recordOrigin.setValue(null);
            }
        }
        // note
        if (periodical.getNote() != null) {
            nlookup.getNote(true).setValue(periodical.getNote());
        } else {
            NoteType note = nlookup.getNote(false);
            if (note != null) {
                note.setValue(periodical.getNote());
            }
        }
        // typeOfResource
        new TypeOfResourceMapper().map(mods, TypeOfResourceMapper.Type.TEXT);
        return mods;
    }

    private static final class NodeLookup {
        private final ObjectFactory factory = new ObjectFactory();
        private final ModsType mods;
        private LocationType location;
        private PhysicalLocationType physicalLocation;
        private NoteType note;
        private RecordInfoType recordInfo;
        private JAXBElement<String> recordOrigin;


        public NodeLookup(ModsType mods) {
            this.mods = mods;
        }

        public LocationType getLocation(boolean create) {
            if (location == null) {
                location = MapperUtils.findFirst(mods.getModsGroup(), LocationType.class);
            }

            if (create && location == null) {
                location = factory.createLocationType();
                MapperUtils.add(mods, location);
            }
            return location;
        }

        public PhysicalLocationType getPhysicalLocation(boolean create) {
            if (physicalLocation == null) {
                if (getLocation(create) != null) {
                    physicalLocation = MapperUtils.findFirst(location.getPhysicalLocation(),
                            PhysicalLocationType.class);
                }
            }
            if (create && physicalLocation == null) {
                physicalLocation = factory.createPhysicalLocationType();
                location.getPhysicalLocation().add(physicalLocation);
            }
            return physicalLocation;
        }

        public NoteType getNote(boolean create) {
            if (note == null) {
                note = MapperUtils.findFirst(mods.getModsGroup(), NoteType.class);
            }
            if (create && note == null) {
                note = factory.createNoteType();
                MapperUtils.add(mods, note);
            }
            return note;
        }

        public RecordInfoType getRecordInfo(boolean create) {
            if (recordInfo == null) {
                recordInfo = MapperUtils.findFirst(mods.getModsGroup(), RecordInfoType.class);
            }
            if (create && recordInfo == null) {
                recordInfo = factory.createRecordInfoType();
                MapperUtils.add(mods, recordInfo);
            }
            return recordInfo;
        }

        public JAXBElement<String> getRecordOrigin(boolean create) {
            if (recordOrigin == null) {
                if (getRecordInfo(create) != null) {
                    List<JAXBElement<?>> group = recordInfo.getRecordContentSourceOrRecordCreationDateOrRecordChangeDate();
                    recordOrigin = (JAXBElement<String>) MapperUtils.findFirst(group, ObjectFactory._RecordInfoTypeRecordOrigin_QNAME);
                }
            }

            if (create && recordOrigin == null) {
                recordOrigin = factory.createRecordInfoTypeRecordOrigin(null);
                recordInfo.getRecordContentSourceOrRecordCreationDateOrRecordChangeDate()
                        .add(recordOrigin);
            }
            return recordOrigin;
        }
        
    }

    @XmlRootElement
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Periodical {

        @XmlElement(name = ModsConstants.FIELD_IDENTIFIERS)
        private List<IdentifierMapper.IdentifierItem> identifiers;
        @XmlElement(name = ModsConstants.FIELD_SIGLA)
        private String sigla;
        @XmlElement(name = ModsConstants.FIELD_SHELF_LOCATORS)
        private List<StringItem> shelfLocators;
        @XmlElement(name = ModsConstants.FIELD_PERIODICITIES)
        private List<StringItem> periodicities;
        @XmlElement(name = ModsConstants.FIELD_TITLES)
        private List<StringItem> titles;
        @XmlElement(name = ModsConstants.FIELD_SUBTITLES)
        private List<StringItem> subtitles;
        @XmlElement(name = ModsConstants.FIELD_KEY_TITLES)
        private List<StringItem> keyTitles;
        @XmlElement(name = ModsConstants.FIELD_ALTERNATIVE_TITLES)
        private List<StringItem> alternativeTitles;
        @XmlElement(name = ModsConstants.FIELD_AUTHORS)
        private List<NameItem> authors;
        @XmlElement(name = ModsConstants.FIELD_CONTRIBUTORS)
        private List<NameItem> contributors;
        @XmlElement(name = ModsConstants.FIELD_PRINTERS)
        private List<PublisherItem> printers;
        @XmlElement(name = ModsConstants.FIELD_PUBLISHERS)
        private List<PublisherItem> publishers;
        @XmlElement(name = ModsConstants.FIELD_LANGUAGES)
        private List<LanguageItem> languages;
        @XmlElement(name = ModsConstants.FIELD_CLASSIFICATIONS)
        private List<ClassificationPair> classifications;
        /**
         * keywords from first mods/subject/topic*
         * <p/><b>NOTE: KNAV Kramerius 3 format</b>
         * @see <a href='https://github.com/ceskaexpedice/kramerius/blob/master/import-cmdtool/src/main/resources/model_periodical_MODS.xsl'>model_periodical_MODS.xsl</a>
         */
        @XmlElement(name = ModsConstants.FIELD_KEYWORDS)
        private List<StringItem> keywords;
        @XmlElement(name = ModsConstants.FIELD_PHYSICAL_DESCRIPTIONS)
        private List<ExtentPair> physicalDescriptions;
        /**
         * Record origin info from first {@code mods/recordInfo/recordOrigin}.
         * <p/><b>NOTE: KNAV Kramerius 3 format /Periodical/DescriptionBasedIssue</b>
         * @see <a href='https://github.com/ceskaexpedice/kramerius/blob/master/import-cmdtool/src/main/resources/model_periodical_MODS.xsl'>model_periodical_MODS.xsl</a>
         */
        @XmlElement(name = ModsConstants.FIELD_RECORD_ORIGIN)
        private String recordOrigin;
        @XmlElement(name = ModsConstants.FIELD_NOTE)
        private String note;
        
        public Periodical() {
        }

        public List<StringItem> getAlternativeTitles() {
            return alternativeTitles;
        }

        public void setAlternativeTitles(List<StringItem> alternativeTitles) {
            this.alternativeTitles = alternativeTitles;
        }

        public List<NameItem> getAuthors() {
            return authors;
        }

        public void setAuthors(List<NameItem> authors) {
            this.authors = authors;
        }

        public List<ClassificationPair> getClassifications() {
            return classifications;
        }

        public void setClassifications(List<ClassificationPair> classifications) {
            this.classifications = classifications;
        }

        public List<NameItem> getContributors() {
            return contributors;
        }

        public void setContributors(List<NameItem> contributors) {
            this.contributors = contributors;
        }

        public List<IdentifierItem> getIdentifiers() {
            return identifiers;
        }

        public void setIdentifiers(List<IdentifierItem> identifiers) {
            this.identifiers = identifiers;
        }

        public List<StringItem> getKeyTitles() {
            return keyTitles;
        }

        public void setKeyTitles(List<StringItem> keyTitles) {
            this.keyTitles = keyTitles;
        }

        public List<StringItem> getKeywords() {
            return keywords;
        }

        public void setKeywords(List<StringItem> keywords) {
            this.keywords = keywords;
        }

        public List<LanguageItem> getLanguages() {
            return languages;
        }

        public void setLanguages(List<LanguageItem> languages) {
            this.languages = languages;
        }

        public String getNote() {
            return note;
        }

        public void setNote(String note) {
            this.note = note;
        }

        public List<StringItem> getPeriodicities() {
            return periodicities;
        }

        public void setPeriodicities(List<StringItem> periodicities) {
            this.periodicities = periodicities;
        }

        public List<ExtentPair> getPhysicalDescriptions() {
            return physicalDescriptions;
        }

        public void setPhysicalDescriptions(List<ExtentPair> physicalDescriptions) {
            this.physicalDescriptions = physicalDescriptions;
        }

        public List<PublisherItem> getPrinters() {
            return printers;
        }

        public void setPrinters(List<PublisherItem> printers) {
            this.printers = printers;
        }

        public List<PublisherItem> getPublishers() {
            return publishers;
        }

        public void setPublishers(List<PublisherItem> publishers) {
            this.publishers = publishers;
        }

        public String getRecordOrigin() {
            return recordOrigin;
        }

        public void setRecordOrigin(String recordOrigin) {
            this.recordOrigin = recordOrigin;
        }

        public List<StringItem> getShelfLocators() {
            return shelfLocators;
        }

        public void setShelfLocators(List<StringItem> shelfLocators) {
            this.shelfLocators = shelfLocators;
        }

        public String getSigla() {
            return sigla;
        }

        public void setSigla(String sigla) {
            this.sigla = sigla;
        }

        public List<StringItem> getSubtitles() {
            return subtitles;
        }

        public void setSubtitles(List<StringItem> subtitles) {
            this.subtitles = subtitles;
        }

        public List<StringItem> getTitles() {
            return titles;
        }

        public void setTitles(List<StringItem> titles) {
            this.titles = titles;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final Periodical other = (Periodical) obj;
            if (this.identifiers != other.identifiers && (this.identifiers == null || !this.identifiers.equals(other.identifiers))) {
                return false;
            }
            if ((this.sigla == null) ? (other.sigla != null) : !this.sigla.equals(other.sigla)) {
                return false;
            }
            if (this.shelfLocators != other.shelfLocators && (this.shelfLocators == null || !this.shelfLocators.equals(other.shelfLocators))) {
                return false;
            }
            if (this.periodicities != other.periodicities && (this.periodicities == null || !this.periodicities.equals(other.periodicities))) {
                return false;
            }
            if (this.titles != other.titles && (this.titles == null || !this.titles.equals(other.titles))) {
                return false;
            }
            if (this.subtitles != other.subtitles && (this.subtitles == null || !this.subtitles.equals(other.subtitles))) {
                return false;
            }
            if (this.keyTitles != other.keyTitles && (this.keyTitles == null || !this.keyTitles.equals(other.keyTitles))) {
                return false;
            }
            if (this.alternativeTitles != other.alternativeTitles && (this.alternativeTitles == null || !this.alternativeTitles.equals(other.alternativeTitles))) {
                return false;
            }
            if (this.authors != other.authors && (this.authors == null || !this.authors.equals(other.authors))) {
                return false;
            }
            if (this.contributors != other.contributors && (this.contributors == null || !this.contributors.equals(other.contributors))) {
                return false;
            }
            if (this.printers != other.printers && (this.printers == null || !this.printers.equals(other.printers))) {
                return false;
            }
            if (this.publishers != other.publishers && (this.publishers == null || !this.publishers.equals(other.publishers))) {
                return false;
            }
            if (this.languages != other.languages && (this.languages == null || !this.languages.equals(other.languages))) {
                return false;
            }
            if (this.classifications != other.classifications && (this.classifications == null || !this.classifications.equals(other.classifications))) {
                return false;
            }
            if (this.keywords != other.keywords && (this.keywords == null || !this.keywords.equals(other.keywords))) {
                return false;
            }
            if (this.physicalDescriptions != other.physicalDescriptions && (this.physicalDescriptions == null || !this.physicalDescriptions.equals(other.physicalDescriptions))) {
                return false;
            }
            if ((this.recordOrigin == null) ? (other.recordOrigin != null) : !this.recordOrigin.equals(other.recordOrigin)) {
                return false;
            }
            if ((this.note == null) ? (other.note != null) : !this.note.equals(other.note)) {
                return false;
            }
            return true;
        }

    }

}
