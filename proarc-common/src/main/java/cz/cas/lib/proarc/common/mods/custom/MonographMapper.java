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
import cz.cas.lib.proarc.common.mods.custom.ArrayMapper.StringItem;
import cz.cas.lib.proarc.common.mods.custom.ClassificationMapper.ClassificationPair;
import cz.cas.lib.proarc.common.mods.custom.IdentifierMapper.IdentifierItem;
import cz.cas.lib.proarc.common.mods.custom.LanguageMapper.LanguageItem;
import cz.cas.lib.proarc.common.mods.custom.Mapping.Mapper;
import cz.cas.lib.proarc.common.mods.custom.MonographMapper.Monograph;
import cz.cas.lib.proarc.common.mods.custom.NameMapper.NameItem;
import cz.cas.lib.proarc.common.mods.custom.OriginInfoMapper.PublisherItem;
import cz.cas.lib.proarc.common.mods.custom.PhysicalDescriptionMapper.ExtentPair;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NoteDefinition;
import cz.cas.lib.proarc.mods.ObjectFactory;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import java.util.List;

/**
 *
 * @author Jan Pokorsky
 */
final class MonographMapper implements Mapper<Monograph> {

    @Override
    public Monograph map(ModsDefinition mods) {
        NodeLookup nlookup = new NodeLookup(mods);
        Monograph result = new Monograph();
        // identifiers
        IdentifierMapper identMap = new IdentifierMapper();
        result.setIdentifiers(identMap.map(mods));
        // sigla + shelf locators
        LocationDefinition location = nlookup.getLocation(false);
        if (location != null) {
            PhysicalLocationDefinition physicalLocation = nlookup.getPhysicalLocation(false);
            result.setSigla(physicalLocation == null ? null : physicalLocation.getValue());
            result.setShelfLocators(ArrayMapper.toStringPlusLanguageItemList(location.getShelfLocator()));
        }
        // publishers + printers
        OriginInfoMapper originInfoMap = new OriginInfoMapper();
        List<OriginInfoMapper.OriginInfoItem> origins = originInfoMap.map(mods);
        result.setPublishers(OriginInfoMapper.filter(origins, true, PublisherItem.Role.PUBLISHER));
        result.setPrinters(OriginInfoMapper.filter(origins, true, PublisherItem.Role.PRINTER));
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
        // physicalDescriptions + preservation
        PhysicalDescriptionMapper physicalDescriptionMap = new PhysicalDescriptionMapper();
        List<ArrayItem> pdItems = physicalDescriptionMap.map(mods);
        result.setPhysicalDescriptions(PhysicalDescriptionMapper.toPairs(pdItems));
        result.setPreservationTreatment(PhysicalDescriptionMapper.getPreservationTreatment(pdItems));
        result.setPreservationStateOfArt(PhysicalDescriptionMapper.getPreservationStateOfArt(pdItems));
        // recordOrigin
        StringPlusLanguage recordOrigin = nlookup.getRecordOrigin(false);
        result.setRecordOrigin(recordOrigin == null ? null : recordOrigin.getValue());
        // note
        NoteDefinition note = nlookup.getNote(false);
        result.setNote(note == null ? null : note.getValue());
        return result;
    }

    @Override
    public ModsDefinition map(ModsDefinition mods, Monograph monograph) {
        NodeLookup nlookup = new NodeLookup(mods);
        // identifiers
        IdentifierMapper identMap = new IdentifierMapper();
        identMap.map(mods, monograph.getIdentifiers());
        // sigla + shelf locators
        if (monograph.getSigla() != null) {
            PhysicalLocationDefinition physicalLocation = nlookup.getPhysicalLocation(true);
            physicalLocation.setValue(monograph.getSigla());
        } else {
            PhysicalLocationDefinition physicalLocation = nlookup.getPhysicalLocation(false);
            if (physicalLocation != null) {
                physicalLocation.setValue(monograph.getSigla());
            }
        }
        if (MapperUtils.isEmpty(monograph.getShelfLocators())) {
            LocationDefinition location = nlookup.getLocation(false);
            if (location != null && !location.getShelfLocator().isEmpty()) {
                location.getShelfLocator().clear();
            }
        } else {
            LocationDefinition location = nlookup.getLocation(true);
            List<StringPlusLanguage> shelfLocators = location.getShelfLocator();
            shelfLocators.clear();
            shelfLocators.addAll(ArrayMapper.toStringPlusLanguageList(monograph.getShelfLocators()));
        }
        // periodicity + publishers + printers
        OriginInfoMapper originInfoMap = new OriginInfoMapper();
        originInfoMap.map(mods, monograph.getPublishers(), monograph.getPrinters(), OriginInfoMapper.ISSUANCE_MONOGRAPHIC);
        // titles
        TitleInfoMapper titleMap = new TitleInfoMapper(mods);
        titleMap.setTitles(ArrayMapper.toStringList(monograph.getTitles()),
                ArrayMapper.toStringList(monograph.getSubtitles()));
        titleMap.setKeyTitles(ArrayMapper.toStringList(monograph.getKeyTitles()));
        titleMap.setAlternativeTitles(ArrayMapper.toStringList(monograph.getAlternativeTitles()));
        // authors + contributors
        NameMapper nameMap = new NameMapper();
        nameMap.map(mods, monograph.getAuthors(), monograph.getContributors());
        // languages
        LanguageMapper languageMap = new LanguageMapper();
        languageMap.map(mods, monograph.getLanguages());
        // classifications
        ClassificationMapper classificationMap = new ClassificationMapper();
        classificationMap.mapPairs(mods, monograph.getClassifications());
        // keywords
        SubjectMapper subjectMap = new SubjectMapper(mods);
        subjectMap.setKeywords(ArrayMapper.toStringList(monograph.getKeywords()));
        // physicalDescriptions + preservation
        PhysicalDescriptionMapper physicalDescriptionMap = new PhysicalDescriptionMapper();
        physicalDescriptionMap.map(mods, monograph.getPhysicalDescriptions(),
                monograph.getPreservationTreatment(), monograph.getPreservationStateOfArt());
        // recordOrigin
        if (monograph.getRecordOrigin() != null) {
            nlookup.getRecordOrigin(true).setValue(monograph.getRecordOrigin());
        } else {
            StringPlusLanguage recordOrigin = nlookup.getRecordOrigin(false);
            if (recordOrigin != null) {
                recordOrigin.setValue(null);
            }
        }
        // note
        if (monograph.getNote() != null) {
            nlookup.getNote(true).setValue(monograph.getNote());
        } else {
            NoteDefinition note = nlookup.getNote(false);
            if (note != null) {
                note.setValue(monograph.getNote());
            }
        }
        // typeOfResource
        new TypeOfResourceMapper().map(mods, TypeOfResourceMapper.Type.TEXT);
        return mods;
    }

    private static final class NodeLookup {
        private final ObjectFactory factory = new ObjectFactory();
        private final ModsDefinition mods;
        private LocationDefinition location;
        private PhysicalLocationDefinition physicalLocation;
        private NoteDefinition note;
        private RecordInfoDefinition recordInfo;
        private StringPlusLanguage recordOrigin;


        public NodeLookup(ModsDefinition mods) {
            this.mods = mods;
        }

        public LocationDefinition getLocation(boolean create) {
            if (location == null) {
                location = mods.getLocation().stream().findFirst().orElse(null);
            }

            if (create && location == null) {
                location = factory.createLocationDefinition();
                mods.getLocation().add(location);
            }
            return location;
        }

        public PhysicalLocationDefinition getPhysicalLocation(boolean create) {
            if (physicalLocation == null) {
                if (getLocation(create) != null) {
                    physicalLocation = location.getPhysicalLocation().stream()
                            .findFirst().orElse(null);
                }
            }
            if (create && physicalLocation == null) {
                physicalLocation = factory.createPhysicalLocationDefinition();
                location.getPhysicalLocation().add(physicalLocation);
            }
            return physicalLocation;
        }

        public NoteDefinition getNote(boolean create) {
            if (note == null) {
                note = mods.getNote().stream().findFirst().orElse(null);
            }
            if (create && note == null) {
                note = factory.createNoteDefinition();
                mods.getNote().add(note);
            }
            return note;
        }

        public RecordInfoDefinition getRecordInfo(boolean create) {
            if (recordInfo == null) {
                recordInfo = mods.getRecordInfo().stream().findFirst().orElse(null);
            }
            if (create && recordInfo == null) {
                recordInfo = factory.createRecordInfoDefinition();
                mods.getRecordInfo().add(recordInfo);
            }
            return recordInfo;
        }

        public StringPlusLanguage getRecordOrigin(boolean create) {
            if (recordOrigin == null) {
                if (getRecordInfo(create) != null) {
                    recordOrigin = recordInfo.getRecordOrigin().stream().findFirst().orElse(null);
                }
            }

            if (create && recordOrigin == null) {
                recordOrigin = factory.createStringPlusLanguage();
                recordInfo.getRecordOrigin().add(recordOrigin);
            }
            return recordOrigin;
        }

    }

    @XmlRootElement()
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Monograph {

        @XmlElement(name = ModsConstants.FIELD_IDENTIFIERS)
        private List<IdentifierMapper.IdentifierItem> identifiers;
        @XmlElement(name = ModsConstants.FIELD_SIGLA)
        private String sigla;
        @XmlElement(name = ModsConstants.FIELD_SHELF_LOCATORS)
        private List<StringItem> shelfLocators;
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
        @XmlElement(name = ModsConstants.FIELD_KEYWORDS)
        private List<StringItem> keywords;
        @XmlElement(name = ModsConstants.FIELD_PHYSICAL_DESCRIPTIONS)
        private List<ExtentPair> physicalDescriptions;
        @XmlElement(name = ModsConstants.FIELD_PRESERVATION_TREATMENT)
        private String preservationTreatment;
        @XmlElement(name = ModsConstants.FIELD_PRESERVATION_STATEOFART)
        private String preservationStateOfArt;
        @XmlElement(name = ModsConstants.FIELD_RECORD_ORIGIN)
        private String recordOrigin;
        @XmlElement(name = ModsConstants.FIELD_NOTE)
        private String note;

        public Monograph() {
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

        public List<ExtentPair> getPhysicalDescriptions() {
            return physicalDescriptions;
        }

        public void setPhysicalDescriptions(List<ExtentPair> physicalDescriptions) {
            this.physicalDescriptions = physicalDescriptions;
        }

        public String getPreservationStateOfArt() {
            return preservationStateOfArt;
        }

        public void setPreservationStateOfArt(String preservationStateOfArt) {
            this.preservationStateOfArt = preservationStateOfArt;
        }

        public String getPreservationTreatment() {
            return preservationTreatment;
        }

        public void setPreservationTreatment(String preservationTreatment) {
            this.preservationTreatment = preservationTreatment;
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

    }
}
