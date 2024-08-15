/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.mods.ndk;

import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.mods.AbstractDefinition;
import cz.cas.lib.proarc.mods.CodeOrText;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.DetailDefinition;
import cz.cas.lib.proarc.mods.DigitalOriginDefinition;
import cz.cas.lib.proarc.mods.ExtentDefinition;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.LanguageDefinition;
import cz.cas.lib.proarc.mods.LanguageTermDefinition;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NameDefinition;
import cz.cas.lib.proarc.mods.NamePartDefinition;
import cz.cas.lib.proarc.mods.NoteDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.PlaceDefinition;
import cz.cas.lib.proarc.mods.PlaceTermDefinition;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

/**
 * The mapping helper.
 *
 * @author Jan Pokorsky
 */
public final class MapperUtils {

    public static ModsDefinition addPid(ModsDefinition mods, String pid) {
        String uuid = FoxmlUtils.pidAsUuid(pid);
        for (IdentifierDefinition id : mods.getIdentifier()) {
            if (uuid.equals(id.getValue()) && "uuid".equals(id.getType())) {
                return mods;
            }
        }
        IdentifierDefinition id = new IdentifierDefinition();
        id.setValue(uuid);
        id.setType("uuid");
        mods.getIdentifier().add(0, id);
        return mods;
    }

    public static GenreDefinition addGenre(ModsDefinition mods, String value) {
        List<GenreDefinition> genres = mods.getGenre();
        GenreDefinition reqGenre = null;
        for (GenreDefinition genre : genres) {
            if (value.equals(genre.getValue())) {
                reqGenre = genre;
                break;
            }
        }
        if (reqGenre == null) {
            reqGenre = new GenreDefinition();
            reqGenre.setValue(value);
            genres.add(0, reqGenre);
        }
        return reqGenre;
    }

    public static void removeGenre(ModsDefinition mods, String value) {
        Iterator<GenreDefinition> iterator = mods.getGenre().iterator();
        while (iterator.hasNext()) {
            GenreDefinition genre = iterator.next();
            if (value.equals(genre.getValue())) {
                iterator.remove();
            }
        }
    }

    public static GenreDefinition replaceGenre(ModsDefinition mods, String oldValue, String newValue) {
        List<GenreDefinition> genres = mods.getGenre();
        for (GenreDefinition genre : genres) {
            if (oldValue.equals(genre.getValue())) {
                genre.setValue(newValue);
                return genre;
            }
        }
        return addGenre(mods, newValue);
    }

    /**
     * Creates title according to LoC {@code MODS->DC} mapping.
     * @param ti MODS titleInfo
     * @return string for DC title
     */
    public static String createTitleString(TitleInfoDefinition ti) {
        StringBuilder title = new StringBuilder();
        addNonSort(title, ti);
        addTitle(title, ti);
        addSubTitle(title, ti);
        addPartNumber(title, ti);
        addPartName(title, ti);
        return title.toString();
    }

    /**
     * Builds title from {@code titleInfo} subelements.
     * @param title the result title
     * @param titleParts parts to add
     * @param prefix an optional parts prefix. Added in case the passed title is not empty.
     * @return the result title
     */
    static StringBuilder addTitlePart(StringBuilder title, List<StringPlusLanguage> titleParts, String prefix) {
        if (!titleParts.isEmpty()) {
            String value = toValue(titleParts.get(0).getValue());
            if (value != null) {
                if (prefix != null && title.length() > 0) {
                    title.append(prefix);
                }
                title.append(value);
            }
        }
        return title;
    }

    static StringBuilder addNonSort(StringBuilder title, TitleInfoDefinition ti) {
        List<StringPlusLanguage> listNonSort = new ArrayList<>();
        listNonSort.addAll(ti.getNonSort());
        return addTitlePart(title, listNonSort, null);
    }

    static StringBuilder addTitle(StringBuilder title, TitleInfoDefinition ti) {
        return addTitlePart(title, ti.getTitle(), " ");
    }

    static StringBuilder addSubTitle(StringBuilder title, TitleInfoDefinition ti) {
        return addTitlePart(title, ti.getSubTitle(), ": ");
    }

    static StringBuilder addPartName(StringBuilder title, TitleInfoDefinition ti) {
        return addTitlePart(title, ti.getPartName(), ". ");
    }

    static StringBuilder addPartNumber(StringBuilder title, TitleInfoDefinition ti) {
        return addTitlePart(title, ti.getPartNumber(), ". ");
    }

    // mods/language/languageTerm @type=code, @authority="iso639‐2b"
    // XXX should it be really checked?
    public static void fillLanguage(ModsDefinition mods) {
//        fillLanguage(mods.getLanguage());
    }

    static void fillLanguage(List<LanguageDefinition> languages) {
//        for (LanguageDefinition language : languages) {
//            for (LanguageTermDefinition languageTerm : language.getLanguageTerm()) {
//                CodeOrText type = languageTerm.getType();
//                if (type == null) {
//                    languageTerm.setType(CodeOrText.CODE);
//                }
//                String authority = toValue(languageTerm.getAuthority());
//                if (authority == null) {
//                    languageTerm.setAuthority("iso639‐2b");
//                }
//            }
//        }
    }

    public static void fillRecordInfo(ModsDefinition mods) {
        Date now = new Date();
        List<RecordInfoDefinition> recordInfos = mods.getRecordInfo();
        if (recordInfos.isEmpty()) {
            recordInfos.add(new RecordInfoDefinition());
        }
        RecordInfoDefinition recordInfo = recordInfos.get(0);
        // mods/recordInfo/recordCreationDate@encoding="iso8601"
        List<DateDefinition> recordCreationDates = recordInfo.getRecordCreationDate();
        DateDefinition creationDate = null;
        for (DateDefinition date : recordCreationDates) {
            if ("iso8601".equals(date.getEncoding())) {
                creationDate = date;
            }
        }
        // mods/recordInfo/recordCreationDate=now if missing
        SimpleDateFormat DATETIME_ISO8601 = new SimpleDateFormat("yyyyMMdd'T'HHmmss");
        if (creationDate == null) {
            // remove dates with other encodings
            recordCreationDates.clear();
            creationDate = new DateDefinition();
            creationDate.setEncoding("iso8601");
            creationDate.setValue(DATETIME_ISO8601.format(now));
            recordCreationDates.add(creationDate);
        }
        // mods/recordInfo/recordChangeDate@encoding="iso8601"
        // mods/recordInfo/recordChangeDate=now
        List<DateDefinition> recordChangeDates = recordInfo.getRecordChangeDate();
        recordChangeDates.clear();
        DateDefinition changeDate = new DateDefinition();
        changeDate.setEncoding("iso8601");
        changeDate.setValue(DATETIME_ISO8601.format(now));
        recordChangeDates.add(changeDate);
        // mods/recordInfo/recordContentSource@authority="marcorg"
        for (StringPlusLanguagePlusAuthority contentSource : recordInfo.getRecordContentSource()) {
            if (contentSource.getAuthority() == null) {
                contentSource.setAuthority("marcorg");
            }
        }
        fillLanguage(recordInfo.getLanguageOfCataloging());
        if (recordInfo.getRecordOrigin().size() != 0 && recordInfo.getRecordOrigin().get(0).getValue().startsWith("Converted from")) {
            NoteDefinition note = new NoteDefinition();
            note.setValue(recordInfo.getRecordOrigin().get(0).getValue());
            recordInfo.getRecordInfoNote().add(note);
            recordInfo.getRecordOrigin().get(0).setValue("machine generated");
        }
    }

    public static void fillLocation(ModsDefinition mods) {
        PhysicalLocationDefinition physical = null;
        for (LocationDefinition location : mods.getLocation()) {
            for (PhysicalLocationDefinition physicalLocation :location.getPhysicalLocation()) {
                if (physicalLocation.getValue() != null) {
                    physical = physicalLocation;
                }
            }
        }
        if (physical != null) {
            if (physical.getAuthority() == null) {
                physical.setAuthority("siglaADR");
            }
            for (LocationDefinition location : mods.getLocation()) {
                if (!location.getShelfLocator().isEmpty() && location.getPhysicalLocation().isEmpty()) {
                    location.getPhysicalLocation().add(physical);
                }
            }
        }
    }

    public static void fillAbstract(ModsDefinition mods) {
        List<AbstractDefinition> abstracts = mods.getAbstract();
        List<AbstractDefinition> newAbstract = new ArrayList<>();
        for (AbstractDefinition abs : abstracts) {
            if (abs.getValue().length() > 2700) {
                List<String> splitedValues = splitAfter2700Characters(abs.getValue());
                for (String value : splitedValues) {
                    AbstractDefinition abstractDefinition = new AbstractDefinition();
                    abstractDefinition.setValue(value);
                    newAbstract.add(abstractDefinition);
                }
            } else {
                newAbstract.add(abs);
            }
        }
        mods.getAbstract().clear();
        mods.getAbstract().addAll(newAbstract);
    }

    public static void addStringPlusLanguage(List<ElementType> dcElms, List<? extends StringPlusLanguage> modsValues) {
        for (StringPlusLanguage modsValue : modsValues) {
            // XXX lang?
            if (modsValue.getValue() != null && modsValue.getValue().length() > 2700) {
                List<String> splitValue = splitAfter2700Characters(modsValue.getValue());
                for (String value : splitValue) {
                    addElementType(dcElms, value, null);
                }
            } else {
                addElementType(dcElms, modsValue.getValue(), null);
            }
        }
    }

    public static void addDigitalOrigin(List<ElementType> dcElms, List<? extends DigitalOriginDefinition> modsValues) {
        for (DigitalOriginDefinition modsValue : modsValues) {
            if (modsValue.value() != null && modsValue.value().length() > 2700) {
                List<String> splitValue = splitAfter2700Characters(modsValue.value());
                for (String value : splitValue) {
                    addElementType(dcElms, value, null);
                }
            } else {
                addElementType(dcElms, modsValue.value(), null);
            }
        }
    }

    private static List<String> splitAfter2700Characters(String value) {
        List<String> tmp = new ArrayList<>();
        int index = 0;
        while (index < value.length()) {
            tmp.add(value.substring(index, Math.min(index + 2700, value.length())));
            index += 2700;
        }
        return tmp;
    }

    public static void addName(List<NameDefinition> modsNames, List<ElementType> dcElms) {
        for (NameDefinition name : modsNames) {
            StringBuilder sbName = new StringBuilder();
            StringBuilder sbFamily = new StringBuilder();
            StringBuilder sbGiven = new StringBuilder();
            StringBuilder sbDate = new StringBuilder();
            for (NamePartDefinition namePart : name.getNamePart()) {
                String type = namePart.getType();
                if (type == null) {
                    if (namePart.getValue().contains(", ")) {
                        sbName.append(namePart.getValue());
                    } else {
                        sbName.append(namePart.getValue()).append(' ');
                    }
                } else if ("family".equals(type)) {
                    sbFamily.append(namePart.getValue()).append(' ');
                } else if ("given".equals(type)) {
                    sbGiven.append(namePart.getValue()).append(' ');
                } else if ("date".equals(type)) {
                    sbDate.append(namePart.getValue()).append(", ");
                }
            }
            if (sbFamily.length() > 0) {
                sbName.append(sbFamily.substring(0, sbFamily.length() - 1));
            }
            if (sbGiven.length() > 0) {
                if (sbName.length() > 0) {
                    sbName.append(", ");
                }
                sbName.append(sbGiven.substring(0, sbGiven.length() - 1));
            }
            if (sbDate.length() > 0) {
                if (sbName.length() > 0) {
                    sbName.append(", ");
                }
                sbName.append(sbDate.substring(0, sbDate.length() - 2));
            }
            if (sbName.length() > 0) {
                addElementType(dcElms, sbName.toString());
            }
        }
    }

    public static void addNameIdentifier(List<NameDefinition> modsNames, List<ElementType> dcElms) {
        for (NameDefinition name : modsNames) {
            if (name.getNameIdentifier() != null && name.getNameIdentifier().size() > 0) {
                for (IdentifierDefinition identifier : name.getNameIdentifier()) {
                    addElementType(dcElms, identifier.getValue());
                }
            }
        }
    }

    static void addDetailWithPageRangeToPart(ModsDefinition mods) {
        List<ExtentDefinition> extents = new ArrayList<>();
        for (PartDefinition part : mods.getPart()) {
            if (part.getType() == null) {
                for (ExtentDefinition extentDefinition : part.getExtent()) {
                    extents.add(extentDefinition);
                }
            }
        }
        if (extents.size() > 0) {
            mods.getPart().clear();

            for (ExtentDefinition extentDefinition : extents) {

                PartDefinition part = new PartDefinition();
                part.getExtent().add(extentDefinition);
                part.setType(extentDefinition.getUnit());
                extentDefinition.setUnit("pages");
                mods.getPart().add(part);
            }

            for (PartDefinition part : mods.getPart()) {
                if (part.getExtent() != null && part.getExtent().size() != 0) {
                    ExtentDefinition extent = part.getExtent().get(0);
                    StringPlusLanguage start = extent.getStart();
                    StringPlusLanguage end = extent.getEnd();
                    DetailDefinition detail = new DetailDefinition();
                    //        detail.setType(extent.getUnit());
                    StringPlusLanguage detailNumber = new StringPlusLanguage();
                    detailNumber.setValue((start == null ? "" : start.getValue())+ "-" + (end == null ? "" : end.getValue()));
                    detail.getNumber().add(detailNumber);
                    part.getDetail().clear();
                    part.getDetail().add(detail);
                }
            }
        }
        for (PartDefinition part : mods.getPart()) {
            ExtentDefinition extentDefinition = null;
            if (part.getExtent() != null && part.getExtent().size() > 0 && part.getExtent().get(0) != null) {
                extentDefinition = part.getExtent().get(0);
            }
            if (extentDefinition != null) {
                if (extentDefinition.getUnit() == null) {
                    extentDefinition.setUnit("pages");
                }
                StringPlusLanguage start = extentDefinition.getStart();
                StringPlusLanguage end = extentDefinition.getEnd();
                part.getDetail().clear();
                DetailDefinition detail = new DetailDefinition();
                StringPlusLanguage detailNumber = new StringPlusLanguage();
                detailNumber.setValue((start == null ? "" : start.getValue())+ "-" + (end == null ? "" : end.getValue()));
                detail.getNumber().add(detailNumber);
                part.getDetail().add(detail);
            }
        }

    }

    static void addNameWithEtal(ModsDefinition mods) {
        NameDefinition nameDefinition = null;
        for (NameDefinition name : mods.getName()) {
            if (name.getEtal() != null && (!name.getEtal().getValue().isEmpty() || null != name.getEtal().getValue())) {
                name.getNameIdentifier().clear();
                name.getNamePart().clear();
                nameDefinition = name;
                break;
            }
        }
        if (nameDefinition != null) {
            mods.getName().clear();
            mods.getName().add(nameDefinition);
        }
    }

    public static void addOriginInfo(List<OriginInfoDefinition> originInfos, OaiDcType dc) {
        for (OriginInfoDefinition originInfo : originInfos) {
            for (PlaceDefinition place : originInfo.getPlace()) {
                for (PlaceTermDefinition placeTerm : place.getPlaceTerm()) {
                    CodeOrText type = placeTerm.getType();
                    if (type == CodeOrText.TEXT || type == null) {
                        addElementType(dc.getCoverages(), placeTerm.getValue());
                    }
                }
            }
            addStringPlusLanguage(dc.getPublishers(), originInfo.getPublisher());
            for (DateDefinition date : originInfo.getDateIssued()) {
                //issue #706 - write dc:data even if point is not present
                //String point = toValue(date.getPoint());
                //if (point != null) {
                    addElementType(dc.getDates(), date.getValue());
                //}
            }
            if (originInfo.getDateIssued().isEmpty()) {
                for (DateDefinition date : originInfo.getDateOther()) {
                    //issue #1705 - write dc:data of dateOther
                    addElementType(dc.getDates(), date.getValue());
                }
            }
            /*for (DateDefinition date : originInfo.getDateOther()) {
                addElementType(dc.getDates(), date.getValue());
            }*/
        }
    }

    public static void addLanguage(List<LanguageDefinition> modsLanguages, OaiDcType dc) {
        for (LanguageDefinition language : modsLanguages) {
            for (LanguageTermDefinition languageTerm : language.getLanguageTerm()) {
                CodeOrText type = languageTerm.getType();
                if (type != CodeOrText.TEXT) {
                    addElementType(dc.getLanguages(), languageTerm.getValue());
                }
            }
        }
    }

    public static List<ElementType> addElementType(List<ElementType> dcElms, String value) {
        return addElementType(dcElms, value, null);
    }

    static List<ElementType> addElementType(List<ElementType> dcElms, String value, String lang) {
        value = toValue(value);
        lang = toValue(lang);
        if (value != null) {
            ElementType element = new ElementType(value, lang);
            if (!dcElms.contains(element)) {
                dcElms.add(element);
            }
        }
        return dcElms;
    }

    static String findPartNumber(ModsDefinition mods) {
        List<TitleInfoDefinition> titleInfos = mods.getTitleInfo();
        if (!titleInfos.isEmpty()) {
            TitleInfoDefinition titleInfo = titleInfos.get(0);
            List<StringPlusLanguage> partNumbers = titleInfo.getPartNumber();
            if (!partNumbers.isEmpty()) {
                return toValue(partNumbers.get(0).getValue());
            }
        }
        return null;
    }

    static String findPartName(ModsDefinition mods) {
        List<TitleInfoDefinition> titleInfos = mods.getTitleInfo();
        if (!titleInfos.isEmpty()) {
            TitleInfoDefinition titleInfo = titleInfos.get(0);
            List<StringPlusLanguage> partNames = titleInfo.getPartName();
            if (!partNames.isEmpty()) {
                return toValue(partNames.get(0).getValue());
            }
        }
        return null;
    }

    public static String toValue(String s) {
        if (s != null) {
            s = s.trim();
        }
        return s == null || s.isEmpty() ? null : s;
    }
}
