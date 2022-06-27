/*
 * Copyright (C) 2020 Lukas Sykora
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
package cz.cas.lib.proarc.common.actions;

import cz.cas.lib.proarc.common.actions.series.SeriesNumber;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.mods.DetailDefinition;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NoteDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author Lukas Sykora
 */
public class UpdatePages {
    private List<String> updatedPids;
    private int index;
    private String pageType;
    private String pagePosition;
    private String model;
    private int applyTo;
    private boolean applyToFirstPage;
    private boolean doubleColumns;
    private int pagePositionIndex;

    public UpdatePages() {
    }

    public UpdatePages(String applyTo, String applyToFirstPage, String doubleColumns) throws DigitalObjectException {
        index = -1;
        this.updatedPids = new ArrayList<>();
        setApplyTo(applyTo);
        setApplyToFirstPage(applyToFirstPage);
        setDoubleColumns(doubleColumns);
    }

    private void setDoubleColumns(String doubleColumns) {
        doubleColumns = trim(doubleColumns, "{", "}");
        if (doubleColumns != null && !doubleColumns.isEmpty()) {
            if ("true".equals(doubleColumns) || "1".equals(doubleColumns.replaceAll("[^0-9]", ""))) {
                this.doubleColumns = true;
            } else {
                this.doubleColumns = false;
            }
        }
    }

    private void setApplyToFirstPage(String applyToFirstPage) {
        applyToFirstPage = trim(applyToFirstPage, "{", "}");
        if (applyToFirstPage != null && !applyToFirstPage.isEmpty()) {
            if ("true".equals(applyToFirstPage) || "1".equals(applyToFirstPage.replaceAll("[^0-9]", ""))) {
                this.applyToFirstPage = true;
            } else {
                this.applyToFirstPage = false;
            }
        }
    }

    private static String trim(String value, String start, String end) {
        if (value != null && !value.isEmpty()) {
            if (value.startsWith(start)) {
                value = value.substring(start.length());
            }
            if (value.endsWith(end)) {
                value = value.substring(0, value.length()-end.length());
            }
        }
        return value;
    }

    private void setApplyTo(String applyTo) throws DigitalObjectException {
        if (applyTo != null && !applyTo.isEmpty()) {
            try {
                this.applyTo = Integer.parseInt(applyTo.replaceAll("[^0-9]", ""));
            } catch (Exception ex) {
                this.applyTo = 1;
            }
        } else {
            this.applyTo = 1;
        }
    }

    public static List<String> createListFromArray(String pidsArray) {
        pidsArray = trim(pidsArray, "{", "}");
        pidsArray = trim(pidsArray, "[", "]");
        String[] selectedPids = pidsArray.split(",");
        return Arrays.asList(selectedPids);
    }

    public void createListOfPids(List<String> pids) {
        if (pids.size() > 0) {
            if (applyToFirstPage) {
                int i = 0;
                for (String pid : pids) {
                    if (i % this.applyTo == 0) {
                        this.updatedPids.add(pid);
                    }
                    i++;
                }
            } else {
                int i = 0;
                for (String pid : pids) {
                    int value = i - this.applyTo + 1;
                    if (value % this.applyTo == 0) {
                        this.updatedPids.add(pid);
                    }
                    i++;
                }
            }
        }
    }

    public void createIndex(String startIndex) throws DigitalObjectException {
        if (startIndex != null && !startIndex.isEmpty()) {
            try {
                index = Integer.parseInt(startIndex.replaceAll("[^0-9]", ""));
            } catch (Exception ex) {
                throw new DigitalObjectException(null, "Nepodařilo se ziskat hodnotu indexu");
            }
        }
    }

    private boolean setUseBrackets(String useBrackets) {
        useBrackets = trim(useBrackets, "{", "}");
        if (useBrackets != null && !useBrackets.isEmpty()) {
            if ("true".equals(useBrackets) || "1".equals(useBrackets.replaceAll("[^0-9]", ""))) {
                return true;
            } else {
                return false;
            }
        }
        return false;
    }

    public void updatePages(String sequenceType, String startNumber, String incrementNumber, String prefix, String suffix, String pageType, String useBrackets, String pagePosition) throws DigitalObjectException {
        this.pageType = pageType;
        this.pagePosition = trim(pagePosition, "{", "}");
        this.pagePositionIndex = 0;

        SeriesNumber series = new SeriesNumber(sequenceType, startNumber, incrementNumber, prefix, suffix, setUseBrackets(useBrackets));

        for (String pid : updatedPids) {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            FedoraObject fo = dom.find(pid, null);
            this.model = new RelationEditor(fo).getModel();
            String model = new RelationEditor(fo).getModel();
            DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());
            NdkMapper.Context context = new NdkMapper.Context(handler);
            NdkMapper mapper = NdkMapper.get(model);
            mapper.setModelId(model);

            XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(
                    MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                    MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
            ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
            ModsDefinition mods = modsStreamEditor.read();
            updateMods(mods, series, model);
            mapper.createMods(mods, context);
            modsStreamEditor.write(mods, modsStreamEditor.getLastModified(), null);
            OaiDcType dc = mapper.toDc(mods, context);
            DcStreamEditor dcEditor = handler.objectMetadata();
            DcStreamEditor.DublinCoreRecord dcr = dcEditor.read();
            dcr.setDc(dc);
            dcEditor.write(handler, dcr, null);

            fo.setLabel(mapper.toLabel(mods));
            fo.flush();
        }
    }

    public void updatePagesLocal(List<ImportBatchManager.BatchItemObject> objects, String sequenceType, String startNumber, String incrementNumber, String prefix, String suffix, String pageType, String useBrackets, String pagePosition) throws DigitalObjectException {
        this.pageType = trim(pageType, "{", "}");
        this.pagePosition = trim(pagePosition, "{", "}");
        this.pagePositionIndex = 0;

        SeriesNumber series = new SeriesNumber(sequenceType, startNumber, incrementNumber, prefix, suffix, setUseBrackets(useBrackets));

        objects = getRelevantObjects(objects, updatedPids);
        if (objects != null && !objects.isEmpty()) {
            for (ImportBatchManager.BatchItemObject object : objects) {
                File foxml = object.getFile();
                if (foxml == null || !foxml.exists() || !foxml.canRead()) {
                    throw new IllegalStateException("Cannot read foxml: " + foxml);
                }
                LocalStorage.LocalObject lobj = new LocalStorage().load(object.getPid(), foxml);
                XmlStreamEditor xml = lobj.getEditor(FoxmlUtils.inlineProfile(
                        MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                        MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
                DigitalObjectHandler handler = new DigitalObjectHandler(lobj, MetaModelRepository.getInstance());
                NdkMapper.Context context = new NdkMapper.Context(handler);
                NdkMapper mapper = NdkMapper.get(handler.getModel().getPid());
                mapper.setModelId(handler.getModel().getPid());
                ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, lobj);
                ModsDefinition mods = modsStreamEditor.read();
                updateMods(mods, series, handler.getModel().getPid());
                mapper.createMods(mods, context);
                modsStreamEditor.write(mods, modsStreamEditor.getLastModified(), null);
                //lobj.flush();
                OaiDcType dc = mapper.toDc(mods, context);
                DcStreamEditor dcEditor = handler.objectMetadata();
                DcStreamEditor.DublinCoreRecord dcr = dcEditor.read();
                dcr.setDc(dc);
                dcEditor.write(handler, dcr, null);

                lobj.setLabel(mapper.toLabel(mods));
                lobj.flush();
            }
        }
    }

    private void updateMods(ModsDefinition mods, SeriesNumber series, String model) throws DigitalObjectException {
        String number = null;
        if (series.isAllowToUpdateNumber()) {
            if (doubleColumns) {
                number = series.getNextValue() + "," + series.getNextValue();
            } else {
                number = series.getNextValue();
            }
        }
        if (NdkPlugin.MODEL_NDK_PAGE.equals(model) || NdkPlugin.MODEL_PAGE.equals(model) || OldPrintPlugin.MODEL_PAGE.equals(model)) {
            if (mods != null) {
                if (NdkPlugin.MODEL_NDK_PAGE.equals(model)) {
                    setPagePosition(mods);
                }
                setPageIndex(mods);
                setPageNumber(mods, number, model);
                setPageType(mods, true);
            }
        } else {
            throw new DigitalObjectException("Unsupported model: " + model);
        }
    }

    private void setPagePosition(ModsDefinition mods) {
        String pagePosition = getPagePosition();
        if (pagePosition != null) {
            NoteDefinition noteDefinition = null;
            for (NoteDefinition note : mods.getNote()) {
                if (ModsConstants.VALUE_PAGE_NOTE_LEFT.equals(note.getValue()) || ModsConstants.VALUE_PAGE_NOTE_RIGHT.equals(note.getValue()) || ModsConstants.VALUE_PAGE_NOTE_SINGLE_PAGE.equals(note.getValue())) {
                    noteDefinition = note;
                    break;
                }
            }
            if (noteDefinition == null) {
                noteDefinition = new NoteDefinition();
                mods.getNote().add(noteDefinition);
            }
            noteDefinition.setValue(pagePosition);
            pagePositionIndex ++;
        }
    }

    private String getPagePosition() {
        if (pagePosition == null) {
            return null;
        }
        switch (pagePosition) {
            case "left":
                return ModsConstants.VALUE_PAGE_NOTE_LEFT;
            case "right":
                return ModsConstants.VALUE_PAGE_NOTE_RIGHT;
            case "singlePage":
                return ModsConstants.VALUE_PAGE_NOTE_SINGLE_PAGE;
            case "left_right":
                if (pagePositionIndex % 2 == 0) {
                    return ModsConstants.VALUE_PAGE_NOTE_LEFT;
                } else {
                    return ModsConstants.VALUE_PAGE_NOTE_RIGHT;
                }
            case "right_left":
                if (pagePositionIndex % 2 == 0) {
                    return ModsConstants.VALUE_PAGE_NOTE_RIGHT;
                } else {
                    return ModsConstants.VALUE_PAGE_NOTE_LEFT;
                }
            default:
                return null;
        }
    }

    private String fixDoubleBrackets(String number) {
        number = number.replace("[[", "[");
        number = number.replace("]]", "]");
        return number;
    }

    private void setPageType(ModsDefinition mods, boolean setGenre) {
        if (this.pageType != null && !this.pageType.isEmpty()) {
            for (PartDefinition part : mods.getPart()) {
                part.setType(this.pageType);
            }
            if (setGenre) {
                for (GenreDefinition genre : mods.getGenre()) {
                    genre.setType(this.pageType);
                }
            }
        }
    }

    private void setPageNumber(ModsDefinition mods, String number, String model) {
        boolean updated = false;
        if (number != null && !number.isEmpty()) {
            number = fixDoubleBrackets(number);
            if (NdkPlugin.MODEL_NDK_PAGE.equals(model)) {
                for (PartDefinition part : mods.getPart()) {
                    if (part.getDetail().size() == 0) {
                        DetailDefinition detail = new DetailDefinition();
                        StringPlusLanguage detailNumber = new StringPlusLanguage();
                        detailNumber.setValue(number);
                        detail.getNumber().add(detailNumber);
                        detail.setType(ModsConstants.FIELD_PAGE_NUMBER);
                        part.getDetail().add(detail);
                    } else {
                        for (DetailDefinition detail : part.getDetail()) {
                            if (ModsConstants.FIELD_PAGE_NUMBER.equals(detail.getType()) || ModsConstants.FIELD_PAGE_NUMBER_SPLIT.equals(detail.getType())) {
                                if (!detail.getNumber().isEmpty()) {
                                    detail.getNumber().get(0).setValue(number);
                                }
                            }
                        }
                    }
                }
            } else {
                DetailDefinition detailDefinition  = null;
                for (PartDefinition part : mods.getPart()) {
                    for (DetailDefinition detail : part.getDetail()) {
                        if (ModsConstants.FIELD_PAGE_NUMBER.equals(detail.getType()) || ModsConstants.FIELD_PAGE_NUMBER_SPLIT.equals(detail.getType())) {
                            detailDefinition = detail;
                        }
                    }
                }
                if (detailDefinition == null) {
                    detailDefinition = new DetailDefinition();
                    detailDefinition.setType(ModsConstants.FIELD_PAGE_NUMBER);
                    if (mods.getPart().isEmpty()) {
                        mods.getPart().add(new PartDefinition());
                    }
                    mods.getPart().get(0).getDetail().add(detailDefinition);
                    StringPlusLanguage detailNumber = new StringPlusLanguage();
                    detailNumber.setValue(number);
                    detailDefinition.getNumber().add(detailNumber);
                } else {
                    detailDefinition.setType(ModsConstants.FIELD_PAGE_NUMBER);
                    if (detailDefinition.getNumber().isEmpty()) {
                        StringPlusLanguage detailNumber = new StringPlusLanguage();
                        detailNumber.setValue(number);
                        detailDefinition.getNumber().add(detailNumber);
                    } else {
                        for (StringPlusLanguage detailNumber : detailDefinition.getNumber()) {
                            detailNumber.setValue(number);
                        }
                    }
                }
            }
        }
    }

    private void setPageIndex(ModsDefinition mods) {
        if (this.index > 0) {
            for (PartDefinition part : mods.getPart()) {
                for (DetailDefinition detail : part.getDetail()) {
                    if ("pageIndex".equals(detail.getType())) {
                        if (!detail.getNumber().isEmpty()) {
                            detail.getNumber().get(0).setValue(String.valueOf(this.index));
                            if (NdkPlugin.MODEL_NDK_PAGE.equals(this.model)) {
                                part.setType(null);
                            }
                            this.index++;
                        }
                    }
                }
            }
        }
    }

    public void editBrackets(List<String> pids, boolean addBrackets, boolean removeBrackets) throws DigitalObjectException {
        for (String pid : pids) {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            FedoraObject fo = dom.find(pid, null);
            this.model = new RelationEditor(fo).getModel();
            XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(
                    MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                    MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
            ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
            ModsDefinition mods = modsStreamEditor.read();
            if (addBrackets) {
                addBrackets(mods, model);
            } else if (removeBrackets) {
                removeBrackets(mods, model);
            }
            modsStreamEditor.write(mods, modsStreamEditor.getLastModified(), null);

            String model = new RelationEditor(fo).getModel();
            DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());
            NdkMapper mapper = NdkMapper.get(model);
            mapper.setModelId(model);

            NdkMapper.Context context = new NdkMapper.Context(handler);
            OaiDcType dc = mapper.toDc(mods, context);
            DcStreamEditor dcEditor = handler.objectMetadata();
            DcStreamEditor.DublinCoreRecord dcr = dcEditor.read();
            dcr.setDc(dc);
            dcEditor.write(handler, dcr, null);

            fo.setLabel(mapper.toLabel(mods));
            fo.flush();
        }
    }

    public void editBracketsLocal(List<ImportBatchManager.BatchItemObject> objects, List<String> pids, boolean addBrackets, boolean removeBrackets) throws DigitalObjectException {
        objects = getRelevantObjects(objects, pids);
        if (objects != null && !objects.isEmpty()) {
            for (ImportBatchManager.BatchItemObject object : objects) {
                File foxml = object.getFile();
                if (foxml == null || !foxml.exists() || !foxml.canRead()) {
                    throw new IllegalStateException("Cannot read foxml: " + foxml);
                }
                LocalStorage.LocalObject lobj = new LocalStorage().load(object.getPid(), foxml);
                XmlStreamEditor xml = lobj.getEditor(FoxmlUtils.inlineProfile(
                        MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                        MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
                DigitalObjectHandler handler = new DigitalObjectHandler(lobj, MetaModelRepository.getInstance());
                NdkMapper.Context context = new NdkMapper.Context(handler);
                NdkMapper mapper = NdkMapper.get(handler.getModel().getPid());
                mapper.setModelId(handler.getModel().getPid());
                ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, lobj);
                ModsDefinition mods = modsStreamEditor.read();
                if (addBrackets) {
                    addBrackets(mods, handler.getModel().getPid());
                } else if (removeBrackets) {
                    removeBrackets(mods, handler.getModel().getPid());
                }
                mapper.createMods(mods, context);
                modsStreamEditor.write(mods, modsStreamEditor.getLastModified(), null);
                //lobj.flush();
                OaiDcType dc = mapper.toDc(mods, context);
                DcStreamEditor dcEditor = handler.objectMetadata();
                DcStreamEditor.DublinCoreRecord dcr = dcEditor.read();
                dcr.setDc(dc);
                dcEditor.write(handler, dcr, null);

                lobj.setLabel(mapper.toLabel(mods));
                lobj.flush();
            }
        }
    }

    private List<ImportBatchManager.BatchItemObject> getRelevantObjects(List<ImportBatchManager.BatchItemObject> objects, List<String> pids) {
        if (pids == null || pids.isEmpty() || objects == null || objects.isEmpty()) {
            return null;
        }
        List<ImportBatchManager.BatchItemObject> selectedList = new ArrayList<>();
        for (String pid : pids) {
            for (ImportBatchManager.BatchItemObject object :objects) {
                if (object.getPid().equals(pid)) {
                    selectedList.add(object);
                }
            }
        }
        return selectedList;
    }

    private void removeBrackets(ModsDefinition mods, String model) throws DigitalObjectException {
        if (NdkPlugin.MODEL_NDK_PAGE.equals(model) || NdkPlugin.MODEL_PAGE.equals(model) || OldPrintPlugin.MODEL_PAGE.equals(model)) {
            if (mods != null) {
                for (PartDefinition part : mods.getPart()) {
                    if (part.getDetail().size() == 0) {
                        DetailDefinition detail = new DetailDefinition();
                        StringPlusLanguage detailNumber = new StringPlusLanguage();
                        detailNumber.setValue(removePageNumberFromBrackets(detailNumber.getValue()));
                        detail.getNumber().add(detailNumber);
                        detail.setType("pageNumber");
                        part.getDetail().add(detail);
                    } else {
                        for (DetailDefinition detail : part.getDetail()) {
                            if ("pageNumber".equals(detail.getType()) || "page number".equals(detail.getType())) {
                                if (!detail.getNumber().isEmpty()) {
                                    detail.getNumber().get(0).setValue(removePageNumberFromBrackets(detail.getNumber().get(0).getValue()));
                                }
                            }
                        }
                    }
                }
            }
        } else {
            throw new DigitalObjectException("Unsupported model: " + model);
        }
    }

    private void addBrackets(ModsDefinition mods, String model) throws DigitalObjectException {
        if (NdkPlugin.MODEL_NDK_PAGE.equals(model) || NdkPlugin.MODEL_PAGE.equals(model) || OldPrintPlugin.MODEL_PAGE.equals(model)) {
            if (mods != null) {
                for (PartDefinition part : mods.getPart()) {
                    if (part.getDetail().size() == 0) {
                        DetailDefinition detail = new DetailDefinition();
                        StringPlusLanguage detailNumber = new StringPlusLanguage();
                        detailNumber.setValue(addNumberIntoBrackets(detailNumber.getValue()));
                        detail.getNumber().add(detailNumber);
                        detail.setType("pageNumber");
                        part.getDetail().add(detail);
                    } else {
                        for (DetailDefinition detail : part.getDetail()) {
                            if ("pageNumber".equals(detail.getType()) || "page number".equals(detail.getType())) {
                                if (!detail.getNumber().isEmpty()) {
                                    detail.getNumber().get(0).setValue(addNumberIntoBrackets(detail.getNumber().get(0).getValue()));
                                }
                            }
                        }
                    }
                }
            }
        } else {
            throw new DigitalObjectException("Unsupported model: " + model);
        }
    }

    private String addNumberIntoBrackets(String value) {
        value = removePageNumberFromBrackets(value);
        return "[" + value + "]";
    }

    private String removePageNumberFromBrackets(String value) {
        return trim(value, "[", "]");
    }
}
