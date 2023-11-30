/*
 * Copyright (C) 2022 Lukas Sykora
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

import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.process.BatchManager;
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
import java.util.List;

import static cz.cas.lib.proarc.common.actions.UpdatePages.getRelevantObjects;
import static cz.cas.lib.proarc.common.actions.UpdatePages.isPageNumberElement;

/**
 * @author Lukas Sykora
 */
public class UpdatePagesMetadata {

    private List<String> sourcePids;
    private List<String> destinationPids;
    private Boolean copyPageIndex;
    private Boolean copyPageNumber;
    private Boolean copyPageType;
    private Boolean copyPagePosition;

    public UpdatePagesMetadata(List<String> sourcePids, List<String> destinationPids, Boolean copyPageIndex, Boolean copyPageNumber, Boolean copyPageType, Boolean copyPagePosition) {
        this.sourcePids = sourcePids;
        this.destinationPids = destinationPids;
        this.copyPageIndex = copyPageIndex;
        this.copyPageNumber = copyPageNumber;
        this.copyPageType = copyPageType;
        this.copyPagePosition = copyPagePosition;
    }

    public void updatePagesLocal(List<BatchManager.BatchItemObject> objects) throws DigitalObjectException {
        List<BatchManager.BatchItemObject> relevantSourceObjects = getRelevantObjects(objects, sourcePids);
        List<BatchManager.BatchItemObject> relevantDestinationObjects = getRelevantObjects(objects, destinationPids);

        if (relevantSourceObjects != null && !relevantSourceObjects.isEmpty()) {
            if (relevantDestinationObjects != null && !relevantDestinationObjects.isEmpty()) {
                int index = 0;
                for (BatchManager.BatchItemObject destinationObject : relevantDestinationObjects) {
                    File foxml = destinationObject.getFile();
                    if (foxml == null || !foxml.exists() || !foxml.canRead()) {
                        throw new IllegalStateException("Cannot read foxml: " + foxml.getAbsolutePath());
                    }
                    LocalStorage.LocalObject lobj = new LocalStorage().load(destinationObject.getPid(), foxml);
                    XmlStreamEditor xml = lobj.getEditor(FoxmlUtils.inlineProfile(
                            MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                            MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
                    DigitalObjectHandler handler = new DigitalObjectHandler(lobj, MetaModelRepository.getInstance());
                    NdkMapper.Context context = new NdkMapper.Context(handler);
                    NdkMapper mapper = NdkMapper.get(handler.getModel().getPid());
                    mapper.setModelId(handler.getModel().getPid());
                    ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, lobj);
                    ModsDefinition mods = modsStreamEditor.read();
                    updateMods(mods, handler.getModel().getPid(), destinationObject.getPid(), getSourceObjectLocal(relevantSourceObjects.get(index % relevantSourceObjects.size())));
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
                    index++;
                }
            }
        } else {
            throw new DigitalObjectException("No source objects to copy metadata");
        }
    }

    public void updatePages() throws DigitalObjectException {
        if (sourcePids != null && !sourcePids.isEmpty()) {
            if (destinationPids != null && !destinationPids.isEmpty()) {
                int index = 0;
                for (String destinationPid : destinationPids) {
                    DigitalObjectManager dom = DigitalObjectManager.getDefault();
                    FedoraObject fo = dom.find(destinationPid, null);
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
                    updateMods(mods, model, destinationPid, getSourceObject(sourcePids.get(index % sourcePids.size())));
                    mapper.createMods(mods, context);
                    modsStreamEditor.write(mods, modsStreamEditor.getLastModified(), null);
                    OaiDcType dc = mapper.toDc(mods, context);
                    DcStreamEditor dcEditor = handler.objectMetadata();
                    DcStreamEditor.DublinCoreRecord dcr = dcEditor.read();
                    dcr.setDc(dc);
                    dcEditor.write(handler, dcr, null);

                    fo.setLabel(mapper.toLabel(mods));
                    fo.flush();
                    index++;
                }
            }
        } else {
            throw new DigitalObjectException("No source objects to copy metadata");
        }
    }

    private void updateMods(ModsDefinition mods, String model, String pid, SourceModsInfo sourceModsInfo) throws DigitalObjectException {
        if (NdkPlugin.MODEL_NDK_PAGE.equals(model) || NdkPlugin.MODEL_PAGE.equals(model) || OldPrintPlugin.MODEL_PAGE.equals(model)) {
            if (mods != null) {
                if (copyPageIndex) {
                    setPageIndex(mods, sourceModsInfo, model);
                }
                if (copyPageType) {
                    setPageType(mods, sourceModsInfo, model, true);
                }
                if (copyPageNumber) {
                    setPageNumber(mods, sourceModsInfo, model);
                }
                if (copyPagePosition) {
                    setPagePosition(mods, sourceModsInfo, model);
                }
            }
        } else {
            throw new DigitalObjectException("Unsupported model: " + model);
        }
    }

    private void setPagePosition(ModsDefinition mods, SourceModsInfo sourceModsInfo, String model) {
        if (sourceModsInfo.getPagePosition() != null && !sourceModsInfo.getPagePosition().isEmpty()) {
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
            noteDefinition.setValue(sourceModsInfo.getPagePosition());
        }
    }

    private void setPageNumber(ModsDefinition mods, SourceModsInfo sourceModsInfo, String model) {
        if (sourceModsInfo.getPageNumber() != null && !sourceModsInfo.getPageNumber().isEmpty()) {
            if (NdkPlugin.MODEL_NDK_PAGE.equals(model)) {
                for (PartDefinition part : mods.getPart()) {
                    if (part.getDetail().size() == 0) {
                        DetailDefinition detail = new DetailDefinition();
                        StringPlusLanguage detailNumber = new StringPlusLanguage();
                        detailNumber.setValue(sourceModsInfo.getPageNumber());
                        detail.getNumber().add(detailNumber);
                        detail.setType(ModsConstants.FIELD_PAGE_NUMBER);
                        part.getDetail().add(detail);
                    } else {
                        for (DetailDefinition detail : part.getDetail()) {
                            if (ModsConstants.FIELD_PAGE_NUMBER.equals(detail.getType()) || ModsConstants.FIELD_PAGE_NUMBER_SPLIT.equals(detail.getType())) {
                                if (!detail.getNumber().isEmpty()) {
                                    detail.getNumber().get(0).setValue(sourceModsInfo.getPageNumber());
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
                    detailNumber.setValue(sourceModsInfo.getPageNumber());
                    detailDefinition.getNumber().add(detailNumber);
                } else {
                    detailDefinition.setType(ModsConstants.FIELD_PAGE_NUMBER);
                    if (detailDefinition.getNumber().isEmpty()) {
                        StringPlusLanguage detailNumber = new StringPlusLanguage();
                        detailNumber.setValue(sourceModsInfo.getPageNumber());
                        detailDefinition.getNumber().add(detailNumber);
                    } else {
                        for (StringPlusLanguage detailNumber : detailDefinition.getNumber()) {
                            detailNumber.setValue(sourceModsInfo.getPageNumber());
                        }
                    }
                }
            }
        }
    }

    private void setPageType(ModsDefinition mods, SourceModsInfo sourceModsInfo, String model, boolean setGenre) {
        boolean updated = false;
        if (sourceModsInfo.getPageType() != null && !sourceModsInfo.getPageType().isEmpty()) {
            for (PartDefinition part : mods.getPart()) {
                if (isPageNumberElement(part)) {
                    part.setType(sourceModsInfo.getPageType());
                    updated = true;
                }
            }
            if (!updated) {
                PartDefinition partDefinition = new PartDefinition();
                partDefinition.setType(sourceModsInfo.getPageType());
                mods.getPart().add(0, partDefinition);
            }
            if (setGenre) {
                for (GenreDefinition genre : mods.getGenre()) {
                    genre.setType(sourceModsInfo.getPageType());
                }
            }
        }
    }

    private void setPageIndex(ModsDefinition mods, SourceModsInfo sourceModsInfo, String model) {
        boolean update = false;
        if (sourceModsInfo.getPageIndex() != null && !sourceModsInfo.getPageIndex().isEmpty()) {
            for (PartDefinition part : mods.getPart()) {
                for (DetailDefinition detail : part.getDetail()) {
                    if ("pageIndex".equals(detail.getType())) {
                        if (!detail.getNumber().isEmpty()) {
                            detail.getNumber().get(0).setValue(sourceModsInfo.getPageIndex());
                            update = true;
                            if (NdkPlugin.MODEL_NDK_PAGE.equals(model)) {
                                part.setType(null);
                            }
                        }
                    }
                }
            }
            if (!update) {
                PartDefinition part = new PartDefinition();
                mods.getPart().add(part);
                DetailDefinition detail = new DetailDefinition();
                part.getDetail().add(detail);
                detail.setType("pageIndex");
                StringPlusLanguage number = new StringPlusLanguage();
                detail.getNumber().add(number);
                number.setValue(sourceModsInfo.getPageIndex());
            }
        }
    }

    private SourceModsInfo getSourceObjectLocal(BatchManager.BatchItemObject sourceObject) throws DigitalObjectException {
        File foxml = sourceObject.getFile();
        if (foxml == null || !foxml.exists() || !foxml.canRead()) {
            throw new IllegalStateException("Cannot read foxml: " + foxml.getAbsolutePath());
        }
        LocalStorage.LocalObject lobj = new LocalStorage().load(sourceObject.getPid(), foxml);
        XmlStreamEditor xml = lobj.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        DigitalObjectHandler handler = new DigitalObjectHandler(lobj, MetaModelRepository.getInstance());
        NdkMapper.Context context = new NdkMapper.Context(handler);
        NdkMapper mapper = NdkMapper.get(handler.getModel().getPid());
        mapper.setModelId(handler.getModel().getPid());
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, lobj);
        ModsDefinition mods = modsStreamEditor.read();
        SourceModsInfo sourceModsInfo = getSourceModsInfo(mods, handler.getModel().getPid(), sourceObject.getPid());
        return sourceModsInfo;
    }

    private SourceModsInfo getSourceObject(String pid) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        FedoraObject fo = dom.find(pid, null);
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
        SourceModsInfo sourceModsInfo = getSourceModsInfo(mods, model, pid);
        return sourceModsInfo;
    }

    private SourceModsInfo getSourceModsInfo(ModsDefinition mods, String model, String pid) throws DigitalObjectException {
        if (NdkPlugin.MODEL_NDK_PAGE.equals(model) || NdkPlugin.MODEL_PAGE.equals(model) || OldPrintPlugin.MODEL_PAGE.equals(model)) {
            if (mods != null) {
                String pageIndex = getPageIndex(mods, model);
                String pageNumber = getPageNumber(mods, model);
                String pageType = getPageType(mods);
                String pagePosition = getPagePosition(mods);
                SourceModsInfo sourceModsInfo = new SourceModsInfo(pageIndex, pageNumber, pageType, pagePosition);
                return sourceModsInfo;
            } else {
                throw new DigitalObjectException("Missing element mods for: " + pid);
            }
        } else {
            throw new DigitalObjectException("Unsupported model: " + model);
        }
    }

    private String getPagePosition(ModsDefinition mods) {
        for (NoteDefinition note : mods.getNote()) {
            if (ModsConstants.VALUE_PAGE_NOTE_LEFT.equals(note.getValue()) || ModsConstants.VALUE_PAGE_NOTE_RIGHT.equals(note.getValue()) || ModsConstants.VALUE_PAGE_NOTE_SINGLE_PAGE.equals(note.getValue())) {
                return note.getValue();
            }
        }
        return null;
    }

    private String getPageType(ModsDefinition mods) {
        for (PartDefinition part : mods.getPart()) {
            if (part.getType() != null) {
                return part.getType();
            }
        }
        return null;
    }

    private String getPageNumber(ModsDefinition mods, String model) {
        for (PartDefinition part : mods.getPart()) {
            for (DetailDefinition detail : part.getDetail()) {
                if (ModsConstants.FIELD_PAGE_NUMBER.equals(detail.getType()) || ModsConstants.FIELD_PAGE_NUMBER_SPLIT.equals(detail.getType())) {
                    if (!detail.getNumber().isEmpty()) {
                        return detail.getNumber().get(0).getValue();
                    }
                }
            }
        }
        return null;
    }

    private String getPageIndex(ModsDefinition mods, String model) {
        for (PartDefinition part : mods.getPart()) {
            for (DetailDefinition detail : part.getDetail()) {
                if ("pageIndex".equals(detail.getType())) {
                    if (!detail.getNumber().isEmpty()) {
                        return detail.getNumber().get(0).getValue();
                    }
                }
            }
        }
        return null;
    }

    public class SourceModsInfo {
        public String pageIndex;
        public String pageNumber;
        public String pageType;
        public String pagePosition;

        public SourceModsInfo(String pageIndex, String pageNumber, String pageType, String pagePosition) {
            this.pageIndex = pageIndex;
            this.pageNumber = pageNumber;
            this.pageType = pageType;
            this.pagePosition = pagePosition;
        }

        public String getPageIndex() {
            return pageIndex;
        }

        public String getPageNumber() {
            return pageNumber;
        }

        public String getPageType() {
            return pageType;
        }

        public String getPagePosition() {
            return pagePosition;
        }
    }
}
