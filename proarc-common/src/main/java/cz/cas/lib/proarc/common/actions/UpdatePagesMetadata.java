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
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.File;
import java.util.List;

import static cz.cas.lib.proarc.common.actions.UpdatePages.getRelevantObjects;

/**
 * @author Lukas Sykora
 */
public class UpdatePagesMetadata {

    private List<String> sourcePids;
    private List<String> destinationPids;
    private Boolean copyPageIndex;
    private Boolean copyPageNumber;
    private Boolean copyPageType;

    public UpdatePagesMetadata(List<String> sourcePids, List<String> destinationPids, Boolean copyPageIndex, Boolean copyPageNumber, Boolean copyPageType) {
        this.sourcePids = sourcePids;
        this.destinationPids = destinationPids;
        this.copyPageIndex = copyPageIndex;
        this.copyPageNumber = copyPageNumber;
        this.copyPageType = copyPageType;
    }

    public void updatePagesLocal(List<ImportBatchManager.BatchItemObject> objects) throws DigitalObjectException {
        List<ImportBatchManager.BatchItemObject> relevantSourceObjects = getRelevantObjects(objects, sourcePids);
        List<ImportBatchManager.BatchItemObject> relevantDestinationObjects = getRelevantObjects(objects, destinationPids);

        if (relevantSourceObjects != null && !relevantSourceObjects.isEmpty()) {
            if (relevantDestinationObjects != null && !relevantDestinationObjects.isEmpty()) {
                int index = 0;
                for (ImportBatchManager.BatchItemObject destinationObject : relevantDestinationObjects) {
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
                if (copyPageNumber) {
                    setPageNumber(mods, sourceModsInfo, model);
                }
                if (copyPageType) {
                    setPageType(mods, sourceModsInfo, model, true);
                }
            }
        } else {
            throw new DigitalObjectException("Unsupported model: " + model);
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
        if (sourceModsInfo.getPageType() != null && !sourceModsInfo.getPageType().isEmpty()) {
            for (PartDefinition part : mods.getPart()) {
                part.setType(sourceModsInfo.getPageType());
            }
            if (setGenre) {
                for (GenreDefinition genre : mods.getGenre()) {
                    genre.setType(sourceModsInfo.getPageType());
                }
            }
        }
    }

    private void setPageIndex(ModsDefinition mods, SourceModsInfo sourceModsInfo, String model) {
        if (sourceModsInfo.getPageIndex() != null && !sourceModsInfo.getPageIndex().isEmpty()) {
            for (PartDefinition part : mods.getPart()) {
                for (DetailDefinition detail : part.getDetail()) {
                    if ("pageIndex".equals(detail.getType())) {
                        if (!detail.getNumber().isEmpty()) {
                            detail.getNumber().get(0).setValue(sourceModsInfo.getPageIndex());
                            if (NdkPlugin.MODEL_NDK_PAGE.equals(model)) {
                                part.setType(null);
                            }
                        }
                    }
                }
            }
        }
    }

    private SourceModsInfo getSourceObjectLocal(ImportBatchManager.BatchItemObject sourceObject) throws DigitalObjectException {
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
                SourceModsInfo sourceModsInfo = new SourceModsInfo(pageIndex, pageNumber, pageType);
                return sourceModsInfo;
            } else {
                throw new DigitalObjectException("Missing element mods for: " + pid);
            }
        } else {
            throw new DigitalObjectException("Unsupported model: " + model);
        }
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

        public SourceModsInfo(String pageIndex, String pageNumber, String pageType) {
            this.pageIndex = pageIndex;
            this.pageNumber = pageNumber;
            this.pageType = pageType;
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
    }
}