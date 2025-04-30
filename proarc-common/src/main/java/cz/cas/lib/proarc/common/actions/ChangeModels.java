package cz.cas.lib.proarc.common.actions;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkNewPageMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.K4Plugin;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.collectionOfClippings.CollectionOfClippingsPlugin;
import cz.cas.lib.proarc.common.object.emods.BornDigitalModsPlugin;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.mods.DateDefinition;
import cz.cas.lib.proarc.mods.DetailDefinition;
import cz.cas.lib.proarc.mods.FormDefinition;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.common.process.export.mets.MetsContext.buildAkubraContext;
import static cz.cas.lib.proarc.common.process.export.mets.MetsContext.buildFedoraContext;


public class ChangeModels {

    private static final Logger LOG = Logger.getLogger(ChangeModels.class.getName());

    private static AppConfiguration appConfig;
    private static AkubraConfiguration akubraConfiguration;
    private static String pid;
    private static String oldModel;
    private static String newModel;
    private List<String> pids;


    public ChangeModels(AppConfiguration appConfig, AkubraConfiguration akubraConfiguration, String pid, String oldModel, String newModel) {
        this.appConfig = appConfig;
        this.akubraConfiguration = akubraConfiguration;
        this.pid = pid;
        this.oldModel = oldModel;
        this.newModel = newModel;
        this.pids = new ArrayList<>();
    }

    public List<String> getPids() {
        return pids;
    }

    public List<String> findObjects() throws DigitalObjectException {
        return findObjects(true);
    }

    public List<String> findObjects(boolean searchInChildren) throws DigitalObjectException {
        IMetsElement element = getElement();
        if (element == null) {
            throw new DigitalObjectException(pid, "ChangeModels:findObjects - object is null");
        }
        findChildrens(element, searchInChildren);
        return pids;
    }

    public String findRootObject() throws DigitalObjectException {
        IMetsElement element = getElement();
        if (element == null) {
            throw new DigitalObjectException(pid, "ChangeModels:findRootObject - object is null");
        }
        return getRootElement(element);
    }

    private String getRootElement(IMetsElement element) {
        return element.getMetsContext().getRootElement().getOriginalPid();
    }

    public ChangeModelResult changeModelsAndRepairMetadata(String parentPid) {
        int updated = 0;
        String updatedPid = "";
        try {
            for (String pid : pids) {
                updatedPid = pid;
                DigitalObjectManager dom = DigitalObjectManager.getDefault();
                changeModel(dom, pid, newModel);
                repairMetadata(dom, pid, parentPid);
                updated++;
            }
            if (updated == 0) {
                LOG.log(Level.WARNING, "No objects with model " + oldModel + " found.");
            } else {
                LOG.log(Level.WARNING, "Object changed from " + oldModel + " to " + newModel + " succesfully. Total items (" + updated + ").");
            }
            return null;
        } catch (DigitalObjectException ex) {
            LOG.log(Level.SEVERE, "Changing objects failed, totaly items (" + pids.size() + "), changed only " + updated + " items.") ;
            return new ChangeModelResult(updatedPid, new DigitalObjectException(pid, "Changing objects failed, totaly items (" + pids.size() + "), changed only " + updated + " items."));
        }
    }

    public void changeModelBack(String pid, String model) throws DigitalObjectException {
        try {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            changeModel(dom, pid, model);
            LOG.info("Model changed to original value for pid:" + pid);
        } catch (DigitalObjectException ex) {
            LOG.warning("Can not changed model back.");
        }
    }

    private void repairMetadata(DigitalObjectManager dom, String pid, String parentPid) throws DigitalObjectException {
        ProArcObject fo = dom.find(pid, null);
        DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());
        NdkMapper.Context context = new NdkMapper.Context(handler);

        NdkMapper mapper = NdkMapper.get(newModel);
        mapper.setModelId(newModel);

        XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
        ModsDefinition mods = modsStreamEditor.read();
        fixMods(pid, mods, parentPid);
        mapper.createMods(mods, context);
        modsStreamEditor.write(mods, modsStreamEditor.getLastModified(), null);


        OaiDcType dc = mapper.toDc(mods, context);
        DcStreamEditor dcEditor = handler.objectMetadata();
        DcStreamEditor.DublinCoreRecord dcr = dcEditor.read();
        dcr.setDc(dc);
        dcEditor.write(handler, dcr, null);

        fo.setLabel(mapper.toLabel(mods));

        handler.commit();
    }

    private void fixMods(String pid, ModsDefinition mods, String parentPid) throws DigitalObjectException {
        if (NdkMapper.isK4Model(oldModel)) {
            fixModsFromK4(pid, mods, parentPid);
        } else if (NdkMapper.isNdkEModel(oldModel)) {
            fixModsFromNdkE(pid, mods, parentPid);
        } else if (NdkMapper.isNdkModel(oldModel)) {
            fixModsFromNdk(pid, mods, parentPid);
        } else if (NdkMapper.isOldprintModel(oldModel)) {
            fixModsFromOldPrint(pid, mods, parentPid);
        } else if (NdkMapper.isBornDigitalModel(oldModel)) {
            fixModsFromBDM(pid, mods, parentPid);
        } else {
            switch (newModel) {
                case NdkPlugin.MODEL_NDK_PAGE:
                    mods = fixNdkPageMods(mods);
                    break;
               case NdkPlugin.MODEL_MONOGRAPHVOLUME:
                    fixNdkMonographVolumeMods(mods, parentPid);
                    break;
               case NdkPlugin.MODEL_MONOGRAPHTITLE:
                    fixNdkMonographVolumeMods(mods, parentPid);
                    break;
                default:
                    throw new DigitalObjectException(pid, "ChangeModels:fixMods - Unsupported model.");
            }
        }
    }

    private void fixModsFromOldPrint(String pid, ModsDefinition mods, String parentPid) throws DigitalObjectException {
        switch (newModel) {
            case NdkPlugin.MODEL_NDK_PAGE:
                fixNdkPageMods(mods);
                break;
            case NdkPlugin.MODEL_PAGE:
            case NdkPlugin.MODEL_CARTOGRAPHIC:
            case NdkPlugin.MODEL_CHAPTER:
            case NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT:
            case NdkPlugin.MODEL_MONOGRAPHVOLUME:
            case NdkPlugin.MODEL_PICTURE:
            case NdkPlugin.MODEL_SHEETMUSIC:
            case OldPrintPlugin.MODEL_GRAPHICS:
            case OldPrintPlugin.MODEL_SHEETMUSIC:
            case OldPrintPlugin.MODEL_MONOGRAPHVOLUME:
            case OldPrintPlugin.MODEL_MONOGRAPHUNIT:
                // no metadata change needed
                break;
            default:
                throw new DigitalObjectException(pid, "ChangeModels:fixMods - Unsupported model.");
        }
    }

    private void fixModsFromBDM(String pid, ModsDefinition mods, String parentPid) throws DigitalObjectException {
        switch (newModel) {
            case NdkEbornPlugin.MODEL_EARTICLE:
                // no metadata change needed
                break;
            default:
                throw new DigitalObjectException(pid, "ChangeModels:fixMods - Unsupported model.");
        }
    }

    private void fixModsFromNdk(String pid, ModsDefinition mods, String parentPid) throws DigitalObjectException {
        switch (newModel) {
            case NdkPlugin.MODEL_PAGE:
                fixPageMods(mods);
                break;
            case NdkPlugin.MODEL_NDK_PAGE:
                fixNdkPageMods(mods);
                break;
            case OldPrintPlugin.MODEL_PAGE:
                switch (oldModel) {
                    case NdkPlugin.MODEL_NDK_PAGE:
                        fixOldPrintMods(mods);
                        break;
                    case NdkPlugin.MODEL_PAGE:
                        break;
                    default:
                        throw new DigitalObjectException(pid, "ChangeModels:fixMods - Unsupported old model (" + oldModel + ").");
                }
                break;
            case NdkPlugin.MODEL_MONOGRAPHVOLUME:
                switch (oldModel) {
                    case NdkPlugin.MODEL_MONOGRAPHTITLE:
                        mods.getGenre().clear();
                        break;
                    case NdkPlugin.MODEL_MONOGRAPHUNIT:
                        // no metadata change needed
                        break;
                    default:
                        throw new DigitalObjectException(pid, "ChangeModels:fixMods - Unsupported previous model (" + oldModel + ").");
                }
                break;
            case NdkPlugin.MODEL_MONOGRAPHTITLE:
                switch (oldModel) {
                    case NdkPlugin.MODEL_MONOGRAPHVOLUME:
                        mods.getGenre().clear();
                        break;
                    default:
                        throw new DigitalObjectException(pid, "ChangeModels:fixMods - Unsupported previous model (" + oldModel + ").");
                }
                break;
            case NdkPlugin.MODEL_MONOGRAPHUNIT:
                switch (oldModel) {
                    case NdkPlugin.MODEL_MONOGRAPHVOLUME:
                        // no metadata change needed
                        break;
                    default:
                        throw new DigitalObjectException(pid, "ChangeModels:fixMods - Unsupported previous model (" + oldModel + ").");
                }
                break;
            case CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_VOLUME:
            case CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_TITLE:
                fixCollectionOfClippingsVolumeMods(mods, parentPid);
                break;
            case NdkEbornPlugin.MODEL_EPERIODICAL:
            case NdkEbornPlugin.MODEL_EPERIODICALVOLUME:
            case NdkEbornPlugin.MODEL_EPERIODICALISSUE:
            case NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT:
                fixPhysicalDescription(mods, "print", "electronic");
                break;
            case OldPrintPlugin.MODEL_CARTOGRAPHIC:
            case OldPrintPlugin.MODEL_CHAPTER:
            case OldPrintPlugin.MODEL_SUPPLEMENT:
            case OldPrintPlugin.MODEL_MONOGRAPHVOLUME:
            case OldPrintPlugin.MODEL_MONOGRAPHUNIT:
            case OldPrintPlugin.MODEL_GRAPHICS:
            case OldPrintPlugin.MODEL_SHEETMUSIC:
            case NdkEbornPlugin.MODEL_EARTICLE:
                // no metadata change needed
                break;
            default:
                throw new DigitalObjectException(pid, "ChangeModels:fixMods - Unsupported model.");
        }
    }

    private void fixPhysicalDescription(ModsDefinition mods, String oldValue, String newValue) {
        if (oldValue == null || newValue == null) {
            return;
        }
        for (PhysicalDescriptionDefinition physicalDescription : mods.getPhysicalDescription()) {
            for (FormDefinition form : physicalDescription.getForm()) {
                if (oldValue.equals(form.getValue())) {
                    form.setValue(newValue);
                }
            }
        }
    }

    private void fixModsFromNdkE(String pid, ModsDefinition mods, String parentPid) throws DigitalObjectException {
        switch (newModel) {
            case NdkPlugin.MODEL_PERIODICAL:
            case NdkPlugin.MODEL_PERIODICALVOLUME:
            case NdkPlugin.MODEL_PERIODICALISSUE:
            case NdkPlugin.MODEL_PERIODICALSUPPLEMENT:
                fixPhysicalDescription(mods, "electronic", "print");
                break;
            case NdkPlugin.MODEL_ARTICLE:
            case BornDigitalModsPlugin.MODEL_ARTICLE:
                // no metadata change needed
                break;
            default:
                throw new DigitalObjectException(pid, "ChangeModels:fixMods - Unsupported model.");
        }
    }

    private void fixModsFromK4(String pid, ModsDefinition mods, String parentPid) throws DigitalObjectException {
        fixModsFromK4(pid, mods, this.newModel, this.oldModel, getParentMods(parentPid));
    }

    public static ModsDefinition fixModsFromK4(String pid, ModsDefinition mods, String newModel, String oldModel, ModsDefinition parentMods) throws DigitalObjectException {
        switch (newModel) {
            case NdkPlugin.MODEL_PERIODICAL:
                return fixNdkPeriodicalFromK4Periodical(mods);
            case NdkPlugin.MODEL_PERIODICALVOLUME:
                return fixNdkPeriodicalVolumeFromK4PeriodicalVolume(mods);
            case NdkPlugin.MODEL_PERIODICALISSUE:
                return fixNdkPeriodicalIssueFromK4PeriodicalIssue(mods, parentMods);
            case NdkPlugin.MODEL_MONOGRAPHVOLUME:
            case NdkPlugin.MODEL_MONOGRAPHUNIT:
                switch (oldModel) {
                    case K4Plugin.MODEL_MONOGRAPH:
                        return fixNdkMonographVolumeFromK4Monograph(mods);
                    case K4Plugin.MODEL_MONOGRAPHUNIT:
                        return fixNdkMonographVolumeOrUnitFromK4MonographUnit(mods, parentMods);
                    default:
                        throw new DigitalObjectException(pid, "ChangeModels:fixMods - Unsupported previous model (" + oldModel + ").");
                }
            default:
                throw new DigitalObjectException(pid, "ChangeModels:fixMods - Unsupported model (previous model - " + oldModel + ").");
        }
    }

    private static ModsDefinition fixNdkMonographVolumeOrUnitFromK4MonographUnit(ModsDefinition mods, ModsDefinition rootMods) throws DigitalObjectException {
        LOG.info("Updating mods for Ndk Monograph Volume (previous mode K4 Monograph Unit).");

        if (rootMods != null) {
            TitleInfoDefinition title = null;
            for (TitleInfoDefinition titleInfo : rootMods.getTitleInfo()) {
                if (titleInfo.getType() == null) {
                    title = titleInfo;
                    break;
                }
            }

            if (title == null) {
                title = new TitleInfoDefinition();
            }

            for (PartDefinition part : mods.getPart()) {
                for (DetailDefinition detail : part.getDetail()) {
                    if (!detail.getNumber().isEmpty()) {
                        for (StringPlusLanguage number : detail.getNumber()) {
                            title.getPartNumber().add(number);
                        }
                    }
                }
            }

            copyMods(mods, rootMods);
            if (!mods.getTitleInfo().contains(title)) {
                mods.getTitleInfo().add(title);
            }
        } else {
            TitleInfoDefinition title = null;
            for (PartDefinition part : mods.getPart()) {
                for (DetailDefinition detail : part.getDetail()) {
                    if (!detail.getNumber().isEmpty()) {
                        for (StringPlusLanguage number : detail.getNumber()) {
                            if (title == null) {
                                title = new TitleInfoDefinition();
                                mods.getTitleInfo().add(title);
                            }
                            title.getPartNumber().add(number);
                        }
                    }
                }
            }
        }
        mods.getPart().clear();
        addRecordInfo(mods);
        return mods;
    }

    private static ModsDefinition copyMods(ModsDefinition mods, ModsDefinition parentMods) {
        mods.getAbstract().addAll(parentMods.getAbstract());
        mods.getClassification().addAll(parentMods.getClassification());
        mods.getExtension().addAll(parentMods.getExtension());
        mods.getGenre().addAll(parentMods.getGenre());
        mods.getIdentifier().addAll(parentMods.getIdentifier());
        mods.getLanguage().addAll(parentMods.getLanguage());
        mods.getLocation().addAll(parentMods.getLocation());
        mods.getName().addAll(parentMods.getName());
        mods.getNote().addAll(parentMods.getNote());
        mods.getOriginInfo().addAll(parentMods.getOriginInfo());
        mods.getPart().addAll(parentMods.getPart());
        mods.getPhysicalDescription().addAll(parentMods.getPhysicalDescription());
        mods.getSubject().addAll(parentMods.getSubject());
        mods.getTableOfContents().addAll(parentMods.getTableOfContents());
        mods.getTitleInfo().addAll(parentMods.getTitleInfo());
        mods.getTypeOfResource().addAll(parentMods.getTypeOfResource());
        return mods;
    }

    private static ModsDefinition addRecordInfo(ModsDefinition mods) {
        if (mods.getRecordInfo().isEmpty()) {
            mods.getRecordInfo().add(new RecordInfoDefinition());
        }
        for (RecordInfoDefinition recordInfo : mods.getRecordInfo()) {
            if (recordInfo.getDescriptionStandard().isEmpty()) {
                StringPlusLanguagePlusAuthority descriptionStandard = new StringPlusLanguagePlusAuthority();
                descriptionStandard.setValue(ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR);
                recordInfo.getDescriptionStandard().add(descriptionStandard);
            }
        }
        return mods;
    }

    private static ModsDefinition fixNdkMonographVolumeFromK4Monograph(ModsDefinition mods) {
        LOG.info("Updating mods for Ndk Monograph Volume (previous mode K4 Monograph).");
        addRecordInfo(mods);
        return mods;
    }

    private static ModsDefinition fixNdkPeriodicalVolumeFromK4PeriodicalVolume(ModsDefinition mods) {
        LOG.info("Updating mods for Ndk Peridical Volume (previous mode K4 Periodical Volume).");
        addRecordInfo(mods);

        for (PartDefinition part : mods.getPart()) {
            for (DateDefinition date : part.getDate()) {
                if (date.getValue() != null) {
                    OriginInfoDefinition originInfo = new OriginInfoDefinition();
                    originInfo.getDateIssued().add(date);
                    mods.getOriginInfo().add(originInfo);
                }
            }
            for (DetailDefinition detail : part.getDetail()) {
                if (detail.getType().equals("volume") && !detail.getNumber().isEmpty()) {
                    for (StringPlusLanguage number : detail.getNumber()) {
                        TitleInfoDefinition titleInfo = new TitleInfoDefinition();
                        titleInfo.getPartNumber().add(number);
                        mods.getTitleInfo().add(titleInfo);
                    }
                }
            }
        }
        mods.getPart().clear();
        return mods;
    }

    private static ModsDefinition fixNdkPeriodicalIssueFromK4PeriodicalIssue(ModsDefinition mods, ModsDefinition rootMods) throws DigitalObjectException {
        LOG.info("Updating mods for Ndk Peridical Issue (previous mode K4 Periodical Issue).");
        addRecordInfo(mods);

        TitleInfoDefinition title = null;
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            title = titleInfo;
            break;
        }
        for (PartDefinition part : mods.getPart()) {
            for (DateDefinition date : part.getDate()) {
                if (date.getValue() != null) {
                    OriginInfoDefinition originInfo = new OriginInfoDefinition();
                    originInfo.getDateIssued().add(date);
                    mods.getOriginInfo().add(originInfo);
                }
            }
            for (DetailDefinition detail : part.getDetail()) {
                if (detail.getType().equals("issue") && !detail.getNumber().isEmpty()) {
                    for (StringPlusLanguage number : detail.getNumber()) {
                        if (title == null) {
                            title = new TitleInfoDefinition();
                        }
                        title.getPartNumber().add(number);
                    }
                }
            }
        }
        if (title != null) {
            if (!mods.getTitleInfo().contains(title)) {
                mods.getTitleInfo().add(title);
            }
        }

        if (rootMods != null) {
            if (mods.getLanguage().isEmpty()) {
                if (!rootMods.getLanguage().isEmpty()) {
                    mods.getLanguage().addAll(rootMods.getLanguage());
                }
            }
            if (mods.getLocation().isEmpty()) {
                if (!rootMods.getLocation().isEmpty()) {
                    mods.getLocation().addAll(rootMods.getLocation());
                }
            }
            TitleInfoDefinition rootTitleInfo = null;
            for (TitleInfoDefinition titleInfo : rootMods.getTitleInfo()) {
                if (titleInfo.getType() == null) {
                    rootTitleInfo = titleInfo;
                    break;
                }
            }
            if (rootTitleInfo != null && title != null) {
                if (title.getTitle().isEmpty() && !rootTitleInfo.getTitle().isEmpty()) {
                    title.getTitle().addAll(rootTitleInfo.getTitle());
                }
                if (title.getSubTitle().isEmpty() && !rootTitleInfo.getSubTitle().isEmpty()) {
                    title.getSubTitle().addAll(rootTitleInfo.getSubTitle());
                }
            }
        }

        mods.getPart().clear();
        return mods;
    }

    private static ModsDefinition fixNdkPeriodicalFromK4Periodical(ModsDefinition mods) {
        LOG.info("Updating mods for Ndk Peridical (previous mode K4 Periodical).");
        return addRecordInfo(mods);
    }

    private void fixCollectionOfClippingsVolumeMods(ModsDefinition mods, String parentPid) throws DigitalObjectException {
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            titleInfo.getTitle().clear();
            titleInfo.getTitle().addAll(titleInfo.getPartName());
            titleInfo.getPartName().clear();
        }
    }

    private void fixNdkMonographVolumeMods(ModsDefinition mods, String parentPid) throws DigitalObjectException {
        String title = null;
        if (parentPid != null) {
            title = getTitle(getParentMods(parentPid));
        }
        if (title != null) {
            fixTitleInfo(title, mods);
            fixOriginInfo(mods);
        }
        fixRecordInfo(mods);
    }

    private void fixOriginInfo(ModsDefinition mods) {
        for (OriginInfoDefinition originInfo : mods.getOriginInfo()) {
            if (originInfo.getDateIssued().isEmpty()) {
                originInfo.getDateIssued().add(new DateDefinition());
            }
            for (DateDefinition date : originInfo.getDateIssued()) {
                date.setValue("20/21. století");
            }
        }
    }

    private void fixTitleInfo(String titleValue, ModsDefinition mods) {
        for (TitleInfoDefinition titleInfo : mods.getTitleInfo()) {
            titleInfo.getPartName().addAll(titleInfo.getTitle());
            titleInfo.getTitle().clear();
            StringPlusLanguage title = new StringPlusLanguage();
            title.setValue(titleValue);
            titleInfo.getTitle().add(title);
        }
    }

    private String getTitle(ModsDefinition mods) {
        if (mods != null) {
            for (TitleInfoDefinition title : mods.getTitleInfo()) {
                for (StringPlusLanguage titleInfo : title.getTitle()) {
                    return titleInfo.getValue();
                }
            }
        }
        return null;
    }

    public static ModsDefinition fixNdkPageMods(ModsDefinition mods) {
        mods = fixPartNdkPage(mods);
        mods = fixGenre(mods, getPageType(mods));
        return mods;
    }

    private void fixPageMods(ModsDefinition mods) {
        fixPartPage(mods);
        fixGenre(mods, null);
    }


    private void fixOldPrintMods(ModsDefinition mods) {
        fixPartPage(mods);
        fixGenre(mods, null);
    }

    private void fixRecordInfo(ModsDefinition mods) {
        for (RecordInfoDefinition recordInfo : mods.getRecordInfo()) {
            if (recordInfo.getDescriptionStandard().size() == 0) {
                StringPlusLanguagePlusAuthority descriptionStandard = new StringPlusLanguagePlusAuthority();
                recordInfo.getDescriptionStandard().add(descriptionStandard);
            }
            for (StringPlusLanguagePlusAuthority descriptionStandard : recordInfo.getDescriptionStandard()) {
                descriptionStandard.setValue(ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR);
            }
        }
    }

    private void fixPartPage(ModsDefinition mods) {
        List<PartDefinition> partDefinitions = new ArrayList<>();
        String pageType = null;
        String pageNumber = null;
        String pageIndex = null;
        for (PartDefinition part : mods.getPart()) {
            if (pageType == null) {
                pageType = part.getType();
            }
            for (DetailDefinition detail : part.getDetail()) {
                if ("pageNumber".equals(detail.getType()) || "page number".equals(detail.getType())) {
                    pageNumber = getValue(detail);
                } else if ("pageIndex".equals(detail.getType())) {
                    pageIndex = getValue(detail);
                }
            }
        }
        if (NdkNewPageMapper.PAGE_TYPE_NORMAL.equals(pageType)) {
            pageType = null;
        }
        partDefinitions.add(createPart(pageType, pageNumber, pageIndex));

        mods.getPart().clear();
        mods.getPart().addAll(partDefinitions);

    }

    private static ModsDefinition fixPartNdkPage(ModsDefinition mods) {
        List<PartDefinition> partDefinitions = new ArrayList<>();
        String pageType = null;
        String pageNumber = null;
        String pageIndex = null;
        for (PartDefinition part : mods.getPart()) {
            if (pageType == null) {
                pageType = part.getType();
            }
            for (DetailDefinition detail : part.getDetail()) {
                if ("pageNumber".equals(detail.getType()) || "page number".equals(detail.getType())) {
                    pageNumber = getValue(detail);
                } else if ("pageIndex".equals(detail.getType())) {
                    pageIndex = getValue(detail);
                }
            }
        }
        if (pageType == null) {
            pageType = NdkNewPageMapper.PAGE_TYPE_NORMAL;
        }
        partDefinitions.add(createPageNumberDetail(pageType, pageNumber));
        partDefinitions.add(createPageIndexDetail(pageIndex));

        mods.getPart().clear();
        mods.getPart().addAll(partDefinitions);
        return mods;
    }

    private static DetailDefinition setValue(String type, String value) {
        StringPlusLanguage number = new StringPlusLanguage();
        number.setValue(value);

        DetailDefinition detail = new DetailDefinition();
        detail.setType(type);
        detail.getNumber().add(number);

        return detail;
    }

    private static PartDefinition createPart(String pageType, String pageNumber, String pageIndex) {
        PartDefinition part = new PartDefinition();
        part.getDetail().add(setValue("pageIndex", pageIndex));
        part.getDetail().add(setValue("pageNumber", pageNumber));
        part.setType(pageType);

        return part;
    }

    private static PartDefinition createPageIndexDetail(String pageIndex) {
        PartDefinition part = new PartDefinition();
        part.getDetail().add(setValue("pageIndex", pageIndex));
        return part;
    }

    private static PartDefinition createPageNumberDetail(String pageType, String pageNumber) {
        PartDefinition part = new PartDefinition();
        part.setType(pageType);
        part.getDetail().add(setValue("pageNumber", pageNumber));
        return part;
    }

    private static String getValue(DetailDefinition detail) {
        String value = null;
        for (StringPlusLanguage number : detail.getNumber()) {
            if (value == null) {
                value = number.getValue();
            }
        }
        return value;
    }

    private static String getPageType(ModsDefinition mods) {
        String pageType = null;
        for (PartDefinition part : mods.getPart()) {
            pageType = part.getType();
            break;
        }
        return pageType;
    }

    private static ModsDefinition fixGenre(ModsDefinition mods, String pageType) {
        if (mods.getGenre().isEmpty()) {
            GenreDefinition genre = new GenreDefinition();
            genre.setValue("page");
            mods.getGenre().add(genre);
        }
        if (mods.getGenre().size() > 0) {
            mods.getGenre().get(0).setType(pageType);
        }
        return mods;
    }

    private ModsDefinition getParentMods(String parentPid) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        ProArcObject fo = dom.find(parentPid, null);
        XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
        return modsStreamEditor.read();
    }


    private void changeModel(DigitalObjectManager dom, String pid, String model) throws DigitalObjectException {
        ProArcObject proArcObject = dom.find(pid, null);
        DigitalObjectHandler handler = dom.createHandler(proArcObject);
        RelationEditor editor = handler.relations();
        editor.setModel(model);
        editor.write(editor.getLastModified(), "Change model");
        handler.commit();
    }

    private void findChildrens(IMetsElement element, boolean searchInChildren) throws DigitalObjectException {
        if (element == null) {
            throw new DigitalObjectException(pid, "ChangeModels:findChildrens - element is null");
        }
        String modelId = element.getModel().substring(12);
        if (oldModel.equals(modelId)) {
            pids.add(element.getOriginalPid());
        }

        if (searchInChildren) {
            for (IMetsElement childElement : element.getChildren()) {
                findChildrens(childElement, searchInChildren);
            }
        }
    }

    public IMetsElement getElement() throws DigitalObjectException {
        try {
            MetsContext metsContext = null;
            ProArcObject object = null;
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                FedoraStorage rstorage = FedoraStorage.getInstance(appConfig);
                object = rstorage.find(pid);
                metsContext = buildFedoraContext(object, null, null, rstorage, appConfig.getNdkExportOptions());
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                object = akubraStorage.find(pid);
                metsContext = buildAkubraContext(object, null, null, akubraStorage, appConfig.getNdkExportOptions());
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
            DigitalObject dobj = MetsUtils.readFoXML(metsContext, object);
            if (dobj == null) {
                return null;
            }
            return MetsElement.getElement(dobj, null, metsContext, true);
        } catch (IOException | MetsExportException ex) {
            throw new DigitalObjectException(pid, "ChangeModels:getElement - impossible to find element", ex);
        }
    }

    public class ChangeModelResult {

        private String pid;
        private DigitalObjectException ex;

        public ChangeModelResult(String updatedPid, DigitalObjectException e) {
            pid = updatedPid;
            ex = e;
        }

        public String getPid() {
            return pid;
        }

        public DigitalObjectException getEx() {
            return ex;
        }
    }
}
