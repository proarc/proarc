package cz.cas.lib.proarc.common.actions;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapperFactory;
import cz.cas.lib.proarc.common.mods.ndk.NdkNewPageMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectExistException;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.mods.DetailDefinition;
import cz.cas.lib.proarc.mods.GenreDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class RepairMetadata {

    private AppConfiguration appConfig;
    private String model;
    private List<String> pids;

    public RepairMetadata(AppConfiguration appConfig, String model, List<String> pids) {
        this.appConfig = appConfig;
        this.model = model;
        this.pids = pids;
    }


    public void repair() throws DigitalObjectException {
        for (String pid : pids) {
            repairObjects(pid);
        }
    }

    private void repairObjects(String pid) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        FedoraObject fo = dom.find(pid, null);
        XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS,
                MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
        ModsDefinition mods = modsStreamEditor.read();
        fixMods(pid, mods);
        modsStreamEditor.write(mods, modsStreamEditor.getLastModified(), null);

        DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());
        NdkMapperFactory mapperFactory = new NdkMapperFactory();
        NdkMapper mapper = mapperFactory.get(model);
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

    private void fixMods(String pid, ModsDefinition mods) throws DigitalObjectException {
        switch (model) {
            case NdkPlugin.MODEL_PAGE:
                fixPageMods(mods);
                break;
            case NdkPlugin.MODEL_NDK_PAGE:
                fixNdkPageMods(mods);
                break;
            default:
                throw new DigitalObjectException(pid, "Process: Repair metadata dailed - Unsupported model.");
        }
    }

    private void fixNdkPageMods(ModsDefinition mods) {
        fixPartNdkPage(mods);
        fixGenre(mods, getPageType(mods));
    }

    private void fixPageMods(ModsDefinition mods) {
        fixPartPage(mods);
        fixGenre(mods, null);
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

    private void fixPartNdkPage(ModsDefinition mods) {
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
    }

    private DetailDefinition setValue(String type, String value) {
        StringPlusLanguage number = new StringPlusLanguage();
        number.setValue(value);

        DetailDefinition detail = new DetailDefinition();
        detail.setType(type);
        detail.getNumber().add(number);

        return detail;
    }

    private PartDefinition createPart(String pageType, String pageNumber, String pageIndex) {
        PartDefinition part = new PartDefinition();
        part.getDetail().add(setValue("pageIndex", pageIndex));
        part.getDetail().add(setValue("pageNumber", pageNumber));
        part.setType(pageType);

        return part;
    }

    private PartDefinition createPageIndexDetail(String pageIndex) {
        PartDefinition part = new PartDefinition();
        part.getDetail().add(setValue("pageIndex", pageIndex));
        return part;
    }

    private PartDefinition createPageNumberDetail(String pageType, String pageNumber) {
        PartDefinition part = new PartDefinition();
        part.setType(pageType);
        part.getDetail().add(setValue("page number", pageNumber));
        return part;
    }

    private String getValue(DetailDefinition detail) {
        String value = null;
        for (StringPlusLanguage number : detail.getNumber()) {
            if (value == null) {
                value = number.getValue();
            }
        }
        return value;
    }

    private String getPageType(ModsDefinition mods) {
        String pageType = null;
        for (PartDefinition part : mods.getPart()) {
            pageType = part.getType();
            break;
        }
        return pageType;
    }

    private void fixGenre(ModsDefinition mods, String pageType) {
        if (mods.getGenre().isEmpty()) {
            GenreDefinition genre = new GenreDefinition();
            genre.setValue("page");
            mods.getGenre().add(genre);
        }
        if (mods.getGenre().size() > 0) {
            mods.getGenre().get(0).setType(pageType);
        }
    }


}
