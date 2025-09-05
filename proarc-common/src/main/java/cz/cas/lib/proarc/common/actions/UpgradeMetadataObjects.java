package cz.cas.lib.proarc.common.actions;

import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.common.actions.ChangeModels.fixNdkPageMods;
import static cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils.STATUS_NEW;
import static cz.cas.lib.proarc.common.process.export.mets.MetsContext.buildAkubraContext;
import static cz.cas.lib.proarc.common.process.export.mets.MetsContext.buildFedoraContext;

public class UpgradeMetadataObjects {

    private static final Logger LOG = Logger.getLogger(UpgradeMetadataObjects.class.getName());

    private static AppConfiguration config;
    private static AkubraConfiguration akubraConfiguration;
    private static UserProfile user;
    private static Locale locale;
    private List<String> pids;
    private int updatedObjects;

    public UpgradeMetadataObjects(AppConfiguration appConfig, AkubraConfiguration akubraConfiguration, UserProfile user, Locale locale) {
        this.config = appConfig;
        this.akubraConfiguration = akubraConfiguration;
        this.user = user;
        this.locale = locale;
        pids = new ArrayList<>();
        updatedObjects = 0;
    }

    public List<String> getPids() {
        return pids;
    }

    public List<SearchViewItem> findAllObjects() throws IOException, FedoraClientException {
        SearchView search = null;
        if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
            FedoraStorage remote = FedoraStorage.getInstance(config);
            search = remote.getSearch(locale);
        } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
            AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
            search = akubraStorage.getSearch(locale);
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
        }
        List<SearchViewItem> items = search.findAllObjects();
        return items;
    }

    public void setOrganization(List<SearchViewItem> items, String defaultProcessor) throws DigitalObjectException {
        for (SearchViewItem item : items) {
            setOrganization(item.getPid(), defaultProcessor);
        }
    }

    private void setOrganization(String pid, String defaultProcesor) throws DigitalObjectException {
        try {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            ProArcObject fo = dom.find(pid, null);
            DigitalObjectHandler doh = dom.createHandler(fo);
            RelationEditor relations = doh.relations();
            if (relations.getOrganization() != null && !relations.getOrganization().equals(".") && relations.getUser() != null && relations.getStatus() != null) {
                return;
            }
            if (relations.getOrganization() == null || (relations.getOrganization() != null && relations.getOrganization().equals("."))) {
                relations.setOrganization(user.getOrganization());
            }
            if (relations.getUser() == null) {
                relations.setUser(defaultProcesor);
            }
            if (relations.getStatus() == null) {
                relations.setStatus(STATUS_NEW);
            }
            relations.write(relations.getLastModified(), "Add organization to foxml");
            doh.commit();
            updatedObjects++;
        } catch (DigitalObjectException ex) {
            LOG.log(Level.WARNING, "Unable to update object " + pid);
        }
    }

    public Map<String, Integer> countObjects(List<SearchViewItem> items) {
        Map<String, Integer> map = new HashMap<>();
        for (SearchViewItem item : items) {
            if (map.containsKey(item.getModel())) {
                Integer value = map.get(item.getModel());
                value++;
                map.put(item.getModel(), value);
            } else {
                map.put(item.getModel(), 1);
            }
        }
        return map;
    }

    public void findObjects(String pid, String model) throws DigitalObjectException {
        IMetsElement element = getElement(pid);
        if (element == null) {
            throw new DigitalObjectException("Process: Update " + model + " - impossimble to get element");
        }
        findChildrens(element, model);
    }


    public List<SearchViewItem> findObjectsWithType(String pid, String model, String pageType) throws DigitalObjectException, IOException, FedoraClientException {
        if (pid != null) {
            IMetsElement element = getElement(pid);
            if (element == null) {
                throw new DigitalObjectException("Process: Update " + model + " - impossimble to get element");
            }
            findChildrens(element, model);
        }

        SearchView search = null;
        try {
            if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
                FedoraStorage rstorage = FedoraStorage.getInstance(config);
                search = rstorage.getSearch(locale);
            } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())){
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                search = akubraStorage.getSearch(locale);
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
            }
        } catch (IOException e) {
            throw new DigitalObjectException(model, e);
        }

        return search.findAdvancedSearchItems(mergePids(pids), pageType, null, null, null, null, MetaModel.MODELS_LEAF, null, null, null, null, "created", "desc", 0, Integer.MAX_VALUE);
    }

    private String mergePids(List<String> pids) {
        if (pids.isEmpty()) {
            return null;
        }
        StringBuilder builder = new StringBuilder();
        boolean isFirst = true;
        for (String pid : pids) {
            if (isFirst) {
                builder.append(pid);
            } else {
                builder.append(",").append(pid);
            }
        }
        return builder.toString();
    }

    private void findChildrens(IMetsElement element, String model) throws DigitalObjectException {
        if (element == null) {
            throw new DigitalObjectException("Process: Update " + model + " - impossimble to get element");
        }
        String modelId = element.getModel().substring(12);
        if (model.equals(modelId)) {
            pids.add(element.getOriginalPid());
        }
        for (IMetsElement child : element.getChildren()) {
            findChildrens(child, model);
        }
    }

    public IMetsElement getElement(String pid) throws DigitalObjectException {
        try {
            MetsContext metsContext = null;
            ProArcObject object = null;
            if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
                FedoraStorage rstorage = FedoraStorage.getInstance(config);
                object = rstorage.find(pid);
                metsContext = buildFedoraContext(object, null, null, rstorage, config.getNdkExportOptions());
            } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                object = akubraStorage.find(pid);
                metsContext = buildAkubraContext(object, null, null, akubraStorage, config.getNdkExportOptions());
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
            }
            DigitalObject dobj = MetsUtils.readFoXML(metsContext, object);
            if (dobj == null) {
                return null;
            }
            return MetsElement.getElement(dobj, null, metsContext, true);
        } catch (IOException | MetsExportException ex) {
            throw new DigitalObjectException("Process: Changing models failed - imposible to find element", ex);
        }
    }

    public void repair(String model) throws DigitalObjectException {
        for (String pid : pids) {
            updateObject(pid, model);
        }
        LOG.log(Level.WARNING, "INFORMATION: Update " + model + " finished succesfully, updated " + updatedObjects + "/" + pids.size() + " object(s).");
    }

    private void updateObject(String pid, String model) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        ProArcObject fo = dom.find(pid, null);
        DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());
        NdkMapper.Context context = new NdkMapper.Context(handler);
        NdkMapper mapper = NdkMapper.get(model);
        mapper.setModelId(model);


        XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
        ModsDefinition mods = modsStreamEditor.read();
        mods = fixMods(mods, model);

        mapper.createMods(mods, context);
        modsStreamEditor.write(mods, modsStreamEditor.getLastModified(), "Update " + model + " - MODS stream");

        OaiDcType dc = mapper.toDc(mods, context);
        DcStreamEditor dcEditor = handler.objectMetadata();
        DcStreamEditor.DublinCoreRecord dcr = dcEditor.read();
        dcr.setDc(dc);
        dcEditor.write(handler, dcr, "Update " + model + " - DC stream");

        fo.setLabel(mapper.toLabel(mods));
        handler.commit();
        updatedObjects++;
    }

    private ModsDefinition fixMods(ModsDefinition mods, String model) {
        if (OldPrintPlugin.MODEL_PAGE.equals(model)) {
            mods = fixNdkPageMods(mods);
        }
        return mods;
    }

    public int getUpdatedObjects() {
        return updatedObjects;
    }


    public void fixPageType(List<SearchViewItem> items, String oldValue, String newValue) throws DigitalObjectException {
        for (SearchViewItem item : items) {
            fixObjectWithPageType(item, oldValue, newValue);
        }
        LOG.log(Level.INFO, "INFORMATION: Update finished, updated " + updatedObjects + "/" + items.size() + " object(s).");
    }

    public void fixObjectWithPageType(SearchViewItem item, String oldValue, String newValue) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        ProArcObject fo = dom.find(item.getPid(), null);
        DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());
        NdkMapper.Context context = new NdkMapper.Context(handler);
        NdkMapper mapper = NdkMapper.get(item.getModel());
        mapper.setModelId(item.getModel());


        XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
        ModsDefinition mods = modsStreamEditor.read();
        mods = updatePageType(mods, oldValue, newValue);

        mapper.createMods(mods, context);
        modsStreamEditor.write(mods, modsStreamEditor.getLastModified(), "Update " + item.getPid() + " - MODS stream");

        OaiDcType dc = mapper.toDc(mods, context);
        DcStreamEditor dcEditor = handler.objectMetadata();
        DcStreamEditor.DublinCoreRecord dcr = dcEditor.read();
        dcr.setDc(dc);
        dcEditor.write(handler, dcr, "Update " + item.getPid() + " - DC stream");

        fo.setLabel(mapper.toLabel(mods));

        handler.commit();
        updatedObjects++;
    }

    private ModsDefinition updatePageType(ModsDefinition mods, String oldValue, String newValue) {
        for (PartDefinition part : mods.getPart()) {
            if (part.getType() != null) {
                if (oldValue.toLowerCase().equals(part.getType().toLowerCase())) {
                    part.setType(newValue);
                }
            }
        }
        return mods;
    }
}
