package cz.cas.lib.proarc.common.actions;

import com.yourmediashelf.fedora.client.FedoraClientException;
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
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapperFactory;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import static cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils.STATUS_NEW;

public class UpdateObjects {

    private static final Logger LOG = Logger.getLogger(UpdateObjects.class.getName());

    private static AppConfiguration config;
    private static UserProfile user;
    private static Locale locale;
    private List<String> pids;
    private int updatedObjects;

    public UpdateObjects(AppConfiguration appConfig, UserProfile user, Locale locale) {
        this.config = appConfig;
        this.user = user;
        this.locale = locale;
        pids = new ArrayList<>();
        updatedObjects = 0;
    }

    public List<SearchView.Item> findAllObjects() throws IOException, FedoraClientException {
        RemoteStorage remote = RemoteStorage.getInstance(config);
        SearchView search = remote.getSearch(locale);
        List<SearchView.Item> items = search.findAllObjects();
        return items;
    }

    public void setOrganization(List<SearchView.Item> items, String defaultProcessor) throws DigitalObjectException {
        for (SearchView.Item item : items) {
            setOrganization(item.getPid(), defaultProcessor);
        }
    }

    private void setOrganization(String pid, String defaultProcesor) throws DigitalObjectException {
        try {
            DigitalObjectManager dom = DigitalObjectManager.getDefault();
            FedoraObject fo = dom.find(pid, null);
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
        } catch (DigitalObjectException ex)  {
            LOG.log(Level.WARNING, "Unable to update object " + pid);
        }
    }

    public Map<String, Integer> countObjects(List<SearchView.Item>  items) {
        Map<String, Integer> map = new HashMap<>();
        for (SearchView.Item item : items) {
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
            throw new DigitalObjectException("Process: Update Ndk Article - impossimble to get element");
        }
        findChildrens(element, model);
    }

    private void findChildrens(IMetsElement element, String model) throws DigitalObjectException {
        if (element == null) {
            throw new DigitalObjectException("Process: Update Ndk Article - impossimble to get element");
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
            RemoteStorage rstorage = RemoteStorage.getInstance(config);
            RemoteStorage.RemoteObject robject = rstorage.find(pid);
            MetsContext metsContext = buildContext(robject, null, rstorage);
            DigitalObject dobj = MetsUtils.readFoXML(robject.getPid(), robject.getClient());
            if (dobj == null) {
                return null;
            }
            return MetsElement.getElement(dobj, null, metsContext, true);
        } catch (IOException | MetsExportException ex) {
            throw new DigitalObjectException("Process: Changing models failed - imposible to find element");
        }
    }

    private MetsContext buildContext(RemoteStorage.RemoteObject fo, String packageId, RemoteStorage rstorage) {
        MetsContext mc = new MetsContext();
        mc.setFedoraClient(fo.getClient());
        mc.setRemoteStorage(rstorage);
        mc.setPackageID(packageId);
        mc.setOutputPath(null);
        mc.setAllowNonCompleteStreams(false);
        mc.setAllowMissingURNNBN(false);
        mc.setConfig(null);
        return mc;
    }

    public void repair(String model) throws DigitalObjectException {
        for (String pid : pids) {
            updateObject(pid, model);
        }
        LOG.log(Level.WARNING, "INFORMATION: Update " + model + " finished succesfully, updated " + updatedObjects + "/" + pids.size() + " object(s).");
    }

    private void updateObject(String pid, String model) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        FedoraObject fo = dom.find(pid, null);
        DigitalObjectHandler handler = new DigitalObjectHandler(fo, MetaModelRepository.getInstance());
        NdkMapper.Context context = new NdkMapper.Context(handler);
        NdkMapperFactory mapperFactory = new NdkMapperFactory();
        NdkMapper mapper = mapperFactory.get(model);
        mapper.setModelId(model);


        XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
        ModsDefinition mods = modsStreamEditor.read();

        mapper.createMods(mods, context);
        modsStreamEditor.write(mods, modsStreamEditor.getLastModified(), "Update Ndk Article - MODS stream");

        OaiDcType dc = mapper.toDc(mods, context);
        DcStreamEditor dcEditor = handler.objectMetadata();
        DcStreamEditor.DublinCoreRecord dcr = dcEditor.read();
        dcr.setDc(dc);
        dcEditor.write(handler, dcr, "Update Ndk Article - DC stream");

        fo.setLabel(mapper.toLabel(mods));
        handler.commit();
        updatedObjects++;
    }

    public int getUpdatedObjects() {
        return updatedObjects;
    }
}
