package cz.cas.lib.proarc.common.actions;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;


public class ChangeModels {

    public static final String CHANGE_MODEL_PAGE_TO_NDK_PAGE = "page-NdkPage";

    private static AppConfiguration appConfig;
    private static String pid;
    private static String modelId;
    private static String oldModel;
    private static String newModel;
    private List<String> pids;


    public ChangeModels(AppConfiguration appConfig, String pid, String modelId, String oldModel, String newModel) {
        this.appConfig = appConfig;
        this.pid = pid;
        this.modelId = modelId;
        this.oldModel = oldModel;
        this.newModel = newModel;
        this.pids = new ArrayList<>();
    }

    public List<String> findObjects() throws DigitalObjectException {
        IMetsElement element = getElement();
        if (element == null) {
            throw new DigitalObjectException("Process: Changing models failed - impossimble to get element");
        }
        findChildrens(element);
        return pids;
    }

    public String findRootObject() throws DigitalObjectException {
        IMetsElement element = getElement();
        if (element == null) {
            throw new DigitalObjectException("Process: Changing models failed - impossimble to get element");
        }
        return getRootElement(element);
    }

    private String getRootElement(IMetsElement element) {
        return element.getMetsContext().getRootElement().getOriginalPid();
    }

    public void changeModels() throws DigitalObjectException {
        if (pids.isEmpty()) {
            throw new DigitalObjectException(pid, "Process: Changing models failed - no models found");
        }
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        for (String pid : pids) {
            changeModel(dom, pid);
        }

    }

    private void changeModel(DigitalObjectManager dom, String pid) throws DigitalObjectException {
        FedoraObject fedoraObject = dom.find(pid, null);
        DigitalObjectHandler handler = dom.createHandler(fedoraObject);
        RelationEditor editor = handler.relations();
        editor.setModel(newModel);
        editor.write(editor.getLastModified(), "Change model");
        handler.commit();
    }

    private void findChildrens(IMetsElement element) throws DigitalObjectException {
        if (element == null) {
            throw new DigitalObjectException("Process: Changing models failed - impossimble to get element");
        }
        String modelId = element.getModel().substring(12);
        if (oldModel.equals(modelId)) {
            pids.add(element.getOriginalPid());
        }

        for (IMetsElement childElement : element.getChildren()) {
            findChildrens(childElement);
        }
    }

    public IMetsElement getElement() throws DigitalObjectException {
        try {
            RemoteStorage rstorage = RemoteStorage.getInstance(appConfig);
            RemoteStorage.RemoteObject robject = rstorage.find(pid);
            MetsContext metsContext = buildContext(robject, null, rstorage);
            DigitalObject dobj = MetsUtils.readFoXML(robject.getPid(), robject.getClient());
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
}
