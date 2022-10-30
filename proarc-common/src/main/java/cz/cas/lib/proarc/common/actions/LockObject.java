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
import cz.cas.lib.proarc.common.fedora.Storage;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.common.export.mets.MetsContext.buildAkubraContext;
import static cz.cas.lib.proarc.common.export.mets.MetsContext.buildFedoraContext;

public class LockObject {

    private static final Logger LOGGER = Logger.getLogger(LockObject.class.getName());

    private static AppConfiguration appConfig;
    private static AkubraConfiguration akubraConfiguration;
    private static String pid;
    private static String username;
    private List<String> pids;

    public LockObject(AppConfiguration config, AkubraConfiguration akubraConfiguration, String pid, String username) {
        this.appConfig = config;
        this.akubraConfiguration = akubraConfiguration;
        this.pid = pid;
        this.username = username;
        this.pids = new ArrayList<>();
    }


    public List<String> findObjects() throws DigitalObjectException {
        IMetsElement element = getElement();
        if (element == null) {
            throw new DigitalObjectException(pid, "LockObject:findObjects - object is null");
        }
        findChildrens(element);
        return pids;
    }

    private void findChildrens(IMetsElement element) throws DigitalObjectException {
        if (element == null) {
            throw new DigitalObjectException(pid, "LockObject:findChildrens - element is null");
        }
        pids.add(element.getOriginalPid());

        for (IMetsElement childElement : element.getChildren()) {
                findChildrens(childElement);
        }
    }

    public IMetsElement getElement() throws DigitalObjectException {
        try {
            MetsContext metsContext = null;
            FedoraObject object = null;
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                RemoteStorage rstorage = RemoteStorage.getInstance(appConfig);
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

    public LockObjectResult setLocked() {
        int updated = 0;
        String updatedPid = "";
        try {
            for (String pid : pids) {
                updatedPid = pid;
                DigitalObjectManager dom = DigitalObjectManager.getDefault();
                setLocked(dom, pid);
                updated++;
            }
            if (updated == 0) {
                LOGGER.log(Level.WARNING, "No objects to locked");
            } else {
                LOGGER.log(Level.WARNING, "Objects locked succesfully. Total objects (" + updated + ").");
            }
            return null;
        } catch (DigitalObjectException ex) {
            String message = "Locking objects failed, totaly items (" + pids.size() + "), locked only " + updated + "objects.";
            LOGGER.log(Level.SEVERE, message);
            return new LockObjectResult(updatedPid, new DigitalObjectException(pid, message));
        }
    }

    public LockObjectResult setUnlocked() {
        int updated = 0;
        String updatedPid = "";
        try {
            for (String pid : pids) {
                updatedPid = pid;
                DigitalObjectManager dom = DigitalObjectManager.getDefault();
                setUnlocked(dom, pid);
                updated++;
            }
            if (updated == 0) {
                LOGGER.log(Level.WARNING, "No objects to locked");
            } else {
                LOGGER.log(Level.WARNING, "Objects locked succesfully. Total objects (" + updated + ").");
            }
            return null;
        } catch (DigitalObjectException ex) {
            String message = "Locking objects failed, totaly items (" + pids.size() + "), locked only " + updated + "objects.";
            LOGGER.log(Level.SEVERE, message);
            return new LockObjectResult(updatedPid, new DigitalObjectException(pid, message));
        }
    }

    private void setLocked(DigitalObjectManager dom, String pid) throws DigitalObjectException {
        FedoraObject fedoraObject = dom.find(pid,null);
        DigitalObjectHandler handler = dom.createHandler(fedoraObject);
        RelationEditor relationEditor = handler.relations();
        relationEditor.setLock(username);
        relationEditor.write(relationEditor.getLastModified(), "Locked object by " + username);
        handler.commit();
    }

    private void setUnlocked(DigitalObjectManager dom, String pid) throws DigitalObjectException {
        FedoraObject fedoraObject = dom.find(pid,null);
        DigitalObjectHandler handler = dom.createHandler(fedoraObject);
        RelationEditor relationEditor = handler.relations();
        relationEditor.setUnlock();
        relationEditor.write(relationEditor.getLastModified(), "Unlocked object by " + username);
        handler.commit();
    }

    public class LockObjectResult {

        private String pid;
        private DigitalObjectException ex;

        public LockObjectResult(String updatedPid, DigitalObjectException e) {
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
