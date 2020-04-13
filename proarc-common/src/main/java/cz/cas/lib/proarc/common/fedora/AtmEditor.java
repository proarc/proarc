/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.common.fedora;

import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Handles administrative and technical metadata of digital objects.
 *
 * @author Jan Pokorsky
 */
public final class AtmEditor {

    /** helper to clear current value */
    public static final String NULL = "null";

    private final FedoraObject fobject;
    private final SearchView search;

    public AtmEditor(FedoraObject object, SearchView search) {
        this.fobject = object;
        this.search = search;
    }

    public AtmItem read() throws DigitalObjectException {
        return findAtm(fobject);
    }

    /**
     * Updates metadata.
     * @param deviceId device ID to update. Use {@link #NULL} for clearing
     * @param organization  ID to update. Use {@link #NULL} for clearing
     * @param message audit message
     * @param role
     * @throws DigitalObjectException
     */
    public void write(String deviceId, String organization, String message, String role) throws DigitalObjectException {
        RelationEditor relationEditor = new RelationEditor(fobject);
        // check deviceId exist
        if (deviceId != null && !deviceId.isEmpty()) {
            String oldVal = relationEditor.getDevice();
            String newVal = NULL.equals(deviceId) ? null : deviceId;
            if (newVal == null ? oldVal != null : !newVal.equals(oldVal)) {
                relationEditor.setDevice(newVal);
                relationEditor.write(relationEditor.getLastModified(), message);
            }
        }
        if (organization != null && !organization.isEmpty()) {
            String oldVal = relationEditor.getOrganization();
            String newVal = NULL.equals(organization) ? null : organization;
            if (newVal == null ? oldVal != null : !newVal.equals(oldVal)) {
                if ("admin".equals(role) || "user".equals(role)) {
                    throw new DigitalObjectException(fobject.getPid(), "Nemate pravo měnit organizaci záznamu.");
                }
                relationEditor.setOrganization(organization);
                relationEditor.write(relationEditor.getLastModified(), message);
            }
        }
    }

    /**
     * Updates metadata.
     * @param organization  ID to update. Use {@link #NULL} for clearing
     * @param message audit message
     * @throws DigitalObjectException
     */
    public void writeOrganization(String organization, String message) throws DigitalObjectException {
        RelationEditor relationEditor = new RelationEditor(fobject);
        // check organization exist
        if (organization != null && !organization.isEmpty()) {
            String oldVal = relationEditor.getOrganization();
            String newVal = NULL.equals(organization) ? null : organization;
            if (newVal == null ? oldVal != null : !newVal.equals(oldVal)) {
                relationEditor.setOrganization(organization);
                relationEditor.write(relationEditor.getLastModified(), message);
            }
        }
    }

    private AtmItem findAtm(FedoraObject fobject) throws DigitalObjectException {
        AtmItem atm = new AtmItem();
        String pid = fobject.getPid();
        atm.pid = pid;
        if (fobject instanceof LocalObject) {
            LocalObject lo = (LocalObject) fobject;
            atm.owner = lo.getOwner();
            // times take from FOXML or File?
        } else {
            try {
                List<Item> searchItems = search.find(pid);
                Item searchItem = searchItems.get(0);
                atm.owner = searchItem.getOwner();
                atm.created = searchItem.getCreated();
                atm.modified = searchItem.getModified();
                atm.state = searchItem.getState();
            } catch (FedoraClientException ex) {
                throw new DigitalObjectException(pid, ex);
            } catch (IOException ex) {
                throw new DigitalObjectException(pid, ex);
            }
        }
        RelationEditor relationEditor = new RelationEditor(fobject);
        atm.deviceId = relationEditor.getDevice();
        atm.importFile = relationEditor.getImportFile();
        atm.model = relationEditor.getModel();
        atm.export = relationEditor.getExportResult();
        atm.organization = relationEditor.getOrganization();
        return atm;
    }

    public void setChildOrganization(String parentPid, String organization, AppConfiguration appConfig, SearchView search, String sessionLog) throws DigitalObjectException, IOException {
        List<String> pids = findElements(parentPid, appConfig);
        for (String pid : pids) {
            FedoraObject fobject = findFedoraObject(pid, appConfig);
            AtmEditor editor = new AtmEditor(fobject, search);
            editor.writeOrganization(organization, sessionLog);
            fobject.flush();
        }
    }

    private FedoraObject findFedoraObject(String pid, AppConfiguration appConfig) throws IOException {
        if (pid == null) {
            throw new NullPointerException("pid");
        }
        return RemoteStorage.getInstance(appConfig).find(pid);
    }

    private List<String> findElements(String parentPid, AppConfiguration config) throws DigitalObjectException {
        List<String> pids = new ArrayList<>();
        IMetsElement element = getElement(parentPid, config);
        if (element == null) {
            throw new DigitalObjectException("Process: Set organization failed - impossible to get element");
        }
        findChildrens(element, pids);
        return pids;
    }

    private void findChildrens(IMetsElement element, List<String> pids) throws DigitalObjectException {
        if (element == null) {
            throw new DigitalObjectException("Process: Set organization failed - impossible to get element");
        }
        pids.add(element.getOriginalPid());
        for (IMetsElement childElement : element.getChildren()) {
            findChildrens(childElement, pids);
        }
    }

    public IMetsElement getElement(String parentPid, AppConfiguration config) throws DigitalObjectException {
        try {
            RemoteStorage rstorage = RemoteStorage.getInstance(config);
            RemoteStorage.RemoteObject robject = rstorage.find(parentPid);
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

    public static class AtmItem {

        private String pid;
        private Integer batchId;
        private String owner;
        private String model;
        private String state;
        private String created;
        private String modified;
        private String deviceId;
        private String importFile;
        private String export;
        private String organization;

        public AtmItem() {
        }

        public String getPid() {
            return pid;
        }

        public Integer getBatchId() {
            return batchId;
        }

        public void setBatchId(Integer batchId) {
            this.batchId = batchId;
        }

        public String getOwner() {
            return owner;
        }

        public String getModel() {
            return model;
        }

        public String getState() {
            return state;
        }

        public String getCreated() {
            return created;
        }

        public String getModified() {
            return modified;
        }

        public String getDeviceId() {
            return deviceId;
        }

        public String getImportFile() {
            return importFile;
        }

        public String getExport() {
            return export;
        }

        public String getOrganization() {
            return organization;
        }
    }

}
