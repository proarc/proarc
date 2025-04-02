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
package cz.cas.lib.proarc.common.storage;

import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import static cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils.STATUS_ASSIGN;
import static cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils.STATUS_CONNECTED;
import static cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils.STATUS_DESCRIBED;
import static cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils.STATUS_EXPORTED;
import static cz.cas.lib.proarc.common.object.DigitalObjectStatusUtils.STATUS_NEW;
import static cz.cas.lib.proarc.common.process.export.mets.MetsContext.buildAkubraContext;
import static cz.cas.lib.proarc.common.process.export.mets.MetsContext.buildFedoraContext;

/**
 * Handles administrative and technical metadata of digital objects.
 *
 * @author Jan Pokorsky
 */
public final class AtmEditor {

    /** helper to clear current value */
    public static final String NULL = "null";

    private final ProArcObject fobject;
    private final SearchView search;

    public AtmEditor(ProArcObject object, SearchView search) {
        this.fobject = object;
        this.search = search;
    }

    public AtmItem read() throws DigitalObjectException {
        return findAtm(fobject);
    }

    /**
     * Updates metadata.
     * @param deviceId device ID to update. Use {@link #NULL} for clearing
     * @param softwareId software ID to update. Use {@link #NULL} for clearing
     * @param organization  ID to update. Use {@link #NULL} for clearing
     * @param message audit message
     * @param role
     * @throws DigitalObjectException
     */
    public void write(String deviceId, String softwareId, String organization, String user, String status, String donator, String archivalCopiesPath, String message, String role) throws DigitalObjectException {
        boolean changedUser = false;
        RelationEditor relationEditor = new RelationEditor(fobject);
        boolean write = false;
        // check deviceId exist
        if (deviceId != null && !deviceId.isEmpty()) {
            String oldVal = relationEditor.getDevice();
            String newVal = NULL.equals(deviceId) ? null : deviceId;
            if (newVal == null ? oldVal != null : !newVal.equals(oldVal)) {
                relationEditor.setDevice(newVal);
                write = true;
            }
        }
        // check softwareId exist
        if (softwareId != null && !softwareId.isEmpty()) {
            String oldVal = relationEditor.getSoftware();
            String newVal = NULL.equals(softwareId) ? null : softwareId;
            if (newVal == null ? oldVal != null : !newVal.equals(oldVal)) {
                relationEditor.setSoftware(newVal);
                write = true;
                // todo check existing premis stream, if yes regenerate??
            }
        }
        if (user != null && !user.isEmpty()) {
            String oldVal = relationEditor.getUser();
            String newVal = NULL.equals(user) ? null : user;
            if (newVal == null ? oldVal != null : !newVal.equals(oldVal)) {
                if ("user".equals(role)) {
                    throw new DigitalObjectException(fobject.getPid(), "Nemáte právo měnit zpracovatele záznamu.");
                }
                relationEditor.setUser(user);
                changedUser = true;
                write = true;
            }
        }
        if (organization != null && !organization.isEmpty()) {
            String oldVal = relationEditor.getOrganization();
            String newVal = NULL.equals(organization) ? null : organization;
            if (newVal == null ? oldVal != null : !newVal.equals(oldVal)) {
                if ("admin".equals(role) || "user".equals(role)) {
                    throw new DigitalObjectException(fobject.getPid(), "Nemáte právo měnit organizaci záznamu.");
                }
                relationEditor.setOrganization(organization);
                write = true;
            }
        }
        if (status != null && !status.isEmpty()) {
            String oldVal = relationEditor.getStatus();
            String newVal = NULL.equals(status) ? null : status;
            if (newVal == null ? oldVal != null : !newVal.equals(oldVal)) {
                relationEditor.setStatus(status);
                write = true;
            } else if (newVal.equals(oldVal) && changedUser){
                relationEditor.setStatus(STATUS_ASSIGN);
                write = true;
            }
        }
        relationEditor.setDonator(donator);
        relationEditor.setArchivalCopiesPath(archivalCopiesPath);
        relationEditor.write(relationEditor.getLastModified(), message);
    }

    /**
     * Updates metadata.
     * @param organization  ID to update. Use {@link #NULL} for clearing
     * @param user  ID to update. Use {@link #NULL} for clearing
     * @param status  ID to update. Use {@link #NULL} for clearing
     * @param message audit message
     * @param fobject
     * @throws DigitalObjectException
     */
    public void writeOrganizationUserState(String organization, String user, String status, String donator, String message, ProArcObject fobject) throws DigitalObjectException {
        RelationEditor relationEditor = new RelationEditor(fobject);
        boolean write = false;
        if (user != null && !user.isEmpty()) {
            String oldVal = relationEditor.getUser();
            String newVal = NULL.equals(user) ? null : user;
            if (newVal == null ? oldVal != null : !newVal.equals(oldVal)) {
                relationEditor.setUser(user);
                write = true;

                UserProfile processor = UserUtil.getDefaultManger().find(user);
                if (processor != null) {
                    organization = processor.getOrganization();
                }
            }
        }

        // check organization exist
        if (organization != null && !organization.isEmpty()) {
            String oldVal = relationEditor.getOrganization();
            String newVal = NULL.equals(organization) ? null : organization;
            if (newVal == null ? oldVal != null : !newVal.equals(oldVal)) {
                relationEditor.setOrganization(organization);
                write = true;
            }
        }
        if (STATUS_NEW.equals(status) || STATUS_DESCRIBED.equals(status) || STATUS_EXPORTED.equals(status) || STATUS_CONNECTED.equals(status)) {
            if (status != null && !status.isEmpty()) {
                String oldVal = relationEditor.getStatus();
                String newVal = NULL.equals(status) ? null : status;
                if (newVal == null ? oldVal != null : !newVal.equals(oldVal)) {
                    relationEditor.setStatus(status);
                    write = true;
                }
            }
        }
        relationEditor.setDonator(donator);
        relationEditor.write(relationEditor.getLastModified(), message);
    }

    /**
     * Updates metadata.
     * @param status  ID to update. Use {@link #NULL} for clearing
     * @param message audit message
     * @throws DigitalObjectException
     */
    public void writeStatus(String status, String message) throws DigitalObjectException {
        RelationEditor relationEditor = new RelationEditor(fobject);
        if (status != null && !status.isEmpty()) {
            String oldVal = relationEditor.getStatus();
            String newVal = NULL.equals(status) ? null : status;
            if (newVal == null ? oldVal != null : !newVal.equals(oldVal)) {
                relationEditor.setStatus(status);
                relationEditor.write(relationEditor.getLastModified(), message);
            }
        }
    }

    private AtmItem findAtm(ProArcObject fobject) throws DigitalObjectException {
        AtmItem atm = new AtmItem();
        String pid = fobject.getPid();
        atm.pid = pid;
        if (fobject instanceof LocalObject) {
            LocalObject lo = (LocalObject) fobject;
            atm.owner = lo.getOwner();
            // times take from FOXML or File?
        } else {
            try {
                List<SearchViewItem> searchItems = search.find(pid);
                SearchViewItem searchItem = searchItems.get(0);
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
        atm.softwareId = relationEditor.getSoftware();
        atm.importFile = relationEditor.getImportFile();
        atm.model = relationEditor.getModel();
        atm.export = relationEditor.getExportResult();
        atm.organization = relationEditor.getOrganization();
        atm.user = relationEditor.getUser();
        atm.status = relationEditor.getStatus();
        atm.archiveExport = relationEditor.getArchiveExportResult();
        atm.krameriusExport = relationEditor.getKrameriusExportResult();
        atm.ndkExport = relationEditor.getNdkExportResult();
        atm.crossrefExport = relationEditor.getCrossrefExportResult();
        atm.isLocked = relationEditor.isLocked();
        atm.lockedBy = relationEditor.getLockedBy();
        atm.lockedDate = relationEditor.getLockedDate();
        atm.donator = relationEditor.getDonator();
        atm.archivalCopies = relationEditor.getArchivalCopiesPath();
        return atm;
    }

    public void setChild(String parentPid, String organization, String user, String state, String donator, AppConfiguration appConfig, AkubraConfiguration akubraConfiguration, SearchView search, String sessionLog) throws DigitalObjectException, IOException, AppConfigurationException {
        List<String> pids = findElements(parentPid, appConfig, akubraConfiguration);
        for (String pid : pids) {
            ProArcObject fobject = findFedoraObject(pid, appConfig);
            AtmEditor editor = new AtmEditor(fobject, search);
            editor.writeOrganizationUserState(organization, user, state, donator, sessionLog, fobject);
            fobject.flush();
        }
    }

    private ProArcObject findFedoraObject(String pid, AppConfiguration appConfig) throws IOException, AppConfigurationException {
        if (pid == null) {
            throw new NullPointerException("pid");
        }
        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            return FedoraStorage.getInstance(appConfig).find(pid);
        } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            AkubraConfiguration akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfig.getConfigHome());
            return AkubraStorage.getInstance(akubraConfiguration).find(pid);
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }
    }

    private List<String> findElements(String parentPid, AppConfiguration config, AkubraConfiguration akubraConfiguration) throws DigitalObjectException {
        List<String> pids = new ArrayList<>();
        IMetsElement element = getElement(parentPid, config, akubraConfiguration);
        if (element == null) {
            throw new DigitalObjectException("Process: Set organization failed - impossible to get element");
        }
        findChildrens(element, pids);
        pids.remove(element.getOriginalPid());
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

    public IMetsElement getElement(String parentPid, AppConfiguration config, AkubraConfiguration akubraConfiguration) throws DigitalObjectException {
        try {
            MetsContext metsContext = null;
            ProArcObject object = null;

            if (Storage.FEDORA.equals(config.getTypeOfStorage())) {
                FedoraStorage fedoraStorage = FedoraStorage.getInstance(config);
                object = fedoraStorage.find(parentPid);
                metsContext = buildFedoraContext(object, null, null, fedoraStorage, config.getNdkExportOptions());
            } else if (Storage.AKUBRA.equals(config.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                object = akubraStorage.find(parentPid);
                metsContext = buildAkubraContext(object, null, null, akubraStorage, config.getNdkExportOptions());
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + config.getTypeOfStorage());
            }
            DigitalObject dobj = MetsUtils.readFoXML(object.getPid(), metsContext);
            if (dobj == null) {
                throw new DigitalObjectException(parentPid, "Process: Changing models failed - imposible to find element");
            }
            return MetsElement.getElement(dobj, null, metsContext, true);
        } catch (IOException | MetsExportException ex) {
            throw new DigitalObjectException(parentPid, "Process: Changing models failed - imposible to find element", ex);
        }
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
        private String softwareId;
        private String importFile;
        private String export;
        private String organization;
        private String user;
        private String ndkExport;
        private String krameriusExport;
        private String archiveExport;
        private String crossrefExport;
        private String status;
        private boolean isLocked;
        private String lockedBy;
        private Date lockedDate;
        private String donator;
        private String archivalCopies;

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

        public String getSoftwareId() {
            return softwareId;
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

        public String getUser() {
            return user;
        }

        public String getNdkExport() {
            return ndkExport;
        }

        public String getKrameriusExport() {
            return krameriusExport;
        }

        public String getArchiveExport() {
            return archiveExport;
        }

        public String getCrossrefExport() {
            return crossrefExport;
        }

        public String getStatus() {
            return status;
        }

        public boolean isLocked() {
            return isLocked;
        }

        public String getLockedBy() {
            return lockedBy;
        }

        public Date getLockedDate() {
            return lockedDate;
        }

        public String getDonator() { return donator;}

        public String getArchivalCopies() {
            return archivalCopies;
        }
    }

}
