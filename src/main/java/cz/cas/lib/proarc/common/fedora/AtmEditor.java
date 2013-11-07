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
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;
import java.io.IOException;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

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
     * @param message audit message
     * @throws DigitalObjectException
     */
    public void write(String deviceId, String message) throws DigitalObjectException {
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
        return atm;
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class AtmItem {

        @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_PID)
        private String pid;
        @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_BATCHID)
        private Integer batchId;
        @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_OWNER)
        private String owner;
        @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_MODEL)
        private String model;
        @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_STATE)
        private String state;
        @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_CREATED)
        private String created;
        @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_MODIFIED)
        private String modified;
        @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_DEVICE)
        private String deviceId;
        @XmlElement(name = DigitalObjectResourceApi.ATM_ITEM_FILENAME)
        private String importFile;

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
    }

}
