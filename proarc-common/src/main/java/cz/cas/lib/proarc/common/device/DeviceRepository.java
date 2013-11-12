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
package cz.cas.lib.proarc.common.device;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import cz.cas.lib.proarc.oaidublincore.ObjectFactory;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import javax.ws.rs.core.Response.Status;
import javax.xml.bind.JAXBElement;

/**
 * The repository of devices producing digital objects.
 *
 * @author Jan Pokorsky
 */
public final class DeviceRepository {

    public static final String METAMODEL_ID = "proarc:device";
    private static final String DEVICE_ID_PREFIX = "device:";

    private final RemoteStorage remoteStorage;

    public DeviceRepository(RemoteStorage remoteStorage) {
        if (remoteStorage == null) {
            throw new NullPointerException("remoteStorage");
        }
        this.remoteStorage = remoteStorage;
    }

    /**
     * Adds a new device.
     * @param owner owner of the object
     * @param label device label
     * @param log log message
     * @return the device
     * @throws DeviceException failure
     */
    public Device addDevice(String owner, String label, String log) throws DeviceException {
        UUID uuid = UUID.randomUUID();
        String pid = DEVICE_ID_PREFIX + uuid.toString();
        try {
            return addDevice(pid, owner, label, log);
        } catch (DigitalObjectException ex) {
            throw new DeviceException(pid, ex);
        } catch (FedoraClientException ex) {
            throw new DeviceException(pid, ex);
        }
    }

    /**
     * Deletes a device.
     * @param id device PID
     * @param log log message
     * @return {@code true} if deleted or {@code false} if the device is connected by any digital object.
     * @throws DeviceException failure
     * @throws DeviceNotFoundException device not found
     */
    public boolean deleteDevice(String id, String log) throws DeviceException, DeviceNotFoundException {
        checkDeviceId(id);
        RemoteObject robject = remoteStorage.find(id);
        try {
            // check fedora usages
            // device may be still used by any import item
            if (remoteStorage.getSearch().isDeviceInUse(id)) {
                return false;
            } else {
                FedoraClient.purgeObject(id).logMessage(log).execute(robject.getClient());
                return true;
            }
        } catch (FedoraClientException ex) {
            if (ex.getStatus() == Status.NOT_FOUND.getStatusCode()) {
                throw new DeviceNotFoundException(null, ex, id);
            } else {
                throw new DeviceException(id, ex);
            }
        } catch (IOException ex) {
            throw new DeviceException(id, ex);
        }
    }

    /**
     * Finds a device.
     * @param id device PID or {@code null} for all devices.
     * @return list of devices
     * @throws DeviceException failure
     */
    public List<Device> find(String id) throws DeviceException {
        try {
            if (id != null) {
                checkDeviceId(id);
                return findDevice(id);
            } else {
                return findAllDevices();
            }
        } catch (IOException ex) {
            throw new DeviceException(id, ex);
        } catch (FedoraClientException ex) {
            throw new DeviceException(id, ex);
        }
    }

    /**
     * Updates a device.
     * @param id device PID
     * @param label label to update
     * @param log log message
     * @return the updated device
     * @throws DeviceException failure
     */
    public Device update(String id, String label, String log) throws DeviceException {
        checkDeviceId(id);
        try {
            RemoteObject robj = remoteStorage.find(id);

            updateDc(robj, id, label, log);

            robj.setLabel(label);
            robj.flush();
            Device device = new Device();
            device.setId(id);
            device.setLabel(label);
            return device;
        } catch (DigitalObjectException ex) {
            throw new DeviceException(id, ex);
        }
    }

    private Device addDevice(String pid, String owner, String label, String log)
            throws DigitalObjectException, FedoraClientException {

        LocalObject lobject = new LocalStorage().create(pid);
        lobject.setLabel(label);
        lobject.setOwner(owner);
        updateDc(lobject, pid, label, log);
        RelationEditor relationEditor = new RelationEditor(lobject);
        relationEditor.setModel(METAMODEL_ID);
        relationEditor.write(relationEditor.getLastModified(), log);
        lobject.flush();

        remoteStorage.ingest(lobject, owner);
        Device device = new Device();
        device.setId(pid);
        device.setLabel(label);
        return device;
    }

    private List<Device> findAllDevices() throws IOException, FedoraClientException {
        List<Item> items = remoteStorage.getSearch().findByModel(METAMODEL_ID);
        return objectAsDevice(items, null);
    }

    private List<Device> findDevice(String... pids) throws IOException, FedoraClientException {
        List<Item> items = remoteStorage.getSearch().findByModel(METAMODEL_ID);
        return objectAsDevice(items, new HashSet<String>(Arrays.asList(pids)));
    }

    private List<Device> objectAsDevice(List<Item> items, Set<String> includes) {
        ArrayList<Device> devices = new ArrayList<Device>(items.size());
        for (Item item : items) {
            String label = item.getLabel();
            String pid = item.getPid();
            if (includes == null || includes.contains(pid)) {
                Device device = new Device();
                device.setId(pid);
                device.setLabel(label);
                devices.add(device);
            }
        }
        return devices;
    }

    private void updateDc(FedoraObject robj, String id, String label, String log) throws DigitalObjectException {
        DcStreamEditor dcEditor = new DcStreamEditor(robj);
        DublinCoreRecord dcr = dcEditor.read();
        ObjectFactory of = new ObjectFactory();
        OaiDcType dc = of.createOaiDcType();
        List<JAXBElement<ElementType>> dcElms = dc.getTitleOrCreatorOrSubject();
        dcElms.add(of.createTitle(createElementType(label, of)));
        dcElms.add(of.createIdentifier(createElementType(id, of)));
        dcElms.add(of.createType(createElementType(METAMODEL_ID, of)));
        dcr.setDc(dc);
        dcEditor.write(dcr, log);
    }

    private ElementType createElementType(String value, ObjectFactory of) {
        ElementType et = of.createElementType();
        et.setValue(value);
        return et;
    }

    static void checkDeviceId(String id) throws DeviceException {
        if (id == null || !id.startsWith(DEVICE_ID_PREFIX)) {
            throw new DeviceException("Unexpected device ID: " + id);
        }
    }

}
