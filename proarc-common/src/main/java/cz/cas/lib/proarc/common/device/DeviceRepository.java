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

import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import cz.cas.lib.proarc.audiopremis.NkManufacturerComplexType;
import cz.cas.lib.proarc.audiopremis.NkSerialNumberComplexType;
import cz.cas.lib.proarc.audiopremis.NkSettingsComplexType;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.fedora.DigitalObjectConcurrentModificationException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor.EditorResult;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mix.MixUtils;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import cz.cas.lib.proarc.premis.PremisComplexType;
import cz.cas.lib.proarc.premis.PremisUtils;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import javax.ws.rs.core.Response.Status;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;

/**
 * The repository of devices producing digital objects.
 *
 * @author Jan Pokorsky
 */
public final class DeviceRepository {

    /**
     * The datastream ID. It holds the device description in XML format.
     */
    public static final String DESCRIPTION_DS_ID = "DESCRIPTION";

    public static final String AUDIODESCRIPTION_DS_ID = "AUDIODESCRIPTION";

    /**
     * The default label of {@link #DESCRIPTION_DS_ID the description datastream}.
     */
    public static final String DESCRIPTION_DS_LABEL = "The device description";

    public static final String AUDIO_DESCRIPTION_DS_LABEL = "The audio device description";

    /**
     * PID of the device model.
     */
    public static final String METAMODEL_ID = "proarc:device";
    public static final String METAMODEL_AUDIODEVICE_ID = "proarc:audiodevice";
    private static final String DEVICE_ID_PREFIX = "device:";

    public static final String METAMODEL_ID_LABEL = "Skener";
    public static final String METAMODEL_AUDIODEVICE_ID_LABEL = "Audio linka";

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
    public Device addDevice(String owner, String model, String label, String log) throws DeviceException {
        UUID uuid = UUID.randomUUID();
        String pid = DEVICE_ID_PREFIX + uuid.toString();
        try {
            return addDevice(pid, owner, model, label, log);
        } catch (DigitalObjectException ex) {
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
                robject.purge(log);
                return true;
            }
        } catch (DigitalObjectNotFoundException ex) {
                throw new DeviceNotFoundException(null, ex, id);
        } catch (FedoraClientException ex) {
            if (ex.getStatus() == Status.NOT_FOUND.getStatusCode()) {
                throw new DeviceNotFoundException(null, ex, id);
            } else {
                throw new DeviceException(id, ex);
            }
        } catch (DigitalObjectException | IOException ex) {
            throw new DeviceException(id, ex);
        }
    }

    /**
     * Finds a device without description.
     * @param id device PID or {@code null} for all devices.
     * @return list of devices
     * @throws DeviceException failure
     */
    public List<Device> find(String id) throws DeviceException {
        return find(id, false);
    }

    /**
     * Finds a device.
     * @param id device PID or {@code null} for all devices.
     * @param fetchDescription whether to include device descriptions in response
     * @return list of devices
     * @throws DeviceException failure
     */
    public List<Device> find(String id, boolean fetchDescription) throws DeviceException {
        try {
            List<Device> devices;
            if (id != null) {
                checkDeviceId(id);
                devices = findDevice(id);
            } else {
                devices = findAllDevices();
            }
            if (fetchDescription) {
                fetchDeviceDescription(devices);
            }
            return devices;
        } catch (IOException ex) {
            throw new DeviceException(id, ex);
        } catch (FedoraClientException ex) {
            throw new DeviceException(id, ex);
        }
    }

    /**
     * Fetches device descriptions.
     * @param devices devices to query
     * @throws DeviceException failure
     */
    void fetchDeviceDescription(List<Device> devices) throws DeviceException {
        for (Device device : devices) {
            fetchDeviceDescription(device);
        }
    }

    /**
     * Fetches a device description.
     * @param device a device with ID
     * @return the device or {@code null} if not found
     * @throws DeviceException failure
     */
    Device fetchDeviceDescription(Device device) throws DeviceException {
        if (device == null || device.getId() == null) {
            return null;
        }
        String id = device.getId();
        try {
            RemoteObject robj = remoteStorage.find(id);
            XmlStreamEditor editor = getMixDescriptionEditor(robj);
            XmlStreamEditor audioeditor = getPremisDescriptionEditor(robj);
            Source src = editor.read();
            Source audiosrc = audioeditor.read();
            Mets audiodesc;
            PremisComplexType premis;
            Mix desc;
            if (src != null) {
                desc = MixUtils.unmarshal(src, Mix.class);
            } else {
                desc = new Mix();
            }
            if (audiosrc != null) {

                try {
                    JAXBContext jaxbContext = JAXBContext.newInstance(Mets.class, PremisComplexType.class);
                    Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
                    audiodesc = (Mets)unmarshaller.unmarshal(audiosrc);
                } catch (JAXBException e) {
                    e.printStackTrace();
                    audiodesc = new Mets();
                }
            } else {
                audiodesc = new Mets();
            }
            device.setDescription(desc);
            device.setAudioDescription(audiodesc);
            device.setTimestamp(editor.getLastModified());
            device.setAudioTimestamp(audioeditor.getLastModified());
            return device;
        } catch (DigitalObjectNotFoundException ex) {
            return null;
        } catch (DigitalObjectException ex) {
            throw new DeviceException(id, ex);
        }
    }

    /**
     * Updates a device.
     * @param update data to update
     * @param log log message
     * @return the updated device
     * @throws DeviceException failure
     */
    public Device update(Device update, String log) throws DeviceException {
        String id = update.getId();
        String label = update.getLabel();
        String model = transformModel(update.getModel());
        checkDeviceId(id);
        try {
            RemoteObject robj = remoteStorage.find(id);

            updateDc(robj, id, model, label, log);

            XmlStreamEditor descriptionEditor = getMixDescriptionEditor(robj);
            Source oldDescSrc = descriptionEditor.read();
            if (oldDescSrc == null) {
                update.setTimestamp(descriptionEditor.getLastModified());
            }
            if (oldDescSrc != null && update.getDescription() == null) {
                update.setDescription(new Mix());
            }
            if (update.getDescription() != null) {
                EditorResult result = descriptionEditor.createResult();
                MixUtils.marshal(result, update.getDescription(), true);
                descriptionEditor.write(result, update.getTimestamp(), log);
            }

            XmlStreamEditor audiodescriptionEditor = getPremisDescriptionEditor(robj);
            Source oldAudioDescSrc = audiodescriptionEditor.read();
            if (oldAudioDescSrc == null) {
                update.setAudioTimestamp(audiodescriptionEditor.getLastModified());
            }
            if (oldAudioDescSrc != null && update.getAudioDescription() == null) {
                update.setAudioDescription(new Mets());
            }
            if (update.getAudioDescription() != null) {
                try {
                    EditorResult result = audiodescriptionEditor.createResult();
                    JAXBContext jaxbContext = JAXBContext.newInstance(Mets.class, PremisComplexType.class, NkSerialNumberComplexType.class, NkManufacturerComplexType.class, NkSettingsComplexType.class);
                    Marshaller marshaller = jaxbContext.createMarshaller();
                    marshaller.setProperty(Marshaller.JAXB_ENCODING, "utf-8");
                    marshaller.marshal(update.getAudioDescription(), result);
                    audiodescriptionEditor.write(result, update.getAudioTimestamp(), log);
                } catch (JAXBException e) {
                    e.printStackTrace();
                }
            }

            robj.setLabel(label);
            robj.flush();

            Device device = new Device();
            device.setId(id);
            device.setModel(model);
            device.setLabel(label);
            device.setDescription(update.getDescription());
            device.setAudioDescription(update.getAudioDescription());
            device.setTimestamp(descriptionEditor.getLastModified());
            device.setAudioTimestamp(audiodescriptionEditor.getLastModified());
            return device;
        } catch (DigitalObjectConcurrentModificationException ex) {
            // XXX handle concurrency
            throw new DeviceException(id, ex);
        } catch (DigitalObjectException ex) {
            throw new DeviceException(id, ex);
        }
    }

    private Device addDevice(String pid, String owner, String model, String label, String log)
            throws DigitalObjectException {

        LocalObject lobject = new LocalStorage().create(pid);
        lobject.setLabel(label);
        lobject.setOwner(owner);
        updateDc(lobject, pid, model, label, log);
        RelationEditor relationEditor = new RelationEditor(lobject);
        relationEditor.setModel(model);
        relationEditor.write(relationEditor.getLastModified(), log);
        lobject.flush();

        remoteStorage.ingest(lobject, owner);
        Device device = new Device();
        device.setId(pid);
        device.setLabel(label);
        device.setModel(model);
        return device;
    }

    private List<Device> findAllDevices() throws IOException, FedoraClientException {
        List<Item> items = remoteStorage.getSearch().findByModel(METAMODEL_ID);
        items.addAll(remoteStorage.getSearch().findByModel(METAMODEL_AUDIODEVICE_ID));
        return objectAsDevice(items, null);
    }

    private List<Device> findDevice(String... pids) throws IOException, FedoraClientException {
        List<Item> items = remoteStorage.getSearch().findByModel(METAMODEL_ID);
        items.addAll(remoteStorage.getSearch().findByModel(METAMODEL_AUDIODEVICE_ID));
        return objectAsDevice(items, new HashSet<String>(Arrays.asList(pids)));
    }

    private List<Device> objectAsDevice(List<Item> items, Set<String> includes) {
        ArrayList<Device> devices = new ArrayList<Device>(items.size());
        for (Item item : items) {
            String label = item.getLabel();
            String pid = item.getPid();
            String model = item.getModel();
            if (includes == null || includes.contains(pid)) {
                Device device = new Device();
                device.setId(pid);
                device.setLabel(label);
                device.setModel(transformModel(model));
                devices.add(device);
            }
        }
        return devices;
    }

    private String transformModel(String model) {
        if (model != null && !model.isEmpty()) {
            if (METAMODEL_ID.equals(model)) {
                return METAMODEL_ID_LABEL;
            } else if (METAMODEL_AUDIODEVICE_ID.equals(model)) {
                return METAMODEL_AUDIODEVICE_ID_LABEL;
            } else if (METAMODEL_ID_LABEL.equals(model)) {
                return METAMODEL_ID;
            } else if (METAMODEL_AUDIODEVICE_ID_LABEL.equals(model)) {
                return METAMODEL_AUDIODEVICE_ID;
            }
        }
        return null;
    }

    private void updateDc(FedoraObject robj, String id, String model, String label, String log) throws DigitalObjectException {
        DcStreamEditor dcEditor = new DcStreamEditor(robj);
        DublinCoreRecord dcr = dcEditor.read();
        OaiDcType dc = new OaiDcType();
        dc.getTitles().add(new ElementType(label, null));
        dc.getIdentifiers().add(new ElementType(id, null));
        dc.getTypes().add(new ElementType(model, null));
        dcr.setDc(dc);
        dcEditor.write(dcr, log);
    }

    static void checkDeviceId(String id) throws DeviceException {
        if (id == null || !id.startsWith(DEVICE_ID_PREFIX)) {
            throw new DeviceException("Unexpected device ID: " + id);
        }
    }

    /**
     * Gets a datastream editor for MIX format.
     * @param robj an object to edit
     * @return the editor
     */
    public static XmlStreamEditor getMixDescriptionEditor(FedoraObject robj) {
        DatastreamProfile dProfile = FoxmlUtils.managedProfile(
                DESCRIPTION_DS_ID, MixUtils.NS, DESCRIPTION_DS_LABEL);
        XmlStreamEditor editor = robj.getEditor(dProfile);
        return editor;
    }

    public static XmlStreamEditor getPremisDescriptionEditor(FedoraObject robj) {
        DatastreamProfile dProfile = FoxmlUtils.managedProfile(
                AUDIODESCRIPTION_DS_ID, PremisUtils.NS, AUDIO_DESCRIPTION_DS_LABEL);
        XmlStreamEditor editor = robj.getEditor(dProfile);
        return editor;
    }


    /**
     * Gets a datastream editor for MIX format.
     *
     * @param id
     *
     * @return
     * @throws DeviceException
     */
    public XmlStreamEditor getDescriptionEditor(String id) throws DeviceException {
        checkDeviceId(id);
        RemoteObject robj = remoteStorage.find(id);
        return getMixDescriptionEditor(robj);
    }

}
