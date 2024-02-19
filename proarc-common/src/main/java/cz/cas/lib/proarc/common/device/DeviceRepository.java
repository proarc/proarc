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
import cz.cas.lib.proarc.audiopremis.NkComplexType;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.storage.DigitalObjectConcurrentModificationException;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage.RemoteObject;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor.EditorResult;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage.AkubraObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.mets.AmdSecType;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mix.MixUtils;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import cz.cas.lib.proarc.premis.AgentComplexType;
import cz.cas.lib.proarc.premis.PremisComplexType;
import cz.cas.lib.proarc.premis.PremisUtils;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.core.Response.Status;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;
import org.w3c.dom.Element;

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

    private FedoraStorage fedoraStorage;
    private final Storage typeOfStorage;
    private AkubraStorage akubraStorage;

    private final Logger LOG = Logger.getLogger(DeviceRepository.class.getName());

    public DeviceRepository(FedoraStorage fedoraStorage) {
        if (fedoraStorage == null) {
            throw new NullPointerException("remoteStorage");
        }
        this.fedoraStorage = fedoraStorage;
        this.typeOfStorage = Storage.FEDORA;
    }

    public DeviceRepository(AkubraStorage akubraStorage) {
        if (akubraStorage == null) {
            throw new NullPointerException("akubraStorage");
        }
        this.akubraStorage = akubraStorage;
        this.typeOfStorage = Storage.AKUBRA;
    }

    /**
     * Adds a new device.
     *
     * @param owner owner of the object
     * @param label device label
     * @param log   log message
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

    public Device addDeviceWithMetadata(String owner, String model, String label, String log, Mix mix, Mets mets) throws DeviceException {
        UUID uuid = UUID.randomUUID();
        String pid = DEVICE_ID_PREFIX + uuid.toString();
        try {
            Device device = addDevice(pid, owner, model, label, log);
            device.setDescription(mix);
            device.setAudioDescription(mets);
            return update(device, "Init device metadata");
        } catch (DigitalObjectException ex) {
            throw new DeviceException(pid, ex);
        }
    }

    /**
     * Deletes a device.
     *
     * @param id  device PID
     * @param log log message
     * @return {@code true} if deleted or {@code false} if the device is connected by any digital object.
     * @throws DeviceException         failure
     * @throws DeviceNotFoundException device not found
     */
    public boolean deleteDevice(String id, String log) throws DeviceException, DeviceNotFoundException {
        checkDeviceId(id);
        try {
            // check fedora usages
            // device may be still used by any import item
            if (Storage.FEDORA.equals(typeOfStorage)) {
                RemoteObject object = fedoraStorage.find(id);
                if (fedoraStorage.getSearch().isDeviceInUse(id)) {
                    return false;
                } else {
                    object.purge(log);
                    return true;
                }
            } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                AkubraObject object = akubraStorage.find(id);
                if (akubraStorage.getSearch().isDeviceInUse(id)) {
                    return false;
                } else {
                    object.purge(log);
                    return true;
                }
            } else {
                throw new DeviceException("Not implemented or missing typeOfStorage");
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
     *
     * @param id device PID or {@code null} for all devices.
     * @return list of devices
     * @throws DeviceException failure
     */
    public List<Device> find(AppConfiguration config, String id) throws DeviceException {
        return find(config, id, false, 0);
    }

    /**
     * Finds a device.
     *
     * @param id               device PID or {@code null} for all devices.
     * @param fetchDescription whether to include device descriptions in response
     * @return list of devices
     * @throws DeviceException failure
     */
    public List<Device> find(AppConfiguration config, String id, boolean fetchDescription, int offset) throws DeviceException {
        try {
            List<Device> devices;
            if (id != null) {
                checkDeviceId(id);
                devices = findDevice(config, id);
            } else {
                devices = findAllDevices(config, offset);
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
     *
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
     *
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
            ProArcObject object = null;
            if (Storage.FEDORA.equals(typeOfStorage)) {
                object = fedoraStorage.find(id);
            } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                object = akubraStorage.find(id);
            }
            XmlStreamEditor editor = getMixDescriptionEditor(object);
            XmlStreamEditor audioeditor = getPremisDescriptionEditor(object);
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
                    JAXBContext jaxbContext = JAXBContext.newInstance(Mets.class, PremisComplexType.class, NkComplexType.class);
                    Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
                    audiodesc = (Mets) unmarshaller.unmarshal(audiosrc);
                    audiodesc = repairNkComplexType(audiodesc);
                } catch (JAXBException e) {
                    LOG.log(Level.SEVERE, "Unable get Mets metadata");
                    audiodesc = new Mets();
                }
            } else {
                audiodesc = new Mets();
            }
            device.setDescription(desc);
            device.setTimestamp(src == null ? null : editor.getLastModified());
            device.setAudioDescription(audiodesc);
            device.setAudioTimestamp(audiosrc == null ? null : audioeditor.getLastModified());
            return device;
        } catch (DigitalObjectNotFoundException ex) {
            return null;
        } catch (DigitalObjectException ex) {
            throw new DeviceException(id, ex);
        }
    }

    /**
     * Updates a device.
     *
     * @param update data to update
     * @param log    log message
     * @return the updated device
     * @throws DeviceException failure
     */
    public Device update(Device update, String log) throws DeviceException {
        String id = update.getId();
        String label = update.getLabel();
        String model = getModel(update.getModel());
        checkDeviceId(id);
        try {
            ProArcObject object = null;
            if (Storage.FEDORA.equals(typeOfStorage)) {
                object = fedoraStorage.find(id);
            } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                object = akubraStorage.find(id);
            }

            updateDc(object, id, model, label, log);

            XmlStreamEditor descriptionEditor = getMixDescriptionEditor(object);
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

            XmlStreamEditor audiodescriptionEditor = null;
            if (METAMODEL_AUDIODEVICE_ID.equals(model)) {
                audiodescriptionEditor = getPremisDescriptionEditor(object);
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
                        JAXBContext jaxbContext = JAXBContext.newInstance(Mets.class, PremisComplexType.class, NkComplexType.class);
                        Marshaller marshaller = jaxbContext.createMarshaller();
                        marshaller.setProperty(Marshaller.JAXB_ENCODING, "utf-8");
                        marshaller.marshal(update.getAudioDescription(), result);
                        audiodescriptionEditor.write(result, update.getAudioTimestamp(), log);
                    } catch (JAXBException e) {
                        LOG.log(Level.SEVERE, "Unable to unmarshall audiodescription");
                    }
                }
            }

            object.setLabel(label);
            object.setModel(model);
            object.flush();

            Device device = new Device();
            device.setId(id);
            device.setModel(model);
            device.setLabel(label);
            device.setDescription(update.getDescription());
            device.setTimestamp(descriptionEditor.getLastModified());
            if (METAMODEL_AUDIODEVICE_ID.equals(model)) {
                device.setAudioDescription(update.getAudioDescription());
                device.setAudioTimestamp(update.getAudioDescription() == null ? null : audiodescriptionEditor.getLastModified());
            }
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
        lobject.setModel(model);

        if (Storage.FEDORA.equals(typeOfStorage)) {
            fedoraStorage.ingest(lobject, owner);
        } else if (Storage.AKUBRA.equals(typeOfStorage)) {
            akubraStorage.ingest(lobject, owner);
        }
        Device device = new Device();
        device.setId(pid);
        device.setLabel(label);
        device.setModel(model);
        return device;
    }

    public List<Device> findAllDevices(AppConfiguration config, int offset) throws DeviceException {
        List<SearchViewItem> items = new ArrayList<>();
        try {
            SearchView searchView = null;
            if (Storage.FEDORA.equals(typeOfStorage)) {
                searchView = fedoraStorage.getSearch();
            } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                searchView = akubraStorage.getSearch().setAllowDevices(true);
            }
            items = searchView.findByModels(offset, METAMODEL_ID, METAMODEL_AUDIODEVICE_ID);
        } catch (IOException | FedoraClientException ex) {
            throw new DeviceException(ex.getMessage());
        }
        return objectAsDevice(config, items, null);
    }

    private List<Device> findDevice(AppConfiguration config, String... pids) throws IOException, FedoraClientException {
        SearchView searchView = null;
        if (Storage.FEDORA.equals(typeOfStorage)) {
            searchView = fedoraStorage.getSearch();
        } else if (Storage.AKUBRA.equals(typeOfStorage)) {
            searchView = akubraStorage.getSearch().setAllowDevices(true);
        }

        List<SearchViewItem> items = searchView.findByModel(METAMODEL_ID);
        items.addAll(searchView.findByModel(METAMODEL_AUDIODEVICE_ID));
        return objectAsDevice(config, items, new HashSet<String>(Arrays.asList(pids)));
    }

    private List<Device> objectAsDevice(AppConfiguration config, List<SearchViewItem> items, Set<String> includes) {
        ArrayList<Device> devices = new ArrayList<Device>(items.size());
        for (SearchViewItem item : items) {
            String label = item.getLabel();
            String pid = item.getPid();
            String model = item.getModel();
            if (includes == null || includes.contains(pid)) {
                if (!ignoredDevice(config, pid)) {
                    Device device = new Device();
                    device.setId(pid);
                    device.setLabel(label);
                    device.setModel(getModelLabel(model));
                    devices.add(device);
                }
            }
        }
        return devices;
    }

    private boolean ignoredDevice(AppConfiguration config, String pid) {
        if (config == null) {
            return false;
        } else {
            String mainPid = config.getDevices().getMainUUid(pid);
            if (mainPid != null && !mainPid.isEmpty()) {
                return true;
            } else {
                return false;
            }
        }
    }

    public static String getModelLabel(String model) {
        if (model != null && !model.isEmpty()) {
            if (METAMODEL_ID.equals(model) || METAMODEL_ID_LABEL.equals(model)) {
                return METAMODEL_ID_LABEL;
            } else if (METAMODEL_AUDIODEVICE_ID.equals(model) || METAMODEL_AUDIODEVICE_ID_LABEL.equals(model)) {
                return METAMODEL_AUDIODEVICE_ID_LABEL;
            }
        }
        return null;
    }

    private String getModel(String modelOld) {
        if(modelOld != null && !modelOld.isEmpty()) {
            if (METAMODEL_ID.equals(modelOld) || METAMODEL_ID_LABEL.equals(modelOld)) {
                return METAMODEL_ID;
            } else if (METAMODEL_AUDIODEVICE_ID.equals(modelOld) || METAMODEL_AUDIODEVICE_ID_LABEL.equals(modelOld)) {
                return METAMODEL_AUDIODEVICE_ID;
            }
        }
        return null;
    }

    private void updateDc(ProArcObject robj, String id, String model, String label, String log) throws DigitalObjectException {
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
     *
     * @param robj an object to edit
     * @return the editor
     */
    public static XmlStreamEditor getMixDescriptionEditor(ProArcObject robj) {
        DatastreamProfile dProfile = FoxmlUtils.managedProfile(
                DESCRIPTION_DS_ID, MixUtils.NS, DESCRIPTION_DS_LABEL);
        XmlStreamEditor editor = robj.getEditor(dProfile);
        return editor;
    }

    public static XmlStreamEditor getPremisDescriptionEditor(ProArcObject robj) {
        DatastreamProfile dProfile = FoxmlUtils.managedProfile(
                AUDIODESCRIPTION_DS_ID, PremisUtils.NS, AUDIO_DESCRIPTION_DS_LABEL);
        XmlStreamEditor editor = robj.getEditor(dProfile);
        return editor;
    }


    /**
     * Gets a datastream editor for MIX format.
     *
     * @param id
     * @return
     * @throws DeviceException
     */
    public XmlStreamEditor getDescriptionEditor(String id) throws DeviceException {
        checkDeviceId(id);
        ProArcObject object = null;
        if (Storage.FEDORA.equals(typeOfStorage)) {
            object = fedoraStorage.find(id);
        } else if (Storage.AKUBRA.equals(typeOfStorage)) {
            object = akubraStorage.find(id);
        }
        return getMixDescriptionEditor(object);
    }

    public Mets repairNkComplexType(Mets mets) {
        for (AmdSecType amdSec : mets.getAmdSec()) {
            AgentComplexType agent = null;
            try {
                agent = ((PremisComplexType) ((JAXBElement) amdSec.getDigiprovMD().get(0).getMdWrap().getXmlData().getAny().get(0)).getValue()).getAgent().get(0);
            } catch (ClassCastException e) {
                LOG.log(Level.SEVERE, "Can not get Premis Tupe from AmdSec");
                return mets;
            }
            Element extension = (Element) agent.getAgentExtension().get(0).getAny().get(0);
            agent.getAgentExtension().get(0).getAny().clear();
            NkComplexType nk = new NkComplexType();
            agent.getAgentExtension().get(0).getAny().add(nk);
            String manufacturer = "";
            String serialNumber = "";
            String settings = "";

            if (extension != null) {
                try {
                    if ("manufacturer".equals(extension.getFirstChild().getLocalName())) {
                        manufacturer = extension.getFirstChild().getFirstChild().getNodeValue();
                    } else if ("serialNumber".equals(extension.getFirstChild().getLocalName())) {
                        serialNumber = extension.getFirstChild().getFirstChild().getNodeValue();
                    } else if ("settings".equals(extension.getFirstChild().getLocalName()))
                        settings = extension.getFirstChild().getFirstChild().getNodeValue();
                } catch (Exception ex) {
                    LOG.log(Level.FINE, "Error in premis:agentExtension");
                }
                try {
                    if ("serialNumber".equals(extension.getFirstChild().getNextSibling().getLocalName())) {
                        serialNumber = extension.getFirstChild().getNextSibling().getFirstChild().getNodeValue();
                    } else if ("settings".equals(extension.getFirstChild().getNextSibling().getLocalName()))
                        settings = extension.getFirstChild().getNextSibling().getFirstChild().getNodeValue();
                } catch (Exception ex) {
                    LOG.log(Level.FINE, "Error in premis:agentExtension");
                }
                try {
                    if ("settings".equals(extension.getFirstChild().getNextSibling().getNextSibling().getLocalName()))
                        settings = extension.getFirstChild().getNextSibling().getNextSibling().getFirstChild().getNodeValue();
                } catch (Exception ex) {
                    LOG.log(Level.FINE, "Error in premis:agentExtension");
                }
            }
            nk.setManufacturer(manufacturer);
            nk.setSerialNumber(serialNumber);
            nk.setSettings(settings);
        }
        return mets;

    }
}
