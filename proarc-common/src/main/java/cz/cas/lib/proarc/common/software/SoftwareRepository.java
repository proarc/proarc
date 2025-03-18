/*
 * Copyright (C) 2025 Lukas Sykora
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
package cz.cas.lib.proarc.common.software;

import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.storage.DigitalObjectConcurrentModificationException;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor.EditorResult;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage.AkubraObject;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage.RemoteObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
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
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;

/**
 * The repository of software producing digital objects.
 *
 * @author Lukas Sykora
 */
public final class SoftwareRepository {

    /**
     * The datastream ID. It holds the software description in XML format.
     */
    public static final String DESCRIPTION_DS_ID = "DESCRIPTION";

    /**
     * The desctiption label for each software
     */
    public static final String DESCRIPTION_DS_AGENT_LABEL = "The software description for premis agent";
    public static final String DESCRIPTION_DS_EVENT_LABEL = "The software description for premis event";
    public static final String DESCRIPTION_DS_OBJECT_LABEL = "The software description for premis object";
    public static final String DESCRIPTION_DS_SET_LABEL = "The software description for premis set";

    /**
     * PID of the software model.
     */
    public static final String METAMODEL_AGENT_ID = "proarc:softwareagent";
    public static final String METAMODEL_EVENT_ID = "proarc:softwareevent";
    public static final String METAMODEL_OBJECT_ID = "proarc:softwareobject";
    public static final String METAMODEL_SET_ID = "proarc:softwareset";
    public static final String SOFTWARE_ID_PREFIX = "software:";

    private FedoraStorage fedoraStorage;
    private final Storage typeOfStorage;
    private AkubraStorage akubraStorage;

    private final Logger LOG = Logger.getLogger(SoftwareRepository.class.getName());

    public SoftwareRepository(FedoraStorage fedoraStorage) {
        if (fedoraStorage == null) {
            throw new NullPointerException("remoteStorage");
        }
        this.fedoraStorage = fedoraStorage;
        this.typeOfStorage = Storage.FEDORA;
    }

    public SoftwareRepository(AkubraStorage akubraStorage) {
        if (akubraStorage == null) {
            throw new NullPointerException("akubraStorage");
        }
        this.akubraStorage = akubraStorage;
        this.typeOfStorage = Storage.AKUBRA;
    }

    /**
     * Adds a new software.
     *
     * @param owner owner of the object
     * @param label software label
     * @param log log message
     * @return the software
     * @throws cz.cas.lib.proarc.common.software.SoftwareException failure
     */
    public Software addSoftware(String owner, String model, String label, String log) throws SoftwareException {
        UUID uuid = UUID.randomUUID();
        String pid = SOFTWARE_ID_PREFIX + uuid.toString();
        try {
            return addSoftware(pid, owner, model, label, log);
        } catch (DigitalObjectException ex) {
            throw new SoftwareException(pid, ex);
        }
    }

    public Software addSoftwareWithMetadata(String owner, String model, String label, String log, PremisComplexType description) throws SoftwareException {
        UUID uuid = UUID.randomUUID();
        String pid = SOFTWARE_ID_PREFIX + uuid.toString();
        try {
            return addSoftware(pid, owner, model, label, description, log);
        } catch (DigitalObjectException ex) {
            throw new SoftwareException(pid, ex);
        }
    }

    /**
     * Deletes a software.
     *
     * @param id  software PID
     * @param log log message
     * @return {@code true} if deleted or throw exception
     * @throws cz.cas.lib.proarc.common.software.SoftwareException         failure
     * @throws cz.cas.lib.proarc.common.software.SoftwareNotFoundException software not found
     */
    public boolean deleteSoftware(String id, String log) throws SoftwareException, SoftwareNotFoundException {
        checkSoftwareId(id);
        try {
            // check fedora usages
            // software may be still used by any import item
            if (Storage.FEDORA.equals(typeOfStorage)) {
                RemoteObject object = fedoraStorage.find(id);
                object.purge(log);
                return true;
            } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                AkubraObject object = akubraStorage.find(id);
                object.purge(log);
                return true;
            } else {
                throw new SoftwareException("Not implemented or missing typeOfStorage");
            }
        } catch (DigitalObjectNotFoundException ex) {
            throw new SoftwareNotFoundException(null, ex, id);
        } catch (DigitalObjectException ex) {
            throw new SoftwareException(id, ex);
        }
    }

    /**
     * Finds a software without description.
     *
     * @param id software PID or {@code null} for all software.
     * @return list of software
     * @throws cz.cas.lib.proarc.common.software.SoftwareException failure
     */
    public List<Software> find(AppConfiguration config, String id) throws SoftwareException {
        return find(config, id, null, false, 0);
    }

    /**
     * Finds a software.
     *
     * @param id software PID or {@code null} for all software.
     * @param fetchDescription whether to include software descriptions in response
     * @return list of software
     * @throws cz.cas.lib.proarc.common.software.SoftwareException failure
     */
    public List<Software> find(AppConfiguration config, String id, String model, boolean fetchDescription, int offset) throws SoftwareException {
        try {
            List<Software> software;
            if (id != null) {
                checkSoftwareId(id);
                software = findSoftware(config, id);
            } else if (model != null) {
                software = findSoftwareByModel(config, model, offset);
            } else {
                software = findAllSoftware(config, offset);
            }
            if (fetchDescription) {
                fetchSoftwareDescription(software);
            }
            return software;
        } catch (IOException ex) {
            throw new SoftwareException(id, ex);
        } catch (FedoraClientException ex) {
            throw new SoftwareException(id, ex);
        }
    }

    /**
     * Fetches software descriptions.
     *
     * @param softwares software to query
     * @throws cz.cas.lib.proarc.common.software.SoftwareException failure
     */
    void fetchSoftwareDescription(List<Software> softwares) throws SoftwareException {
        for (Software software : softwares) {
            fetchSoftwareDescription(software);
        }
    }

    /**
     * Fetches a software description.
     *
     * @param software a software with ID
     * @return the software or {@code null} if not found
     * @throws cz.cas.lib.proarc.common.software.SoftwareException failure
     */
    Software fetchSoftwareDescription(Software software) throws SoftwareException {
        if (software == null || software.getId() == null) {
            return null;
        }
        String id = software.getId();
        try {
            ProArcObject object = null;
            if (Storage.FEDORA.equals(typeOfStorage)) {
                object = fedoraStorage.find(id);
            } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                object = akubraStorage.find(id);
            }
            XmlStreamEditor editor = getPremisDescriptionEditor(object);
            Source src = editor.read();
            PremisComplexType premisComplexType;
            if (src != null) {
                try {
                    JAXBContext jaxbContext = JAXBContext.newInstance(PremisComplexType.class);
                    Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
                    premisComplexType = (PremisComplexType) unmarshaller.unmarshal(src);
                } catch (JAXBException e) {
                    LOG.log(Level.SEVERE, "Unable get Premis metadata");
                    premisComplexType = new PremisComplexType();
                }
            } else {
                premisComplexType = new PremisComplexType();
            }
            if (METAMODEL_AGENT_ID.equals(software.getModel())) {
                software.setAgentDescription(premisComplexType);
                software.setAgentTimestamp(src == null ? null : editor.getLastModified());
            } else if (METAMODEL_EVENT_ID.equals(software.getModel())) {
                software.setEventDescription(premisComplexType);
                software.setEventTimestamp(src == null ? null : editor.getLastModified());
            } else if (METAMODEL_OBJECT_ID.equals(software.getModel())) {
                software.setObjectDescription(premisComplexType);
                software.setObjectTimestamp(src == null ? null : editor.getLastModified());
            } else {
                throw new SoftwareException("Unknown object model: " + software.getModel());
            }
            return software;
        } catch (DigitalObjectNotFoundException ex) {
            return null;
        } catch (DigitalObjectException ex) {
            throw new SoftwareException(id, ex);
        }
    }

    /**
     * Updates a software.
     *
     * @param update data to update
     * @param log    log message
     * @return the updated software
     * @throws cz.cas.lib.proarc.common.software.SoftwareException failure
     */
    public Software update(Software update, String log) throws SoftwareException {
        String id = update.getId();
        String label = update.getLabel();
        String model = update.getModel();
        checkSoftwareId(id);
        try {
            ProArcObject object = null;
            if (Storage.FEDORA.equals(typeOfStorage)) {
                object = fedoraStorage.find(id);
            } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                object = akubraStorage.find(id);
            }

            updateDc(object, id, model, label, log);

            XmlStreamEditor descriptionEditor = getPremisDescriptionEditor(object);
            Source oldDescSrc = descriptionEditor.read();
            if (oldDescSrc == null) {
                update.setTimestamp(descriptionEditor.getLastModified());
            }
            if (oldDescSrc != null && update.getDescription() == null) {
                update.setDescription(new PremisComplexType());
            }
            if (update.getDescription() != null) {
                EditorResult result = descriptionEditor.createResult();
                PremisUtils.marshal(result, update.getDescription(), true);
                descriptionEditor.write(result, update.getTimestamp(), log);
            }

            object.setLabel(label);
            object.setModel(model);
            object.flush();

            Software software = new Software();
            software.setId(id);
            software.setModel(model);
            software.setLabel(label);
            software.setDescription(update.getDescription());
            software.setTimestamp(descriptionEditor.getLastModified());
            return software;
        } catch (DigitalObjectConcurrentModificationException ex) {
            // XXX handle concurrency
            throw new SoftwareException(id, ex);
        } catch (DigitalObjectException ex) {
            throw new SoftwareException(id, ex);
        }
    }

    private Software addSoftware(String pid, String owner, String model, String label, String log) throws DigitalObjectException, SoftwareException {
        return addSoftware(pid, owner, model, label, null, log);
    }

    private Software addSoftware(String pid, String owner, String model, String label, PremisComplexType description, String log) throws DigitalObjectException, SoftwareException {

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
        Software software = new Software();
        software.create(pid, label, model, description, null, null);
        return software;
    }

    public List<Software> findAllSoftware(AppConfiguration config, int offset) throws SoftwareException {
        List<SearchViewItem> items = new ArrayList<>();
        try {
            SearchView searchView = null;
            if (Storage.FEDORA.equals(typeOfStorage)) {
                searchView = fedoraStorage.getSearch();
            } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                searchView = akubraStorage.getSearch().setAllowDevicesAndSoftware(true);
            }
            items = searchView.findByModels(offset, METAMODEL_AGENT_ID, METAMODEL_EVENT_ID, METAMODEL_EVENT_ID, METAMODEL_SET_ID);
        } catch (IOException | FedoraClientException ex) {
            throw new SoftwareException(ex.getMessage());
        }
        return objectAsSoftware(config, items, null);
    }

    public List<Software> findSoftwareByModel(AppConfiguration config, String model, int offset) throws SoftwareException {
        List<SearchViewItem> items = new ArrayList<>();
        try {
            SearchView searchView = null;
            if (Storage.FEDORA.equals(typeOfStorage)) {
                searchView = fedoraStorage.getSearch();
            } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                searchView = akubraStorage.getSearch().setAllowDevicesAndSoftware(true);
            }
            items = searchView.findByModel(offset, model);
        } catch (IOException | FedoraClientException ex) {
            throw new SoftwareException(ex.getMessage());
        }
        return objectAsSoftware(config, items, null);
    }

    private List<Software> findSoftware(AppConfiguration config, String... pids) throws IOException, FedoraClientException {
        SearchView searchView = null;
        if (Storage.FEDORA.equals(typeOfStorage)) {
            searchView = fedoraStorage.getSearch();
        } else if (Storage.AKUBRA.equals(typeOfStorage)) {
            searchView = akubraStorage.getSearch().setAllowDevicesAndSoftware(true);
        }

        List<SearchViewItem> items = searchView.findByModel(METAMODEL_AGENT_ID);
        items.addAll(searchView.findByModel(METAMODEL_EVENT_ID));
        items.addAll(searchView.findByModel(METAMODEL_OBJECT_ID));
        items.addAll(searchView.findByModel(METAMODEL_SET_ID));
        return objectAsSoftware(config, items, new HashSet<String>(Arrays.asList(pids)));
    }

    private List<Software> objectAsSoftware(AppConfiguration config, List<SearchViewItem> items, Set<String> includes) {
        ArrayList<Software> softwares = new ArrayList<Software>(items.size());
        for (SearchViewItem item : items) {
            String label = item.getLabel();
            String pid = item.getPid();
            String model = item.getModel();
            if (includes == null || includes.contains(pid)) {
                Software software = new Software();
                software.setId(pid);
                software.setLabel(label);
                software.setModel(model);
                softwares.add(software);
            }
        }
        return softwares;
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

    static void checkSoftwareId(String id) throws SoftwareException {
        if (id == null || !id.startsWith(SOFTWARE_ID_PREFIX)) {
            throw new SoftwareException("Unexpected software ID: " + id);
        }
    }

    /**
     * Gets a datastream editor for Premis format.
     *
     * @param robj an object to edit
     * @return the editor
     */
    public static XmlStreamEditor getPremisDescriptionEditor(ProArcObject robj) throws SoftwareException {
        String labelValue = null;
        if (METAMODEL_AGENT_ID.equals(robj.getModel())) {
            labelValue = DESCRIPTION_DS_AGENT_LABEL;
        } else if (METAMODEL_EVENT_ID.equals(robj.getModel())) {
            labelValue = DESCRIPTION_DS_EVENT_LABEL;
        } else if (METAMODEL_OBJECT_ID.equals(robj.getModel())) {
            labelValue = DESCRIPTION_DS_OBJECT_LABEL;
        } else if (METAMODEL_SET_ID.equals(robj.getModel())) {
            labelValue = DESCRIPTION_DS_SET_LABEL;
        } else {
            throw new SoftwareException("Unknown object model: " + robj.getModel());
        }
        DatastreamProfile dProfile = FoxmlUtils.managedProfile(
                DESCRIPTION_DS_ID, PremisUtils.NS, labelValue);
        XmlStreamEditor editor = robj.getEditor(dProfile);
        return editor;
    }
}
