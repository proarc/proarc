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
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
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
import cz.cas.lib.proarc.common.storage.akubra.SolrUtils;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage.RemoteObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.xml.SimpleNamespaceContext;
import cz.cas.lib.proarc.mets.AmdSecType;
import cz.cas.lib.proarc.mets.MdSecType;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsConstants;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import cz.cas.lib.proarc.oaidublincore.OaiDcType;
import cz.cas.lib.proarc.premis.AgentComplexType;
import cz.cas.lib.proarc.premis.EventComplexType;
import cz.cas.lib.proarc.premis.File;
import cz.cas.lib.proarc.premis.LinkingAgentIdentifierComplexType;
import cz.cas.lib.proarc.premis.LinkingEventIdentifierComplexType;
import cz.cas.lib.proarc.premis.ObjectComplexType;
import cz.cas.lib.proarc.premis.PremisUtils;
import cz.cas.lib.proarc.premis.RelatedEventIdentificationComplexType;
import cz.cas.lib.proarc.premis.RelationshipComplexType;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.logging.Logger;
import javax.ws.rs.core.Response;
import javax.xml.bind.JAXBElement;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.Document;

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
     * @throws SoftwareException failure
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

    public Software addSoftwareWithMetadata(String owner, String model, String label, String log, Mets description) throws SoftwareException {
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
     * @throws SoftwareException         failure
     * @throws SoftwareNotFoundException software not found
     */
    public boolean deleteSoftware(String id, String log) throws SoftwareException, SoftwareNotFoundException {
        checkSoftwareId(id);
        try {
            // check fedora usages
            // software may be still used by any import item
            if (Storage.FEDORA.equals(typeOfStorage)) {
                RemoteObject object = fedoraStorage.find(id);
                if (fedoraStorage.getSearch().isSoftwareInUse(id)) {
                    return false;
                } else {
                    object.purge(log);
                    return true;
                }
            } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                AkubraObject object = akubraStorage.find(id);
                if (akubraStorage.getSearch().isSoftwareInUse(id)) {
                    return false;
                } else {
                    object.purge(log);
                    return true;
                }
            } else {
                throw new SoftwareException("Not implemented or missing typeOfStorage");
            }
        } catch (DigitalObjectNotFoundException ex) {
            throw new SoftwareNotFoundException(null, ex, id);
        } catch (FedoraClientException ex) {
            if (ex.getStatus() == Response.Status.NOT_FOUND.getStatusCode()) {
                throw new SoftwareNotFoundException(null, ex, id);
            } else {
                throw new SoftwareException(id, ex);
            }
        } catch (DigitalObjectException | IOException ex) {
            throw new SoftwareException(id, ex);
        }
    }

    /**
     * Finds a software without description.
     *
     * @param id software PID or {@code null} for all software.
     * @return list of software
     * @throws SoftwareException failure
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
     * @throws SoftwareException failure
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

    public List<Software> find(String id) throws SoftwareException {
        try {
            List<Software> software = Collections.emptyList();
            if (id != null) {
                checkSoftwareId(id);
                software = findSoftware(null, id);
            }
            fetchSoftwarePreview(software);
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
     * @throws SoftwareException failure
     */
    private void fetchSoftwareDescription(List<Software> softwares) throws SoftwareException {
        for (Software software : softwares) {
            fetchSoftwareDescription(software);
        }
    }

    private void fetchSoftwarePreview(List<Software> softwares) throws SoftwareException {
        for (Software software : softwares) {
            fetchSoftwarePreview(software);
        }
    }

    /**
     * Fetches a software description.
     *
     * @param software a software with ID
     * @return the software or {@code null} if not found
     * @throws SoftwareException failure
     */
    private Software fetchSoftwareDescription(Software software) throws SoftwareException {
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
            object.setModel(software.getModel());
            XmlStreamEditor editor = getMetsDescriptionEditor(object);
            Source src = editor.read();
            Mets mets;
            if (src != null) {
                mets = MetsUtils.unmarshalMets(src);
//                mets = fixMetsAccordingModel(software.getModel(), mets);    Pri nacitani by mel byt mets uz upraven, musi se upravit pri ulozeni
            } else {
                mets = new Mets();
            }
            RelationEditor relationEditor = new RelationEditor(object);
            List<String> setOfIds = relationEditor.getMembers();
            if (setOfIds != null && !setOfIds.isEmpty()) {
                software.setSetOfLinkedIds(setOfIds);
            } else {
                software.setSetOfLinkedIds(new ArrayList<>());
            }
            software.setDescription(mets);
            software.setTimestamp(editor.getLastModified());
            return software;
        } catch (DigitalObjectNotFoundException ex) {
            return null;
        } catch (DigitalObjectException ex) {
            throw new SoftwareException(id, ex);
        }
    }

    private Software fetchSoftwarePreview(Software software) throws SoftwareException {
        if (software == null || software.getId() == null) {
            return null;
        }
        String id = software.getId();

        HashMap<String, SoftwareMets> metsList = new HashMap<>();
        metsList.putAll(getMets(id));
        Mets mets = combineMets(metsList);
        software.setDescription(mets);
        return software;
    }

    private Mets combineMets(HashMap<String, SoftwareMets> metsList) {
        Mets newMets = new Mets();
        AmdSecType newAmdSec = new AmdSecType();
        newMets.getAmdSec().add(newAmdSec);

        List<MdSecType> agentList = new ArrayList<>();
        int agentIndex = 1;
        List<MdSecType> eventList = new ArrayList<>();
        int eventIndex = 1;
        int objetIndex = 1;
        for (String pid : metsList.keySet()) {
            SoftwareMets softwareMets = metsList.get(pid);
            if (softwareMets.getMets() != null) {
                for (AmdSecType amdSec : softwareMets.getMets().getAmdSec()) {
                    if (!amdSec.getTechMD().isEmpty()) {
                        for (MdSecType techMD : amdSec.getTechMD()) {
                            techMD.setID("OBJ_" + String.format("%03d", objetIndex++));
                            newAmdSec.getTechMD().add(techMD);
                        }
                    }
                    if (!amdSec.getDigiprovMD().isEmpty()) {
                        if (SoftwareRepository.METAMODEL_AGENT_ID.equals(softwareMets.getModelId())) {
                            for (MdSecType digiprovMD : amdSec.getDigiprovMD()) {
                                digiprovMD.setID("AGENT_" + String.format("%03d", agentIndex++));
                                agentList.add(digiprovMD);
                            }
                        } else if (SoftwareRepository.METAMODEL_EVENT_ID.equals(softwareMets.getModelId())) {
                            for (MdSecType digiprovMD : amdSec.getDigiprovMD()) {
                                digiprovMD.setID("EVENT_" + String.format("%03d", eventIndex++));
                                eventList.add(digiprovMD);
                            }
                        }
                    }
                }
            }
        }
        newAmdSec.getDigiprovMD().addAll(eventList);
        newAmdSec.getDigiprovMD().addAll(agentList);
        return newMets;
    }

    private HashMap<String, SoftwareMets> getMets(String pid) throws SoftwareException {
        HashMap<String, SoftwareMets> metsList = new HashMap<>();
        try {
            ProArcObject object = null;
            if (Storage.FEDORA.equals(typeOfStorage)) {
                object = fedoraStorage.find(pid);
            } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                object = akubraStorage.find(pid);
            }
            XmlStreamEditor editor = getMetsDescriptionEditor(object);
            Source src = editor.read();
            Mets mets = null;
            if (src != null) {
                mets = MetsUtils.unmarshalMets(src);
            }
            RelationEditor relationEditor = new RelationEditor(object);
            List<String> setOfIds = relationEditor.getMembers();
            String model = relationEditor.getModel();
            for (String member : setOfIds) {
                String agentIdentifierType = getNodeValue(member, "//premis:agentIdentifierType/text()");
                if (agentIdentifierType != null && !agentIdentifierType.isEmpty()) {
                    String agentIdentifierValue = getNodeValue(member, "//premis:agentIdentifierValue/text()");
                    String agentType = getNodeValue(member, "//premis:agentType/text()");
                    mets = addLinkingAgent(relationEditor.getModel(), mets, agentIdentifierType, agentIdentifierValue, agentType);
                } else {
                    String eventIdentifierType = getNodeValue(member, "//premis:eventIdentifierType/text()");
                    if (eventIdentifierType != null && !eventIdentifierType.isEmpty()) {
                        String eventIdentifierValue = getNodeValue(member, "//premis:eventIdentifierValue/text()");
                        mets = addLinkingEvent(relationEditor.getModel(), mets, eventIdentifierType, eventIdentifierValue);
                    }
                }
            }
            if (mets != null) {
                metsList.put(pid, new SoftwareMets(mets, model));
            }
            for (String member : setOfIds) {
                metsList.putAll(getMets(member));
            }
        } catch (Exception ex) {
            LOG.warning(ex.getMessage());
            ex.printStackTrace();
            throw new SoftwareException("Nepodarilo se zkombinovat METS pro objekt " + pid);
        }
        return metsList;
    }

    private Mets addLinkingEvent(String modelId, Mets mets, String eventIdentifierType, String eventIdentifierValue) {
        MdSecType.MdWrap.XmlData xmlData = getXmlData(modelId, mets);
        if (xmlData == null) {
            return mets;
        }
        try {
            File object = (File) ((JAXBElement) xmlData.getAny().get(0)).getValue();
            if (object != null) {
                if (!object.getRelationship().isEmpty()) {
                    RelationshipComplexType relationship = object.getRelationship().get(0);
                    RelatedEventIdentificationComplexType relationEvent = new RelatedEventIdentificationComplexType();
                    relationEvent.setRelatedEventIdentifierType(eventIdentifierType);
                    relationEvent.setRelatedEventIdentifierValue(eventIdentifierValue);
                    relationship.getRelatedEventIdentification().add(relationEvent);
                } else {
                    LinkingEventIdentifierComplexType linkingEvent = new LinkingEventIdentifierComplexType();
                    linkingEvent.setLinkingEventIdentifierType(eventIdentifierType);
                    linkingEvent.setLinkingEventIdentifierValue(eventIdentifierValue);
                    object.getLinkingEventIdentifier().add(linkingEvent);
                }
            }
        } catch (Exception ex) {
            LOG.warning(ex.getMessage());
            ex.printStackTrace();
        }
        return mets;
    }

    private Mets addLinkingAgent(String modelId, Mets mets, String agentIdentifierType, String agentIdentifierValue, String agentType) {
        MdSecType.MdWrap.XmlData xmlData = getXmlData(modelId, mets);
        if (xmlData == null) {
            return mets;
        }
        try {
            LinkingAgentIdentifierComplexType linkingAgent = new LinkingAgentIdentifierComplexType();
            linkingAgent.setLinkingAgentIdentifierType(agentIdentifierType);
            linkingAgent.setLinkingAgentIdentifierValue(agentIdentifierValue);
            linkingAgent.getLinkingAgentRole().add(agentType);
            EventComplexType event = (EventComplexType) ((JAXBElement) xmlData.getAny().get(0)).getValue();
            event.getLinkingAgentIdentifier().add(linkingAgent);
        } catch (Exception ex) {
            LOG.warning(ex.getMessage());
            ex.printStackTrace();
        }
        return mets;
    }

    private MdSecType.MdWrap.XmlData getXmlData(String modelId, Mets mets) {
        for (AmdSecType amdSec : mets.getAmdSec()) {
            if (METAMODEL_EVENT_ID.equals(modelId)) {
                for (MdSecType mdSec : amdSec.getDigiprovMD()) {
                    if (mdSec.getMdWrap() != null && mdSec.getMdWrap().getXmlData() != null) {
                        MdSecType.MdWrap.XmlData xmlData = mdSec.getMdWrap().getXmlData();
                        return xmlData;
                    }
                }
            } else if (METAMODEL_OBJECT_ID.equals(modelId)) {
                for (MdSecType mdSec : amdSec.getTechMD()) {
                    if (mdSec.getMdWrap() != null && mdSec.getMdWrap().getXmlData() != null) {
                        MdSecType.MdWrap.XmlData xmlData = mdSec.getMdWrap().getXmlData();
                        return xmlData;
                    }
                }
            }
        }
        return null;
    }

    private String getNodeValue(String pid, String expression) throws SoftwareException {
        try {
            ProArcObject object = null;
            if (Storage.FEDORA.equals(typeOfStorage)) {
                object = fedoraStorage.find(pid);
            } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                object = akubraStorage.find(pid);
            }
            XmlStreamEditor editor = getMetsDescriptionEditor(object);
            InputStream src = editor.readStream();

            if (src == null) {
                return null;
            }
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document document = builder.parse(src);

            // XPath pro nalezení agentIdentifierType
            XPathFactory xPathFactory = XPathFactory.newInstance();
            XPath xpath = xPathFactory.newXPath();
            xpath.setNamespaceContext(new SimpleNamespaceContext().add(MetsConstants.PREFIX_NS_METS, MetsConstants.NS_METS).add("premis", PremisUtils.NS));

            XPathExpression xPathExpression = xpath.compile(expression);

            // Výsledek
            String result = (String) xPathExpression.evaluate(document, XPathConstants.STRING);
            return result;
        } catch (Exception ex) {
            LOG.warning(ex.getMessage());
            ex.printStackTrace();
            throw new SoftwareException("Nepodarilo se zkombinovat METS pro objekt " + pid);
        }
    }

    /**
     * Updates a software.
     *
     * @param update data to update
     * @param log    log message
     * @return the updated software
     * @throws SoftwareException failure
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
            object.setModel(update.getModel());

            updateDc(object, id, model, label, log);
            XmlStreamEditor descriptionEditor = getMetsDescriptionEditor(object);
            Source oldDescSrc = descriptionEditor.read();
            if (oldDescSrc == null) {
                update.setTimestamp(descriptionEditor.getLastModified());
            }
            if (oldDescSrc != null && update.getDescription() == null) {
                update.setDescription(new Mets());
            }
            if (update.getDescription() != null) {
                EditorResult result = descriptionEditor.createResult();
                MetsUtils.marshal(result, update.getDescription(), true);
                descriptionEditor.write(result, update.getTimestamp(), log);
            }
            if (update.getSetOfLinkedIds() != null) {
                RelationEditor relationEditor = new RelationEditor(object);
                relationEditor.setMembers(update.getSetOfLinkedIds());
                relationEditor.write(relationEditor.getLastModified(), log);
            }

            object.setLabel(label);
            object.setModel(model);
            object.flush();

            Software software = new Software();
            software.setId(id);
            software.setModel(model);
            software.setLabel(label);
            software.setDescription(update.getDescription());
            software.setSetOfLinkedIds(update.getSetOfLinkedIds());
            software.setTimestamp(descriptionEditor.getLastModified());
            return software;
        } catch (DigitalObjectConcurrentModificationException ex) {
            throw new SoftwareConcurentModificationException(id, ex);
        } catch (DigitalObjectException ex) {
            throw new SoftwareException(id, ex);
        }
    }

    private Software addSoftware(String pid, String owner, String model, String label, String log) throws DigitalObjectException, SoftwareException {
        return addSoftware(pid, owner, model, label, null, log);
    }

    private Software addSoftware(String pid, String owner, String model, String label, Mets description, String log) throws DigitalObjectException, SoftwareException {

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
            akubraStorage.ingest(lobject, SolrUtils.PROPERTY_PARENTPID_NO_PARENT, owner);
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
            items = searchView.findByModels(offset, METAMODEL_AGENT_ID, METAMODEL_EVENT_ID, METAMODEL_OBJECT_ID, METAMODEL_SET_ID);
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
     * Gets a datastream editor for Mets format.
     *
     * @param robj an object to edit
     * @return the editor
     */
    public static XmlStreamEditor getMetsDescriptionEditor(ProArcObject robj) {
        String labelValue = DESCRIPTION_DS_AGENT_LABEL;
        DatastreamProfile dProfile = FoxmlUtils.managedProfile(
                DESCRIPTION_DS_ID, MetsConstants.NS_METS, labelValue);
        XmlStreamEditor editor = robj.getEditor(dProfile);
        return editor;
    }

    public Mets fixMetsAccordingModel(String model, Mets mets) throws SoftwareException {
        if (model == null || model.isEmpty()) {
            return mets;
        } else if (SoftwareRepository.METAMODEL_SET_ID.equals(model)) {
            return null;
        } else {
            for (AmdSecType amdSecType : mets.getAmdSec()) {
                for (MdSecType mdSec : amdSecType.getDigiprovMD()) {
                    if (mdSec.getMdWrap() != null && mdSec.getMdWrap().getXmlData() != null) {
                        MdSecType.MdWrap.XmlData xmlData = mdSec.getMdWrap().getXmlData();
                        if (SoftwareRepository.METAMODEL_AGENT_ID.equals(model)) {
                            try {
                                AgentComplexType agent = (AgentComplexType) ((JAXBElement) xmlData.getAny().get(0)).getValue();
                            } catch (Exception ex) {
                                xmlData.getAny().clear();
                            }
                            amdSecType.getTechMD().clear();
                        } else if (SoftwareRepository.METAMODEL_EVENT_ID.equals(model)) {
                            try {
                                EventComplexType event = (EventComplexType) ((JAXBElement) xmlData.getAny().get(0)).getValue();
                            } catch (Exception ex) {
                                xmlData.getAny().clear();
                            }
                            amdSecType.getTechMD().clear();
                        } else {
                            throw new SoftwareException("Nepodporovany model v zanoreni amdSec/digiprovMD/mdWrap/xmlData");
                        }
                    }
                }
                for (MdSecType mdSec : amdSecType.getTechMD()) {
                    if (mdSec.getMdWrap() != null && mdSec.getMdWrap().getXmlData() != null) {
                        MdSecType.MdWrap.XmlData xmlData = mdSec.getMdWrap().getXmlData();
                        if (SoftwareRepository.METAMODEL_OBJECT_ID.equals(model)) {
                            try {
                                ObjectComplexType object = (ObjectComplexType) ((JAXBElement) xmlData.getAny().get(0)).getValue();
                            } catch (Exception ex) {
                                xmlData.getAny().clear();
                            }
                            amdSecType.getDigiprovMD().clear();
                        } else {
                            throw new SoftwareException("Nepodporovany model v zanoreni amdSec/techMD/mdWrap/xmlData");
                        }
                    }
                }
            }
        }
        return mets;
    }

    public List<String> checkSetOfIds(List<String> setOfIds, String model) throws SoftwareException {
        for (String member : setOfIds) {
            checkSoftwareId(member);
        }
        if (METAMODEL_AGENT_ID.equals(model) && !setOfIds.isEmpty()) {
            throw new SoftwareException("AGENT nesmí mít odkaz na ostatní software.");
        }
        List<SearchViewItem> items = new ArrayList<>();
        try {
            SearchView searchView = null;
            if (Storage.FEDORA.equals(typeOfStorage)) {
                searchView = fedoraStorage.getSearch();
            } else if (Storage.AKUBRA.equals(typeOfStorage)) {
                searchView = akubraStorage.getSearch().setAllowDevicesAndSoftware(true);
            }
            items = searchView.find(setOfIds);
        } catch (IOException | FedoraClientException ex) {
            throw new SoftwareException(ex.getMessage());
        }

        for (SearchViewItem item : items) {
            if (METAMODEL_EVENT_ID.equals(model)) {
                if (!METAMODEL_AGENT_ID.equals(item.getModel())) {
                    throw new SoftwareException("EVENT musí mít odkaz jen na AGENT. Nalezena v vazba na " + item.getModel() + " -> " + item.getPid() + ").");
                }
            } else if (METAMODEL_OBJECT_ID.equals(model)) {
                if (!METAMODEL_EVENT_ID.equals(item.getModel())) {
                    throw new SoftwareException("OBJECT musí mít odkaz jen na EVENT. Nalezena v vazba na " + item.getModel() + " -> " + item.getPid() + ").");
                }
            } else if (METAMODEL_SET_ID.equals(model)) {
                if (!METAMODEL_OBJECT_ID.equals(item.getModel())) {
                    throw new SoftwareException("SET musí mít odkaz jen na OBJECT. Nalezena v vazba na " + item.getModel() + " -> " + item.getPid() + ").");
                }
            }
        }

        return setOfIds;
    }

    public class SoftwareMets {

        private Mets mets;
        private String modelId;

        public SoftwareMets(Mets mets, String modelId) {
            this.mets = mets;
            this.modelId = modelId;
        }

        public Mets getMets() {
            return mets;
        }

        public void setMets(Mets mets) {
            this.mets = mets;
        }

        public String getModelId() {
            return modelId;
        }

        public void setModelId(String modelId) {
            this.modelId = modelId;
        }
    }
}
