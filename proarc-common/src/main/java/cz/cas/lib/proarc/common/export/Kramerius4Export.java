/*
 * Copyright (C) 2012 Jan Pokorsky
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
package cz.cas.lib.proarc.common.export;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.XmlContentType;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.export.ExportResultLog.ExportResult;
import cz.cas.lib.proarc.common.export.ExportResultLog.ResultError;
import cz.cas.lib.proarc.common.export.ExportResultLog.ResultStatus;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.fedora.relation.RelationResource;
import cz.cas.lib.proarc.common.fedora.relation.Relations;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.oaidublincore.DcConstants;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Exports digital object and transforms its data streams to Kramerius4 format.
 *
 * For now it exports FOXML in archive format. It is memory intensive but fast.
 * In case of OOME it should be rewritten to export FOXML in public or migrate
 * format and fetch each managed data streams with REST or build the whole FOXML
 * from scratch.
 *
 * @author Jan Pokorsky
 */
public final class Kramerius4Export {

    public static final String KRAMERIUS_RELATION_NS = "http://www.nsdl.org/ontologies/relationships#";
    public static final String KRAMERIUS_RELATION_PREFIX = "kramerius";
    public static final String OAI_NS = "http://www.openarchives.org/OAI/2.0/";

    public static final String[] ALLOWED_POLICY = {"policy:private", "policy:public"};

    private RemoteStorage rstorage;
    private LocalStorage lstorage = new LocalStorage();
    private DigitalObjectCrawler crawler;

    private final SearchView search;
    /** already exported PIDs to prevent loops */
    private HashSet<String> exportedPids = new HashSet<String>();
    /** PIDs scheduled for export */
    private Queue<String> toExport = new LinkedList<String>();
    
    private final Kramerius4ExportOptions options;

    private final String policy;

    public Kramerius4Export(RemoteStorage rstorage, Kramerius4ExportOptions options) {
        this(rstorage, options, options.getPolicy());
    }

    public Kramerius4Export(RemoteStorage rstorage, Kramerius4ExportOptions options, String policy) {
        this.rstorage = rstorage;
        this.options = options;
        this.search = rstorage.getSearch();
        this.crawler = new DigitalObjectCrawler(DigitalObjectManager.getDefault(), search);

        if (Arrays.asList(ALLOWED_POLICY).contains(policy)) {
            this.policy = policy;
        } else {
            this.policy = options.getPolicy();
        }
    }

    public File export(File output, boolean hierarchy, String log, String... pids) {
        if (!output.exists() || !output.isDirectory()) {
            throw new IllegalStateException(String.valueOf(output));
        }
        if (pids == null || pids.length == 0) {
            throw new IllegalArgumentException();
        }

        ExportResultLog reslog = new ExportResultLog();
        ExportResult result = new ExportResult();
        result.setInputPid(pids[0]);
        reslog.getExports().add(result);

        File target = ExportUtils.createFolder(output, "k4_" + FoxmlUtils.pidAsUuid(pids[0]));
        HashSet<String> selectedPids = new HashSet<String>(Arrays.asList(pids));
        toExport.addAll(selectedPids);
        try {
            for (String pid = toExport.poll(); pid != null; pid = toExport.poll()) {
                exportPid(target, hierarchy, pid);
            }
            exportParents(target, selectedPids);
            storeExportResult(target, log);
        } catch (RuntimeException ex) {
            result.setStatus(ResultStatus.FAILED);
            reslog.getExports().add(result);
            result.getError().add(new ResultError(null, ex));
            result.setEnd();
            ExportUtils.writeExportResult(target, reslog);
            throw ex;
        }

        result.setStatus(ResultStatus.OK);
        result.setEnd();
        ExportUtils.writeExportResult(target, reslog);
        return target;
    }

    void exportPid(File output, boolean hierarchy, String pid) {
        try {
            if (exportedPids.contains(pid)) {
                return ;
            }
            exportedPids.add(pid);
            RemoteObject robject = rstorage.find(pid);
            FedoraClient client = robject.getClient();
            DigitalObject dobj = FedoraClient.export(pid).context("archive")
                    .format("info:fedora/fedora-system:FOXML-1.1")
                    .execute(client).getEntity(DigitalObject.class);
            File foxml = ExportUtils.pidAsXmlFile(output, pid);
            LocalObject local = lstorage.create(foxml, dobj);
            RelationEditor editor = new RelationEditor(local);
            if (hierarchy) {
                List<String> children = editor.getMembers();
                toExport.addAll(children);
            }
            exportDatastreams(local, editor);
            local.flush();
        } catch (DigitalObjectException ex) {
            throw new IllegalStateException(pid, ex);
        } catch (FedoraClientException ex) {
            // replace with ExportException
            throw new IllegalStateException(pid, ex);
        }
    }

    /**
     * Exports hierarchy of parent objects. Leafs of the hierarchy are PIDs
     * that were selected for export.
     * <p/>RELS-EXT of exported parent objects contains only PIDs that are subject to export.
     * Other relations are excluded.
     *
     * @param output output folder
     * @param pids PIDs selected for export
     */
    private void exportParents(File output, Collection<String> pids) {
        Map<String, Set<String>> buildPidTree = buildPidTree(pids, exportedPids);
        for (Entry<String, Set<String>> node : buildPidTree.entrySet()) {
            String pid = node.getKey();
            Set<String> children = node.getValue();
            exportParentPid(output, pid, children);
        }
    }

    void exportParentPid(File output, String pid, Collection<String> includeChildPids) {
        try {
            exportedPids.add(pid);
            RemoteObject robject = rstorage.find(pid);
            FedoraClient client = robject.getClient();
            DigitalObject dobj = FedoraClient.export(pid).context("archive")
                    .format("info:fedora/fedora-system:FOXML-1.1")
                    .execute(client).getEntity(DigitalObject.class);
            File foxml = ExportUtils.pidAsXmlFile(output, pid);
            LocalObject local = lstorage.create(foxml, dobj);
            exportParentDatastreams(local, includeChildPids);
            local.flush();
        } catch (DigitalObjectException ex) {
            throw new IllegalStateException(pid, ex);
        } catch (FedoraClientException ex) {
            // replace with ExportException
            throw new IllegalStateException(pid, ex);
        }
    }

    void storeExportResult(File output, String log) {
        for (String pid : exportedPids) {
            try {
                File foxml = ExportUtils.pidAsXmlFile(output, pid);
                ExportUtils.storeObjectExportResult(pid, foxml.toURI().toASCIIString(), log);
            } catch (DigitalObjectException ex) {
                throw new IllegalStateException(ex);
            }
        }
    }

    /**
     * Builds tree of digital objects as map of parent nodes and their children.
     *
     * @param pids PIDs that will be leafs of the tree
     * @param exportedPids collection of already exported PIDs
     * @return {@code Map<parent-PID, Set<child-PID>>}
     */
    private Map<String, Set<String>> buildPidTree(Collection<String> pids, Collection<String> exportedPids) {
        // P1/R1/C1
        // P1/R1/C2
        // P1/R3/C3
        // pids={C1, C2, C3}
        // Map<PID, Set<PID>> tree  P1={R1, R3}, R1={C1, C2}, R3={C3}
        Map<String, Set<String>> pidTree = new HashMap<String, Set<String>>();
        for (String pid : pids) {
            try {
                fillPidTree(pid, pidTree);
            } catch (DigitalObjectException ex) {
                throw new IllegalStateException(pid, ex);
            }
        }
        return pidTree;
    }

    private void fillPidTree(String selectedPid, Map<String, Set<String>> pidTree) throws DigitalObjectException {
        List<DigitalObjectElement> reversePath = crawler.getReversePath(selectedPid);
        reversePath.add(crawler.getEntry(selectedPid));
        Set<String> lastChildren = null;
        for (Iterator<DigitalObjectElement> it = reversePath.iterator(); it.hasNext();) {
            DigitalObjectElement elm = it.next();
            if (lastChildren != null) {
                lastChildren.add(elm.getPid());
            }
            if (it.hasNext() && !exportedPids.contains(elm.getPid())) {
                lastChildren = pidTree.get(elm.getPid());
                if (lastChildren == null) {
                    lastChildren = new HashSet<String>();
                    pidTree.put(elm.getPid(), lastChildren);
                }
            }
        }
    }

    private void exportDatastreams(LocalObject local, RelationEditor editor) {
        DigitalObject dobj = local.getDigitalObject();
        // XXX replace DS only for other than image/* MIMEs?
        DatastreamType fullDs = FoxmlUtils.findDatastream(dobj, BinaryEditor.FULL_ID);
        DatastreamType rawDs = fullDs != null ? null : FoxmlUtils.findDatastream(dobj, BinaryEditor.RAW_ID);
        for (Iterator<DatastreamType> it = dobj.getDatastream().iterator(); it.hasNext();) {
            DatastreamType datastream = it.next();
            if (options.getExcludeDatastreams().contains(datastream.getID())) {
                // use RAW if FULL is not available
                if (rawDs != datastream ) {
                    it.remove();
                    continue;
                }
            }
            excludeVersions(datastream);
            renameDatastream(datastream);
            processDublinCore(datastream);
            processMods(datastream);
            processRelsExt(dobj.getPID(), datastream, editor, null);
        }
    }

    private void exportParentDatastreams(LocalObject local, Collection<String> includeChildPids) {
        DigitalObject dobj = local.getDigitalObject();
        RelationEditor editor = new RelationEditor(local);
        for (Iterator<DatastreamType> it = dobj.getDatastream().iterator(); it.hasNext();) {
            DatastreamType datastream = it.next();
            if (options.getExcludeDatastreams().contains(datastream.getID())) {
                it.remove();
                continue;
            }
            excludeVersions(datastream);
            renameDatastream(datastream);
            processDublinCore(datastream);
            processMods(datastream);
            processRelsExt(dobj.getPID(), datastream, editor, includeChildPids);
        }
    }

    private void excludeVersions(DatastreamType datastream) {
        List<DatastreamVersionType> versions = datastream.getDatastreamVersion();
        final int size = versions.size();
        if (size > 1) {
            DatastreamVersionType actual = versions.get(size - 1);
            versions.retainAll(Collections.singletonList(actual));
        }
        datastream.setVERSIONABLE(false);
    }

    private void renameDatastream(DatastreamType datastream) {
        String id = datastream.getID();
        String newId = options.getDsIdMap().get(id);
        if (newId != null) {
            datastream.setID(newId);
            for (DatastreamVersionType version : datastream.getDatastreamVersion()) {
                String versionId = version.getID();
                String newVersionId = versionId.replace(id, newId);
                version.setID(newVersionId);
            }
        }
    }

    private void processDublinCore(DatastreamType datastream) {
        if (!DcStreamEditor.DATASTREAM_ID.equals(datastream.getID())) {
            return ;
        }
        DatastreamVersionType version = datastream.getDatastreamVersion().get(0);
        XmlContentType xmlContent = version.getXmlContent();
        Element dcElm = xmlContent.getAny().get(0);
        FoxmlUtils.fixFoxmlDc(dcElm);
        // add policy
        if (policy != null) {
            Element elmRights = dcElm.getOwnerDocument().createElementNS(
                    DcConstants.NS_PURL, DcConstants.PREFIX_NS_PURL + ':' + DcConstants.RIGHTS);
            elmRights.setTextContent(policy);
            dcElm.appendChild(elmRights);
        }
        // map proarc/K4 models
        NodeList typeNodes = dcElm.getElementsByTagNameNS(DcConstants.NS_PURL, DcConstants.TYPE);
        for (int i = 0; i < typeNodes.getLength(); i++) {
            Element typeElm = (Element) typeNodes.item(i);
            String type = typeElm.getTextContent();
            String k4ModelId = options.getModelMap().get(type);
            if (k4ModelId != null) {
                typeElm.setTextContent(k4ModelId);
            }
        }
    }

    private void processMods(DatastreamType datastream) {
        if (!ModsStreamEditor.DATASTREAM_ID.equals(datastream.getID())) {
            return ;
        }
        DatastreamVersionType version = datastream.getDatastreamVersion().get(0);
        XmlContentType xmlContent = version.getXmlContent();
        Element mods = xmlContent.getAny().get(0);
        removeNils(mods);
        wrapModsInCollection(xmlContent);
    }

    /**
     * Removes all subelements with xsi:nil attribute as they are worthless.
     *
     * JAXB optimizes namespace declarations and moves them to common parent elements
     * but Fedora ingest ignores it. Then some ingested datastreams may be broken
     * as they miss optimized namespace declarations (xsi in this case).
     */
    public static void removeNils(Element elm) {
        NodeList children = elm.getChildNodes();
        for (int i = children.getLength() - 1; i >= 0; i--) {
            Node item = children.item(i);
            if (item.getNodeType() == Node.ELEMENT_NODE) {
                Element itemElm = (Element) item;
                if (itemElm.hasAttributeNS(XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI, "nil")) {
                    itemElm.getParentNode().removeChild(item);
                } else {
                    removeNils(itemElm);
                }
            }
        }
    }

    /**
     * Wraps mods root element in modsCollection element like Kramerius expects it.
     */
    private static void wrapModsInCollection(XmlContentType xmlContent) {
        Element mods = xmlContent.getAny().get(0);
        if ("modsCollection".equals(mods.getLocalName())) {
            return ;
        }
        Element modsCollection = mods.getOwnerDocument().createElementNS(
                ModsStreamEditor.DATASTREAM_FORMAT_URI, "mods:modsCollection");
        modsCollection.appendChild(mods);
        xmlContent.getAny().clear();
        xmlContent.getAny().add(modsCollection);
    }

    private void processRelsExt(String pid, DatastreamType datastream,
            RelationEditor editor, Collection<String> includePids
            ) {

        if (!RelationEditor.DATASTREAM_ID.equals(datastream.getID())) {
            return ;
        }
        try {
            List<Item> childDescriptors = search.findChildren(pid);
            transformRelation2Kramerius(pid, editor, childDescriptors, includePids);
        } catch (DigitalObjectException ex) {
            throw new IllegalStateException(ex);
        } catch (FedoraClientException ex) {
            throw new IllegalStateException(ex);
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
        DatastreamVersionType version = datastream.getDatastreamVersion().get(0);
        XmlContentType xmlContent = version.getXmlContent();
        Element get = xmlContent.getAny().get(0);
        // optimize XML namespace declaration
        get.setAttributeNS(XMLConstants.XMLNS_ATTRIBUTE_NS_URI,
                XMLConstants.XMLNS_ATTRIBUTE + ":" + KRAMERIUS_RELATION_PREFIX,
                KRAMERIUS_RELATION_NS);
    }

    private void transformRelation2Kramerius(
            String pid, RelationEditor editor, List<Item> childDescriptors,
            Collection<String> includePids
            ) throws DigitalObjectException {

        List<String> children = editor.getMembers();
        try {
            DocumentBuilderFactory dfactory = DocumentBuilderFactory.newInstance();
            dfactory.setNamespaceAware(true);
            Document doc = dfactory.newDocumentBuilder().newDocument();
            List<Element> relations = editor.getRelations();

            setOaiId(pid, relations, doc);
            
            setImportFile(editor, relations, doc);

            setPolicy(policy, relations, doc);

            editor.setDevice(null);
            editor.setExportResult(null);
            editor.setOwners(Collections.<String>emptyList());
            editor.setMembership(Collections.<String>emptyList());

            String modelId = editor.getModel();
            String k4ModelId = options.getModelMap().get(modelId);
            k4ModelId = k4ModelId == null ? modelId : k4ModelId;
            editor.setModel(k4ModelId);

            editor.setMembers(Collections.<String>emptyList());
            for (String childPid : children) {
                Item desc = remove(childPid, childDescriptors);
                if (desc == null) {
                    throw new IllegalStateException("Child " + childPid + " of " + pid + " not found in resource index!");
                }
                if (includePids != null && !includePids.contains(childPid)) {
                    continue;
                }
                String krelation = options.getRelationMap().get(desc.getModel());
                if (krelation == null) {
                    throw new IllegalStateException(String.format(
                            "Cannot map to Kramerius relation! Child: %s, model: %s, parent: %s ",
                            childPid, desc.getModel(), pid));
                }
                Element elm = doc.createElementNS(KRAMERIUS_RELATION_NS, KRAMERIUS_RELATION_PREFIX + ":" + krelation);
                elm.setAttributeNS(Relations.RDF_NS,
                        "rdf:resource",
                        RelationResource.fromPid(childPid).getResource());
                relations.add(elm);
            }
            editor.setRelations(relations);
            editor.write(editor.getLastModified(), null);
        } catch (ParserConfigurationException ex) {
            throw new IllegalStateException(ex);
        }
    }

    private static Item remove(String pid, List<Item> childDescriptors) {
        for (Iterator<Item> it = childDescriptors.iterator(); it.hasNext();) {
            Item desc = it.next();
            if (pid.equals(desc.getPid())) {
                it.remove();
                return desc;
            }
        }
        return null;
    }

    /**
     * Sets OAI ID as relation if necessary.
     * @param pid PID to use as OAI ID
     * @param relations list of existing relations
     * @param doc DOM
     */
    private static void setOaiId(String pid, List<Element> relations, Document doc) {
        for (Element relation : relations) {
            if (OAI_NS.equals(relation.getNamespaceURI()) && "itemID".equals(relation.getLocalName())) {
                return ;
            }
        }
        Element elm = doc.createElementNS(OAI_NS, "oai:itemID");
        elm.setTextContent(pid);
        relations.add(elm);
    }

    /**
     * Sets kramerius:policy as relation if necessary.
     * @param policy access policy
     * @param relations list of existing relations
     * @param doc DOM
     */
    private static void setPolicy(String policy, List<Element> relations, Document doc) {
        if (policy != null) {
            Element elmPolicy = doc.createElementNS(KRAMERIUS_RELATION_NS, KRAMERIUS_RELATION_PREFIX + ":policy");
            elmPolicy.setTextContent(policy);
            relations.add(elmPolicy);
        }
    }

    /**
     * Replaces proarc-rels:importFile with kramerius:file.
     */
    private void setImportFile(RelationEditor editor, List<Element> relations, Document doc) throws DigitalObjectException {
        String importFile = editor.getImportFile();
        if (importFile != null) {
            editor.setImportFile(null);
            Element elm = doc.createElementNS(KRAMERIUS_RELATION_NS, KRAMERIUS_RELATION_PREFIX + ":file");
            elm.setTextContent(importFile);
            relations.add(0, elm);
        }
    }

}
