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
package cz.incad.pas.editor.server.export;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.XmlContentType;
import cz.incad.pas.editor.server.dublincore.DcStreamEditor;
import cz.incad.pas.editor.server.dublincore.DcUtils;
import cz.incad.pas.editor.server.fedora.DigitalObjectException;
import cz.incad.pas.editor.server.fedora.LocalStorage;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalObject;
import cz.incad.pas.editor.server.fedora.RemoteStorage;
import cz.incad.pas.editor.server.fedora.RemoteStorage.RemoteObject;
import cz.incad.pas.editor.server.fedora.SearchView;
import cz.incad.pas.editor.server.fedora.SearchView.Item;
import cz.incad.pas.editor.server.fedora.relation.RelationEditor;
import cz.incad.pas.editor.server.fedora.relation.RelationResource;
import cz.incad.pas.editor.server.fedora.relation.Relations;
import cz.incad.pas.editor.server.mods.ModsStreamEditor;
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

    private RemoteStorage rstorage;
    private LocalStorage lstorage = new LocalStorage();
    private final SearchView search;
    /** already exported PIDs to prevent loops */
    private HashSet<String> exportedPids = new HashSet<String>();
    /** PIDs scheduled for export */
    private Queue<String> toExport = new LinkedList<String>();
    
    private final Kramerius4ExportOptions options;

    public Kramerius4Export(RemoteStorage rstorage, Kramerius4ExportOptions options) {
        this.rstorage = rstorage;
        this.options = options;
        this.search = rstorage.getSearch();
    }

    public File export(File output, boolean hierarchy, String... pids) {
        if (!output.exists() || !output.isDirectory()) {
            throw new IllegalStateException(String.valueOf(output));
        }
        if (pids == null || pids.length == 0) {
            throw new IllegalArgumentException();
        }

        File target = ExportUtils.createFolder(output, pids[0]);
        HashSet<String> selectedPids = new HashSet<String>(Arrays.asList(pids));
        toExport.addAll(selectedPids);
        for (String pid = toExport.poll(); pid != null; pid = toExport.poll()) {
            exportPid(target, hierarchy, pid);
        }
        exportParents(target, selectedPids);
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
            File foxml = pidAsFile(output, pid);
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
            RemoteObject robject = rstorage.find(pid);
            FedoraClient client = robject.getClient();
            DigitalObject dobj = FedoraClient.export(pid).context("archive")
                    .format("info:fedora/fedora-system:FOXML-1.1")
                    .execute(client).getEntity(DigitalObject.class);
            File foxml = pidAsFile(output, pid);
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
            String parentPid = getParent(pid);
            if (parentPid != null) {
                if (exportedPids.contains(parentPid)) {
                    continue;
                }
                Set<String> children = pidTree.get(parentPid);
                if (children == null) {
                    children = new HashSet<String>();
                    pidTree.put(parentPid, children);
                }
                children.add(pid);
            }
        }
        return pidTree;
    }

    private String getParent(String pid) {
        try {
            List<Item> referrers = search.findReferrers(pid);
            return referrers.isEmpty() ? null : referrers.get(0).getPid();
        } catch (Exception ex) {
            throw new IllegalStateException(pid, ex);
        }
    }
    
    private void exportDatastreams(LocalObject local, RelationEditor editor) {
        DigitalObject dobj = local.getDigitalObject();
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
        // remove xsi:schemaLocation attribute to make FOXML valid for Fedora ingest
        dcElm.removeAttributeNS(XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI, "schemaLocation");
        // add policy
        String policy = options.getPolicy();
        if (policy != null) {
            Element elmRights = dcElm.getOwnerDocument().createElementNS(
                    DcUtils.DC_NAMESPACE, DcUtils.DC_PREFIX + ":rights");
            elmRights.setTextContent(policy);
            dcElm.appendChild(elmRights);

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
    private static void removeNils(Element elm) {
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

            setPolicy(options.getPolicy(), relations, doc);

            editor.setDevice(null);

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

    static File pidAsFile(File output, String pid) {
        File foxml = new File(output, pid + ".foxml");
        return foxml;
    }

}
