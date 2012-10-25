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
import cz.incad.pas.editor.server.fedora.LocalStorage;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalObject;
import cz.incad.pas.editor.server.fedora.RemoteStorage;
import cz.incad.pas.editor.server.fedora.RemoteStorage.RemoteObject;
import cz.incad.pas.editor.server.fedora.SearchView.Item;
import cz.incad.pas.editor.server.fedora.relation.RelationEditor;
import cz.incad.pas.editor.server.fedora.relation.RelationResource;
import cz.incad.pas.editor.server.fedora.relation.Relations;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

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
    /** already exported PIDs to prevent loops */
    private HashSet<String> exportedPids = new HashSet<String>();
    /** PIDs scheduled for export */
    private Queue<String> toExport = new LinkedList<String>();
    
    // config options; it should go to proarc.properties
    private final HashSet<String> excludeDatastreams = new HashSet<String>(Arrays.asList("AUDIT", "RAW", "PRIVATE_NOTE"));
    private final HashMap<String, String> dsIdMap = new HashMap<String, String>() {{
        put("FULL", "IMG_FULL");
        put("PREVIEW", "IMG_PREVIEW");
        put("THUMBNAIL", "IMG_THUMBNAIL");
    }};
    private final HashMap<String, String> relationMap = new HashMap<String, String>() {{
        put("model:page", "hasPage");
        put("model:monographunit", "hasUnit");
        put("model:periodicalvolume", "hasVolume");
        put("model:periodicalitem", "hasItem");
    }};

    public Kramerius4Export(RemoteStorage rstorage) throws IOException {
        this.rstorage = rstorage;

    }

    public File export(File output, boolean hierarchy, String... pids) {
        if (!output.exists() || !output.isDirectory()) {
            throw new IllegalStateException(String.valueOf(output));
        }
        if (pids == null || pids.length == 0) {
            throw new IllegalArgumentException();
        }

        File target = ExportUtils.createFolder(output, pids[0]);
        toExport.addAll(Arrays.asList(pids));
        for (String pid = toExport.poll(); pid != null; pid = toExport.poll()) {
            exportPid(target, hierarchy, pid);
        }
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
            List<String> children = editor.getMembers();
            if (hierarchy) {
                toExport.addAll(children);
            }
            exportDatastreams(local, editor);
            local.flush();
        } catch (FedoraClientException ex) {
            // replace with ExportException
            throw new IllegalStateException(pid, ex);
        }
    }
    
    private void exportDatastreams(LocalObject local, RelationEditor editor) {
        DigitalObject dobj = local.getDigitalObject();
        for (Iterator<DatastreamType> it = dobj.getDatastream().iterator(); it.hasNext();) {
            DatastreamType datastream = it.next();
            if (excludeDatastreams.contains(datastream.getID())) {
                it.remove();
                continue;
            }
            excludeVersions(datastream);
            renameDatastream(datastream);
            processRelsExt(dobj.getPID(), datastream, editor);
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
        String newId = dsIdMap.get(id);
        if (newId != null) {
            datastream.setID(newId);
            for (DatastreamVersionType version : datastream.getDatastreamVersion()) {
                String versionId = version.getID();
                String newVersionId = versionId.replace(id, newId);
                version.setID(newVersionId);
            }
        }
    }

    private void processRelsExt(String pid, DatastreamType datastream, RelationEditor editor) {
        if (!RelationEditor.DATASTREAM_ID.equals(datastream.getID())) {
            return ;
        }
        try {
            List<Item> childDescriptors = rstorage.getSearch().findChildren(pid);
            transformRelation2Kramerius(pid, editor, childDescriptors);
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

    private void transformRelation2Kramerius(String pid, RelationEditor editor, List<Item> childDescriptors) {
        List<String> children = editor.getMembers();
        try {
            DocumentBuilderFactory dfactory = DocumentBuilderFactory.newInstance();
            dfactory.setNamespaceAware(true);
            Document doc = dfactory.newDocumentBuilder().newDocument();
            List<Element> relations = editor.getRelations();

            setOaiId(pid, relations, doc);

            editor.setMembers(Collections.<String>emptyList());
            for (String childPid : children) {
                Item desc = remove(childPid, childDescriptors);
                if (desc == null) {
                    throw new IllegalStateException("Child " + childPid + " of " + pid + " not found in resource index!");
                }
                String krelation = relationMap.get(desc.getModel());
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
            editor.write(editor.getLastModified());
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

    static File pidAsFile(File output, String pid) {
        File foxml = new File(output, pid + ".foxml");
        return foxml;
    }

}
