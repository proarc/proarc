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
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.export.ExportResultLog.ExportResult;
import cz.cas.lib.proarc.common.export.ExportResultLog.ResultError;
import cz.cas.lib.proarc.common.export.ExportResultLog.ResultStatus;
import cz.cas.lib.proarc.common.export.mets.MetsContext;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.common.fedora.StringEditor;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.fedora.relation.RelationResource;
import cz.cas.lib.proarc.common.fedora.relation.Relations;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.K4Plugin;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.chronicle.ChroniclePlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.mods.DetailDefinition;
import cz.cas.lib.proarc.mods.ExtentDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.oaidublincore.DcConstants;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
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

import static cz.cas.lib.proarc.common.export.ExportUtils.containPageNumber;
import static cz.cas.lib.proarc.common.export.ExportUtils.containPageType;
import static cz.cas.lib.proarc.common.export.ExportUtils.getPageIndex;

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
    private Queue<Pair> toExport = new LinkedList<Pair>();
    private K4Informations informations = new K4Informations();
    
    private final Kramerius4ExportOptions kramerius4ExportOptions;
    private AppConfiguration appConfig;
    private ExportOptions exportOptions;

    private final String policy;

    private String exportPageContext;

    public Kramerius4Export(RemoteStorage rstorage, Kramerius4ExportOptions options) {
        this(rstorage, options, options.getPolicy(), null);
    }

    public Kramerius4Export(RemoteStorage rstorage, Kramerius4ExportOptions options, String policy, String exportPageContext) {
        this.rstorage = rstorage;
        this.kramerius4ExportOptions = options;
        this.search = rstorage.getSearch();
        this.crawler = new DigitalObjectCrawler(DigitalObjectManager.getDefault(), search);
        this.exportPageContext = exportPageContext;

        if (Arrays.asList(ALLOWED_POLICY).contains(policy)) {
            this.policy = policy;
        } else {
            this.policy = kramerius4ExportOptions.getPolicy();
        }
    }

    public Kramerius4Export(RemoteStorage rstorage, AppConfiguration configuration) {
        this(rstorage, configuration, configuration.getKramerius4Export().getPolicy());
    }

    public Kramerius4Export(RemoteStorage rstorage, AppConfiguration appConfiguration, String policy) {
        this.appConfig = appConfiguration;
        this.rstorage = rstorage;
        this.kramerius4ExportOptions = appConfiguration.getKramerius4Export();
        this.exportOptions = appConfiguration.getExportOptions();
        this.search = rstorage.getSearch();
        this.crawler = new DigitalObjectCrawler(DigitalObjectManager.getDefault(), search);

        if (Arrays.asList(ALLOWED_POLICY).contains(policy)) {
            this.policy = policy;
        } else {
            this.policy = kramerius4ExportOptions.getPolicy();
        }
    }

    public Result export(File output, boolean hierarchy, String log, String... pids) {
        if (!output.exists() || !output.isDirectory()) {
            throw new IllegalStateException(String.valueOf(output));
        }
        if (pids == null || pids.length == 0) {
            throw new IllegalArgumentException();
        }

        ExportResultLog reslog = new ExportResultLog();
        ExportResult result = new ExportResult();
        Result krameriusResult = new Result();
        result.setInputPid(pids[0]);
        reslog.getExports().add(result);

        File target = ExportUtils.createFolder(output, "k4_" + FoxmlUtils.pidAsUuid(pids[0]), exportOptions.isOverwritePackage());
        krameriusResult.setFile(target);
        HashSet<String> selectedPids = new HashSet<String>(Arrays.asList(pids));
        toExport.addAll(createPair(null, selectedPids));
        try {
            String[] parentModels = {NdkPlugin.MODEL_MONOGRAPHTITLE, OldPrintPlugin.MODEL_MONOGRAPHTITLE};
            //boolean hasParent = hasParent(target, hierarchy, selectedPids, parentModels);
            for (Pair object = toExport.poll(); object != null; object = toExport.poll()) {
                exportPid(target, hierarchy, object, hasParent(object.getPid()));
            }
            exportParents(target, selectedPids);
            storeExportResult(target, log);
            result.setStatus(ResultStatus.OK);
            result.setEnd();
            ExportUtils.writeExportResult(target, reslog);
            krameriusResult.setPageCount(exportedPids.size());
        } catch (RuntimeException ex) {
            result.setStatus(ResultStatus.FAILED);
            reslog.getExports().add(result);
            result.getError().add(new ResultError(null, ex));
            result.setEnd();
            ExportUtils.writeExportResult(target, reslog);
            throw ex;
        } catch (DigitalObjectException ex) {
            result.setStatus(ResultStatus.FAILED);
            reslog.getExports().add(result);
            result.getError().add(new ResultError(null, ex));
            result.setEnd();
            ExportUtils.writeExportResult(target, reslog);
            throw new IllegalStateException(ex.getMessage());
        } catch (MetsExportException ex ) {
            result.setStatus(ResultStatus.FAILED);
            result.getError().add(new ResultError(null, ex));
            result.setEnd();
            ExportUtils.writeExportResult(target, reslog);
            krameriusResult.setValidationError(ex);
        } catch (Exception ex) {
            result.setStatus(ResultStatus.FAILED);
            result.getError().add(new ResultError(null, ex));
            result.setEnd();
            krameriusResult.setException(ex);
            ExportUtils.writeExportResult(target, reslog);
        }
        return krameriusResult;
    }

    private Collection<Pair> createPair(String parentsPid, Collection<String> selectedPids) {
        List<Pair> list = new ArrayList<>();
        Iterator<String> iterator = selectedPids.iterator();

        while (iterator.hasNext()) {
            list.add(new Pair(iterator.next(), parentsPid));
        }
        return list;
    }

    private boolean hasParent(String pid) throws DigitalObjectException {
        Set<String> parentModels = Collections.unmodifiableSet(new HashSet<>(Arrays.asList(NdkPlugin.MODEL_MONOGRAPHTITLE, OldPrintPlugin.MODEL_MONOGRAPHTITLE, ChroniclePlugin.MODEL_CHRONICLETITLE)));
        IMetsElement element = getElement(pid);
        if (element == null || element.getParent() == null || element.getParent().getModel() == null) {
            return false;
        } else {
            return parentModels.contains(element.getParent().getModel().substring(12));
        }
    }

    public IMetsElement getElement(String pid) throws DigitalObjectException {
        try {
            RemoteStorage rstorage = RemoteStorage.getInstance(appConfig);
            RemoteStorage.RemoteObject robject = rstorage.find(pid);
            MetsContext metsContext = buildContext(robject, null, rstorage);
            DigitalObject dobj = MetsUtils.readFoXML(robject.getPid(), robject.getClient());
            if (dobj == null) {
                return null;
            }
            return MetsElement.getElement(dobj, null, metsContext, true);
        } catch (IOException | MetsExportException ex) {
            throw new DigitalObjectException(pid, "K4 export: imposible to find element.", ex);
        }
    }

    private MetsContext buildContext(RemoteStorage.RemoteObject fo, String packageId, RemoteStorage rstorage) {
        MetsContext mc = new MetsContext();
        mc.setFedoraClient(fo.getClient());
        mc.setRemoteStorage(rstorage);
        mc.setPackageID(packageId);
        mc.setOutputPath(null);
        mc.setAllowNonCompleteStreams(false);
        mc.setAllowMissingURNNBN(false);
        mc.setConfig(null);
        return mc;
    }

    private boolean hasParent(File output, boolean hierarchy, HashSet<String> selectedPids, String[] models) {
        HashSet<String> pidsToExport = new HashSet<>();
        Queue<String> queueToExport = new LinkedList<String>();
        queueToExport.addAll(selectedPids);
        if (isMonographTitle(queueToExport, pidsToExport, output, hierarchy, models)) {
            return true;
        }
        return selectedPidHasParent(pidsToExport, output, selectedPids, models);
    }


    private boolean isMonographTitle(Queue<String> queueToExport, HashSet<String> pidsToExport, File output, boolean hierarchy, String[] models) {
        for (String pid = queueToExport.poll(); pid != null; pid = queueToExport.poll()) {
            try {
                if (pidsToExport.contains(pid)) {
                    return false;
                }
                pidsToExport.add(pid);
                RemoteObject robject = rstorage.find(pid);
                FedoraClient client = robject.getClient();
                DigitalObject dobj = FedoraClient.export(pid).context("public")
                        .format("info:fedora/fedora-system:FOXML-1.1")
                        .execute(client).getEntity(DigitalObject.class);
                File foxml = ExportUtils.pidAsXmlFile(output, pid);
                LocalObject local = lstorage.create(foxml, dobj);
                RelationEditor editor = new RelationEditor(local);
                if (hierarchy) {
                    List<String> children = editor.getMembers();
                    queueToExport.addAll(children);
                }
                DatastreamType fullDs = FoxmlUtils.findDatastream(dobj, BinaryEditor.FULL_ID);
                DatastreamType rawDs = fullDs != null ? null : FoxmlUtils.findDatastream(dobj, BinaryEditor.RAW_ID);
                for (Iterator<DatastreamType> it = dobj.getDatastream().iterator(); it.hasNext(); ) {
                    DatastreamType datastream = it.next();
                    if (kramerius4ExportOptions.getExcludeDatastreams().contains(datastream.getID())) {
                        // use RAW if FULL is not available
                        if (rawDs != datastream) {
                            it.remove();
                            continue;
                        }
                    }
                    if (hasParentFromDublincore(datastream, models)) {
                        return true;
                    }
                }
            } catch (DigitalObjectException ex) {
                throw new IllegalStateException(pid, ex);
            } catch (FedoraClientException ex) {
                // replace with ExportException
                throw new IllegalStateException(pid, ex);
            }
        }
       return false;
    }

    private boolean selectedPidHasParent(HashSet<String> pidsToExport, File output,HashSet<String> selectedPids, String[] models) {
        Map<String, Set<String>> buildPidTree = buildPidTree(selectedPids, pidsToExport);
        for (Entry<String, Set<String>> node : buildPidTree.entrySet()) {
            String pid = node.getKey();
            try {
                pidsToExport.add(pid);
                RemoteObject robject = rstorage.find(pid);
                FedoraClient client = robject.getClient();
                DigitalObject dobj = FedoraClient.export(pid).context("public")
                        .format("info:fedora/fedora-system:FOXML-1.1")
                        .execute(client).getEntity(DigitalObject.class);
                for (Iterator<DatastreamType> it = dobj.getDatastream().iterator(); it.hasNext();) {
                    DatastreamType datastream = it.next();
                    if (kramerius4ExportOptions.getExcludeDatastreams().contains(datastream.getID())) {
                        it.remove();
                        continue;
                    }
                    if (hasParentFromDublincore(datastream, models)) {
                        return true;
                    }
                }
            } catch (FedoraClientException ex) {
                // replace with ExportException
                throw new IllegalStateException(pid, ex);
            }
        }
        return false;
    }



    private boolean hasParentFromDublincore(DatastreamType datastream, String[] models) {
        if (DcStreamEditor.DATASTREAM_ID.equals(datastream.getID())) {
            DatastreamVersionType version = datastream.getDatastreamVersion().get(0);
            XmlContentType xmlContent = version.getXmlContent();
            Element dcElm = xmlContent.getAny().get(0);
            FoxmlUtils.fixFoxmlDc(dcElm);
            NodeList typeNodes = dcElm.getElementsByTagNameNS(DcConstants.NS_PURL, DcConstants.TYPE);
            for (int i = 0; i < typeNodes.getLength(); i++) {
                Element typeElm = (Element) typeNodes.item(i);
                String type = typeElm.getTextContent();
                for (int j = 0; j < models.length; j++) {
                    String model = models[j];
                    if (!model.isEmpty() && model.equals(type)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    void exportPid(File output, boolean hierarchy, Pair object, boolean hasParent) throws MetsExportException {
        try {
            if (exportedPids.contains(object.getPid())) {
                return ;
            }
            exportedPids.add(object.getPid());
            RemoteObject robject = rstorage.find(object.getPid());
            FedoraClient client = robject.getClient();
            DigitalObject dobj = FedoraClient.export(object.getPid()).context(exportPageContext == null ? "archive" : exportPageContext)
                    .format("info:fedora/fedora-system:FOXML-1.1")
                    .execute(client).getEntity(DigitalObject.class);
            File foxml = ExportUtils.pidAsXmlFile(output, object.getPid());
            LocalObject local = lstorage.create(foxml, dobj);
            validate(object.getPid(), object.getParentPid());
            RelationEditor editor = new RelationEditor(local);
            if (hierarchy) {
                List<String> children = editor.getMembers();
                toExport.addAll(createPair(object.getPid(), children));
            }
            exportDatastreams(local, editor, hasParent);
            local.flush();
        } catch (DigitalObjectException ex) {
            throw new IllegalStateException(object.getPid(), ex);
        } catch (FedoraClientException ex) {
            // replace with ExportException
            throw new IllegalStateException(object.getPid(), ex);
        }
    }

    private void validate(String pid, String parentPid) throws MetsExportException, DigitalObjectException {
        IMetsElement element = getElement(pid);
        if (element == null) {
            throw new MetsExportException(pid, "K4 export - nevytvoren element pro " + pid, false, null);
        } else {
            String model = ExportUtils.getModel(element.getModel());
            if (NdkPlugin.MODEL_PAGE.equals(model) || NdkPlugin.MODEL_NDK_PAGE.equals(model) || OldPrintPlugin.MODEL_PAGE.equals(model) || NdkAudioPlugin.MODEL_PAGE.equals(model)) {
                ModsDefinition mods = getMods(element.getOriginalPid());
                int pageIndex = getPageIndex(mods);
                Integer expectedPageIndex = informations.getExpectedPageIndex(parentPid);

                if (!containPageNumber(mods)) {
                    throw new MetsExportException(pid, "Strana nemá vyplněné číslo stránky.", false, null);
                }
                if (NdkPlugin.MODEL_NDK_PAGE.equals(model) && !containPageType(mods)) {
                    throw new MetsExportException(pid, "Strana nemá vyplněný typ stránky.", false, null);
                }
                if (expectedPageIndex != pageIndex) {
                    if (pageIndex == -1) {
                        throw new MetsExportException(pid, "Strana nemá vyplněný index strany. Očekávaná hodnota " + expectedPageIndex + ".", false, null);
                    } else {
                        throw new MetsExportException(pid, "Strana má neočekávaný index strany. Očekávaná hodnota " + expectedPageIndex + ", ale byl nalezen index "+ pageIndex + ".", false, null);
                    }
                } else {
                    informations.add(pid, model, pageIndex, parentPid);
                }
            } else {
                informations.add(pid, model, null, parentPid);
            }
        }
    }

    /**
     * Exports hierarchy of parent objects. Leafs of the hierarchy are PIDs
     * that were selected for export.
     * <p/>RELS-EXT of exported parent objects contains only PIDs that are subject to export.
     * Other relations are excluded.
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
            exportParentDatastreams(local, includeChildPids, hasParent(pid));
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
                ExportUtils.storeObjectExportResult(pid, output.toURI().toASCIIString(), "KRAMERIUS", log);
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

    private void exportDatastreams(LocalObject local, RelationEditor editor,  boolean hasParent) {
        DigitalObject dobj = local.getDigitalObject();
        // XXX replace DS only for other than image/* MIMEs?
        DatastreamType fullDs = FoxmlUtils.findDatastream(dobj, BinaryEditor.FULL_ID);
        DatastreamType rawDs = fullDs != null ? null : FoxmlUtils.findDatastream(dobj, BinaryEditor.RAW_ID);
        for (Iterator<DatastreamType> it = dobj.getDatastream().iterator(); it.hasNext();) {
            DatastreamType datastream = it.next();
            if (kramerius4ExportOptions.getExcludeDatastreams().contains(datastream.getID())) {
                // use RAW if FULL is not available
                if (rawDs != datastream ) {
                    it.remove();
                    continue;
                }
            }
            excludeVersions(datastream);
            renameDatastream(datastream);
            processDublinCore(datastream, hasParent);
            processMods(datastream);
            processOcr(datastream);
            processRelsExt(dobj.getPID(), datastream, editor, null, hasParent);
        }
    }

    private void exportParentDatastreams(LocalObject local, Collection<String> includeChildPids, boolean hasParent) {
        DigitalObject dobj = local.getDigitalObject();
        RelationEditor editor = new RelationEditor(local);
        for (Iterator<DatastreamType> it = dobj.getDatastream().iterator(); it.hasNext();) {
            DatastreamType datastream = it.next();
            if (kramerius4ExportOptions.getExcludeDatastreams().contains(datastream.getID())) {
                it.remove();
                continue;
            }
            excludeVersions(datastream);
            renameDatastream(datastream);
            processDublinCore(datastream, hasParent);
            processMods(datastream);
            processRelsExt(dobj.getPID(), datastream, editor, includeChildPids, hasParent);
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
        String newId = kramerius4ExportOptions.getDsIdMap().get(id);
        if (newId != null) {
            datastream.setID(newId);
            for (DatastreamVersionType version : datastream.getDatastreamVersion()) {
                String versionId = version.getID();
                String newVersionId = versionId.replace(id, newId);
                version.setID(newVersionId);
            }
        }
    }

    private void processDublinCore(DatastreamType datastream,  boolean hasParent) {
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
            String k4ModelId;
            if (hasParent && (NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(type) || OldPrintPlugin.MODEL_VOLUME.equals(type) || K4Plugin.MODEL_MONOGRAPH.equals(type))) {
                k4ModelId = K4Plugin.MODEL_MONOGRAPHUNIT;
            } else {
                k4ModelId = kramerius4ExportOptions.getModelMap().get(type);
            }
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
     * Replace empty OCR with lineseparator
     * Fedora 3.8 can't ingest empty stream (https://github.com/proarc/proarc/issues/658)
     *
     * @param datastream
     */
    private void processOcr(DatastreamType datastream) {
        if (!StringEditor.OCR_ID.equals(datastream.getID())) {
            return ;
        }
        DatastreamVersionType version = datastream.getDatastreamVersion().get(0);
        if (version.getBinaryContent() == null || version.getBinaryContent().length == 0) {
            version.setBinaryContent(System.lineSeparator().getBytes());
        }
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
            RelationEditor editor, Collection<String> includePids,
            boolean hasParent) {

        if (!RelationEditor.DATASTREAM_ID.equals(datastream.getID())) {
            return ;
        }
        try {
            List<Item> childDescriptors = search.findSortedChildren(pid);
            transformRelation2Kramerius(pid, editor, childDescriptors, includePids, hasParent);
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
            Collection<String> includePids, boolean hasParent
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

            setDonator(relations, doc, editor);

            editor.setDevice(null);
            editor.setExportResult(null);
            editor.setKrameriusExportResult(null);
            editor.setArchiveExportResult(null);
            editor.setNdkExportResult(null);
            editor.setCrossrefExportResult(null);
            editor.setStatus(null);
            editor.setEmptyUser();
            editor.setEmptyOrganization();
            editor.setOwners(Collections.<String>emptyList());
            editor.setMembership(Collections.<String>emptyList());

            String modelId = editor.getModel();
            String k4ModelId;
            if (hasParent && (NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(modelId) || OldPrintPlugin.MODEL_VOLUME.equals(modelId))) {
                k4ModelId = K4Plugin.MODEL_MONOGRAPHUNIT;
            } else {
                k4ModelId = kramerius4ExportOptions.getModelMap().get(modelId);
            }
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
                String krelation = kramerius4ExportOptions.getRelationMap().get(desc.getModel());
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
            if (NdkPlugin.MODEL_CHAPTER.equals(modelId) || NdkPlugin.MODEL_ARTICLE.equals(modelId)) {
                List<String> childrens = getChildren(pid);
                for (String childrensPid : childrens) {
                    String childKRelations = "isOnPage";
                    Element childElement = doc.createElementNS(KRAMERIUS_RELATION_NS, KRAMERIUS_RELATION_PREFIX + ":" + childKRelations);
                    childElement.setAttributeNS(Relations.RDF_NS, "rdf:resource", RelationResource.fromPid(childrensPid).getResource());
                    relations.add(childElement);
                }
            }
            editor.setRelations(relations);
            editor.write(editor.getLastModified(), null);
        } catch (ParserConfigurationException ex) {
            throw new IllegalStateException(ex);
        } catch (MetsExportException e) {
            throw new IllegalStateException(e);
        } catch (IOException e) {
            throw new IllegalStateException(e);
        } catch (FedoraClientException e) {
            throw new IllegalStateException(e);
        }
    }

    private void setDonator(List<Element> relations, Document doc, RelationEditor editor) throws DigitalObjectException {
        for (Element relation : relations) {
            if ("hasDonator".equals(relation.getTagName())) {
                return ;
            }
        }
        if (editor.getDonator() != null) {
            Element hasDonator = doc.createElementNS(KRAMERIUS_RELATION_NS, KRAMERIUS_RELATION_PREFIX + ":hasDonator");
            hasDonator.setAttributeNS(Relations.RDF_NS, "rdf:resource", RelationResource.fromPid(editor.getDonator()).getResource());
            relations.add(hasDonator);
            editor.setEmptyDonator();
        }
    }

    private List<String> getChildren(String pid) throws DigitalObjectException, IOException, MetsExportException, FedoraClientException {
        ModsDefinition mods = getMods(pid);
        int pageIndexStart = Integer.MIN_VALUE + 1;
        int pageIndexEnd = Integer.MAX_VALUE - 1;
        int pageNumberStart = Integer.MIN_VALUE + 1;
        int pageNumberEnd = Integer.MAX_VALUE - 1;
        for (PartDefinition part : mods.getPart()) {
            for (ExtentDefinition extent : part.getExtent()) {
                if ("pageIndex".equals(part.getType())) {
                    if (pageIndexStart == Integer.MIN_VALUE + 1) {
                        if (extent.getStart() != null) {
                            pageIndexStart = Integer.parseInt(extent.getStart().getValue());
                        }
                    }
                    if (pageIndexEnd == Integer.MAX_VALUE - 1) {
                        if (extent.getEnd() != null) {
                            pageIndexEnd = Integer.parseInt(extent.getEnd().getValue());
                        }
                    }
                }
                if ("pageNumber".equals(part.getType())) {
                    if (pageNumberStart == Integer.MIN_VALUE + 1) {
                        if (extent.getStart() != null) {
                            String value = extent.getStart().getValue();
                            value = value.replaceAll("\\D+","");
                            pageNumberStart = Integer.parseInt(value);
                        }
                    }
                    if (pageNumberEnd == Integer.MAX_VALUE - 1) {
                        if (extent.getEnd() != null) {
                            String value = extent.getEnd().getValue();
                            value = value.replaceAll("\\D+", "");
                            pageNumberEnd = Integer.parseInt(value);
                        }
                    }
                }
            }
        }
        if (pageIndexStart >= 0) {
            return getChildren("pageIndex", pageIndexStart, pageIndexEnd, getAllChildren(pid));
        } else if (pageNumberStart >= 0) {
            return getChildren("pageNumber", pageNumberStart, pageNumberEnd, getAllChildren(pid));
        }
        return new ArrayList<>();
    }

    private List<String> getChildren(String type, int start, int end, List<String> children) throws DigitalObjectException {
        List<String> childrensPid = new ArrayList<>();
        for (String pid : children) {
            ModsDefinition mods = getMods(pid);
            if ("pageIndex".equals(type)) {
                int index = getNumber(type, mods);
                if (start <= index && index <= end) {
                    childrensPid.add(pid);
                }
            }
            if ("pageNumber".equals(type)) {
                int number = Integer.MIN_VALUE;
                if (number == Integer.MIN_VALUE) {
                    number = getNumber("pageNumber", mods);
                }
                if (number == Integer.MIN_VALUE) {
                    number = getNumber("page number", mods);
                }
                if (start <= number && number <= end) {
                    childrensPid.add(pid);
                }
            }
        }
        return childrensPid;
    }

    private int getNumber(String type, ModsDefinition mods) {
        for (PartDefinition part : mods.getPart()) {
            for (DetailDefinition detail : part.getDetail()) {
                if (type.equals(detail.getType())) {
                    for (StringPlusLanguage number : detail.getNumber()) {
                        String value = number.getValue();
                        value = value.replaceAll("\\D+", "");
                        return Integer.parseInt(value);
                    }
                }
            }
        }
        return Integer.MIN_VALUE;
    }

    private List<String> getAllChildren(String pid) throws IOException, MetsExportException, FedoraClientException, DigitalObjectException {
        String parentPid = getParentPid(pid);
        List<Item> allChildrens = new ArrayList<>();
        if (parentPid != null && !parentPid.isEmpty()) {
            allChildrens = search.findSortedChildren(parentPid);
        }
        List<String> children = new ArrayList<>();
        for (Item child : allChildrens) {
            if (NdkPlugin.MODEL_PAGE.equals(child.getModel()) || NdkPlugin.MODEL_NDK_PAGE.equals(child.getModel())) {
                children.add(child.getPid());
            }
        }
        return children;
    }

    private String getParentPid(String pid) throws IOException, MetsExportException {
        RemoteStorage rstorage = RemoteStorage.getInstance(appConfig);
        RemoteStorage.RemoteObject robject = rstorage.find(pid);
        MetsContext metsContext = buildContext(robject, null, null, rstorage);
        return MetsUtils.getParent(pid, metsContext.getRemoteStorage());
    }

    private MetsContext buildContext(RemoteStorage.RemoteObject fo, String packageId, File targetFolder, RemoteStorage rstorage) {
        MetsContext mc = new MetsContext();
        mc.setFedoraClient(fo.getClient());
        mc.setRemoteStorage(rstorage);
        mc.setPackageID(packageId);
        mc.setOutputPath(null);
        mc.setAllowNonCompleteStreams(false);
        mc.setAllowMissingURNNBN(false);
        mc.setConfig(null);
        return mc;
    }

    private ModsDefinition getMods(String pid) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        FedoraObject fo = dom.find(pid, null);
        XmlStreamEditor streamEditorOld = fo.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditorOld = new ModsStreamEditor(streamEditorOld, fo);
        return modsStreamEditorOld.read();
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

    public static class Result {
        private File file;
        private Integer pageCount;
        private MetsExportException validationError;
        private Exception ex;

        public File getFile() {
            return file;
        }

        public void setFile(File file) {
            this.file = file;
        }

        public Integer getPageCount() {
            return pageCount;
        }

        public void setPageCount(Integer pageCount) {
            this.pageCount = pageCount;
        }

        public MetsExportException getValidationError() {
            return validationError;
        }

        public void setValidationError(MetsExportException validationError) {
            this.validationError = validationError;
        }

        public Exception getException() {
            return ex;
        }

        public void setException(Exception ex) {
            this.ex = ex;
        }
    }

    private class Pair {
        private String pid;
        private String parentPid;

        public Pair(String pid, String parentPid) {
            this.pid = pid;
            this.parentPid = parentPid;
        }

        public String getPid() {
            return pid;
        }

        public String getParentPid() {
            return parentPid;
        }
    }

    private class K4Information {
        private String pid;
        private String model;
        private Integer index;
        private String parentPid;

        public K4Information(String pid, String model, Integer index, String parentPid) {
            this.pid = pid;
            this.model = model;
            this.index = index;
            this.parentPid = parentPid;
        }

        public String getPid() {
            return pid;
        }

        public String getModel() {
            return model;
        }

        public Integer getIndex() {
            return index;
        }

        public String getParentPid() {
            return parentPid;
        }
    }

    private class K4Informations {
        public List<K4Information> list = new ArrayList<>();

        public void add(String pid, String model, Integer pageIndex, String parentPid) {
            list.add(new K4Information(pid, model, null, parentPid));
        }

        public Integer getExpectedPageIndex(String parentPid) {
            if (parentPid == null || parentPid.isEmpty()) {
                return 1;
            } else {
                int count = 0;
                for (K4Information information : list) {
                    String model = information.getModel();
                    if (parentPid.equals(information.getParentPid()) && (NdkPlugin.MODEL_PAGE.equals(model) ||
                            NdkPlugin.MODEL_NDK_PAGE.equals(model) || OldPrintPlugin.MODEL_PAGE.equals(model) || NdkAudioPlugin.MODEL_PAGE.equals(model))) {
                        count++;
                    }
                }
                return count + 1;
            }
        }
    }
}
