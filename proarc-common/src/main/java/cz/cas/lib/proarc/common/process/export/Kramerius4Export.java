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
package cz.cas.lib.proarc.common.process.export;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.foxml.PropertyType;
import com.yourmediashelf.fedora.generated.foxml.XmlContentType;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.kramerius.KImporter;
import cz.cas.lib.proarc.common.kramerius.KUtils;
import cz.cas.lib.proarc.common.kramerius.KrameriusOptions;
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
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.StringEditor;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage.AkubraObject;
import cz.cas.lib.proarc.common.storage.akubra.AkubraUtils;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage.RemoteObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.storage.relation.RelationResource;
import cz.cas.lib.proarc.common.storage.relation.Relations;
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
import javax.xml.bind.JAXBException;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_FAILED_V5;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_FAILED_V7;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_FINISHED_V5;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_FINISHED_V7;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_KILLED_V7;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_NO_BATCH_V5;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_FAILED;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_FINISHED;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_WARNING;
import static cz.cas.lib.proarc.common.kramerius.KrameriusOptions.KRAMERIUS_INSTANCE_LOCAL;
import static cz.cas.lib.proarc.common.kramerius.KrameriusOptions.findKrameriusInstance;
import static cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin.isNdkAudioModel;

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

    private FedoraStorage rstorage;
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
    private AkubraConfiguration akubraConfiguration;
    private ExportParams exportParams;
    private boolean isArchive;

    private final String policy;
    private final String license;

    private String exportPageContext;

    public Kramerius4Export(FedoraStorage rstorage, AppConfiguration appConfiguration, AkubraConfiguration akubraConfiguration) {
        this.appConfig = appConfiguration;
        this.rstorage = rstorage;
        this.kramerius4ExportOptions = appConfiguration.getKramerius4Export();
        this.exportParams = appConfiguration.getExportParams();
        this.search = rstorage.getSearch();
        this.crawler = new DigitalObjectCrawler(DigitalObjectManager.getDefault(), search);
        this.license = null;
        if (Arrays.asList(ALLOWED_POLICY).contains(appConfiguration.getKramerius4Export().getPolicy())) {
            this.policy = appConfiguration.getKramerius4Export().getPolicy();
        } else {
            this.policy = kramerius4ExportOptions.getPolicy();
        }
    }

    public Kramerius4Export(AppConfiguration appConfiguration, AkubraConfiguration akubraConfiguration, String policy, String license, boolean isArchive) throws IOException {
        this.appConfig = appConfiguration;
        this.akubraConfiguration = akubraConfiguration;
        this.kramerius4ExportOptions = appConfiguration.getKramerius4Export();
        this.exportParams = appConfiguration.getExportParams();
        this.isArchive = isArchive;

        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            this.rstorage = FedoraStorage.getInstance(this.appConfig);
            this.search = this.rstorage.getSearch();
        } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            this.search = AkubraStorage.getInstance(akubraConfiguration).getSearch();
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }
        this.crawler = new DigitalObjectCrawler(DigitalObjectManager.getDefault(), search);

        if (license != null && !license.isEmpty()) {
            this.license = license;
            this.policy = "policy:private";
        } else {
            this.license = null;
            if (Arrays.asList(ALLOWED_POLICY).contains(policy)) {
                this.policy = policy;
            } else {
                this.policy = kramerius4ExportOptions.getPolicy();
            }
        }
    }

    public Result export(File output, boolean hierarchy, String log, String krameriusInstanceId, Batch batch, String... pids) {
        if (!output.exists() || !output.isDirectory()) {
            throw new IllegalStateException(String.valueOf(output));
        }
        if (pids == null || pids.length == 0) {
            throw new IllegalArgumentException();
        }

        ExportResultLog reslog = new ExportResultLog();
        ExportResultLog.ExportResult result = new ExportResultLog.ExportResult();
        Result krameriusResult = new Result();
        result.setInputPid(pids[0]);
        reslog.getExports().add(result);

        File target = ExportUtils.createFolder(output, "k4_" + FoxmlUtils.pidAsUuid(pids[0]), exportParams.isOverwritePackage());
        if (batch != null) {
            BatchUtils.updateExportingBatch(BatchManager.getInstance(), batch, target);
        }
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
            result.setStatus(ExportResultLog.ResultStatus.OK);
            result.setEnd();
            ExportUtils.writeExportResult(target, reslog);
            krameriusResult.setPageCount(exportedPids.size());

            if (!(krameriusInstanceId == null || krameriusInstanceId.isEmpty() || KRAMERIUS_INSTANCE_LOCAL.equals(krameriusInstanceId))) {
                KrameriusOptions.KrameriusInstance instance = findKrameriusInstance(appConfig.getKrameriusOptions().getKrameriusInstances(), krameriusInstanceId);
                KImporter kImporter = new KImporter(appConfig, instance);
                KUtils.ImportState state = kImporter.importToKramerius(krameriusResult.getFile(), false, KUtils.EXPORT_KRAMERIUS, policy);
                if (KRAMERIUS_PROCESS_FINISHED.equals(state.getProcessState()) && (KRAMERIUS_BATCH_FINISHED_V5.equals(state.getBatchState()) || KRAMERIUS_BATCH_FINISHED_V7.equals(state.getBatchState()))) {
                    if (instance.deleteAfterImport()) {
                        MetsUtils.deleteFolder(krameriusResult.getFile());
                    }
                }
                switch (state.getBatchState()) {
                    case KRAMERIUS_BATCH_FINISHED_V5:
                    case KRAMERIUS_BATCH_FINISHED_V7:
                        krameriusResult.setMessage("Import do Krameria (" + instance.getId() + " --> " + instance.getUrl() + ") prošel bez chyby.");
                        krameriusResult.setKrameriusImportState(KRAMERIUS_PROCESS_FINISHED);
                        break;
                    case KRAMERIUS_BATCH_FAILED_V5:
                    case KRAMERIUS_BATCH_FAILED_V7:
                    case KRAMERIUS_BATCH_KILLED_V7:
                        krameriusResult.setMessage("Import do Krameria (" + instance.getId() + " --> " + instance.getUrl() + ") selhal.");
                        krameriusResult.setKrameriusImportState(KRAMERIUS_PROCESS_FAILED);
                        break;
                    case KRAMERIUS_BATCH_NO_BATCH_V5:
                        switch (state.getProcessState()) {
                            case KRAMERIUS_PROCESS_FINISHED:
                                krameriusResult.setMessage("Import do Krameria (" + instance.getId() + " --> " + instance.getUrl() + ") prošel, ale nebyla spuštěna indexace.");
                                krameriusResult.setKrameriusImportState(KRAMERIUS_PROCESS_WARNING);
                                break;
                            case KRAMERIUS_PROCESS_FAILED:
                                krameriusResult.setMessage("Import do Krameria (" + instance.getId() + " --> " + instance.getUrl() + ") selhal.");
                                krameriusResult.setKrameriusImportState(KRAMERIUS_PROCESS_FAILED);
                                break;
                            case KRAMERIUS_PROCESS_WARNING:
                                krameriusResult.setMessage("Import do Krameria (" + instance.getId() + " --> " + instance.getUrl() + ") prošel s chybou.");
                                krameriusResult.setKrameriusImportState(KRAMERIUS_PROCESS_WARNING);
                                break;
                        }
                        break;
                    default:
                        krameriusResult.setKrameriusImportState(KRAMERIUS_PROCESS_FAILED);
                        krameriusResult.setMessage("Unknown status: " + state.getBatchState());
                        break;
                }
            }
        } catch (RuntimeException ex) {
            result.setStatus(ExportResultLog.ResultStatus.FAILED);
            reslog.getExports().add(result);
            result.getError().add(new ExportResultLog.ResultError(null, ex));
            result.setEnd();
            ExportUtils.writeExportResult(target, reslog);
            krameriusResult.setException(ex);
            //throw ex;
        } catch (DigitalObjectException ex) {
            result.setStatus(ExportResultLog.ResultStatus.FAILED);
            reslog.getExports().add(result);
            result.getError().add(new ExportResultLog.ResultError(null, ex));
            result.setEnd();
            ExportUtils.writeExportResult(target, reslog);
            krameriusResult.setException(ex);
            //throw new IllegalStateException(ex.getMessage());
        } catch (MetsExportException ex ) {
            result.setStatus(ExportResultLog.ResultStatus.FAILED);
            result.getError().add(new ExportResultLog.ResultError(null, ex));
            result.setEnd();
            ExportUtils.writeExportResult(target, reslog);
            krameriusResult.setValidationError(ex);
        } catch (Exception ex) {
            result.setStatus(ExportResultLog.ResultStatus.FAILED);
            result.getError().add(new ExportResultLog.ResultError(null, ex));
            result.setEnd();
            krameriusResult.setException(ex);
            ExportUtils.writeExportResult(target, reslog);
        } catch (Throwable ex) {
            result.setStatus(ExportResultLog.ResultStatus.FAILED);
            result.getError().add(new ExportResultLog.ResultError(null, ex));
            result.setEnd();
            krameriusResult.setException(new DigitalObjectException(null, ex.getMessage()));
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
        SearchViewItem parentItem = getParentItem(pid);
        if (parentItem == null || parentItem.getModel() == null) {
            return false;
        } else {
            return parentModels.contains(parentItem.getModel());
        }

//        IMetsElement element = getElement(pid);
//        if (element == null || element.getParent() == null || element.getParent().getModel() == null) {
//            return false;
//        } else {
//            return parentModels.contains(element.getParent().getModel().substring(12));
//        }
    }

    public IMetsElement getElement(String pid) throws DigitalObjectException {
        try {
            MetsContext metsContext = null;
            ProArcObject object = null;

            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                object = rstorage.find(pid);
                metsContext = MetsContext.buildFedoraContext(object, null, null, rstorage, appConfig.getNdkExportOptions());
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                object = akubraStorage.find(pid);
                metsContext = MetsContext.buildAkubraContext(object, null, null, akubraStorage, appConfig.getNdkExportOptions());
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
            DigitalObject dobj = MetsUtils.readFoXML(metsContext, object);
            if (dobj == null) {
                return null;
            }
            return MetsElement.getElement(dobj, null, metsContext, true);
        } catch (IOException | MetsExportException ex) {
            throw new DigitalObjectException(pid, "K4 export: imposible to find element.", ex);
        }
    }

    private SearchViewItem getParentItem(String pid) throws DigitalObjectException {
        try {
            List<SearchViewItem> parentsList;

            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                parentsList = this.search.findReferrers(pid);
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                parentsList = this.search.findReferrers(pid);
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
            if (parentsList.isEmpty()) {
                return null;
            } else {
                return parentsList.get(0);
            }
        } catch (IOException | FedoraClientException ex) {
            throw new DigitalObjectException(pid, "K4 export: imposible to find parent.", ex);
        }
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
                DigitalObject dobj = null;
                if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                    RemoteObject robject = rstorage.find(pid);
                    FedoraClient client = robject.getClient();
                    dobj = FedoraClient.export(pid).context("public")
                            .format("info:fedora/fedora-system:FOXML-1.1")
                            .execute(client).getEntity(DigitalObject.class);
                } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                    AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                    AkubraObject object = akubraStorage.find(pid);
                    dobj = AkubraUtils.getDigitalObjectToExport(object.getManager(), pid);
                } else {
                    throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
                }
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
                    if (!isArchive && kramerius4ExportOptions.getExcludeDatastreams().contains(datastream.getID())) {
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
            } catch (DigitalObjectException | FedoraClientException | JAXBException | IOException e) {
                throw new IllegalStateException(pid, e);
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
                DigitalObject dobj = null;
                if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                    RemoteObject robject = rstorage.find(pid);
                    FedoraClient client = robject.getClient();
                    dobj = FedoraClient.export(pid).context("public")
                            .format("info:fedora/fedora-system:FOXML-1.1")
                            .execute(client).getEntity(DigitalObject.class);
                } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                    AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                    AkubraObject object = akubraStorage.find(pid);
                    dobj = AkubraUtils.getDigitalObjectToExport(object.getManager(), pid);
                } else {
                    throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
                }
                for (Iterator<DatastreamType> it = dobj.getDatastream().iterator(); it.hasNext();) {
                    DatastreamType datastream = it.next();
                    if (!isArchive && kramerius4ExportOptions.getExcludeDatastreams().contains(datastream.getID())) {
                        it.remove();
                        continue;
                    }
                    if (hasParentFromDublincore(datastream, models)) {
                        return true;
                    }
                }
            } catch (FedoraClientException | JAXBException | IOException ex) {
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
                return;
            }
            exportedPids.add(object.getPid());
            DigitalObject dobj = null;
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                RemoteObject robject = rstorage.find(object.getPid());
                FedoraClient client = robject.getClient();
                dobj = FedoraClient.export(object.getPid()).context(exportPageContext == null ? "archive" : exportPageContext)
                        .format("info:fedora/fedora-system:FOXML-1.1")
                        .execute(client).getEntity(DigitalObject.class);
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                AkubraObject aobject = akubraStorage.find(object.getPid());
                dobj = AkubraUtils.getDigitalObjectToExport(aobject.getManager(), object.getPid());
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
            File foxml = ExportUtils.pidAsXmlFile(output, object.getPid());
            LocalObject local = lstorage.create(foxml, dobj);
            local = replaceObjectOwnerId(local);
            validate(object.getPid(), object.getParentPid());
            RelationEditor editor = new RelationEditor(local);
            if (NdkAudioPlugin.MODEL_PAGE.equals(editor.getModel())) { // ndk audio page se nexportuje ale jeho streamy se priradi k urovni vyse
                return;
            }
            if (hierarchy) {
                List<String> children = editor.getMembers();
                toExport.addAll(createPair(object.getPid(), children));
            }

            MissingObject missingObject = new MissingObject();
            if (containsNdkAudioPage(object.getPid())) {
                if (NdkAudioPlugin.MODEL_SONG.equals(editor.getModel())) {
                    String missingModelPid = createMissingModel(output, local);
                    missingObject.setNewObjectPid(missingModelPid);
                } else if (NdkAudioPlugin.MODEL_TRACK.equals(editor.getModel())) {
                    insertAudioStreams(local, getFirstNdkAudioPage(object.getPid()));
                } else {
                    throw new DigitalObjectException("Unexpected element ( " + NdkAudioPlugin.MODEL_PAGE + ") under " + editor.getModel() + ".");
                }
            }
            exportDatastreams(local, editor, hasParent, missingObject);
            local.flush();
        } catch (DigitalObjectException | FedoraClientException | JAXBException | IOException ex) {
            throw new IllegalStateException(object.getPid(), ex);
        }
    }

    private LocalObject replaceObjectOwnerId(LocalObject local) {
        if (kramerius4ExportOptions.getNewOwnerId() != null && !kramerius4ExportOptions.getNewOwnerId().isEmpty()) {
            if (local.getDigitalObject() != null) {
                for (PropertyType property : local.getDigitalObject().getObjectProperties().getProperty()) {
                    if (FoxmlUtils.PROPERTY_OWNER.equals(property.getNAME())) {
                        property.setVALUE(kramerius4ExportOptions.getNewOwnerId());
                        break;
                    }
                }
            }
        }
        return local;
    }

    private String createMissingModel(File outputFolder, LocalObject songObject) throws DigitalObjectException {
        String localObjectPid = FoxmlUtils.createPid();
        File foxml = ExportUtils.pidAsXmlFile(outputFolder, localObjectPid);
        LocalObject localObj = lstorage.create(localObjectPid, foxml);
        localObj.setOwner(songObject.getOwner());
        localObj.setLabel(songObject.getLabel());
        List<DatastreamType> selectedStreams = selectDatastreams(songObject.getDigitalObject().getDatastream(), ModsStreamEditor.DATASTREAM_ID, DcStreamEditor.DATASTREAM_ID, RelationEditor.DATASTREAM_ID);
        localObj.getDigitalObject().getDatastream().addAll(selectedStreams);

        insertAudioStreams(localObj, getFirstNdkAudioPage(songObject.getPid()));
        RelationEditor relationEditor = new RelationEditor(localObj);

        RelationEditor songRelationEditor = new RelationEditor(songObject);

        relationEditor.setAbout(localObjectPid);
        relationEditor.setMembers(songRelationEditor.getMembers());
        relationEditor.setModel(NdkAudioPlugin.MODEL_TRACK);
        relationEditor.write(relationEditor.getLastModified(), "Update");

        MissingObject missingObject = new MissingObject();
        missingObject.setParentPid(songObject.getPid());
        exportDatastreams(localObj, relationEditor, true, missingObject);
        localObj.flush();
        return localObj.getPid();
    }

    private void insertAudioStreams(LocalObject desctinationObject, SearchViewItem sourceItem) {
        String pid = sourceItem.getPid();
        DigitalObject dobj = null;
        try {
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                RemoteObject robject = rstorage.find(pid);
                FedoraClient client = robject.getClient();
                dobj = FedoraClient.export(pid).context(exportPageContext == null ? "archive" : exportPageContext)
                        .format("info:fedora/fedora-system:FOXML-1.1")
                        .execute(client).getEntity(DigitalObject.class);
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                AkubraObject aobject = akubraStorage.find(pid);
                dobj = AkubraUtils.getDigitalObjectToExport(aobject.getManager(), pid);
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
//            String foxml = object.asText();
//            dobj = FoxmlUtils.unmarshal(foxml, DigitalObject.class);
            List<DatastreamType> datastreamTypes = selectDatastreams(dobj.getDatastream(), BinaryEditor.NDK_AUDIO_ARCHIVAL_ID, BinaryEditor.NDK_AUDIO_USER_ID);
            if (datastreamTypes.size() > 0 ){
                desctinationObject.getDigitalObject().getDatastream().addAll(datastreamTypes);
            }
            return;
        } catch (FedoraClientException | JAXBException | IOException ex) {
            throw new IllegalStateException(pid, ex);
        }
    }

    private List<DatastreamType> selectDatastreams(List<DatastreamType> datastreams, String... streams) {
        List<String> acceptedStreams = filterStreams(streams);
        List<DatastreamType> selectedDatastreams = new ArrayList<>();
        for (DatastreamType datastream : datastreams) {
            if (acceptedStreams.contains(datastream.getID())) {
                selectedDatastreams.add(datastream);
            }
        }
        return selectedDatastreams;
    }

    private List<String> filterStreams(String[] streams) {
        List<String> acceptedStreams = new ArrayList<>();
        for (String stream : streams) {
            if (isArchive) {
                acceptedStreams.add(stream);
            } else {
                if (!kramerius4ExportOptions.getExcludeDatastreams().contains(stream)) {
                    acceptedStreams.add(stream);
                }
            }
        }
        return acceptedStreams;
    }

    private boolean containsNdkAudioPage(String pid) {
        SearchViewItem item = getFirstNdkAudioPage(pid);
        return item != null;
    }

    private SearchViewItem getFirstNdkAudioPage(String pid) {
        try {
            List<SearchViewItem> childDescriptors = search.findChildren(pid);
            for (SearchViewItem childDescriptor : childDescriptors) {
                if (NdkAudioPlugin.MODEL_PAGE.equals(childDescriptor.getModel())) {
                    return childDescriptor;
                }
            }
        } catch (Exception e) {
            return null;
        }
        return null;
    }

    private void validate(String pid, String parentPid) throws MetsExportException, DigitalObjectException {
        IMetsElement element = getElement(pid);
        if (element == null) {
            throw new MetsExportException(pid, "K4 export - nevytvoren element pro " + pid, false, null);
        } else {
            String model = ExportUtils.getModel(element.getModel());
            if (NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(model)) {
                if (element.getParent() != null && NdkPlugin.MODEL_MONOGRAPHTITLE.equals(ExportUtils.getModel(element.getParent().getModel()))) {
                    throw new MetsExportException("Nepovolená vazba - Ndk Svazek monografie pod Ndk Vícedílnou monografii.", false);
                }
            }
//            if (NdkPlugin.MODEL_PAGE.equals(model) || NdkPlugin.MODEL_NDK_PAGE.equals(model) || OldPrintPlugin.MODEL_PAGE.equals(model) || NdkAudioPlugin.MODEL_PAGE.equals(model)) {
            if (NdkPlugin.MODEL_PAGE.equals(model) || NdkPlugin.MODEL_NDK_PAGE.equals(model) || OldPrintPlugin.MODEL_PAGE.equals(model)) {
                ModsDefinition mods = getMods(element.getOriginalPid());
                int pageIndex = ExportUtils.getPageIndex(mods);
                Integer expectedPageIndex = informations.getExpectedPageIndex(parentPid);

                if (!ExportUtils.containPageNumber(mods)) {
                    throw new MetsExportException(pid, "Strana (" + element.getOriginalPid() + ") nemá vyplněné číslo stránky.", false, null);
                }
                if (NdkPlugin.MODEL_NDK_PAGE.equals(model) && !ExportUtils.containPageType(mods)) {
                    throw new MetsExportException(pid, "Strana (" + element.getOriginalPid() + ") nemá vyplněný typ stránky.", false, null);
                }
                if (expectedPageIndex != pageIndex) {
                    if (pageIndex == -1) {
                        throw new MetsExportException(pid, "Strana (" + element.getOriginalPid() + ") nemá vyplněný index strany. Očekávaná hodnota " + expectedPageIndex + ".", false, null);
                    } else {
                        throw new MetsExportException(pid, "Strana (" + element.getOriginalPid() + ") má neočekávaný index strany. Očekávaná hodnota " + expectedPageIndex + ", ale byl nalezen index "+ pageIndex + ".", false, null);
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
            DigitalObject dobj = null;
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                RemoteObject robject = rstorage.find(pid);
                FedoraClient client = robject.getClient();
                dobj = FedoraClient.export(pid).context("archive")
                        .format("info:fedora/fedora-system:FOXML-1.1")
                        .execute(client).getEntity(DigitalObject.class);
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                AkubraObject object = akubraStorage.find(pid);
                dobj = AkubraUtils.getDigitalObjectToExport(object.getManager(), pid);
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
            File foxml = ExportUtils.pidAsXmlFile(output, pid);
            LocalObject local = lstorage.create(foxml, dobj);
            exportParentDatastreams(local, includeChildPids, hasParent(pid));
            local.flush();
        } catch (DigitalObjectException | FedoraClientException | JAXBException | IOException ex) {
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

    private void exportDatastreams(LocalObject local, RelationEditor editor,  boolean hasParent, MissingObject missingObject) {
        DigitalObject dobj = local.getDigitalObject();
        // XXX replace DS only for other than image/* MIMEs?
        DatastreamType fullDs = FoxmlUtils.findDatastream(dobj, BinaryEditor.FULL_ID);
        DatastreamType rawDs = fullDs != null ? null : FoxmlUtils.findDatastream(dobj, BinaryEditor.RAW_ID);
        for (Iterator<DatastreamType> it = dobj.getDatastream().iterator(); it.hasNext();) {
            DatastreamType datastream = it.next();
            if (!isArchive && kramerius4ExportOptions.getExcludeDatastreams().contains(datastream.getID())) {
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
            processRelsExt(dobj.getPID(), datastream, editor, null, hasParent, missingObject);
        }
    }

    private void exportParentDatastreams(LocalObject local, Collection<String> includeChildPids, boolean hasParent) {
        DigitalObject dobj = local.getDigitalObject();
        RelationEditor editor = new RelationEditor(local);
        for (Iterator<DatastreamType> it = dobj.getDatastream().iterator(); it.hasNext();) {
            DatastreamType datastream = it.next();
            if (!isArchive && kramerius4ExportOptions.getExcludeDatastreams().contains(datastream.getID())) {
                it.remove();
                continue;
            }
            excludeVersions(datastream);
            renameDatastream(datastream);
            processDublinCore(datastream, hasParent);
            processMods(datastream);
            processRelsExt(dobj.getPID(), datastream, editor, includeChildPids, hasParent, null);
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
        if (license == null || license.isEmpty()) {
            if (policy != null) {
                Element elmRights = dcElm.getOwnerDocument().createElementNS(
                        DcConstants.NS_PURL, DcConstants.PREFIX_NS_PURL + ':' + DcConstants.RIGHTS);
                elmRights.setTextContent(policy);
                dcElm.appendChild(elmRights);
            }
        }
        // map proarc/K4 models
        NodeList typeNodes = dcElm.getElementsByTagNameNS(DcConstants.NS_PURL, DcConstants.TYPE);
        for (int i = 0; i < typeNodes.getLength(); i++) {
            Element typeElm = (Element) typeNodes.item(i);
            String type = typeElm.getTextContent();
            String k4ModelId;
            if (hasParent && (NdkPlugin.MODEL_MONOGRAPHUNIT.equals(type) || OldPrintPlugin.MODEL_VOLUME.equals(type) || K4Plugin.MODEL_MONOGRAPH.equals(type))) {
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
            boolean hasParent, MissingObject missingObject) {

        if (!RelationEditor.DATASTREAM_ID.equals(datastream.getID())) {
            return ;
        }
        try {
            transformRelation2Kramerius(pid, editor, includePids, hasParent, missingObject);
        } catch (DigitalObjectException ex) {
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

    private void transformRelation2Kramerius(String pid, RelationEditor editor, Collection<String> includePids, boolean hasParent, MissingObject missingObject
            ) throws DigitalObjectException {
        List<SearchViewItem> childDescriptors = null;
        List<String> children = null;

        try {
            if (missingObject != null && missingObject.getParentPid() != null) {
                childDescriptors = search.findSortedChildrenWithPagesFirst(missingObject.getParentPid());
                children = transformToMembers(childDescriptors);
            } else {
                if (isNdkAudioModel(editor.getModel())) {
                    childDescriptors = search.findSortedChildrenWithPagesFirst(pid);
                    children = transformToMembers(childDescriptors);
                    if (missingObject != null && missingObject.getNewObjectPid() != null) {
                        children.add(missingObject.getNewObjectPid());
                        SearchViewItem missingItem = new SearchViewItem();
                        missingItem.setModel(NdkAudioPlugin.MODEL_TRACK);
                        missingItem.setPid(missingObject.getNewObjectPid());
                        childDescriptors.add(missingItem);
                    }
                } else {
                    childDescriptors = search.findSortedChildren(pid);
                    children = editor.getMembers();
                }
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        } catch (FedoraClientException e) {
            throw new RuntimeException(e);
        }

        try {
            DocumentBuilderFactory dfactory = DocumentBuilderFactory.newInstance();
            dfactory.setNamespaceAware(true);
            Document doc = dfactory.newDocumentBuilder().newDocument();
            List<Element> relations = editor.getRelations();

            setOaiId(pid, relations, doc);

            setImportFile(editor, relations, doc);

            if (license != null && !license.isEmpty()) {
                setLicense(license, relations, doc);
            } else {
                setPolicy(policy, relations, doc);
            }

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
            if (hasParent && (OldPrintPlugin.MODEL_VOLUME.equals(modelId))) {
                k4ModelId = K4Plugin.MODEL_MONOGRAPHUNIT;
            } else {
                k4ModelId = kramerius4ExportOptions.getModelMap().get(modelId);
            }
            k4ModelId = k4ModelId == null ? modelId : k4ModelId;
            editor.setModel(k4ModelId);

            editor.setMembers(Collections.<String>emptyList());
            for (String childPid : children) {
                SearchViewItem desc = remove(childPid, childDescriptors);
                if (desc == null) {
                    throw new IllegalStateException("Child " + childPid + " of " + pid + " not found in resource index!");
                }
                if (includePids != null && !includePids.contains(childPid)) {
                    continue;
                }
                if (NdkAudioPlugin.MODEL_PAGE.equals(desc.getModel())) {
                    continue; // ndk audio page se nexportuje, jeho stremy se pouziji o uroven vyse, takze nesmi byt ani v relations
                } else {
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

    private List<String> transformToMembers(List<SearchViewItem> childDescriptors) {
        List<String> members = new ArrayList<>();
        for (SearchViewItem childDescriptor : childDescriptors) {
            members.add(childDescriptor.getPid());
        }
        return members;
    }

    private void setDonator(List<Element> relations, Document doc, RelationEditor editor) throws DigitalObjectException {
        for (Element relation : relations) {
            if ("hasDonator".equals(relation.getTagName())) {
                return ;
            }
        }
        if (editor.getDonator() != null) {
            Element hasDonator = doc.createElementNS(KRAMERIUS_RELATION_NS, KRAMERIUS_RELATION_PREFIX + ":hasDonator");
            hasDonator.setAttributeNS(Relations.RDF_NS, "rdf:resource", RelationResource.fromPid(editor.getElementDonator()).getResource());
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
                            if (value != null && !value.isEmpty()) {
                                pageNumberStart = Integer.parseInt(value);
                            }
                        }
                    }
                    if (pageNumberEnd == Integer.MAX_VALUE - 1) {
                        if (extent.getEnd() != null) {
                            String value = extent.getEnd().getValue();
                            value = value.replaceAll("\\D+", "");
                            if (value != null && !value.isEmpty()) {
                                pageNumberEnd = Integer.parseInt(value);
                            }
                        }
                    }
                }
            }
            if (pageIndexStart != (Integer.MIN_VALUE + 1) && pageIndexEnd != (Integer.MAX_VALUE - 1)) {
                break;
            } else if (pageNumberStart != (Integer.MIN_VALUE + 1) && pageNumberEnd != (Integer.MAX_VALUE - 1)) {
                break;
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
        List<SearchViewItem> allChildrens = new ArrayList<>();
        if (parentPid != null && !parentPid.isEmpty()) {
            allChildrens = search.findSortedChildren(parentPid);
        }
        List<String> children = new ArrayList<>();
        for (SearchViewItem child : allChildrens) {
            if (NdkPlugin.MODEL_PAGE.equals(child.getModel()) || NdkPlugin.MODEL_NDK_PAGE.equals(child.getModel())) {
                children.add(child.getPid());
            }
        }
        return children;
    }

    private String getParentPid(String pid) throws IOException, MetsExportException {
        MetsContext metsContext = null;
        ProArcObject object = null;
        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            FedoraStorage rstorage = FedoraStorage.getInstance(appConfig);
            object = rstorage.find(pid);
            metsContext = MetsContext.buildFedoraContext(object, null, null, rstorage, appConfig.getNdkExportOptions());
        } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
            object = akubraStorage.find(pid);
            metsContext = MetsContext.buildAkubraContext(object, null, null, akubraStorage, appConfig.getNdkExportOptions());
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }
        return MetsUtils.getParent(pid, metsContext);
    }

    private ModsDefinition getMods(String pid) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        ProArcObject fo = dom.find(pid, null);
        XmlStreamEditor streamEditorOld = fo.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditorOld = new ModsStreamEditor(streamEditorOld, fo);
        return modsStreamEditorOld.read();
    }

    private static SearchViewItem remove(String pid, List<SearchViewItem> childDescriptors) {
        for (Iterator<SearchViewItem> it = childDescriptors.iterator(); it.hasNext();) {
            SearchViewItem desc = it.next();
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

    private static void setLicense(String license, List<Element> relations, Document doc) {
        if (license != null) {
            Element elmLicense = doc.createElementNS(KRAMERIUS_RELATION_NS, "license:license");
            elmLicense.setTextContent(license);
            relations.add(elmLicense);
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
        private String message;
        private String krameriusImportState;

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

        public String getMessage() {
            return message;
        }

        public void setMessage(String message) {
            this.message = message;
        }

        public String getKrameriusImportState() {
            return krameriusImportState;
        }

        public void setKrameriusImportState(String krameriusImportState) {
            this.krameriusImportState = krameriusImportState;
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

    public class MissingObject {

        private String parentPid;
        private String newObjectPid;

        public String getParentPid() {
            return parentPid;
        }

        public void setParentPid(String parentPid) {
            this.parentPid = parentPid;
        }

        public String getNewObjectPid() {
            return newObjectPid;
        }

        public void setNewObjectPid(String newObjectPid) {
            this.newObjectPid = newObjectPid;
        }
    }
}
