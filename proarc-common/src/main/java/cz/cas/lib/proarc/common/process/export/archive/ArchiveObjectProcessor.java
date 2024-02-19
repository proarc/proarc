/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.process.export.archive;

import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.device.DeviceRepository;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.LocalStorage;
import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.storage.MixEditor;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.StringEditor;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.K4Plugin;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.ReadonlyDisseminationHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.DcConstants;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * Processes a path of digital objects to build a package.
 *
 * @author Jan Pokorsky
 */
public class ArchiveObjectProcessor {

    private final DigitalObjectCrawler crawler;
    private final LocalStorage ls = new LocalStorage();
    private PackageBuilder builder;
    private final File targetFolder;

    public HashSet<String> getDevicePids() {
        return devicePids;
    }

    private final HashSet<String> devicePids = new HashSet<String>();
    private AppConfiguration appConfig;
    private AkubraConfiguration akubraConfiguration;
    private boolean ignoreMissingUrnNbn = false;

    public static final Set<String> ARCHIVE_VALIDATION_MODELS = Collections.unmodifiableSet(new HashSet<>(
            Arrays.asList(NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, NdkPlugin.MODEL_MONOGRAPHVOLUME, NdkPlugin.MODEL_MONOGRAPHUNIT,
                    NdkPlugin.MODEL_PERIODICALSUPPLEMENT, NdkPlugin.MODEL_PERIODICALISSUE, OldPrintPlugin.MODEL_VOLUME)));

    public ArchiveObjectProcessor(DigitalObjectCrawler crawler, File targetFolder, AppConfiguration appConfiguration, AkubraConfiguration akubraConfiguration,  boolean ignoreMissingUrnNbn) {
        this.crawler = crawler;
        this.targetFolder = targetFolder;
        this.appConfig = appConfiguration;
        this.akubraConfiguration = akubraConfiguration;
        this.ignoreMissingUrnNbn = ignoreMissingUrnNbn;
    }

    /**
     * Processes a path of digital objects to build a package.
     * @param objectPath a leaf to root list of objects.
     * @throws DigitalObjectException a failure
     */
    public void process(List<DigitalObjectElement> objectPath) throws DigitalObjectException, MetsExportException, IOException {
        builder = new PackageBuilder(targetFolder);
        DigitalObjectElement entry = objectPath.get(0);
        DigitalObjectHandler handler = entry.getHandler();
        LocalObject lobj = getLocalObject(handler.getFedoraObject());
        builder.prepare(objectPath, lobj, appConfig, getElement(objectPath.get(0)));
        processParents(objectPath);
        processObject(1, objectPath, lobj);
        builder.build();
    }

    private MetsElement getElement(DigitalObjectElement digitalObjectElement) throws DigitalObjectException {
        if (digitalObjectElement == null) {
            return null;
        } else {
            try {
                MetsContext metsContext = null;
                ProArcObject object = null;
                if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                    FedoraStorage rstorage = FedoraStorage.getInstance(appConfig);
                    object = rstorage.find(digitalObjectElement.getPid());
                    metsContext = MetsContext.buildFedoraContext(object, null, null, rstorage, appConfig.getNdkExportOptions());
                } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                    AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                    object = akubraStorage.find(digitalObjectElement.getPid());
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
                throw new DigitalObjectException("Process: Archive export failed - imposible to find element.", ex);
            }
        }
    }



    private void processParents(List<DigitalObjectElement> objectPath) throws DigitalObjectException, MetsExportException, IOException {
        for (int i = objectPath.size() - 1; i >= 1 ; i--) {
            DigitalObjectElement elm = objectPath.get(i);
            LocalObject elmCache = getLocalObject(elm.getHandler().getFedoraObject());
            processDatastreams(1, objectPath.subList(i, objectPath.size()), elmCache, new RelationEditor(elmCache));
        }
    }

    private void processObject(int siblingIdx, List<DigitalObjectElement> objectPath, LocalObject cache) throws DigitalObjectException, MetsExportException, IOException {
        DigitalObjectElement entry = objectPath.get(0);
        RelationEditor relsEditor = new RelationEditor(cache);

        processDatastreams(siblingIdx, objectPath, cache, relsEditor);

        List<String> members = relsEditor.getMembers();
        if (!members.isEmpty()) {
            // read children with single query
            List<DigitalObjectElement> children = crawler.getChildren(entry.getPid());
            processChildren(objectPath, children);
        }
    }

    private void processDatastreams(
            int siblingIdx, List<DigitalObjectElement> objectPath, LocalObject cache,
            RelationEditor relsEditor
    ) throws DigitalObjectException, MetsExportException, IOException {
        DigitalObjectElement elm = objectPath.get(0);
        DigitalObjectElement parentElm = objectPath.size() <= 1 ? null : objectPath.get(1);
        DigitalObjectHandler handler = elm.getHandler();
        builder.addObject(siblingIdx, elm, parentElm);
        for (DatastreamType dt : cache.getDigitalObject().getDatastream()) {
            String dsId = dt.getID();
            if (ModsStreamEditor.DATASTREAM_ID.equals(dsId)) {
                // XXX might not be mods! It should rather go to fileGrp.

                if (!(parentElm != null &&
                        ((NdkPlugin.MODEL_PERIODICALISSUE.equals(parentElm.getModelId()) && NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(elm.getModelId()))
                        || (NdkPlugin.MODEL_PERIODICALVOLUME.equals(parentElm.getModelId()) && NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(elm.getModelId()))
                        || (NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(parentElm.getModelId()) && NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT.equals(elm.getModelId()))
                        || (NdkPlugin.MODEL_MONOGRAPHUNIT.equals(parentElm.getModelId()) && NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT.equals(elm.getModelId()))))
                        || (OldPrintPlugin.MODEL_VOLUME.equals(elm.getModelId()))) {
                    checkUrnNbn(cache);
                }

                builder.addStreamAsMdSec(siblingIdx, dt, cache.getPid(), elm.getModelId(), PackageBuilder.MdType.MODS);
            } else if (DcStreamEditor.DATASTREAM_ID.equals(dsId)) {
                Element dcElm = dt.getDatastreamVersion().get(0).getXmlContent().getAny().get(0);
                FoxmlUtils.fixFoxmlDc(dcElm);
                NodeList typeNodes = dcElm.getElementsByTagNameNS(DcConstants.NS_PURL, DcConstants.TYPE);
                for (int i = 0; i < typeNodes.getLength(); i++) {
                    Element typeElm = (Element) typeNodes.item(i);
                    String type = typeElm.getTextContent();
                    if (parentElm != null && NdkPlugin.MODEL_MONOGRAPHTITLE.equals(parentElm.getModelId()) && (NdkPlugin.MODEL_MONOGRAPHUNIT.equals(type) || OldPrintPlugin.MODEL_VOLUME.equals(type) || K4Plugin.MODEL_MONOGRAPH.equals(type))) {
                        typeElm.setTextContent(K4Plugin.MODEL_MONOGRAPHUNIT);
                    }
                }
                builder.addStreamAsMdSec(siblingIdx, dt, cache.getPid(), elm.getModelId(), PackageBuilder.MdType.DC);
            } else if (RelationEditor.DATASTREAM_ID.equals(dsId)) {
                processDevice(relsEditor.getDevice(), cache.getPid());
                builder.addStreamAsFile(siblingIdx, dt, cache.getPid(), elm.getModelId(), null);
            } else if (FoxmlUtils.DS_AUDIT_ID.equals(dsId)) {
                builder.addStreamAsFile(siblingIdx, dt, cache.getPid(), elm.getModelId(), null);
            } else if (AltoDatastream.ALTO_ID.equals(dsId) || BinaryEditor.NDK_ARCHIVAL_ID.equals(dsId)
                    || MixEditor.NDK_ARCHIVAL_ID.equals(dsId) || BinaryEditor.NDK_USER_ID.equals(dsId)
                    || StringEditor.OCR_ID.equals(dsId)){
                //DO NOTHING - contains NDK folder
            } else {
                builder.addStreamAsFile(siblingIdx, dt, cache.getPid(), elm.getModelId(), handler.dissemination(dsId));
            }
        }
        builder.addFoxmlAsFile(siblingIdx, elm, cache);
    }

    private void checkUrnNbn(LocalObject cache) throws MetsExportException, DigitalObjectException {
        String pid = cache.getPid();
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        ProArcObject foNew = dom.find(pid, null);
        XmlStreamEditor streamEditorNew = foNew.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditorNew = new ModsStreamEditor(streamEditorNew, foNew);
        ModsDefinition mods = modsStreamEditorNew.read();
        RelationEditor relationEditor = new RelationEditor(foNew);
        String model = relationEditor.getModel();
        if (ARCHIVE_VALIDATION_MODELS.contains(model) && !containUrnNbn(mods.getIdentifier())) {
            if (!(isOldPrintPlugin(model) && ignoreMissingUrnNbn)) {
                throw new MetsExportException(pid, "URNNBN identifier is missing", true, null);
            }
        }
    }

    private static boolean isOldPrintPlugin(String element) {
        return element != null && element.contains("oldprint");
    }

    private boolean containUrnNbn(List<IdentifierDefinition> identifiers) {
        for (IdentifierDefinition identifier : identifiers) {
            if (Const.URNNBN.equals(identifier.getType())) {
                if (identifier.getInvalid() == null || "false".equals(identifier.getInvalid())) {
                    return true;
                }
            }
        }
        return false;
    }

    private void processDevice(String devicePid, String objPid) throws DigitalObjectException, IOException {
        if (devicePid == null) {
            return ;
        }
        boolean contains = devicePids.contains(devicePid);
        if (!contains) {
            devicePids.add(devicePid);
            ProArcObject object = null;
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                FedoraStorage rstorage = FedoraStorage.getInstance(appConfig);
                object = rstorage.find(devicePid);
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                object = akubraStorage.find(devicePid);
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
            LocalObject cache = getLocalObject(object);
            builder.addDevice(cache);
            final int deviceIdx = devicePids.size();
            final String modelId = DeviceRepository.METAMODEL_ID;
            builder.addFoxmlAsFile(deviceIdx, modelId, cache);
            DigitalObject dobj = cache.getDigitalObject();
            // write dc
            DatastreamType dcDs = FoxmlUtils.findDatastream(dobj, DcStreamEditor.DATASTREAM_ID);
            Element dcElm = dcDs.getDatastreamVersion().get(0).getXmlContent().getAny().get(0);
            FoxmlUtils.fixFoxmlDc(dcElm);
            builder.addStreamAsMdSec(deviceIdx, dcDs, devicePid, modelId, PackageBuilder.MdType.DC);
            // write description
            builder.addStreamAsFile(deviceIdx,
                    FoxmlUtils.findDatastream(dobj, DeviceRepository.DESCRIPTION_DS_ID),
                    devicePid, modelId, new ReadonlyDisseminationHandler(object, DeviceRepository.DESCRIPTION_DS_ID));
            // write audit
            if (object instanceof FedoraStorage.RemoteObject) {
                builder.addStreamAsFile(deviceIdx,
                        FoxmlUtils.findDatastream(dobj, FoxmlUtils.DS_AUDIT_ID),
                        devicePid, modelId, null);
            }
            // write rels-ext
            builder.addStreamAsFile(deviceIdx,
                    FoxmlUtils.findDatastream(dobj, RelationEditor.DATASTREAM_ID),
                    devicePid, modelId, null);
        }
    }

    private void processChildren(
            List<DigitalObjectElement> objectPath,
            List<DigitalObjectElement> children
    ) throws DigitalObjectException, MetsExportException, IOException {
        int i = 1;
        for (DigitalObjectElement child : children) {
            LocalObject lObj = getLocalObject(child.getHandler().getFedoraObject());
            ArrayList<DigitalObjectElement> childPath = new ArrayList<DigitalObjectElement>(objectPath.size() + 1);
            childPath.add(child);
            childPath.addAll(objectPath);
            processObject(i++, childPath, lObj);
        }
    }

    private LocalObject getLocalObject(ProArcObject fo) throws DigitalObjectException {
        // get FOXML copy and query it locally
        if (fo instanceof LocalObject) {
            return (LocalObject) fo;
        }
        String foxml = fo.asText();
        DigitalObject dobj = FoxmlUtils.unmarshal(foxml, DigitalObject.class);
        return ls.create(dobj);
    }

}
