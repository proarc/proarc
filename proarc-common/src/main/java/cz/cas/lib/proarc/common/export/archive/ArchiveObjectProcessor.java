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
package cz.cas.lib.proarc.common.export.archive;

import com.yourmediashelf.fedora.generated.foxml.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.device.DeviceRepository;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.export.archive.PackageBuilder.MdType;
import cz.cas.lib.proarc.common.export.mets.Const;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.ReadonlyDisseminationHandler;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.mods.IdentifierDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import org.w3c.dom.Element;

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
    private final HashSet<String> devicePids = new HashSet<String>();

    public static final Set<String> ARCHIVE_VALIDATION_MODELS = Collections.unmodifiableSet(new HashSet<>(
            Arrays.asList(NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, NdkPlugin.MODEL_MONOGRAPHVOLUME,
                    NdkPlugin.MODEL_PERIODICALSUPPLEMENT, NdkPlugin.MODEL_PERIODICALISSUE,
                    OldPrintPlugin.MODEL_VOLUME, OldPrintPlugin.MODEL_SUPPLEMENT)));

    public ArchiveObjectProcessor(DigitalObjectCrawler crawler, File targetFolder) {
        this.crawler = crawler;
        this.targetFolder = targetFolder;
    }

    /**
     * Processes a path of digital objects to build a package.
     * @param objectPath a leaf to root list of objects.
     * @throws DigitalObjectException a failure
     */
    public void process(List<DigitalObjectElement> objectPath) throws DigitalObjectException, MetsExportException {
        builder = new PackageBuilder(targetFolder);
        DigitalObjectElement entry = objectPath.get(0);
        DigitalObjectHandler handler = entry.getHandler();
        LocalObject lobj = getLocalObject(handler.getFedoraObject());
        builder.prepare(objectPath, lobj);
        processParents(objectPath);
        processObject(1, objectPath, lobj);
        builder.build();
    }

    private void processParents(List<DigitalObjectElement> objectPath) throws DigitalObjectException, MetsExportException {
        for (int i = objectPath.size() - 1; i >= 1 ; i--) {
            DigitalObjectElement elm = objectPath.get(i);
            LocalObject elmCache = getLocalObject(elm.getHandler().getFedoraObject());
            processDatastreams(1, objectPath.subList(i, objectPath.size()), elmCache, new RelationEditor(elmCache));
        }
    }

    private void processObject(int siblingIdx, List<DigitalObjectElement> objectPath, LocalObject cache) throws DigitalObjectException, MetsExportException {
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
    ) throws DigitalObjectException, MetsExportException {
        DigitalObjectElement elm = objectPath.get(0);
        DigitalObjectElement parentElm = objectPath.size() <= 1 ? null : objectPath.get(1);
        DigitalObjectHandler handler = elm.getHandler();
        builder.addObject(siblingIdx, elm, parentElm);
        for (DatastreamType dt : cache.getDigitalObject().getDatastream()) {
            String dsId = dt.getID();
            if (ModsStreamEditor.DATASTREAM_ID.equals(dsId)) {
                // XXX might not be mods! It should rather go to fileGrp.
                checkUrnNbn(cache);
                builder.addStreamAsMdSec(siblingIdx, dt, cache.getPid(), elm.getModelId(), MdType.MODS);
            } else if (DcStreamEditor.DATASTREAM_ID.equals(dsId)) {
                Element dcElm = dt.getDatastreamVersion().get(0).getXmlContent().getAny().get(0);
                FoxmlUtils.fixFoxmlDc(dcElm);
                builder.addStreamAsMdSec(siblingIdx, dt, cache.getPid(), elm.getModelId(), MdType.DC);
            } else if (RelationEditor.DATASTREAM_ID.equals(dsId)) {
                processDevice(relsEditor.getDevice(), cache.getPid());
                builder.addStreamAsFile(siblingIdx, dt, cache.getPid(), elm.getModelId(), null);
            } else if (FoxmlUtils.DS_AUDIT_ID.equals(dsId)) {
                builder.addStreamAsFile(siblingIdx, dt, cache.getPid(), elm.getModelId(), null);
            } else {
                builder.addStreamAsFile(siblingIdx, dt, cache.getPid(), elm.getModelId(), handler.dissemination(dsId));
            }
        }
        builder.addFoxmlAsFile(siblingIdx, elm, cache);
    }

    private void checkUrnNbn(LocalObject cache) throws MetsExportException, DigitalObjectException {
        String pid = cache.getPid();
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        FedoraObject foNew = dom.find(pid, null);
        XmlStreamEditor streamEditorNew = foNew.getEditor(FoxmlUtils.inlineProfile(
                MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditorNew = new ModsStreamEditor(streamEditorNew, foNew);
        ModsDefinition mods = modsStreamEditorNew.read();
        RelationEditor relationEditor = new RelationEditor(foNew);
        String model = relationEditor.getModel();
        if (ARCHIVE_VALIDATION_MODELS.contains(model) && !containUrnNbn(mods.getIdentifier())) {
            throw new MetsExportException(pid, "URNNBN identifier is missing", true, null);
        }
    }

    private boolean containUrnNbn(List<IdentifierDefinition> identifiers) {
        for (IdentifierDefinition identifier : identifiers) {
            if (Const.URNNBN.equals(identifier.getType())) {
                return true;
            }
        }
        return false;
    }

    private void processDevice(String devicePid, String objPid) throws DigitalObjectException {
        if (devicePid == null) {
            return ;
        }
        boolean contains = devicePids.contains(devicePid);
        if (!contains) {
            devicePids.add(devicePid);
            RemoteStorage remoteStorage = RemoteStorage.getInstance();
            RemoteObject ro = remoteStorage.find(devicePid);
            LocalObject cache = getLocalObject(ro);
            builder.addDevice(cache);
            final int deviceIdx = devicePids.size();
            final String modelId = DeviceRepository.METAMODEL_ID;
            builder.addFoxmlAsFile(deviceIdx, modelId, cache);
            DigitalObject dobj = cache.getDigitalObject();
            // write dc
            DatastreamType dcDs = FoxmlUtils.findDatastream(dobj, DcStreamEditor.DATASTREAM_ID);
            Element dcElm = dcDs.getDatastreamVersion().get(0).getXmlContent().getAny().get(0);
            FoxmlUtils.fixFoxmlDc(dcElm);
            builder.addStreamAsMdSec(deviceIdx, dcDs, devicePid, modelId, MdType.DC);
            // write description
            builder.addStreamAsFile(deviceIdx,
                    FoxmlUtils.findDatastream(dobj, DeviceRepository.DESCRIPTION_DS_ID),
                    devicePid, modelId, new ReadonlyDisseminationHandler(ro, DeviceRepository.DESCRIPTION_DS_ID));
            // write audit
            builder.addStreamAsFile(deviceIdx,
                    FoxmlUtils.findDatastream(dobj, FoxmlUtils.DS_AUDIT_ID),
                    devicePid, modelId, null);
            // write rels-ext
            builder.addStreamAsFile(deviceIdx,
                    FoxmlUtils.findDatastream(dobj, RelationEditor.DATASTREAM_ID),
                    devicePid, modelId, null);
        }
    }

    private void processChildren(
            List<DigitalObjectElement> objectPath,
            List<DigitalObjectElement> children
    ) throws DigitalObjectException, MetsExportException {
        int i = 1;
        for (DigitalObjectElement child : children) {
            LocalObject lObj = getLocalObject(child.getHandler().getFedoraObject());
            ArrayList<DigitalObjectElement> childPath = new ArrayList<DigitalObjectElement>(objectPath.size() + 1);
            childPath.add(child);
            childPath.addAll(objectPath);
            processObject(i++, childPath, lObj);
        }
    }

    private LocalObject getLocalObject(FedoraObject fo) throws DigitalObjectException {
        // get FOXML copy and query it locally
        if (fo instanceof LocalObject) {
            return (LocalObject) fo;
        }
        String foxml = fo.asText();
        DigitalObject dobj = FoxmlUtils.unmarshal(foxml, DigitalObject.class);
        return ls.create(dobj);
    }

}
