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
import com.yourmediashelf.fedora.client.response.FedoraResponse;
import com.yourmediashelf.fedora.client.response.ListDatastreamsResponse;
import com.yourmediashelf.fedora.generated.access.DatastreamType;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.process.export.mets.MimeType;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElement;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage.RemoteObject;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage.AkubraObject;
import cz.cas.lib.proarc.common.storage.akubra.AkubraUtils;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.JAXBException;
import javax.xml.transform.TransformerException;

/**
 * Exports particular data streams of queried digital objects.
 * It can traverse hierarchy of digital objects.
 *
 * @author Jan Pokorsky
 */
public final class DataStreamExport {

    private static final Logger LOG = Logger.getLogger(DataStreamExport.class.getName());

    private FedoraStorage rstorage;
    /**
     * already exported PIDs to prevent loops
     */
    private HashSet<String> exportedPids = new HashSet<String>();
    /**
     * PIDs scheduled for export
     */
    private Queue<String> toExport = new LinkedList<String>();
    private byte[] buffer = new byte[10 * 1024];
    private final AppConfiguration appConfig;
    private final AkubraConfiguration akubraConfiguration;



    public DataStreamExport(AppConfiguration configuration, AkubraConfiguration akubraConfiguration) throws IOException {
        this.appConfig = configuration;
        this.akubraConfiguration = akubraConfiguration;
        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            this.rstorage = FedoraStorage.getInstance(appConfig);
        }
    }

    public DataStreamExport(FedoraStorage fedoraStorage, AppConfiguration appConfig, AkubraConfiguration akubraConfiguration) {
        this.rstorage = fedoraStorage;
        this.appConfig = appConfig;
        this.akubraConfiguration = akubraConfiguration;
    }

    public File export(File output, boolean hierarchy, List<String> pids, List<String> dsIds) throws ExportException {
        if (!output.exists() || !output.isDirectory()) {
            throw new IllegalStateException(String.valueOf(output));
        }
        if (pids == null || pids.isEmpty() || dsIds == null || dsIds.isEmpty()) {
            throw new IllegalArgumentException();
        }

        File target = ExportUtils.createFolder(output, (dsIds.get(0) + "_" + FoxmlUtils.pidAsUuid(pids.get(0))).toLowerCase(), appConfig.getExportParams().isOverwritePackage());
        toExport.addAll(pids);
        for (String pid = toExport.poll(); pid != null; pid = toExport.poll()) {
            exportPid(target, hierarchy, pid, dsIds);
        }
        return target;
    }

    private void exportPid(File target, boolean hierarchy, String pid, List<String> dsIds) throws ExportException {
        String extension;
        if (exportedPids.contains(pid)) {
            return ;
        }
        exportedPids.add(pid);

        ProArcObject object = null;
        try {
            if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
                object = rstorage.find(pid);
                extension = getRemoteExtension(dsIds, getDataStreams((RemoteObject) object));
                dsIds = filterRemoteDataStreams(dsIds, getDataStreams((RemoteObject) object));
            } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
                AkubraStorage akubraStorage = AkubraStorage.getInstance(akubraConfiguration);
                object = akubraStorage.find(pid);
                extension = getAkubraExtension(dsIds, (AkubraObject) object);
                dsIds = filterAkubraDataStreams(dsIds, (AkubraObject) object);
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
            }
        } catch (DigitalObjectException | IOException | FedoraClientException e) {
            throw new ExportException(pid, e);
        }

        if (hierarchy) {
            try {
                RelationEditor relationEditor = new RelationEditor(object);
                List<String> members = relationEditor.getMembers();
                toExport.addAll(members);
            } catch (DigitalObjectException ex) {
                throw new ExportException(ex);
            }
        }

        for (String dsId : dsIds) {
            try {
                exportPid(target, object, dsId, extension);
            } catch (TransformerException | JAXBException | DigitalObjectException | IOException | FedoraClientException |
                     MetsExportException ex) {
                throw new ExportException(filename(pid, dsId), ex);
            }
        }

    }

    private void exportPid(File target, ProArcObject object, String dsId, String extension) throws FedoraClientException, IOException, MetsExportException, DigitalObjectException, JAXBException, TransformerException {
        InputStream input = getDataStreamDissemination(object, dsId);
        String filename = filename(object.getPid(), MimeType.getExtension(extension));
        if (Const.RAW_GRP_ID.equals(dsId)) {
            IMetsElement element = getElement(object.getPid());
            filename = (Const.RAW_GRP_ID + "_" + element.getElementType() + "_" + String.format("%04d", element.getModsStart()) + "_" + FoxmlUtils.pidAsUuid(object.getPid())).toLowerCase() + '.' + MimeType.getExtension(extension);
        }
        File f = new File(target, filename);
        boolean done = false;
        try {
            FileOutputStream output = new FileOutputStream(f);
            try {
                copy(input, output);
                done = true;
            } finally {
                try {
                    output.close();
                } catch (IOException ex) {
                    if (done) {
                        throw ex;
                    } else {
                        LOG.log(Level.SEVERE, f.toString(), ex);
                    }
                }
            }
        } finally {
            try {
                input.close();
            } catch (IOException ex) {
                if (done) {
                    throw ex;
                } else {
                    LOG.log(Level.SEVERE, f.toString(), ex);
                }
            }
        }
    }

    private IMetsElement getElement(String pid) throws DigitalObjectException {
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
            throw new DigitalObjectException(pid, "Export Data Stream - element not found", ex);
        }
    }

    private InputStream getDataStreamDissemination(ProArcObject fo, String dsId) throws FedoraClientException, JAXBException, IOException, TransformerException {
        if (Storage.FEDORA.equals(appConfig.getTypeOfStorage())) {
            FedoraResponse response = FedoraClient.getDatastreamDissemination(fo.getPid(), dsId)
                    .execute(((RemoteObject) fo).getClient());
            InputStream input = response.getEntityInputStream();
            return input;
        } else if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            InputStream inputStream = AkubraUtils.getDatastreamDissemination((AkubraObject) fo, dsId);
            return inputStream;
        } else {
            throw new IllegalStateException("Unsupported type of storage: " + appConfig.getTypeOfStorage());
        }
    }

    private List<DatastreamType> getDataStreams(RemoteObject remote) throws FedoraClientException {
        ListDatastreamsResponse response = FedoraClient.listDatastreams(remote.getPid())
                .execute(remote.getClient());
        return response.getDatastreams();
    }

    /**
     * FedoraClient.getDatastreamDissemination throws HTTP 404 in case of
     * undefined streams.
     */
    private List<String> filterRemoteDataStreams(List<String> dsIds, List<DatastreamType> streams) {
        ArrayList<String> result = new ArrayList<String>(dsIds.size());
        for (DatastreamType stream : streams) {
            if (dsIds.contains(stream.getDsid())) {
                result.add(stream.getDsid());
                if (dsIds.size() == result.size()) {
                    break;
                }
            }
        }
        return result;
    }

    private List<String> filterAkubraDataStreams(List<String> dsIds, AkubraObject object) throws DigitalObjectException {
        List<DatastreamProfile> streams = object.getStreamProfile(null);
        ArrayList<String> result = new ArrayList<String>(dsIds.size());
        for (DatastreamProfile datastreamProfile : streams) {
            if (dsIds.contains(datastreamProfile.getDsID())) {
                result.add(datastreamProfile.getDsID());
                if (dsIds.size() == result.size()) {
                    break;
                }
            }
        }
        return result;
    }

    private String getRemoteExtension(List<String> dsId, List<DatastreamType> streams) {
        for (DatastreamType stream : streams) {
            if (dsId.contains(stream.getDsid())) {
                return stream.getMimeType();
            }
        }
        return "";
    }

    private String getAkubraExtension(List<String> dsIds, AkubraObject object) throws DigitalObjectException {
        List<DatastreamProfile> streams = object.getStreamProfile(null);
        for (DatastreamProfile datastreamProfile : streams) {
            if (dsIds.contains(datastreamProfile.getDsID())) {
                return datastreamProfile.getDsMIME();
            }
        }
        return "";
    }

    private void copy(InputStream is, OutputStream os) throws IOException {
        for (int length = 0; (length = is.read(buffer)) > 0; ) {
            os.write(buffer, 0, length);
        }
        os.close();
    }

    static String filename(String pid, String dsId) {
        return FoxmlUtils.pidAsUuid(pid) + '.' + dsId;
    }

}
