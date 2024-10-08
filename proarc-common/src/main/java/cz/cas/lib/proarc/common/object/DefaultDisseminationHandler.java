/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.common.object;

import com.sun.jersey.api.client.ClientResponse;
import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.device.DeviceRepository;
import cz.cas.lib.proarc.common.process.external.GenericExternalProcess;
import cz.cas.lib.proarc.common.storage.AesEditor;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
import cz.cas.lib.proarc.common.storage.CodingHistoryEditor;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.storage.MixEditor;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage.RemoteObject;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.XmlStreamEditor;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage.AkubraObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.process.imports.FileSet;
import cz.cas.lib.proarc.common.process.imports.TiffAsJp2Importer;
import cz.cas.lib.proarc.common.process.external.TiffToJpgConvert;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.sql.Timestamp;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.Response.Status;

import org.apache.commons.configuration.Configuration;

import static cz.cas.lib.proarc.common.device.DeviceRepository.getMixDescriptionEditor;
import static cz.cas.lib.proarc.common.storage.BinaryEditor.NDK_ARCHIVAL_ID;
import static cz.cas.lib.proarc.common.storage.BinaryEditor.NDK_USER_ID;
import static cz.cas.lib.proarc.common.storage.BinaryEditor.RAW_ID;

/**
 * Handles datastream contents in its raw form.
 *
 * <p>For {@link BinaryEditor#RAW_ID} it also updates thumb, ocr, rels-ext.
 *
 * @author Jan Pokorsky
 */
public class DefaultDisseminationHandler implements DisseminationHandler {

    private static final Logger LOG = Logger.getLogger(DefaultDisseminationHandler.class.getName());

    private final String dsId;
    private final DigitalObjectHandler handler;
    private final ProArcObject fobject;

    public DefaultDisseminationHandler(String dsId, DigitalObjectHandler handler) {
        this.dsId = dsId;
        this.handler = handler;
        fobject = handler.getFedoraObject();
    }

    public String getDsId() {
        return dsId;
    }

    public DigitalObjectHandler getHandler() {
        return handler;
    }

    @Override
    public Response getDissemination(Request httpRequest) throws DigitalObjectException, DigitalObjectNotFoundException {
        String pid = fobject.getPid();
        if (dsId == null) {
            return Response.ok(fobject.asText(), MediaType.TEXT_XML_TYPE)
                    .header("Content-Disposition", "inline; filename=\"" + pid + ".xml\"")
                    .build();
        }
        if (fobject instanceof LocalObject) {
            LocalObject lobject = (LocalObject) fobject;
            BinaryEditor loader = BinaryEditor.dissemination(lobject, dsId);
            if (loader == null) {
                throw new DigitalObjectNotFoundException(pid, null, dsId, null, null);
            }
            File entity = loader.read();
            if (entity == null) {
                throw new DigitalObjectNotFoundException(pid, null, dsId, "no content", null);
            }

            Date lastModification = new Date(loader.getLastModified());
            ResponseBuilder evaluatePreconditions = httpRequest == null
                    ? null : httpRequest.evaluatePreconditions(lastModification);
            if (evaluatePreconditions != null) {
                return evaluatePreconditions.build();
            }

  /*          //transform jp2 or tiff to jpg
            if (NDK_ARCHIVAL_ID.equals(dsId) || NDK_USER_ID.equals(dsId) || RAW_ID.equals(dsId)) {
                try {
                    return Response.ok(convertToBrowserCompatible(entity, dsId), "image/jpeg")
                            .header("Content-Disposition", "inline; filename=\"" + entity.getName() + ".jpg\"")
                            .lastModified(lastModification)
                            .build();
                } catch (Exception e) {
                    LOG.log(Level.SEVERE,"Converting " + dsId + " to jpg failed.");
                    return Response.status(Status.INTERNAL_SERVER_ERROR).build();
                }
            } else {
*/
            return Response.ok(entity, loader.getProfile().getDsMIME())
                        .header("Content-Disposition", "inline; filename=\"" + entity.getName() + '"')
                        .lastModified(lastModification).build();
//            }
        } else if (fobject instanceof AkubraObject) {
            AkubraObject akubraObject = (AkubraObject) fobject;
            return getResponse(akubraObject, dsId);
        } else if (fobject instanceof RemoteObject) {
            RemoteObject remote = (RemoteObject) fobject;
            return getResponse(remote, dsId);
        }
        throw new IllegalStateException("unsupported: " + fobject.getClass());
    }

    public static Response getResponse(ProArcObject object, String dsId) throws DigitalObjectException {
        // This should limit fedora calls to 1.
        // XXX It works around FedoraClient.FedoraClient.getDatastreamDissemination that hides HTTP headers of the response.
        // Unfortunattely fedora does not return modification date as HTTP header
        // In case of large images it could be faster to ask datastream for modification date first.
        String pid = object.getPid();
        String path = String.format("objects/%s/datastreams/%s/content", pid, dsId);
        if (object instanceof RemoteObject) {
            ClientResponse response = ((RemoteObject) object).getClient().resource().path(path).get(ClientResponse.class);
            if (Status.fromStatusCode(response.getStatus()) != Status.OK) {
                throw new DigitalObjectNotFoundException(pid, null, dsId, response.getEntity(String.class), null);
            }
            MultivaluedMap<String, String> headers = response.getHeaders();
            String filename = headers.getFirst("Content-Disposition");
            filename = filename != null ? filename : "inline; filename=" + pid + '-' + dsId;
/*
        //transform jp2 or tiff to jpg
        if (NDK_ARCHIVAL_ID.equals(dsId) || NDK_USER_ID.equals(dsId) || RAW_ID.equals(dsId)) {

            try {
                return Response.ok(convertToBrowserCompatible(response.getEntity(InputStream.class), dsId), "image/jpeg")
                        .header("Content-Disposition", filename + ".jpg")
                        .build();
            } catch (Exception e) {
                LOG.log(Level.SEVERE,"Converting " + dsId + " to jpg failed.");
                return Response.status(Status.INTERNAL_SERVER_ERROR).build();
            }
        } else {
*/
            return Response.ok(response.getEntity(InputStream.class), headers.getFirst("Content-Type"))
                    .header("Content-Disposition", filename)
                    .build();
//        }
        } else if (object instanceof AkubraObject) {
            if (DeviceRepository.DESCRIPTION_DS_ID.equals(dsId)) {
                XmlStreamEditor editor = getMixDescriptionEditor(object);
                InputStream inputStream = editor.readStream();
                if (inputStream == null) {
                    throw new DigitalObjectNotFoundException(pid, null, dsId, "no content", null);
                }
                Date lastModification = new Date(editor.getLastModified());
                return Response.ok(inputStream, "text/xml").lastModified(lastModification).build();
            } else if (MixEditor.RAW_ID.equals(dsId)) {
                MixEditor editor = MixEditor.raw(object);
                String mix = editor.readAsString();
                if (mix == null) {
                    throw new DigitalObjectNotFoundException(pid, null, dsId, "no content", null);
                }
                InputStream inputStream = new ByteArrayInputStream(mix.getBytes());
                Date lastModification = new Date(editor.getLastModified());
                return Response.ok(inputStream, "text/xml").lastModified(lastModification).build();
            } else if (MixEditor.NDK_ARCHIVAL_ID.equals(dsId)) {
                MixEditor editor = MixEditor.ndkArchival(object);
                String mix = editor.readAsString();
                if (mix == null) {
                    throw new DigitalObjectNotFoundException(pid, null, dsId, "no content", null);
                }
                InputStream inputStream = new ByteArrayInputStream(mix.getBytes());
                Date lastModification = new Date(editor.getLastModified());
                return Response.ok(inputStream, "text/xml").lastModified(lastModification).build();
            } else if (AesEditor.RAW_ID.equals(dsId)) {
                AesEditor editor = AesEditor.raw(object);
                String aes = editor.readAsString();
                if (aes == null) {
                    throw new DigitalObjectNotFoundException(pid, null, dsId, "no content", null);
                }
                InputStream inputStream = new ByteArrayInputStream(aes.getBytes());
                Date lastModification = new Date(editor.getLastModified());
                return Response.ok(inputStream, "text/xml").lastModified(lastModification).build();
            } else if (AesEditor.NDK_ARCHIVAL_ID.equals(dsId)) {
                AesEditor editor = AesEditor.ndkArchival(object);
                String aes = editor.readAsString();
                if (aes == null) {
                    throw new DigitalObjectNotFoundException(pid, null, dsId, "no content", null);
                }
                InputStream inputStream = new ByteArrayInputStream(aes.getBytes());
                Date lastModification = new Date(editor.getLastModified());
                return Response.ok(inputStream, "text/xml").lastModified(lastModification).build();
            } else if (CodingHistoryEditor.RAW_ID.equals(dsId)) {
                CodingHistoryEditor editor = CodingHistoryEditor.raw(object);
                String codingHistory = editor.readAsString();
                if (codingHistory == null) {
                    throw new DigitalObjectNotFoundException(pid, null, dsId, "no content", null);
                }
                InputStream inputStream = new ByteArrayInputStream(codingHistory.getBytes());
                Date lastModification = new Date(editor.getLastModified());
                return Response.ok(inputStream, "text/xml").lastModified(lastModification).build();
            } else if (CodingHistoryEditor.NDK_ARCHIVAL_ID.equals(dsId)) {
                CodingHistoryEditor editor = CodingHistoryEditor.ndkArchival(object);
                String codingHistory = editor.readAsString();
                if (codingHistory == null) {
                    throw new DigitalObjectNotFoundException(pid, null, dsId, "no content", null);
                }
                InputStream inputStream = new ByteArrayInputStream(codingHistory.getBytes());
                Date lastModification = new Date(editor.getLastModified());
                return Response.ok(inputStream, "text/xml").lastModified(lastModification).build();
            } else {
                BinaryEditor loader = BinaryEditor.dissemination((AkubraObject) object, dsId);
                if (loader == null) {
                    throw new DigitalObjectNotFoundException(pid, null, dsId, null, null);
                }
                File entity = loader.read();
                if (entity == null) {
                    //throw new DigitalObjectNotFoundException(pid, null, dsId, "no content", null);
                    InputStream inputStream = loader.readStream();
                    if (inputStream == null) {
                        if ("PREVIEW".equals(dsId)) {
                            return Response.noContent().build();
                        }
                        throw new DigitalObjectNotFoundException(pid, null, dsId, "no content", null);
                    }
                    Date lastModification = new Date(loader.getLastModified());
//                ResponseBuilder evaluatePreconditions = httpRequest == null ? null : httpRequest.evaluatePreconditions(lastModification);
//                if (evaluatePreconditions != null) {
//                    return evaluatePreconditions.build();
//                }
                    return Response.ok(inputStream, loader.getProfile().getDsMIME()).lastModified(lastModification).build();
                }
                Date lastModification = new Date(loader.getLastModified());
//            ResponseBuilder evaluatePreconditions = httpRequest == null
//                    ? null : httpRequest.evaluatePreconditions(lastModification);
//            if (evaluatePreconditions != null) {
//                return evaluatePreconditions.build();
//            }
                return Response.ok(entity, loader.getProfile().getDsMIME())
                        .header("Content-Disposition", "inline; filename=\"" + entity.getName() + '"')
                        .lastModified(lastModification).build();
            }
        } else {
            throw new DigitalObjectException(pid, "Missing implementation for DefaultDisseminationHandler:getResponse.");
        }
    }

    private static byte[] convertToBrowserCompatible(InputStream entity, String dsId) throws IOException, AppConfigurationException {
        File inFile = File.createTempFile(String.valueOf(new Timestamp(System.currentTimeMillis())),".jp2");

        OutputStream out = new FileOutputStream(inFile);
        org.apache.commons.io.IOUtils.copy(entity, out);
        out.close();

        byte[] bFile = convertToBrowserCompatible(inFile, dsId);

        inFile.delete();

        return bFile;
    }

    private static byte[] convertToBrowserCompatible(File entity, String dsId) throws IOException, AppConfigurationException {

        FileSet.FileEntry tiff = null;

        if (NDK_ARCHIVAL_ID.equals(dsId) || NDK_USER_ID.equals(dsId)) {
            //convert JP2 to TIFF

            tiff = TiffAsJp2Importer.convertToTiff(
                    new FileSet.FileEntry(entity),
                    AppConfigurationFactory.getInstance().defaultInstance().getImportConfiguration().getConvertorJp2Processor()
            );
        }

        if (NDK_ARCHIVAL_ID.equals(dsId) || NDK_USER_ID.equals(dsId) || RAW_ID.equals(dsId)) {
            //convert TIFF to JPG

            if (RAW_ID.equals(dsId)) {
                tiff = new FileSet.FileEntry(entity);
            }

            File out = File.createTempFile(String.valueOf(new Timestamp(System.currentTimeMillis())), ".jpg");

            new TiffToJpgConvert(
                    AppConfigurationFactory.getInstance().defaultInstance().getImportConfiguration().getConvertorTiffToJpgProcessor(),
                    tiff.getFile(),
                    out,
                    500 ,
                    500).run();

            if (!RAW_ID.equals(dsId)) {
                tiff.getFile().delete();
            }

            byte[] bFile = Files.readAllBytes(out.toPath());

            out.delete();

            return bFile;
        }

        return Files.readAllBytes(entity.toPath());
    }

    // XXX add impl of other data streams (PREVIEW, THUMB)
    @Override
    public void setDissemination(DisseminationInput input, Storage storageType, String message) throws DigitalObjectException {
        if (RAW_ID.equals(dsId)) {
            setRawDissemination(input.getFile(), input.getFilename(), input.getMime(), storageType, message);
            createThumbnail(input.getFile(), message);
        } else {
            throw new UnsupportedOperationException(dsId);
        }
    }

    private void createThumbnail(File inputFile, String message) throws DigitalObjectException {
        Configuration thumbConf = getConfig().getImportConfiguration().getThumbnailProcessor();
        if (thumbConf != null && !thumbConf.isEmpty()) {
            GenericExternalProcess thumbProc = new GenericExternalProcess(thumbConf)
                    .addInputFile(inputFile)
                    .addOutputFile(new File(inputFile.getAbsolutePath() + ".jpg"));
            thumbProc.run();
            if (thumbProc.isOk()) {
                setDsDissemination(BinaryEditor.THUMB_ID, thumbProc.getOutputFile(),
                        BinaryEditor.THUMB_LABEL, BinaryEditor.IMAGE_JPEG, message);
            }
        }
    }

    private AppConfiguration getConfig() throws DigitalObjectException {
        try {
            return AppConfigurationFactory.getInstance().defaultInstance();
        } catch (AppConfigurationException ex) {
            throw new DigitalObjectException(handler.getFedoraObject().getPid(), null, dsId,
                    "Broken configuration! ", ex);
        }
    }

    @Override
    public void deleteDissemination(String message) throws DigitalObjectException {

        fobject.purgeDatastream(dsId, message);
    }

    public void setRawDissemination(File contents, String filename, MediaType mime, Storage storageType, String message) throws DigitalObjectException {
        setDsDissemination(RAW_ID, contents, filename, mime, message);

        RelationEditor relationEditor = handler.relations();
        relationEditor.setImportFile(filename);
        relationEditor.write(relationEditor.getLastModified(), message);
        // generate preview, thumb, ocr if possible
    }

    public void setPreviewDissemination(File contents, String filename, MediaType mime, String message) throws DigitalObjectException {
        setDsDissemination(BinaryEditor.PREVIEW_ID, contents, filename, mime, message);
    }

    public void setIconAsDissemination(MediaType origMime, String dsLabel, Storage storageType, String message) throws DigitalObjectException {
        setIconAsDissemination(dsId, origMime, dsLabel, storageType, message);
    }

    /**
     * Writes an icon URI to represent the given MIME. It searches for {@code icon:MIME/dsId}
     * or {@code icon:MIME/THUMBNAIL}. If there is no icon and stream found, nothing is written.
     *
     * @param dsId stream ID where to write an icon location
     * @param origMime MIME to derive icon URI
     * @param dsLabel
     * @param message
     * @throws DigitalObjectException failure
     */
    public void setIconAsDissemination(String dsId, MediaType origMime, String dsLabel, Storage storageType, String message) throws DigitalObjectException {
        DatastreamProfile newProfile = FoxmlUtils.externalProfile(dsId, BinaryEditor.IMAGE_JPEG, dsLabel);
        BinaryEditor editor = new BinaryEditor(fobject, newProfile);
        DatastreamProfile profile = editor.getProfile();

        // default icon datastream ID for givent MIME
        final String defaultDsId = BinaryEditor.THUMB_ID;
        String dsLocation = profile.getDsLocation();
        URI newLocation = toIconUri(origMime, dsId);
        URI newLocationDefault;
        if (newLocation.toASCIIString().equals(dsLocation)) {
            // ok
            return ;
        } else {
            newLocationDefault = toIconUri(origMime, defaultDsId);
            if (newLocationDefault.toASCIIString().equals(dsLocation)) {
                // ok
                return ;
            }
        }
        //  check icon:mime/DS exists
        List<DatastreamProfile> iconStreams;
        try {

            if (Storage.FEDORA.equals(storageType)) {
                RemoteObject object = FedoraStorage.getInstance().find(mime2iconPid(origMime));
                iconStreams = object.getDatastreams();
            } else if (Storage.AKUBRA.equals(storageType)) {
                AkubraObject object = AkubraStorage.getInstance().find(mime2iconPid(origMime));
                iconStreams = object.getDatastreamProfiles();
            } else {
                throw new IllegalStateException("Unsupported type of storage: " + storageType);
            }
        } catch (DigitalObjectNotFoundException ex) {
            // no icon
            LOG.log(Level.WARNING, "Missing object ''{0}''! No datastream ''{1}'' created for ''{2}''.",
                    new Object[]{mime2iconPid(origMime), dsId, fobject.getPid()});
            return ;
        }

        DatastreamProfile iconStream = findProfile(dsId, iconStreams);
        if (iconStream == null) {
            //  check default icon:mime/THUMBNAIL exists
            iconStream = findProfile(defaultDsId, iconStreams);
            if (iconStream == null) {
                // no icon
                return ;
            }
            newLocation = newLocationDefault;
        }
        if (iconStream.getDsMIME() != null) {
            newProfile.setDsMIME(iconStream.getDsMIME());
        }
        editor.setProfile(newProfile);
        editor.write(newLocation, editor.getLastModified(), message);
    }

    private URI toIconUri(MediaType origMime, String dsId) throws DigitalObjectException {
        URI newLocation;
        try {
            newLocation = FoxmlUtils.localFedoraUri(mime2iconPid(origMime), dsId);
        } catch (URISyntaxException ex) {
            throw new DigitalObjectException(fobject.getPid(), null, dsId, null, ex);
        }
        return newLocation;
    }

    public void setDsDissemination(String dsId, File contents, String filename, MediaType mime, String message) throws DigitalObjectException {
        BinaryEditor editor = BinaryEditor.dissemination(fobject, dsId, mime);
        DatastreamProfile profile = editor.getProfile();
        profile.setDsMIME(mime.toString());
        // fedora adds own extensions :-(
        profile.setDsLabel(filename);
        editor.setProfile(profile);
        editor.write(contents, editor.getLastModified(), message);
    }

    static DatastreamProfile findProfile(String dsId, List<DatastreamProfile> profiles) {
        for (DatastreamProfile p : profiles) {
            if (dsId.equals(p.getDsID())) {
                return p;
            }
        }
        return null;
    }

    static String mime2iconPid(MediaType mime) {
        return mime2iconPid(mime.toString());
    }

    static String mime2iconPid(String mime) {
        // object-id syntax: ( [A-Z] / [a-z] / [0-9] / "-" / "." / "~" / "_" / escaped-octet ) 1+
        return "icon:" + mime.replaceAll("[^A-Za-z0-9\\-\\.~_]", "_");
    }

}
