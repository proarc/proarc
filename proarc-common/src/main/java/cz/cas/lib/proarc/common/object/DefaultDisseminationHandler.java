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
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import java.io.File;
import java.io.InputStream;
import java.util.Date;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.Response.Status;

/**
 * Handles datastream contents in its raw form.
 *
 * <p>For {@link BinaryEditor#RAW_ID} it also updates thumb, ocr, rels-ext.
 *
 * @author Jan Pokorsky
 */
public class DefaultDisseminationHandler implements DisseminationHandler {

    private final String dsId;
    private final DigitalObjectHandler handler;
    private final FedoraObject fobject;

    public DefaultDisseminationHandler(String dsId, DigitalObjectHandler handler) {
        this.dsId = dsId;
        this.handler = handler;
        fobject = handler.getFedoraObject();
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
//                throw RestException.plainNotFound(DigitalObjectResourceApi.DISSEMINATION_DATASTREAM, dsId);
            }
            File entity = loader.read();
            if (entity == null) {
                return Response.status(Status.NOT_FOUND).type(MediaType.TEXT_PLAIN_TYPE)
                        .entity("content not found").build();
            }

            Date lastModification = new Date(loader.getLastModified());
            ResponseBuilder evaluatePreconditions = httpRequest == null
                    ? null : httpRequest.evaluatePreconditions(lastModification);
            if (evaluatePreconditions != null) {
                return evaluatePreconditions.build();
            }

            return Response.ok(entity, loader.getProfile().getDsMIME())
                    .header("Content-Disposition", "inline; filename=\"" + entity.getName() + '"')
                    .lastModified(lastModification)
//                    .cacheControl(null)
//                    .expires(new Date(2100, 1, 1))
                    .build();
        } else if (fobject instanceof RemoteObject) {
            // This should limit fedora calls to 1.
            // XXX It works around FedoraClient.FedoraClient.getDatastreamDissemination that hides HTTP headers of the response.
            // Unfortunattely fedora does not return modification date as HTTP header
            // In case of large images it could be faster to ask datastream for modification date first.
            RemoteObject remote = (RemoteObject) fobject;
            String path = String.format("objects/%s/datastreams/%s/content", remote.getPid(), dsId);
            ClientResponse response = remote.getClient().resource().path(path).get(ClientResponse.class);
            MultivaluedMap<String, String> headers = response.getHeaders();
            String filename = headers.getFirst("Content-Disposition");
            filename = filename != null ? filename : "inline; filename=" + pid + '-' + dsId;
            return Response.ok(response.getEntity(InputStream.class), headers.getFirst("Content-Type"))
                    .header("Content-Disposition", filename)
                    .build();
        }
        throw new IllegalStateException("unsupported: " + fobject.getClass());
    }

    // XXX add impl of other data streams (PREVIEW, THUMB)
    @Override
    public void setDissemination(DisseminationInput input, String message) throws DigitalObjectException {
        if (BinaryEditor.RAW_ID.equals(dsId)) {
            setRawDissemination(input.getFile(), input.getFilename(), input.getMime(), message);
        } else {
            throw new UnsupportedOperationException(dsId);
        }
    }

    public void setRawDissemination(File contents, String filename, MediaType mime, String message) throws DigitalObjectException {
        BinaryEditor editor = BinaryEditor.dissemination(fobject, BinaryEditor.RAW_ID, mime);
        DatastreamProfile profile = editor.getProfile();
        profile.setDsMIME(mime.toString());
        // fedora adds own extensions :-(
        profile.setDsLabel(filename);
        editor.setProfile(profile);
        // XXX generate preview, thumb, ocr if possible
        editor.write(contents, editor.getLastModified(), message);

        RelationEditor relationEditor = handler.relations();
        relationEditor.setImportFile(filename);
        relationEditor.write(relationEditor.getLastModified(), message);
    }

}
