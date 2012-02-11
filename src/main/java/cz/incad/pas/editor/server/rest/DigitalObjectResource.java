/*
 * Copyright (C) 2011 Jan Pokorsky
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.server.rest;

import com.yourmediashelf.fedora.generated.foxml.DatastreamVersionType;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.server.fedora.DigitalObjectRepository;
import cz.incad.pas.editor.server.fedora.DigitalObjectRepository.DublinCoreRecord;
import cz.incad.pas.editor.server.fedora.DigitalObjectRepository.OcrRecord;
import cz.incad.pas.oaidublincore.ElementType;
import cz.incad.pas.oaidublincore.OaiDcType;
import cz.incad.pas.oaidublincore.ObjectFactory;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.ws.Action;

/**
 *
 * @author Jan Pokorsky
 */
@Path("/object")
public class DigitalObjectResource {

    private static final Logger LOG = Logger.getLogger(DigitalObjectResource.class.getName());

    private final MetaModelRepository metamodels = MetaModelRepository.getInstance();
    private final DigitalObjectRepository repository = DigitalObjectRepository.getInstance();
    private final Request httpRequest;

    public DigitalObjectResource(@Context Request request) {
        this.httpRequest = request;
    }

    @GET
    @Path("/dc")
    @Produces(MediaType.APPLICATION_XML)
    public DublinCoreRecord getDublinCore(
            @QueryParam("pid") String pid
            ) {

        try {
            // dev mode http://127.0.0.1:8888/rest/object/dc?pid=uuid:4a7c2e50-af36-11dd-9643-000d606f5dc6
            DublinCoreRecord dc = repository.getDc(pid);
            return dc;
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, pid, ex);
            throw new WebApplicationException(
                    Response.status(Response.Status.NOT_FOUND)
                    .entity("Some smart message.").type(MediaType.TEXT_PLAIN)
                    .build());
        }
    }

    @POST
    @Path("/dc")
    @Consumes({MediaType.TEXT_XML, MediaType.APPLICATION_XML})
    @Produces(MediaType.APPLICATION_XML)
    public DublinCoreRecord updateDublinCore(DublinCoreRecord record) {
        repository.updateDc(record, 1);
        return record;
    }

    @GET
    @Path("/metamodel")
    @Produces({MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON})
    public MetaModelList listModels() {
        Collection<MetaModel> models = metamodels.find();
        return new MetaModelList(models);
    }

    @GET
    @Path("/preview")
    @Produces("image/*")
    public Response getPreview(@QueryParam("pid") String pid) {
        DatastreamVersionType preview = repository.getPreview(pid);
        if (preview == null) {
            return Response.status(Response.Status.NOT_FOUND).build();
        }

        Date lastModification = preview.getCREATED().toGregorianCalendar().getTime();
        ResponseBuilder evaluatePreconditions = httpRequest.evaluatePreconditions(lastModification);
        if (evaluatePreconditions != null) {
            return evaluatePreconditions.build();
        }

//        Response.temporaryRedirect("URI");
        return Response.ok(preview.getBinaryContent(), preview.getMIMETYPE())
                .lastModified(lastModification)
//                .cacheControl(null)
//                .expires(new Date(2100, 1, 1))
                .build();
    }

    @GET
    @Path("/thumb")
    @Produces("image/*")
    public Response getThumbnail(@QueryParam("pid") String pid) {
        DatastreamVersionType preview = repository.getThumbnail(pid);
        if (preview == null) {
            return Response.status(Response.Status.NOT_FOUND).build();
        }

        Date lastModification = preview.getCREATED().toGregorianCalendar().getTime();
        ResponseBuilder evaluatePreconditions = httpRequest.evaluatePreconditions(lastModification);
        if (evaluatePreconditions != null) {
            return evaluatePreconditions.build();
        }

//        Response.temporaryRedirect("URI");
        return Response.ok(preview.getBinaryContent(), preview.getMIMETYPE())
                .lastModified(lastModification)
//                .cacheControl(null)
//                .expires(new Date(2100, 1, 1))
                .build();
    }

    @GET
    @Path("/ocr")
//    @Produces(MediaType.APPLICATION_XML)
    @Produces(MediaType.APPLICATION_JSON)
    public OcrRecord getOcr(@QueryParam("pid") String pid) {
        OcrRecord ocr = repository.getOcr(pid);
        if (ocr == null) {
            throw new WebApplicationException(Response.status(Response.Status.NOT_FOUND).build());
        }
        return ocr;
    }

    @POST
    @Path("/ocr")
    @Consumes({MediaType.TEXT_XML, MediaType.APPLICATION_XML})
    @Produces(MediaType.APPLICATION_XML)
    public OcrRecord updateOcr(OcrRecord record) {
        repository.updateOcr(record, 1);
        return record;
    }

    @POST
    @Path("/ocr")
//    @Consumes({MediaType.TEXT_XML, MediaType.APPLICATION_XML})
    @Produces(MediaType.APPLICATION_JSON)
    public OcrRecord updateOcrJson(
            @FormParam("pid") String pid,
            @FormParam("timestamp") long timestamp,
            @FormParam("ocr") String ocr
            ) {
        OcrRecord record = new OcrRecord(ocr, timestamp, pid);
        repository.updateOcr(record, 1);
        return record;
    }

    public static final class MetaModelRepository {

        private static final MetaModelRepository INSTANCE = new MetaModelRepository();

        private Collection<MetaModel> repository;

        public static MetaModelRepository getInstance() {
            return INSTANCE;
        }


        private MetaModelRepository() {
            repository = new ArrayList<MetaModel>();
            repository.add(new MetaModel("model:monograph", true, null, "Monograph"));
            repository.add(new MetaModel("model:monographunit", null, null, "Monograph Unit"));
            repository.add(new MetaModel("model:page", null, true, "Page", MetaModelDataSource.EDITOR_PAGE));
            repository.add(new MetaModel("model:periodical", true, null, "Periodical", MetaModelDataSource.EDITOR_PERIODICAL));
            repository.add(new MetaModel("model:periodicalvolume", null, null, "Periodical Volume", MetaModelDataSource.EDITOR_PERIODICAL_VOLUME));
            repository.add(new MetaModel("model:periodicalitem", null, null, "Periodical Item"));
//            repository.add(new MetaModel("model:internalpart", null, null, "Internalpart"));
        }

        public Collection<MetaModel> find() {
            return repository;
        }
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class MetaModel {
        
        @XmlElement(type=String.class, required=true)
        private Object pid;
        private Boolean root;
        private Boolean leaf;
        private String displayName;
        @XmlElement(name="editorId")
        private String editor;

        private MetaModel() {
        }

        public MetaModel(Object pid, Boolean root, Boolean leaf, String displayName) {
            this(pid, root, leaf, displayName, null);
        }
        
        public MetaModel(Object pid, Boolean root, Boolean leaf, String displayName, String editor) {
            this.pid = pid;
            this.root = root;
            this.leaf = leaf;
            this.displayName = displayName;
            this.editor = editor;
        }

        public String getDisplayName() {
            return displayName;
        }

        public Boolean isLeaf() {
            return leaf;
        }

        public Object getPid() {
            return pid;
        }

        public Boolean isRoot() {
            return root;
        }

    }

    @XmlRootElement(name="models")
    public static class MetaModelList {
        
        @XmlElement(name="model")
        public Collection<MetaModel> models;

        public MetaModelList() {
        }

        public MetaModelList(Collection<MetaModel> models) {
            this.models = models;
        }

    }
}
