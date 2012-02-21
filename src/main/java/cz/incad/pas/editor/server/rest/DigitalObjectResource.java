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
import cz.fi.muni.xkremser.editor.server.mods.ModsCollection;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.server.ModsGwtServiceProvider;
import cz.incad.pas.editor.server.fedora.DigitalObjectRepository;
import cz.incad.pas.editor.server.fedora.DigitalObjectRepository.DigitalObjectRecord;
import cz.incad.pas.editor.server.fedora.DigitalObjectRepository.DublinCoreRecord;
import cz.incad.pas.editor.server.fedora.DigitalObjectRepository.ModsRecord;
import cz.incad.pas.editor.server.fedora.DigitalObjectRepository.OcrRecord;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.Consumes;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

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
    private final HttpHeaders httpHeaders;

    public DigitalObjectResource(
            @Context Request request,
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo
            ) {
        this.httpRequest = request;
        this.httpHeaders = httpHeaders;
    }

    /**
     * Creates a new digital object
     *
     * @param modelId model ID (model:page, ...) of the digital object; required
     * @param mods MODS XML used to create new object; optional
     * @return
     * @throws URISyntaxException
     * @throws IOException
     */
    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public DigitalObjectList newObject(
            @FormParam("model") String modelId,
            @FormParam("mods") String mods
            ) throws URISyntaxException, IOException {

        if (modelId == null) {
            // XXX validate modelId values
            throw new IllegalArgumentException("missing model");
        }
        mods = (mods == null || mods.isEmpty() || "null".equals(mods)) ? null : mods;
        LOG.log(Level.INFO, "import model: {0} as mods: {1}", new Object[] {modelId, mods});

        DigitalObjectRecord dobj = repository.createDigitalObject();
        ModsCollection modsCollection = ModsGwtServiceProvider.newMods(dobj.getId(), mods);
        dobj.setMods(new ModsRecord(dobj.getId(), modsCollection, System.currentTimeMillis()));

        repository.add(dobj, 1);
        return new DigitalObjectList(Arrays.asList(new DigitalObject(dobj.getId(), modelId)));
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
        List<Locale> acceptableLanguages = httpHeaders.getAcceptableLanguages();
        Locale locale = acceptableLanguages.isEmpty() ? Locale.ENGLISH : acceptableLanguages.get(0);

        Collection<MetaModel> models = metamodels.find(locale);
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

//        private Collection<MetaModel> repository;

        public static MetaModelRepository getInstance() {
            return INSTANCE;
        }


        private MetaModelRepository() {
//            repository = new ArrayList<MetaModel>();
        }

        public Collection<MetaModel> find(Locale locale) {
            // for now it is read only repository
            String lang = locale.getLanguage();
            List<MetaModel> models = new ArrayList<MetaModel>();
            models.add(new MetaModel("model:periodical", true, null, "cs".equals(lang) ? "Periodikum" : "Periodical", MetaModelDataSource.EDITOR_PERIODICAL));
            models.add(new MetaModel("model:periodicalvolume", null, null, "cs".equals(lang) ? "Ročník" : "Periodical Volume", MetaModelDataSource.EDITOR_PERIODICAL_VOLUME));
            models.add(new MetaModel("model:periodicalitem", null, null, "cs".equals(lang) ? "Výtisk" : "Periodical Item", MetaModelDataSource.EDITOR_PERIODICAL_ISSUE));
            models.add(new MetaModel("model:monograph", true, null, "cs".equals(lang) ? "Monografie" : "Monograph", MetaModelDataSource.EDITOR_MONOGRAPH));
            models.add(new MetaModel("model:monographunit", null, null, "cs".equals(lang) ? "Monografie - volná část" : "Monograph Unit", MetaModelDataSource.EDITOR_MONOGRAPH_UNIT));
            models.add(new MetaModel("model:page", null, true, "cs".equals(lang) ? "Strana" : "Page", MetaModelDataSource.EDITOR_PAGE));

            return models;
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

    @XmlRootElement
    public static class DigitalObjectList {

        @XmlElement(name="record")
        public Collection<DigitalObject> records;

        public DigitalObjectList() {
        }

        public DigitalObjectList(Collection<DigitalObject> records) {
            this.records = records;
        }

    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class DigitalObject {
        private String pid;
        private String model;

        public DigitalObject(String pid, String model) {
            this.pid = pid;
            this.model = model;
        }

        public DigitalObject() {
        }
    }
}
