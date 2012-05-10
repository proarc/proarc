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

import com.sun.jersey.api.client.ClientResponse;
import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.server.config.PasConfiguration;
import cz.incad.pas.editor.server.config.PasConfigurationException;
import cz.incad.pas.editor.server.config.PasConfigurationFactory;
import cz.incad.pas.editor.server.dublincore.DcStreamEditor;
import cz.incad.pas.editor.server.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.incad.pas.editor.server.fedora.BinaryEditor;
import cz.incad.pas.editor.server.fedora.DigitalObjectRepository;
import cz.incad.pas.editor.server.fedora.FedoraObject;
import cz.incad.pas.editor.server.fedora.LocalStorage;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalObject;
import cz.incad.pas.editor.server.fedora.RemoteStorage;
import cz.incad.pas.editor.server.fedora.RemoteStorage.RemoteObject;
import cz.incad.pas.editor.server.fedora.StringEditor;
import cz.incad.pas.editor.server.fedora.StringEditor.StringRecord;
import cz.incad.pas.editor.server.imports.ImportBatchManager;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportItem;
import cz.incad.pas.editor.server.json.JsonUtils;
import cz.incad.pas.editor.server.mods.ModsStreamEditor;
import cz.incad.pas.editor.server.mods.ModsUtils;
import cz.incad.pas.editor.server.mods.custom.Mapping;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
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
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import org.codehaus.jackson.map.ObjectMapper;

/**
 *
 * @author Jan Pokorsky
 */
@Path("/object")
public class DigitalObjectResource {

    private static final Logger LOG = Logger.getLogger(DigitalObjectResource.class.getName());

    private final PasConfiguration pasConfig;
    private final MetaModelRepository metamodels = MetaModelRepository.getInstance();
    private final DigitalObjectRepository repository;
    private final ImportBatchManager importManager;
    private final Request httpRequest;
    private final HttpHeaders httpHeaders;

    public DigitalObjectResource(
            @Context Request request,
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo
            ) throws PasConfigurationException {
        this.httpRequest = request;
        this.httpHeaders = httpHeaders;
        this.pasConfig = PasConfigurationFactory.getInstance().defaultInstance();
        this.repository = DigitalObjectRepository.getInstance(pasConfig);
        this.importManager = ImportBatchManager.getInstance(pasConfig);
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
            ) throws URISyntaxException, IOException, FedoraClientException {

        if (modelId == null) {
            // XXX validate modelId values
            throw new IllegalArgumentException("missing model");
        }
        mods = (mods == null || mods.isEmpty() || "null".equals(mods)) ? null : mods;
        LOG.log(Level.INFO, "import model: {0} as mods: {1}", new Object[] {modelId, mods});

        LocalObject localObject = new LocalStorage().create();
        // MODS
        ModsStreamEditor modsEditor = new ModsStreamEditor(localObject);
        ModsType modsType;
        if (mods == null) {
            modsType = modsEditor.create(localObject.getPid(), modelId);
        } else {
            modsType = modsEditor.create(localObject.getPid(), modelId, mods);
        }
        modsEditor.write(modsType, 0);

        // DC
        DcStreamEditor dcEditor = new DcStreamEditor(localObject);
        dcEditor.write(modsType, modelId, 0);

        // XXX RELS-EXT

        RemoteStorage fedora = RemoteStorage.getInstance(pasConfig);
        fedora.ingest(localObject, "label", "XXX");

        return new DigitalObjectList(Arrays.asList(new DigitalObject(localObject.getPid(), modelId)));
    }

    @GET
    @Path("/dc")
    @Produces(MediaType.APPLICATION_XML)
    public DublinCoreRecord getDublinCore(
            @QueryParam("pid") String pid,
            @QueryParam("batchId") String batchId
            ) throws IOException {

        // dev mode http://127.0.0.1:8888/rest/object/dc?pid=uuid:4a7c2e50-af36-11dd-9643-000d606f5dc6
        FedoraObject fobject = findFedoraObject(pid, batchId);
        DcStreamEditor dcEditor = new DcStreamEditor(fobject);
        DublinCoreRecord dc = dcEditor.read();
        if (dc == null) {
            throw new NotFoundException("pid not found!");
        }
        dc.setBatchId(batchId);
        return dc;
    }

    @POST
    @Path("/dc")
    @Consumes({MediaType.TEXT_XML, MediaType.APPLICATION_XML})
    @Produces(MediaType.APPLICATION_XML)
    public DublinCoreRecord updateDublinCore(DublinCoreRecord update) throws IOException {
        if (update == null || update.getDc() == null) {
            throw new IllegalArgumentException();
        }
        FedoraObject fobject = findFedoraObject(update.getPid(), update.getBatchId());
        DcStreamEditor dcEditor = new DcStreamEditor(fobject);
        dcEditor.write(update);
        DublinCoreRecord result = dcEditor.read();
        result.setBatchId(update.getBatchId());
        return result;
    }

    /**
     * Gets subset of MODS properties in JSON.
     *
     * @param pid PID of requested digital object
     * @param editorId view defining subset of MODS properties
     */
    @GET
    @Path("/custom_mods")
    @Produces(MediaType.APPLICATION_JSON)
    public CustomMods getCustomMods(
            @QueryParam("pid") String pid,
            @QueryParam("editorId") String editorId
            ) throws IOException {
        
        if (pid == null || pid.isEmpty()) {
            throw new WebApplicationException(Response.status(Response.Status.BAD_REQUEST)
                    .entity("invalid pid").type(MediaType.TEXT_PLAIN).build());
        }

        RemoteStorage storage = RemoteStorage.getInstance(pasConfig);
        RemoteObject remote = storage.find(pid);
        ModsStreamEditor modsEditor = new ModsStreamEditor(remote);

        ModsType mods = modsEditor.read();
        Mapping mapping = new Mapping();
        Object customData = mapping.read(mods, editorId);
        CustomMods<Object> result = new CustomMods<Object>();
        result.setPid(pid);
        result.setEditor(editorId);
        result.setTimestamp(modsEditor.getLastModified());
        result.setData(customData);
        return result;
    }

    @POST
    @Path("/custom_mods")
    @Produces({MediaType.APPLICATION_JSON})
    public CustomMods updateCustomMods(
            @FormParam("pid") String pid,
            @FormParam("editorId") String editorId,
            @FormParam("timestamp") Long timestamp,
            @FormParam("customJsonData") String customJsonData
            ) throws IOException {

        LOG.severe(String.format("pid: %s, editor: %s, timestamp: %s, json: %s", pid, editorId, timestamp, customJsonData));
        if (pid == null || pid.isEmpty()) {
            throw new WebApplicationException(Response.status(Response.Status.BAD_REQUEST)
                    .entity("invalid pid").type(MediaType.TEXT_PLAIN).build());
        }
        if (timestamp == null) {
            throw new WebApplicationException(Response.status(Response.Status.BAD_REQUEST)
                    .entity("invalid timestamp").type(MediaType.TEXT_PLAIN).build());
        }
        RemoteStorage storage = RemoteStorage.getInstance(pasConfig);
        RemoteObject remote = storage.find(pid);
        ModsStreamEditor modsEditor = new ModsStreamEditor(remote);

        ModsType mods = modsEditor.read();
        Mapping mapping = new Mapping();
        Class<?> type = mapping.getType(editorId);
        ObjectMapper jsMapper = JsonUtils.defaultObjectMapper();
        try {
            Object customData = jsMapper.readValue(customJsonData, type);
            mapping.update(mods, customData, editorId);
            String toXml = ModsUtils.toXml(mods, true);
            LOG.severe(toXml);
            modsEditor.write(mods, timestamp);

            // DC
            DcStreamEditor dcEditor = new DcStreamEditor(remote);
            dcEditor.write(mods, "XXX:model", dcEditor.getLastModified());
            remote.flush();

            CustomMods<Object> result = new CustomMods<Object>();
            result.setPid(pid);
            result.setEditor(editorId);
            result.setTimestamp(modsEditor.getLastModified());
            result.setData(customData);
            return result;
        } catch (IOException ex) {
            LOG.log(Level.SEVERE, pid, ex);
            throw new WebApplicationException(ex);
        }
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
    public Response getPreview(
            @QueryParam("pid") String pid,
            @QueryParam("batchId") String batchId
            ) throws IOException {

        return getDissemination(pid, batchId, BinaryEditor.PREVIEW_ID);
    }

    @GET
    @Path("/dissemination")
    @Produces("image/*")
    public Response getDissemination(
            @QueryParam("pid") String pid,
            @QueryParam("batchId") String batchId,
            @QueryParam("datastream") String dsId
            ) throws IOException {

        FedoraObject fobject = findFedoraObject(pid, batchId);
        if (fobject instanceof LocalObject) {
            LocalObject lobject = (LocalObject) fobject;
            BinaryEditor loader = BinaryEditor.dissemination(lobject, dsId);
            if (loader == null) {
                throw new NotFoundException("dsId", dsId);
            }
            File entity = loader.read();
            if (entity == null) {
                throw new NotFoundException("content not found");
            }

            Date lastModification = new Date(loader.getLastModified());
            ResponseBuilder evaluatePreconditions = httpRequest.evaluatePreconditions(lastModification);
            if (evaluatePreconditions != null) {
                return evaluatePreconditions.build();
            }

            return Response.ok(entity, loader.getMimetype())
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
            return Response.ok(response.getEntity(InputStream.class), headers.getFirst("Content-Type"))
                    .header("Content-Disposition", filename)
                    .build();
        }
        throw new IllegalStateException("unsupported: " + fobject.getClass());
    }

    @GET
    @Path("/thumb")
    @Produces("image/*")
    public Response getThumbnail(
            @QueryParam("pid") String pid,
            @QueryParam("batchId") String batchId
            ) throws IOException {

        return getDissemination(pid, batchId, BinaryEditor.THUMB_ID);
    }

    @GET
    @Path("/ocr")
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getOcr(
            @QueryParam("pid") String pid,
            @QueryParam("batchId") String batchId
            ) throws IOException {

        FedoraObject fobject = findFedoraObject(pid, batchId);
        StringEditor ocrEditor = StringEditor.ocr(fobject);
        StringRecord ocr = ocrEditor.readRecord();
        if (ocr == null) {
            throw new NotFoundException("pid", pid);
        }
        ocr.setBatchId(batchId);
        return ocr;
    }

    @POST
    @Path("/ocr")
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord updateOcr(
            @FormParam("pid") String pid,
            @FormParam("batchId") String batchId,
            @FormParam("timestamp") Long timestamp,
            @FormParam("content") String content
            ) throws IOException {

        if (timestamp == null) {
            throw new IllegalArgumentException();
        }
        FedoraObject fobject = findFedoraObject(pid, batchId);
        StringEditor ocrEditor = StringEditor.ocr(fobject);
        ocrEditor.write(content, timestamp);
        fobject.flush();
        StringRecord result = ocrEditor.readRecord();
        result.setBatchId(batchId);
        return result;
    }

    private FedoraObject findFedoraObject(String pid, String batchId) throws IOException {
        FedoraObject fobject;
        if (batchId != null) {
            ImportItem item = importManager.findItem(pid);
            if (item == null) {
                throw new NotFoundException("pid", pid);
            }
            fobject = new LocalStorage().load(pid, item.getFoxmlAsFile());
        } else {
            fobject = RemoteStorage.getInstance(pasConfig).find(pid);
        }
        return fobject;
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

        public MetaModel find(String model) {
            for (MetaModel metaModel : find(Locale.ENGLISH)) {
                if (metaModel.getPid().equals(model)) {
                    return metaModel;
                }
            }
            return null;
        }
    }

    @XmlRootElement(name="mods")
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class CustomMods<T> {
        
        private String pid;
        private long timestamp;
        @XmlElement(name = "editorId")
        private String editor;
        @XmlElement(name = "customJsonData")
        private T data;

        public CustomMods() {
        }

        public T getData() {
            return data;
        }

        public void setData(T data) {
            this.data = data;
        }

        public String getEditor() {
            return editor;
        }

        public void setEditor(String editor) {
            this.editor = editor;
        }

        public String getPid() {
            return pid;
        }

        public void setPid(String pid) {
            this.pid = pid;
        }

        public long getTimestamp() {
            return timestamp;
        }

        public void setTimestamp(long timestamp) {
            this.timestamp = timestamp;
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

        public String getEditor() {
            return editor;
        }

    }

    @XmlRootElement(name="models")
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class MetaModelList {
        
        @XmlElement(name="model")
        public Collection<MetaModel> models;

        public MetaModelList() {
        }

        public MetaModelList(Collection<MetaModel> models) {
            this.models = models;
        }

    }

    @XmlRootElement(name="records")
    @XmlAccessorType(XmlAccessType.FIELD)
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
