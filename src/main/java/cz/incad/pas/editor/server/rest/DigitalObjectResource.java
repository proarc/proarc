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
import cz.incad.pas.editor.server.config.AppConfiguration;
import cz.incad.pas.editor.server.config.AppConfigurationException;
import cz.incad.pas.editor.server.config.AppConfigurationFactory;
import cz.incad.pas.editor.server.dublincore.DcStreamEditor;
import cz.incad.pas.editor.server.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.incad.pas.editor.server.fedora.BinaryEditor;
import cz.incad.pas.editor.server.fedora.DigitalObjectException;
import cz.incad.pas.editor.server.fedora.DigitalObjectNotFoundException;
import cz.incad.pas.editor.server.fedora.FedoraObject;
import cz.incad.pas.editor.server.fedora.LocalStorage;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalObject;
import cz.incad.pas.editor.server.fedora.PurgeFedoraObject;
import cz.incad.pas.editor.server.fedora.PurgeFedoraObject.PurgeException;
import cz.incad.pas.editor.server.fedora.RemoteStorage;
import cz.incad.pas.editor.server.fedora.RemoteStorage.RemoteObject;
import cz.incad.pas.editor.server.fedora.SearchView;
import cz.incad.pas.editor.server.fedora.SearchView.Item;
import cz.incad.pas.editor.server.fedora.StringEditor;
import cz.incad.pas.editor.server.fedora.StringEditor.StringRecord;
import cz.incad.pas.editor.server.fedora.relation.RelationEditor;
import cz.incad.pas.editor.server.imports.ImportBatchManager;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportBatch;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportItem;
import cz.incad.pas.editor.server.json.JsonUtils;
import cz.incad.pas.editor.server.mods.ModsStreamEditor;
import cz.incad.pas.editor.server.mods.ModsUtils;
import cz.incad.pas.editor.server.mods.custom.Mapping;
import cz.incad.pas.editor.server.user.UserManager;
import cz.incad.pas.editor.server.user.UserProfile;
import cz.incad.pas.editor.server.user.UserUtil;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi.DatastreamEditorType;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi.SearchType;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
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
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import org.codehaus.jackson.map.ObjectMapper;

/**
 * Resource to manage digital objects.
 * 
 *      /object/{pid}/ GET - read DigObjDesc:{pid, displayname, date, owner};
 *      /object/ GET - lists all DigObjDesc
 *      /object/{pid}/foxml
 *      /object/{pid}/scan
 *      /object/{pid}/preview
 *      /object/{pid}/thumb
 *      /object/{pid}/ocr
 *      /object/{pid}/metadata
 *      /object/{pid}/relations
 *      /object/metamodel/ GET - lists model:{pid, displayname, type:(TOP|LEAF)}
 *
 * @author Jan Pokorsky
 */
@Path(DigitalObjectResourceApi.PATH)
public class DigitalObjectResource {

    private static final Logger LOG = Logger.getLogger(DigitalObjectResource.class.getName());

    private final AppConfiguration appConfig;
    private final MetaModelRepository metamodels = MetaModelRepository.getInstance();
    private final ImportBatchManager importManager;
    private final UserManager userManager;
    private final Request httpRequest;
    private final HttpHeaders httpHeaders;
    private final UserProfile user;
    private final SessionContext session;

    public DigitalObjectResource(
            @Context Request request,
            @Context SecurityContext securityCtx,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpRequest
            ) throws AppConfigurationException {
        
        this.httpRequest = request;
        this.httpHeaders = httpHeaders;
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
        this.importManager = ImportBatchManager.getInstance(appConfig);
        this.userManager = UserUtil.getDefaultManger();

        Principal userPrincipal = securityCtx.getUserPrincipal();
        String userName;
        if (userPrincipal != null) {
            userName = userPrincipal.getName();
        } else {
            userName = UserManager.GUEST_ID;
        }
        user = userManager.find(userName);
        session = SessionContext.from(httpRequest);
        LOG.fine(user.toString());
    }

    /**
     * Creates a new digital object
     *
     * @param modelId model ID (model:page, ...) of the digital object; required
     * @param pid PID of the digital object from external Kramerius. PID must not be already assigned. Optional
     * @param mods MODS XML used to create new object; optional
     * @return
     * @throws URISyntaxException
     * @throws IOException
     */
    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DigitalObject> newObject(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.NEWOBJECT_MODS_PARAM) String mods
            ) throws URISyntaxException, IOException, FedoraClientException, DigitalObjectException {

        if (modelId == null) {
            // XXX validate modelId values
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_MODEL, modelId);
        }
        if (pid != null) {
            boolean invalid = pid.length() < 5;
            try {
                if (!invalid) {
                    UUID uuid = UUID.fromString(pid.substring("uuid:".length()));
                    pid = "uuid:" + uuid.toString();
                }
            } catch (IllegalArgumentException e) {
                invalid = true;
            }
            if (invalid) {
                return SmartGwtResponse.<DigitalObject>asError().error(
                        DigitalObjectResourceApi.DIGITALOBJECT_PID, "Invalid PID!").build();
            }
        }
        mods = (mods == null || mods.isEmpty() || "null".equals(mods)) ? null : mods;
        LOG.log(Level.FINE, "import model: {0} as mods: {1}", new Object[] {modelId, mods});

        LocalObject localObject = new LocalStorage().create(pid);
        // MODS
        ModsStreamEditor modsEditor = new ModsStreamEditor(localObject);
        ModsType modsType;
        if (mods == null) {
            modsType = modsEditor.create(localObject.getPid(), modelId);
        } else {
            modsType = modsEditor.create(localObject.getPid(), modelId, mods);
        }
        modsEditor.write(modsType, 0, session.asFedoraLog());

        // DC
        DcStreamEditor dcEditor = new DcStreamEditor(localObject);
        dcEditor.write(modsType, modelId, 0, session.asFedoraLog());

        String label = ModsUtils.getLabel(modsType, modelId);
        localObject.setLabel(label);
        localObject.setOwner(user.getUserName());

        // RELS-EXT
        RelationEditor relsExt = new RelationEditor(localObject);
        relsExt.setModel(modelId);
        relsExt.write(0, session.asFedoraLog());

        localObject.flush();

        RemoteStorage fedora = RemoteStorage.getInstance(appConfig);
        try {
            fedora.ingest(localObject, user.getUserName(), session.asFedoraLog());
        } catch (FedoraClientException ex) {
            // XXX hack: Fedora server does not notify existing object conflict with HTTP 409.
            // Workaround parses error message.
            // Check for existence before ingest would be insufficient as Fedora does not yet support transactions.
            String message = ex.getMessage();
            if (message != null && message.contains("org.fcrepo.server.errors.ObjectExistsException")) {
                return SmartGwtResponse.<DigitalObject>asError().error("pid", "Object already exists!").build();
            }
            throw ex;
        }
        return new SmartGwtResponse<DigitalObject>(new DigitalObject(localObject.getPid(), modelId));
    }

    /**
     * @see PurgeFedoraObject
     */
    @DELETE
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<DigitalObject> deleteObject(
            @QueryParam(DigitalObjectResourceApi.DELETE_PID_PARAM) List<String> pids,
            @QueryParam(DigitalObjectResourceApi.DELETE_HIERARCHY_PARAM)
            @DefaultValue("true") boolean hierarchy,
            @QueryParam(DigitalObjectResourceApi.DELETE_PURGE_PARAM)
            @DefaultValue("false") boolean purge
            ) throws IOException, PurgeException {

        RemoteStorage fedora = RemoteStorage.getInstance(appConfig);
        ArrayList<DigitalObject> result = new ArrayList<DigitalObject>(pids.size());
        PurgeFedoraObject service = new PurgeFedoraObject(fedora);
        if (purge) {
            service.purge(pids, hierarchy, session.asFedoraLog());
        } else {
            service.delete(pids, hierarchy, session.asFedoraLog());
        }
        for (String pid : pids) {
            result.add(new DigitalObject(pid, null));
        }
        return new SmartGwtResponse<DigitalObject>(result);
    }

    @GET
    @Path(DigitalObjectResourceApi.SEARCH_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Item> search(
            @QueryParam(DigitalObjectResourceApi.SEARCH_OWNER_PARAM) String owner,
            @DefaultValue(SearchType.DEFAULT)
            @QueryParam(DigitalObjectResourceApi.SEARCH_TYPE_PARAM) SearchType type,
            @QueryParam(DigitalObjectResourceApi.SEARCH_PID_PARAM) List<String> pids,
            @QueryParam(DigitalObjectResourceApi.SEARCH_BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.SEARCH_PHRASE_PARAM) String phrase,
            @QueryParam(DigitalObjectResourceApi.SEARCH_QUERY_IDENTIFIER_PARAM) String queryIdentifier,
            @QueryParam(DigitalObjectResourceApi.SEARCH_QUERY_LABEL_PARAM) String queryLabel,
            @QueryParam(DigitalObjectResourceApi.SEARCH_QUERY_MODEL_PARAM) String queryModel,
            @QueryParam(DigitalObjectResourceApi.SEARCH_QUERY_TITLE_PARAM) String queryTitle,
            @QueryParam(DigitalObjectResourceApi.SEARCH_START_ROW_PARAM) int startRow
            ) throws FedoraClientException, IOException {

        SearchView search = RemoteStorage.getInstance(appConfig).getSearch();
        List<Item> items;
        int page = 20;
        switch (type) {
            case LAST_MODIFIED:
                items = search.findLastModified(startRow, queryModel, 100);
                break;
            case QUERY:
                items = search.findQuery(queryTitle, queryLabel, queryIdentifier, owner, queryModel);
                page = 1;
                break;
            case PIDS:
                items = search.find(pids);
                page = 1;
                break;
            case PHRASE:
                items = search.findPhrase(phrase);
                page = 1;
                break;
            case PARENT:
                items = searchParent(batchId, pids, search);
                page = 1;
                break;
            default:
                items = search.findLastCreated(startRow, queryModel);
        }
        int count = items.size();
        int endRow = startRow + count - 1;
        int total = count == 0 ? startRow : endRow + page;
        return new SmartGwtResponse<Item>(SmartGwtResponse.STATUS_SUCCESS, startRow, endRow, total, items);
    }

    private List<Item> searchParent(Integer batchId, List<String> pids, SearchView search)
            throws IOException, FedoraClientException {
        
        if (batchId != null) {
            List<ImportBatch> batches = importManager.find(null, batchId, null);
            if (batches.isEmpty()) {
                return Collections.emptyList();
            } else {
                String parentPid = batches.get(0).getParentPid();
                return search.find(parentPid);
            }
        } else {
            if (pids == null || pids.size() != 1) {
                throw RestException.plainText(Status.BAD_REQUEST, "parent search requires single pid parameter");
            }
            return search.findReferrers(pids.get(0));
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.MEMBERS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Item> findMembers(
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parent,
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ROOT_PARAM) String root
            ) throws FedoraClientException, IOException, DigitalObjectException {

        SearchView search = RemoteStorage.getInstance(appConfig).getSearch();
        List<Item> items;
        String parentPid;
        if (parent == null || "null".equals(parent)) {
            items = search.find(root);
            parentPid = root;
        } else {
            items = search.findSortedChildren(parent);
            parentPid = parent;
        }
        for (Item item : items) {
            item.setParentPid(parentPid);
        }
        return new SmartGwtResponse<Item>(items);
    }

    /**
     * Adds new object members. Members that already exists remain untouched.
     * 
     * @param parentPid PID of parent object
     * @param toAddPids list of PIDs to add; cannot contain parent PID
     * @return list of added members
     */
    @POST
    @Path(DigitalObjectResourceApi.MEMBERS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Item> addMembers(
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parentPid,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PID) List<String> toAddPids
            ) throws IOException, FedoraClientException, DigitalObjectException {

        if (parentPid == null) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT, null);
        }
        if (toAddPids == null || toAddPids.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.MEMBERS_ITEM_PID, null);
        }
        if (toAddPids.contains(parentPid)) {
            throw RestException.plainText(Status.BAD_REQUEST, "parent and pid are same!");
        }

        HashSet<String> toAddPidSet = new HashSet<String>(toAddPids);
        if (toAddPidSet.isEmpty()) {
            return new SmartGwtResponse<Item>(Collections.<Item>emptyList());
        }

        RemoteStorage storage = RemoteStorage.getInstance(appConfig);
        // fetch PID[] -> Item[]
        List<Item> memberSearch = storage.getSearch().find(toAddPids);
        HashMap<String, Item> memberSearchMap = new HashMap<String, Item>(memberSearch.size());
        for (Item item : memberSearch) {
            memberSearchMap.put(item.getPid(), item);
        }

        // check if fetched items corresponds to queried pids
        if (memberSearch.size() != toAddPidSet.size()) {
            ArrayList<String> memberSearchAsPids = new ArrayList<String>(memberSearch.size());
            for (Item item : memberSearch) {
                memberSearchAsPids.add(item.getPid());
            }
            toAddPidSet.removeAll(memberSearchAsPids);
            throw RestException.plainNotFound(
                    DigitalObjectResourceApi.MEMBERS_ITEM_PID, toAddPidSet.toString());
        }
        // load current members
        RemoteObject remote = storage.find(parentPid);
        RelationEditor editor = new RelationEditor(remote);
        List<String> members = editor.getMembers();
        // add new members
        ArrayList<Item> added = new ArrayList<Item>();
        for (String addPid : toAddPids) {
            if (!members.contains(addPid)) {
                members.add(addPid);
                Item item = memberSearchMap.get(addPid);
                if (item == null) {
                    throw RestException.plainNotFound("pid", toAddPidSet.toString());
                }
                item.setParentPid(parentPid);
                added.add(item);
            }
        }
        // write if any change
        if (!added.isEmpty()) {
            editor.setMembers(members);
            editor.write(editor.getLastModified(), session.asFedoraLog());
            remote.flush();
        }
        return new SmartGwtResponse<Item>(added);
    }

    /**
     * Deletes object members from digital object.
     * @param parentPid digital object ID
     * @param toRemovePids member IDs to remove
     * @return list of removed IDs
     */
    @DELETE
    @Path(DigitalObjectResourceApi.MEMBERS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Item> deleteMembers(
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parentPid,
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ITEM_PID) List<String> toRemovePids
            ) throws IOException, DigitalObjectException {

        if (parentPid == null) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing parent parameter!");
        }
        if (toRemovePids == null || toRemovePids.isEmpty()) {
            throw RestException.plainText(Status.BAD_REQUEST, "Missing pid parameter!");
        }
        if (toRemovePids.contains(parentPid)) {
            throw RestException.plainText(Status.BAD_REQUEST, "parent and pid are same!");
        }

        HashSet<String> toRemovePidSet = new HashSet<String>(toRemovePids);
        if (toRemovePidSet.isEmpty()) {
            return new SmartGwtResponse<Item>(Collections.<Item>emptyList());
        }

        RemoteStorage storage = RemoteStorage.getInstance(appConfig);
        RemoteObject remote = storage.find(parentPid);
        RelationEditor editor = new RelationEditor(remote);
        List<String> members = editor.getMembers();
        if (members.removeAll(toRemovePidSet)) {
            editor.setMembers(members);
            editor.write(editor.getLastModified(), session.asFedoraLog());
            remote.flush();
        }

        ArrayList<Item> removed = new ArrayList<Item>(toRemovePidSet.size());
        for (String removePid : toRemovePidSet) {
            Item item = new Item(removePid);
            item.setParentPid(parentPid);
            removed.add(item);
        }

        return new SmartGwtResponse<Item>(removed);
    }
    
    @GET
    @Path(DigitalObjectResourceApi.DC_PATH)
    @Produces(MediaType.APPLICATION_XML)
    public DublinCoreRecord getDublinCore(
            @QueryParam(DigitalObjectResourceApi.DUBLINCORERECORD_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.DUBLINCORERECORD_BATCHID) Integer batchId
            ) throws IOException, DigitalObjectException {

        FedoraObject fobject = findFedoraObject(pid, batchId);
        DcStreamEditor dcEditor = new DcStreamEditor(fobject);
        try {
            DublinCoreRecord dc = dcEditor.read();
            dc.setBatchId(batchId);
            return dc;
        } catch (DigitalObjectNotFoundException ex) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.DC_PATH)
    @Consumes({MediaType.TEXT_XML, MediaType.APPLICATION_XML})
    @Produces(MediaType.APPLICATION_XML)
    public DublinCoreRecord updateDublinCore(DublinCoreRecord update) throws IOException, DigitalObjectException {
        if (update == null || update.getDc() == null) {
            throw new IllegalArgumentException();
        }
        FedoraObject fobject = findFedoraObject(update.getPid(), update.getBatchId(), false);
        DcStreamEditor dcEditor = new DcStreamEditor(fobject);
        dcEditor.write(update, session.asFedoraLog());

        fobject.flush();
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
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public CustomMods<?> getCustomMods(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId
            ) throws IOException, DigitalObjectException {
        
        if (pid == null || pid.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }

        RemoteStorage storage = RemoteStorage.getInstance(appConfig);
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

    @PUT
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_CUSTOM_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public CustomMods<?> updateCustomMods(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMJSONDATA) String customJsonData
            ) throws IOException, DigitalObjectException {

        LOG.fine(String.format("pid: %s, editor: %s, timestamp: %s, json: %s", pid, editorId, timestamp, customJsonData));
        if (pid == null || pid.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
        if (timestamp == null) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.TIMESTAMP_PARAM, pid);
        }
        RemoteStorage storage = RemoteStorage.getInstance(appConfig);
        RemoteObject remote = storage.find(pid);
        ModsStreamEditor modsEditor = new ModsStreamEditor(remote);

        ModsType mods = modsEditor.read();
        Mapping mapping = new Mapping();
        Class<?> type = mapping.getType(editorId);
        ObjectMapper jsMapper = JsonUtils.defaultObjectMapper();
        try {
            Object customData = jsMapper.readValue(customJsonData, type);
            mapping.update(mods, customData, editorId);
            if (LOG.isLoggable(Level.FINE)) {
                String toXml = ModsUtils.toXml(mods, true);
                LOG.fine(toXml);
            }
            modsEditor.write(mods, timestamp, session.asFedoraLog());

            // DC
            String model = new RelationEditor(remote).getModel();
            DcStreamEditor dcEditor = new DcStreamEditor(remote);
            dcEditor.write(mods, model, dcEditor.getLastModified(), session.asFedoraLog());

            String label = ModsUtils.getLabel(mods, model);
            remote.setLabel(label);

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
    @Path(DigitalObjectResourceApi.METAMODEL_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<MetaModel> listModels() {
        Locale locale = session.getLocale(httpHeaders);

        Collection<MetaModel> models = metamodels.find(locale);
        return new SmartGwtResponse<MetaModel>(new ArrayList<MetaModel>(models));
    }

    @GET
    @Path(DigitalObjectResourceApi.PREVIEW_PATH)
    @Produces("*/*")
    public Response getPreview(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
            ) throws IOException, DigitalObjectException {

        return getDissemination(pid, batchId, BinaryEditor.PREVIEW_ID);
    }

    /**
     * Default alias for FULL dissemination.
     *
     * @param pid digital object PID (required)
     * @param batchId import batch ID (optional)
     * @return raw version of the archived object
     */
    @GET
    @Path(DigitalObjectResourceApi.FULL_PATH)
    @Produces("*/*")
    public Response getFull(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
            ) throws IOException, DigitalObjectException {

        return getDissemination(pid, batchId, BinaryEditor.FULL_ID);
    }

    /**
     * Default alias for raw dissemination.
     *
     * @param pid digital object PID (required)
     * @param batchId import batch ID (optional)
     * @return raw version of the archived object
     */
    @GET
    @Path(DigitalObjectResourceApi.RAW_PATH)
    @Produces("*/*")
    public Response getRaw(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
            ) throws IOException, DigitalObjectException {

        return getDissemination(pid, batchId, BinaryEditor.RAW_ID);
    }

    /**
     * Gets digital object dissemination.
     *
     * @param pid PID (required)
     * @param batchId import batch ID (optional)
     * @param dsId data stream ID. If missing the whole digital object is returned as XML.
     * @return digital object dissemination
     * @throws IOException
     */
    @GET
    @Path(DigitalObjectResourceApi.DISSEMINATION_PATH)
    @Produces("*/*")
    public Response getDissemination(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.DISSEMINATION_DATASTREAM) String dsId
            ) throws IOException, DigitalObjectException {

        FedoraObject fobject = findFedoraObject(pid, batchId);
        if (dsId == null) {
            return Response.ok(fobject.asText(), MediaType.TEXT_XML_TYPE)
                    .header("Content-Disposition", "inline; filename=" + pid + ".xml")
                    .build();
        }
        if (fobject instanceof LocalObject) {
            LocalObject lobject = (LocalObject) fobject;
            BinaryEditor loader = BinaryEditor.dissemination(lobject, dsId);
            if (loader == null) {
                RestException.plainNotFound(DigitalObjectResourceApi.DISSEMINATION_DATASTREAM, dsId);
            }
            File entity = loader.read();
            if (entity == null) {
                RestException.plainNotFound("content not found");
            }

            Date lastModification = new Date(loader.getLastModified());
            ResponseBuilder evaluatePreconditions = httpRequest.evaluatePreconditions(lastModification);
            if (evaluatePreconditions != null) {
                return evaluatePreconditions.build();
            }

            return Response.ok(entity, loader.getMimetype())
                    .header("Content-Disposition", "inline; filename=" + entity.getName())
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

    @GET
    @Path(DigitalObjectResourceApi.THUMB_PATH)
    @Produces("image/*")
    public Response getThumbnail(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
            ) throws IOException, DigitalObjectException {

        return getDissemination(pid, batchId, BinaryEditor.THUMB_ID);
    }

    @GET
    @Path(DigitalObjectResourceApi.MODS_PATH + '/' + DigitalObjectResourceApi.MODS_PLAIN_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getModsTxt(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
            ) throws IOException, DigitalObjectException {

        FedoraObject fobject = findFedoraObject(pid, batchId, false);
        ModsStreamEditor meditor = new ModsStreamEditor(fobject);
        try {
            String content = meditor.readAsString();
            StringRecord mods = new StringRecord(content, meditor.getLastModified(), pid);
            mods.setBatchId(batchId);
            return mods;
        } catch (DigitalObjectNotFoundException ex) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.OCR_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getOcr(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
            ) throws IOException, DigitalObjectException {

        FedoraObject fobject = findFedoraObject(pid, batchId);
        StringEditor ocrEditor = StringEditor.ocr(fobject);
        try {
            StringRecord ocr = ocrEditor.readRecord();
            ocr.setBatchId(batchId);
            return ocr;
        } catch (DigitalObjectNotFoundException ex) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.OCR_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord updateOcr(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.STRINGRECORD_CONTENT) String content
            ) throws IOException, DigitalObjectException {

        if (timestamp == null) {
            RestException.plainText(Status.BAD_REQUEST, "Missing timestamp!");
        }
        FedoraObject fobject = findFedoraObject(pid, batchId, false);
        StringEditor ocrEditor = StringEditor.ocr(fobject);
        try {
            ocrEditor.write(content, timestamp, session.asFedoraLog());
            fobject.flush();
            StringRecord result = ocrEditor.readRecord();
            result.setBatchId(batchId);
            return result;
        } catch (DigitalObjectNotFoundException ex) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
    }

    @GET
    @Path(DigitalObjectResourceApi.PRIVATENOTE_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord getPrivateNote(
            @QueryParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId
            ) throws IOException, DigitalObjectException {

        FedoraObject fobject = findFedoraObject(pid, batchId);
        StringEditor editor = StringEditor.privateNote(fobject);
        try {
            StringRecord content = editor.readRecord();
            content.setBatchId(batchId);
            return content;
        } catch (DigitalObjectNotFoundException ex) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }
    }

    @PUT
    @Path(DigitalObjectResourceApi.PRIVATENOTE_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public StringRecord updatePrivateNote(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.TIMESTAMP_PARAM) Long timestamp,
            @FormParam(DigitalObjectResourceApi.STRINGRECORD_CONTENT) String content
            ) throws IOException, DigitalObjectException {

        if (timestamp == null) {
            RestException.plainText(Status.BAD_REQUEST, "Missing timestamp!");
        }
        FedoraObject fobject = findFedoraObject(pid, batchId, false);
        StringEditor editor = StringEditor.privateNote(fobject);
        editor.write(content, timestamp, session.asFedoraLog());
        fobject.flush();
        StringRecord result = editor.readRecord();
        result.setBatchId(batchId);
        return result;
    }

    private FedoraObject findFedoraObject(String pid, Integer batchId) throws IOException {
        return findFedoraObject(pid, batchId, true);
    }

    private FedoraObject findFedoraObject(String pid, Integer batchId, boolean readonly) throws IOException {
        FedoraObject fobject;
        if (batchId != null) {
            ImportItem item = importManager.findItem(pid);
            if (item == null) {
                throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
            }
            if (!readonly) {
                List<ImportBatch> batches = importManager.find(null, batchId, null);
                ImportResource.checkBatchState(batches.get(0));
            }
            fobject = new LocalStorage().load(pid, item.getFoxmlAsFile());
        } else {
            fobject = RemoteStorage.getInstance(appConfig).find(pid);
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
            models.add(new MetaModel(
                    "model:periodical", true, null,
                    "cs".equals(lang) ? "Periodikum" : "Periodical",
                    MetaModelDataSource.EDITOR_PERIODICAL,
                    EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE)
                    ));
            models.add(new MetaModel(
                    "model:periodicalvolume", null, null,
                    "cs".equals(lang) ? "Ročník" : "Periodical Volume",
                    MetaModelDataSource.EDITOR_PERIODICAL_VOLUME,
                    EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE, DatastreamEditorType.PARENT)
                    ));
            models.add(new MetaModel(
                    "model:periodicalitem", null, null,
                    "cs".equals(lang) ? "Výtisk" : "Periodical Item",
                    MetaModelDataSource.EDITOR_PERIODICAL_ISSUE,
                    EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE, DatastreamEditorType.PARENT)
                    ));
            models.add(new MetaModel(
                    "model:monograph", true, null,
                    "cs".equals(lang) ? "Monografie" : "Monograph",
                    MetaModelDataSource.EDITOR_MONOGRAPH,
                    EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE)
                    ));
            models.add(new MetaModel(
                    "model:monographunit", null, null,
                    "cs".equals(lang) ? "Monografie - volná část" : "Monograph Unit",
                    MetaModelDataSource.EDITOR_MONOGRAPH_UNIT,
                    EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE, DatastreamEditorType.PARENT)
                    ));
            models.add(new MetaModel(
                    "model:page", null, true,
                    "cs".equals(lang) ? "Strana" : "Page",
                    MetaModelDataSource.EDITOR_PAGE,
                    EnumSet.allOf(DatastreamEditorType.class)
                    ));

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

    @XmlRootElement(name = DigitalObjectResourceApi.CUSTOMMODS_ELEMENT)
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class CustomMods<T> {

        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_PID)
        private String pid;
        @XmlElement(name = DigitalObjectResourceApi.TIMESTAMP_PARAM)
        private long timestamp;
        @XmlElement(name = DigitalObjectResourceApi.MODS_CUSTOM_EDITORID)
        private String editor;
        @XmlElement(name = DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMJSONDATA)
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
        
        @XmlElement(name = DigitalObjectResourceApi.METAMODEL_PID_PARAM,
                type = String.class, required = true)
        private Object pid;
        @XmlElement(name = DigitalObjectResourceApi.METAMODEL_ROOT_PARAM)
        private Boolean root;
        @XmlElement(name = DigitalObjectResourceApi.METAMODEL_LEAF_PARAM)
        private Boolean leaf;
        @XmlElement(name = DigitalObjectResourceApi.METAMODEL_DISPLAYNAME_PARAM)
        private String displayName;
        @XmlElement(name = DigitalObjectResourceApi.METAMODEL_MODSCUSTOMEDITORID_PARAM)
        private String modsCustomEditor;
        @XmlElement(name = DigitalObjectResourceApi.METAMODEL_DATASTREAMEDITOR_PARAM)
        private EnumSet<DatastreamEditorType> dataStreamEditors;

        private MetaModel() {
        }

        public MetaModel(Object pid, Boolean root, Boolean leaf, String displayName) {
            this(pid, root, leaf, displayName, null, null);
        }

        public MetaModel(Object pid, Boolean root, Boolean leaf, String displayName,
                String modsCustomEditor, EnumSet<DatastreamEditorType> dataStreamEditors) {

            this.pid = pid;
            this.root = root;
            this.leaf = leaf;
            this.displayName = displayName;
            this.modsCustomEditor = modsCustomEditor;
            this.dataStreamEditors = dataStreamEditors;
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

        public String getModsCustomEditor() {
            return modsCustomEditor;
        }

        public Set<DatastreamEditorType> getDataStreamEditors() {
            return dataStreamEditors;
        }

    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class DigitalObject {
        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_PID)
        private String pid;
        @XmlElement(name = DigitalObjectResourceApi.DIGITALOBJECT_MODEL)
        private String model;

        public DigitalObject(String pid, String model) {
            this.pid = pid;
            this.model = model;
        }

        public DigitalObject() {
        }
    }
}
