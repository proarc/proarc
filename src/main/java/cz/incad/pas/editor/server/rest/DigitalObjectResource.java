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
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor.DublinCoreRecord;
import cz.cas.lib.proarc.common.fedora.AtmEditor;
import cz.cas.lib.proarc.common.fedora.AtmEditor.AtmItem;
import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.LocalStorage;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.PurgeFedoraObject;
import cz.cas.lib.proarc.common.fedora.PurgeFedoraObject.PurgeException;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.common.fedora.StringEditor;
import cz.cas.lib.proarc.common.fedora.StringEditor.StringRecord;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.json.JsonUtils;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.Mapping;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
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
import java.util.Map;
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
     * @param parentPid optional PID of parent object to link the newly created object
     * @param mods MODS XML used to create new object; optional
     * @return
     * @throws URISyntaxException
     * @throws IOException
     */
    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Item> newObject(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_MODEL) String modelId,
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) String pid,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parentPid,
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
                    UUID uuid = UUID.fromString(FoxmlUtils.pidAsUuid(pid));
                    pid = FoxmlUtils.pidFromUuid(uuid.toString());
                }
            } catch (IllegalArgumentException e) {
                invalid = true;
            }
            if (invalid) {
                return SmartGwtResponse.<Item>asError().error(
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

        // attach to parent object
        RemoteObject parentObject = null;
        if (parentPid != null) {
            parentObject = fedora.find(parentPid);
            RelationEditor parentRelsExt = new RelationEditor(parentObject);
            List<String> members = parentRelsExt.getMembers();
            members.add(localObject.getPid());
            parentRelsExt.setMembers(members);
            parentRelsExt.write(parentRelsExt.getLastModified(), session.asFedoraLog());
        }

        try {
            fedora.ingest(localObject, user.getUserName(), session.asFedoraLog());
            if (parentObject != null) {
                parentObject.flush();
            }
        } catch (FedoraClientException ex) {
            // XXX hack: Fedora server does not notify existing object conflict with HTTP 409.
            // Workaround parses error message.
            // Check for existence before ingest would be insufficient as Fedora does not yet support transactions.
            String message = ex.getMessage();
            if (message != null && message.contains("org.fcrepo.server.errors.ObjectExistsException")) {
                return SmartGwtResponse.<Item>asError().error("pid", "Object already exists!").build();
            }
            throw ex;
        }
        Item item = new Item(localObject.getPid());
        item.setLabel(localObject.getLabel());
        item.setModel(modelId);
        item.setOwner(localObject.getOwner());
        item.setParentPid(parentPid);
        return new SmartGwtResponse<Item>(item);
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

        Locale locale = session.getLocale(httpHeaders);
        SearchView search = RemoteStorage.getInstance(appConfig).getSearch(locale);
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
            Batch batch = importManager.get(batchId);
            String parentPid = batch == null ? null : batch.getParentPid();
            if (parentPid == null) {
                return Collections.emptyList();
            } else {
                return search.find(parentPid);
            }
        } else {
            if (pids == null || pids.size() != 1) {
                throw RestException.plainText(Status.BAD_REQUEST, "parent search requires single pid parameter");
            }
            return search.findReferrers(pids.get(0));
        }
    }

    /**
     * Gets members of a digital object.
     * @param parent PID of digital object to query its members. {@code root} parameter is ignored.
     * @param root PID of digital object to return itself as a member with {@link Item#parent} as {@code null}.
     *          Useful to show root of the member hierarchy.
     * @return ordered list of members
     */
    @GET
    @Path(DigitalObjectResourceApi.MEMBERS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Item> findMembers(
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parent,
            @QueryParam(DigitalObjectResourceApi.MEMBERS_ROOT_PARAM) String root
            ) throws FedoraClientException, IOException, DigitalObjectException {

        SearchView search = RemoteStorage.getInstance(appConfig).getSearch(session.getLocale(httpHeaders));
        List<Item> items;
        String parentPid;
        if (parent == null || "null".equals(parent)) {
            items = search.find(root);
            parentPid = null;
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
     * Sets new member sequence of given parent digital object.
     *
     * @param parentPid parent PID
     * @param batchId batch import ID
     * @param toSetPids list of member PIDS
     * @return ordered list of members
     * @throws RestException
     */
    @PUT
    @Path(DigitalObjectResourceApi.MEMBERS_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<Item> setMembers(
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT) String parentPid,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID) Integer batchId,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_PID) List<String> toSetPids
            // XXX long timestamp
            ) throws IOException, FedoraClientException, DigitalObjectException {

        if (batchId == null && parentPid == null) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.MEMBERS_ITEM_PARENT, null);
        }
        boolean batchImportMembers = batchId != null;
        if (toSetPids == null || toSetPids.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.MEMBERS_ITEM_PID, null);
        }
        if (!batchImportMembers && toSetPids.contains(parentPid)) {
            throw RestException.plainText(Status.BAD_REQUEST, "parent and pid are same!");
        }

        HashSet<String> toSetPidSet = new HashSet<String>(toSetPids);
        if (toSetPidSet.size() != toSetPids.size()) {
            throw RestException.plainText(Status.BAD_REQUEST, "duplicates in PIDs to set!\n" + toSetPids.toString());
        }

        // fetch PID[] -> Item[]
        Map<String, Item> memberSearchMap;
        if (batchImportMembers) {
            memberSearchMap = loadLocalSearchItems(batchId);
        } else {
            memberSearchMap = loadSearchItems(toSetPidSet);
        }
        checkSetMembers(toSetPidSet, memberSearchMap);
        // load current members
        FedoraObject fobject = findFedoraObject(parentPid, batchId, false);
        RelationEditor editor = new RelationEditor(fobject);
        List<String> members = editor.getMembers();
        members.clear();
        // add new members
        ArrayList<Item> added = new ArrayList<Item>();
        for (String addPid : toSetPids) {
            if (!members.contains(addPid)) {
                members.add(addPid);
                Item item = memberSearchMap.get(addPid);
                if (item == null) {
                    throw RestException.plainNotFound(DigitalObjectResourceApi.MEMBERS_ITEM_PID,
                            toSetPids.toString());
                }
                item.setParentPid(parentPid);
                added.add(item);
            }
        }
        editor.setMembers(members);
        editor.write(editor.getLastModified(), session.asFedoraLog());
        fobject.flush();
        return new SmartGwtResponse<Item>(added);
    }

    private Map<String, Item> loadSearchItems(Set<String> pids) throws IOException, FedoraClientException {
        RemoteStorage storage = RemoteStorage.getInstance(appConfig);
        HashMap<String, Item> memberSearchMap = new HashMap<String, Item>();
        List<Item> memberSearch = storage.getSearch().find(new ArrayList<String>(pids));
        for (Item item : memberSearch) {
            memberSearchMap.put(item.getPid(), item);
        }
        return memberSearchMap;
    }

    private Map<String, Item> loadLocalSearchItems(int batchId) throws IOException, DigitalObjectException {
        HashMap<String, Item> memberSearchMap = new HashMap<String, Item>();
        List<BatchItemObject> batchObjects = importManager.findBatchObjects(batchId, null);
        for (BatchItemObject batchObject : batchObjects) {
            if (batchObject.getState() != ObjectState.LOADED) {
                continue;
            }
            LocalObject lfo = (LocalObject) findFedoraObject(batchObject.getPid(), batchId);
            Item item = new Item(batchObject.getPid());
            item.setBatchId(batchId);
            item.setLabel(lfo.getLabel());
            item.setOwner(lfo.getOwner());
            RelationEditor relationEditor = new RelationEditor(lfo);
            item.setModel(relationEditor.getModel());
            memberSearchMap.put(batchObject.getPid(), item);
        }
        return memberSearchMap;
    }

    private void checkSetMembers(Set<String> pids, Map<String, Item> memberSearchMap) throws IllegalStateException {
        if (!pids.equals(memberSearchMap.keySet())) {
            HashSet<String> notMembers = new HashSet<String>(pids);
            notMembers.removeAll(memberSearchMap.keySet());
            HashSet<String> missingPids = new HashSet<String>(memberSearchMap.keySet());
            missingPids.removeAll(pids);
            throw new IllegalStateException("PIDs not members: " + notMembers.toString()
                    + "\nMissing PIDs: " + missingPids.toString());
        }
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

        Locale locale = session.getLocale(httpHeaders);
        HashSet<String> toAddPidSet = new HashSet<String>(toAddPids);
        if (toAddPidSet.isEmpty()) {
            return new SmartGwtResponse<Item>(Collections.<Item>emptyList());
        }

        RemoteStorage storage = RemoteStorage.getInstance(appConfig);
        // fetch PID[] -> Item[]
        List<Item> memberSearch = storage.getSearch(locale).find(toAddPids);
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
        // check that PIDs being removed are members of parent object
        HashSet<String> toRemovePidSetCopy = new HashSet<String>(toRemovePidSet);
        toRemovePidSetCopy.removeAll(members);
        if (!toRemovePidSetCopy.isEmpty()) {
            String msg = String.format("Parent: %s does not contain members: %s",
                    parentPid, toRemovePidSetCopy.toString());
            throw RestException.plainText(Status.BAD_REQUEST, msg);
        }
        // remove
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
            @QueryParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @QueryParam(DigitalObjectResourceApi.MODS_CUSTOM_EDITORID) String editorId
            ) throws IOException, DigitalObjectException {
        
        if (pid == null || pid.isEmpty()) {
            throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
        }

        FedoraObject fobject = findFedoraObject(pid, batchId);
        ModsStreamEditor modsEditor = new ModsStreamEditor(fobject);

        ModsType mods = modsEditor.read();
        Mapping mapping = new Mapping();
        Object customData = mapping.read(mods, editorId);
        CustomMods<Object> result = new CustomMods<Object>();
        result.setPid(pid);
        result.setBatchId(batchId);
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
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
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
        FedoraObject fobject = findFedoraObject(pid, batchId);
        ModsStreamEditor modsEditor = new ModsStreamEditor(fobject);

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
            String model = new RelationEditor(fobject).getModel();
            DcStreamEditor dcEditor = new DcStreamEditor(fobject);
            dcEditor.write(mods, model, dcEditor.getLastModified(), session.asFedoraLog());

            String label = ModsUtils.getLabel(mods, model);
            fobject.setLabel(label);

            fobject.flush();

            CustomMods<Object> result = new CustomMods<Object>();
            result.setPid(pid);
            result.setBatchId(batchId);
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

    /**
     * Gets digital object administration and technical data.
     *
     * @param pid PID (required)
     * @param batchId import batch ID (optional)
     * @return digital object dissemination
     * @throws IOException
     */
    @GET
    @Path(DigitalObjectResourceApi.ATM_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<AtmItem> getAtm(
            @QueryParam(DigitalObjectResourceApi.ATM_ITEM_PID) String pid,
            @QueryParam(DigitalObjectResourceApi.ATM_ITEM_BATCHID) Integer batchId
            ) throws IOException, DigitalObjectException, FedoraClientException {

        if (pid == null) {
            return new SmartGwtResponse<AtmItem>();
        }
        FedoraObject fobject = findFedoraObject(pid, batchId);
        Locale locale = session.getLocale(httpHeaders);
        RemoteStorage storage = RemoteStorage.getInstance(appConfig);
        AtmEditor editor = new AtmEditor(fobject, storage.getSearch(locale));
        AtmItem atm = editor.read();
        atm.setBatchId(batchId);
        return new SmartGwtResponse<AtmItem>(atm);
    }

    @PUT
    @Path(DigitalObjectResourceApi.ATM_PATH)
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<AtmItem> updateAtm(
            @FormParam(DigitalObjectResourceApi.DIGITALOBJECT_PID) Set<String> pids,
            @FormParam(DigitalObjectResourceApi.BATCHID_PARAM) Integer batchId,
            @FormParam(DigitalObjectResourceApi.MEMBERS_ITEM_OWNER) String owner,
            @FormParam(DigitalObjectResourceApi.ATM_ITEM_DEVICE) String deviceId
            ) throws IOException, DigitalObjectException {

        ArrayList<AtmItem> result = new ArrayList<AtmItem>(pids.size());
        Locale locale = session.getLocale(httpHeaders);
        RemoteStorage storage = RemoteStorage.getInstance(appConfig);
        SearchView search = storage.getSearch(locale);
        for (String pid : pids) {
            FedoraObject fobject = findFedoraObject(pid, batchId);
            AtmEditor editor = new AtmEditor(fobject, search);
            editor.write(deviceId, session.asFedoraLog());
            fobject.flush();
            AtmItem atm = editor.read();
            atm.setBatchId(batchId);
            result.add(atm);
        }
        return new SmartGwtResponse<AtmItem>(result);
    }

    private FedoraObject findFedoraObject(String pid, Integer batchId) throws IOException {
        return findFedoraObject(pid, batchId, true);
    }

    private FedoraObject findFedoraObject(String pid, Integer batchId, boolean readonly) throws IOException {
        FedoraObject fobject;
        if (batchId != null) {
            Batch batch = importManager.get(batchId);
            if (batch == null) {
                throw RestException.plainNotFound(DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID, String.valueOf(batchId));
            }
            if (!readonly) {
                ImportResource.checkBatchState(batch);
            }
            if (pid == null || ImportBatchManager.ROOT_ITEM_PID.equals(pid)) {
                fobject = importManager.getRootObject(batch);
            } else {
                BatchItemObject item = importManager.findBatchObject(batchId, pid);
                if (item == null) {
                    throw RestException.plainNotFound(DigitalObjectResourceApi.DIGITALOBJECT_PID, pid);
                }
                fobject = new LocalStorage().load(pid, item.getFile());
            }
        } else {
            if (pid == null) {
                throw new NullPointerException("pid");
            }
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
                    EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                            DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM)
                    ));
            models.add(new MetaModel(
                    "model:periodicalvolume", null, null,
                    "cs".equals(lang) ? "Ročník" : "Periodical Volume",
                    MetaModelDataSource.EDITOR_PERIODICAL_VOLUME,
                    EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                            DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                            DatastreamEditorType.ATM)
                    ));
            models.add(new MetaModel(
                    "model:periodicalitem", null, null,
                    "cs".equals(lang) ? "Výtisk" : "Periodical Item",
                    MetaModelDataSource.EDITOR_PERIODICAL_ISSUE,
                    EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                            DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                            DatastreamEditorType.ATM)
                    ));
            models.add(new MetaModel(
                    "model:monograph", true, null,
                    "cs".equals(lang) ? "Monografie" : "Monograph",
                    MetaModelDataSource.EDITOR_MONOGRAPH,
                    EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                            DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM)
                    ));
            models.add(new MetaModel(
                    "model:monographunit", null, null,
                    "cs".equals(lang) ? "Monografie - volná část" : "Monograph Unit",
                    MetaModelDataSource.EDITOR_MONOGRAPH_UNIT,
                    EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                            DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                            DatastreamEditorType.ATM)
                    ));
            models.add(new MetaModel(
                    "model:page", null, true,
                    "cs".equals(lang) ? "Strana" : "Page",
                    MetaModelDataSource.EDITOR_PAGE,
                    EnumSet.complementOf(EnumSet.of(DatastreamEditorType.CHILDREN))
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
        @XmlElement(name = DigitalObjectResourceApi.BATCHID_PARAM)
        private Integer batchId;
        @XmlElement(name = DigitalObjectResourceApi.TIMESTAMP_PARAM)
        private long timestamp;
        @XmlElement(name = DigitalObjectResourceApi.MODS_CUSTOM_EDITORID)
        private String editor;
        @XmlElement(name = DigitalObjectResourceApi.MODS_CUSTOM_CUSTOMJSONDATA)
        private T data;

        public CustomMods() {
        }

        public Integer getBatchId() {
            return batchId;
        }

        public void setBatchId(Integer batchId) {
            this.batchId = batchId;
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
        private String pid;
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

        public MetaModel(String pid, Boolean root, Boolean leaf, String displayName) {
            this(pid, root, leaf, displayName, null, null);
        }

        public MetaModel(String pid, Boolean root, Boolean leaf, String displayName,
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

        public String getPid() {
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
