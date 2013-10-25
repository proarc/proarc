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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.server.fedora;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.client.request.RiSearch;
import com.yourmediashelf.fedora.client.response.FindObjectsResponse;
import com.yourmediashelf.fedora.client.response.RiSearchResponse;
import cz.incad.pas.editor.server.fedora.RemoteStorage.RemoteObject;
import cz.incad.pas.editor.server.fedora.relation.RelationEditor;
import cz.incad.pas.editor.server.fedora.relation.RelationResource;
import cz.incad.pas.editor.server.json.JsonUtils;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Scanner;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import org.codehaus.jackson.map.ObjectMapper;

/**
 *
 * @author Jan Pokorsky
 */
public final class SearchView {

    private static final Logger LOG = Logger.getLogger(SearchView.class.getName());

    private static final String QUERY_LAST_CREATED = readQuery("lastCreated.itql");
    private static final String QUERY_FIND_MEMBERS = readQuery("findMembers.itql");
    private static final String QUERY_FIND_MEMBER_HIERARCHY = readQuery("findMemberHierarchy.itql");
    private static final String QUERY_FIND_PIDS = readQuery("findPids.itql");
    private static final String QUERY_FIND_REFERRERS = readQuery("findReferrers.itql");

    private final FedoraClient fedora;
    private final int maxLimit;
    private final RemoteStorage storage;
    private Locale locale = Locale.ENGLISH;

    SearchView(RemoteStorage storage) {
        this(storage, 100);
    }

    SearchView(RemoteStorage storage, int maxLimit) {
        this.storage = storage;
        this.fedora = storage.getClient();
        this.maxLimit = maxLimit;
    }

    public void setLocale(Locale locale) {
        if (locale == null) {
            throw new NullPointerException("locale");
        }
        this.locale = locale;
    }

    /**
     * Finds objects matching passed fields using the Fedora Basic Search.
     * Matching objects are filtered with {@link #find(java.lang.String[]) }
     * to return only ProArc objects.
     *
     * @return limited list of objects.
     * @see <a href='https://wiki.duraspace.org/display/FEDORA35/Basic+Search'>Fedora Basic Search</a>
     */
    public List<Item> findQuery(String title, String label, String identifier, String owner, String model)
            throws FedoraClientException, IOException {
        
        final int objectsLimit = 80;
        StringBuilder query = new StringBuilder();
        if (model != null && !model.isEmpty()) {
            query.append("type~").append(model);
        }
        buildQuery(query, "title", title);
        buildQuery(query, "label", label);
        buildQuery(query, "identifier", identifier);
        buildQuery(query, "ownerId", owner);
        FindObjectsResponse response = FedoraClient.findObjects().query(query.toString()).resultFormat("xml")
                .pid()
                .maxResults(objectsLimit)
                .execute(fedora);
        List<String> pids = response.getPids();
        if (LOG.isLoggable(Level.FINE)) {
            LOG.fine("pids count: " + pids.size() + ", token: " + response.getToken() + ", pids: " + pids.toString());
        }

        List<Item> result = new ArrayList<Item>(maxLimit);
        while (!pids.isEmpty()) {
            List<Item> items = find(pids.toArray(new String[pids.size()]));
            result.addAll(items);
            String token = response.getToken();
            if (token == null || result.size() + objectsLimit > maxLimit) {
                break;
            }
            response = FedoraClient.findObjects().query(query.toString()).resultFormat("xml").pid()
                    .maxResults(objectsLimit).sessionToken(token)
                    .execute(fedora);
            pids = response.getPids();
            if (LOG.isLoggable(Level.FINE)) {
                LOG.fine("resumed: pids count: " + pids.size() + ", token: " + response.getToken() + ", pids: " + pids.toString());
            }
        }
        return result;
    }

    /**
     * Finds objects matching passed phrase using the Fedora Basic Search.
     * Matching objects are filtered with {@link #find(java.lang.String[]) }
     * to return only ProArc objects.
     *
     * @param phrase phrase to search in any field of the object
     * @return limited list of objects.
     * @see <a href='https://wiki.duraspace.org/display/FEDORA35/Basic+Search'>Fedora Basic Search</a>
     */
    public List<Item> findPhrase(String phrase) throws FedoraClientException, IOException {
        final int objectsLimit = 80;
        phrase = normalizePhrase(phrase);
        FindObjectsResponse response = FedoraClient.findObjects().terms(phrase).resultFormat("xml")
                .pid()
                .maxResults(objectsLimit)
                .execute(fedora);
        List<String> pids = response.getPids();
        if (LOG.isLoggable(Level.FINE)) {
            LOG.fine("pids count: " + pids.size() + ", token: " + response.getToken() + ", pids: " + pids.toString());
        }
        List<Item> result = new ArrayList<Item>(maxLimit);
        while (!pids.isEmpty()) {
            List<Item> items = find(pids.toArray(new String[pids.size()]));
            result.addAll(items);
            String token = response.getToken();
            if (token == null || result.size() + objectsLimit > maxLimit) {
                break;
            }
            response = FedoraClient.findObjects().terms(phrase).resultFormat("xml").pid()
                    .maxResults(objectsLimit).sessionToken(token)
                    .execute(fedora);
            pids = response.getPids();
            if (LOG.isLoggable(Level.FINE)) {
                LOG.fine("resumed: pids count: " + pids.size() + ", token: " + response.getToken() + ", pids: " + pids.toString());
            }
        }
        return result;
    }

    private static StringBuilder buildQuery(StringBuilder builder, String field, String value) {
        value = normalizePhrase(value);
        if (!"*".equals(value)) {
            if (builder.length() > 0) {
                builder.append(' ');
            }
            builder.append(field).append('~').append(value);
        }
        return builder;
    }

    /**
     * Removes superfluous chars a and optimizes phrase to match the most relevant records.
     * <p/>For ITQL it trims leading and trailing whitespaces and asterisks
     * and wraps the result with asterisks.
     */
    static String normalizePhrase(String phrase) {
        phrase = phrase == null ? "" : phrase;
        phrase = phrase.replaceAll("^[\\s\\*]+|[\\s\\*]+$", "");
        phrase = phrase.isEmpty() ? "*" : "*" + phrase + "*";
        return phrase;
    }

    public List<Item> find(String... pids) throws FedoraClientException, IOException {
        return find(Arrays.asList(pids));
    }

    /**
     * Finds descriptors of passed PIDs.
     *
     * @param pids PIDs of digital objects
     * @return list of descriptors
     * @throws FedoraClientException
     * @throws IOException
     */
    public List<Item> find(List<String> pids) throws FedoraClientException, IOException {
        // issue 85: reasonable count of PIDs per query to prevent StackOverflowError.
        // Greater query page sizes (>1000, <2000) are acceptable but Mulgara responses are really slow.
        // It does not make sence to add paging to API as load on demand of SmartGWT Tree
        // does not support paging and it is not expected to have monograph or
        // issue page counts grater than 10000.
        final int queryPageSize = 100;
        final int size = pids.size();
        ArrayList<Item> result = new ArrayList<Item>(size);
        for (int startOffset = 0; startOffset < size; ) {
            int endOffset = Math.min(size, startOffset + queryPageSize);
            List<String> subList = pids.subList(startOffset, endOffset);
            List<Item> members = findImpl(subList);
            startOffset = endOffset;
            result.addAll(members);
        }
        return result;
    }

    List<Item> findImpl(List<String> pids) throws FedoraClientException, IOException {
        if (pids.isEmpty()) {
            return Collections.emptyList();
        }
        StringBuilder expr = new StringBuilder(256);
        for (String pid : pids) {
            if (expr.length() > 0) {
                expr.append("\n  or ");
            }
            expr.append(String.format(
                    "$pid <http://mulgara.org/mulgara#is> <info:fedora/%s>",
                    pid));
        }
        String query = QUERY_FIND_PIDS.replace("${pids.expression}", expr);
        LOG.fine(query);
        RiSearch search = buildSearch(query);
        return consumeSearch(search.execute(fedora));
    }

    /**
     * Finds children of the passed remote object. The result list is sorted
     * using RELS-EXT stream.
     * 
     * @param parent PID of parent to query
     * @return the sorted list
     * @throws FedoraClientException
     * @throws IOException
     */
    public List<Item> findSortedChildren(String parentPid)
            throws FedoraClientException, IOException, DigitalObjectException {
        
        RemoteObject parent = storage.find(parentPid);
        List<String> memberPids = new RelationEditor(parent).getMembers();
        List<Item> items = find(memberPids);
        ArrayList<Item> sortedItems = new ArrayList<Item>(memberPids.size());
        for (String memberPid : memberPids) {
            for (Iterator<Item> it = items.iterator(); it.hasNext();) {
                Item item = it.next();
                if (memberPid.equals(item.getPid())) {
                    sortedItems.add(item);
                    it.remove();
                    break;
                }
            }
        }
        return sortedItems;
    }

    public List<Item> findChildren(String pid) throws FedoraClientException, IOException {
        String query = QUERY_FIND_MEMBERS.replace("${parent}", RelationResource.fromPid(pid).getResource());
        RiSearch search = buildSearch(query);
        return consumeSearch(search.execute(fedora));
    }

    /**
     * Traverses a graph of PID's members.
     *
     * @param pid PID to traverse
     * @return list of all PID's members
     */
    public List<Item> findChildrenHierarchy(String pid) throws FedoraClientException, IOException {
        String query = QUERY_FIND_MEMBER_HIERARCHY.replace("${ROOT}", RelationResource.fromPid(pid).getResource());
        RiSearch search = buildSearch(query);
        return consumeSearch(search.execute(fedora));
    }

    public List<Item> findLastCreated(int offset, String model) throws FedoraClientException, IOException {
        return findLastCreated(offset, model, 100);
    }
    
    public List<Item> findLastCreated(int offset, String model, int limit) throws FedoraClientException, IOException {
        return findLast(offset, model, limit, "$created desc");
    }

    public List<Item> findLastModified(int offset, String model, int limit) throws FedoraClientException, IOException {
        return findLast(offset, model, limit, "$modified desc");
    }

    private List<Item> findLast(int offset, String model, int limit, String orderBy) throws FedoraClientException, IOException {
        String modelFilter = "";
        if (model != null && !model.isEmpty()) {
            modelFilter = String.format("and        $pid     <info:fedora/fedora-system:def/model#hasModel>        <info:fedora/%s>", model);
        }
        String query = QUERY_LAST_CREATED.replace("${OFFSET}", String.valueOf(offset));
        query = query.replace("${MODEL_FILTER}", modelFilter);
        query = query.replace("${ORDERBY}", orderBy);
        LOG.fine(query);
        RiSearch search = buildSearch(query);

        if (limit > 0) {
            limit = Math.min(limit, maxLimit);
            search.limit(limit);
        }
        return consumeSearch(search.execute(fedora));
    }

    public List<Item> findReferrers(String pid) throws IOException, FedoraClientException {
        String query = QUERY_FIND_REFERRERS.replace("${PID}", pid);
        RiSearch search = buildSearch(query);
        return consumeSearch(search.execute(fedora));
    }

    private List<Item> consumeSearch(RiSearchResponse response) throws IOException {
        String json = response.getEntity(String.class);
        ObjectMapper om = JsonUtils.defaultObjectMapper();
        Result result = om.readValue(json, Result.class);
        return consumeSearch(result.results);
    }

    private List<Item> consumeSearch(List<Item> items) {
        for (Item item : items) {
            replaceUriWithPid(item);
            resolveObjectLabel(item);
        }
        return items;
    }
    
    private static String replaceUriWithPid(String uri) {
        return uri == null ? uri : RelationResource.toPid(uri);
    }

    private static Item replaceUriWithPid(Item item) {
        item.pid = replaceUriWithPid(item.pid);
        item.model = replaceUriWithPid(item.model);
        item.state = replaceUriWithPid(item.state);
        return item;
    }

    void resolveObjectLabel(Item item) {
        String label = resolveObjectLabel(item.getLabel(), item.getModel(), locale);
        item.setLabel(label);
    }

    private static String resolveObjectLabel(String label, String model, Locale locale) {
        if ("model:page".equals(model)) {
            label = PageView.resolveFedoraObjectLabel(label, locale);
        }
        return label;
    }

    private static RiSearch buildSearch(String query) {
        RiSearch search = FedoraClient.riSearch(query).distinct(true)
                .type("tuples").lang("itql")
                .flush(true) // required to get reliable responses
//                .format("json")
                .xParam("format", "json");
        return search;
    }

    private static String readQuery(String file) {
        InputStream is = SearchView.class.getResourceAsStream(file);
        if (is == null) {
            throw new IllegalStateException(file + " not found!");
        }
        Scanner scanner = new Scanner(is, "UTF-8").useDelimiter("\\A");
        String content = scanner.next();
        scanner.close();
        return content;
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Item {

        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_PID)
        private String pid;
        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_MODEL)
        private String model;
        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_OWNER)
        private String owner;
        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_LABEL)
        private String label;
        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_STATE)
        private String state;
        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_CREATED)
        private String created;
        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_MODIFIED)
        private String modified;
        /** Parent PID. Optional for some queries */
        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_PARENT)
        private String parent;
        /** batch import ID. Optional for some queries */
        @XmlElement(name = DigitalObjectResourceApi.MEMBERS_ITEM_BATCHID)
        private Integer batchId;

        public Item() {
        }

        public Item(String pid) {
            this.pid = pid;
        }

        public String getCreated() {
            return created;
        }

        public void setCreated(String created) {
            this.created = created;
        }

        public String getLabel() {
            return label;
        }

        public void setLabel(String label) {
            this.label = label;
        }

        public String getModel() {
            return model;
        }

        public void setModel(String model) {
            this.model = model;
        }

        public String getModified() {
            return modified;
        }

        public void setModified(String modified) {
            this.modified = modified;
        }

        public String getOwner() {
            return owner;
        }

        public void setOwner(String owner) {
            this.owner = owner;
        }

        public String getPid() {
            return pid;
        }

        public void setPid(String pid) {
            this.pid = pid;
        }

        public String getState() {
            return state;
        }

        public void setState(String state) {
            this.state = state;
        }

        public String getParentPid() {
            return parent;
        }

        public void setParentPid(String parentPid) {
            this.parent = parentPid;
        }

        public Integer getBatchId() {
            return batchId;
        }

        public void setBatchId(Integer batchId) {
            this.batchId = batchId;
        }

    }

    @XmlAccessorType(XmlAccessType.FIELD)
    static class Result {

        private List<Item> results;

    }

}
