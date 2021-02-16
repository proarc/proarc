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
package cz.cas.lib.proarc.common.fedora;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.client.request.RiSearch;
import com.yourmediashelf.fedora.client.response.FindObjectsResponse;
import com.yourmediashelf.fedora.client.response.RiSearchResponse;
import cz.cas.lib.proarc.common.fedora.RemoteStorage.RemoteObject;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.fedora.relation.RelationResource;
import cz.cas.lib.proarc.common.json.JsonUtils;
import cz.cas.lib.proarc.common.object.HasDataHandler;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Scanner;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Implements search queries with ITQL.
 *
 * <p>It will require an interface to implement an alternative search that
 * does not support ITQL.
 *
 * @author Jan Pokorsky
 */
public final class SearchView {

    private static final Logger LOG = Logger.getLogger(SearchView.class.getName());

    private static final String QUERY_LAST_CREATED = readQuery("lastCreated.itql");
    private static final String QUERY_LAST_CREATED_WITHOUT_EXTENSION = readQuery("lastCreatedWithoutExtension.itql");
    private static final String QUERY_COUNT_MODELS = readQuery("countModels.itql");
    private static final String QUERY_FIND_BY_MODEL = readQuery("findByModel.itql");
    private static final String QUERY_FIND_BY_MODELS = readQuery("findByModels.itql");
    private static final String QUERY_FIND_MEMBERS = readQuery("findMembers.itql");
    private static final String QUERY_FIND_MEMBER_HIERARCHY = readQuery("findMemberHierarchy.itql");
    private static final String QUERY_FIND_PIDS = readQuery("findPids.itql");
    private static final String QUERY_FIND_PIDS_WITHOUT_EXTENSION = readQuery("findPidsWithoutExtension.itql");
    private static final String QUERY_FIND_REFERRERS = readQuery("findReferrers.itql");
    private static final String QUERY_FIND_DEVICE_REFERRERS = readQuery("findDeviceReferrers.itql");
    private static final String QUERY_FIND_ALL_OBJECTS = readQuery("findAll.itql");
    private static final String QUERY_ADVANCED_SEARCH = readQuery("advancedSearch.itql");
    private static final String QUERY_ADVANCED_SEARCH_WITHOUT_EXTENSION = readQuery("advancedSearchWithoutExtension.itql");
    private static final String QUERY_ADVANCED_SEARCH_COUNT = readQuery("advancedSearchCount.itql");
    private static final String QUERY_ADVANCED_SEARCH_COUNT_WITHOUT_EXTENSION = readQuery("advancedSearchCountWithoutExtension.itql");

    private final FedoraClient fedora;
    private final int maxLimit;
    private final RemoteStorage storage;
    private Locale locale = Locale.ENGLISH;
    private ObjectMapper mapper;

    SearchView(RemoteStorage storage) {
        this(storage, Integer.MAX_VALUE);
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
     * @see #findQuery(cz.cas.lib.proarc.common.fedora.SearchView.Query, String)
     */
    public List<Item> findQuery(String title, String label, String identifier, String owner, String model, Collection<String> hasOwners)
            throws FedoraClientException, IOException {

        return findQuery(new Query().setTitle(title).setLabel(label)
                .setIdentifier(identifier).setOwner(owner).setModel(model)
                .setHasOwners(hasOwners), "active");
    }

    /**
     * Finds objects matching passed fields using the Fedora Basic Search.
     * Matching objects are filtered with {@link #find(java.lang.String[]) }
     * to return only ProArc objects.
     *
     * @return limited list of objects.
     * @see <a href='https://wiki.duraspace.org/display/FEDORA35/Basic+Search'>Fedora Basic Search</a>
     */
    public List<Item> findQuery(Query q, String status)
            throws FedoraClientException, IOException {

        boolean active =  "active".equals(status) ? true : false;
        final int objectsLimit = 80;
        StringBuilder query = new StringBuilder();
        if (q.getModel() != null && !q.getModel().isEmpty()) {
        //    query.append("type~").append(translateModels(q.getModel()));
        }
        // FedoraClient.findObjects() does not support OR operator!
        if (!q.getHasOwners().isEmpty()) {
            query.append(" rights~").append(q.getHasOwners().iterator().next());
        }
        buildQuery(query, "title", q.getTitle());
        buildQuery(query, "label", q.getLabel());
        buildQuery(query, "identifier", q.getIdentifier());
        buildQuery(query, "ownerId", q.getOwner());
        buildQuery(query, "creator", q.getCreator());
        if (!active) {
            buildQuery(query, "State", "D");
        }
        final String queryString = query.toString().trim();
        LOG.fine(queryString);
        FindObjectsResponse response = FedoraClient.findObjects().query(queryString).resultFormat("xml")
                .pid()
                .maxResults(objectsLimit)
                .execute(fedora);
        List<String> pids = response.getPids();
        if (LOG.isLoggable(Level.FINE)) {
            LOG.fine("pids count: " + pids.size() + ", token: " + response.getToken() + ", pids: " + pids.toString());
        }

        List<Item> result = new ArrayList<Item>();
        while (!pids.isEmpty()) {
            List<Item> items = new ArrayList<>();
            if (active) {
                items = find(true, q.getModel(), pids.toArray(new String[pids.size()]));
            } else {
                items = find(false, q.getModel(), pids.toArray(new String[pids.size()]));
            }
            result.addAll(items);
            String token = response.getToken();
            if (token == null || result.size() + objectsLimit > maxLimit) {
                break;
            }
            response = FedoraClient.findObjects().query(queryString).resultFormat("xml").pid()
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
        List<Item> result = new ArrayList<Item>();
        while (!pids.isEmpty()) {
            List<Item> items = find(true, pids.toArray(new String[pids.size()]));
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

    static StringBuilder buildQuery(StringBuilder builder, String field, String value) {
        if (value == null || value.isEmpty()) {
            return builder;
        }
        // remove leading and trailing white spaces and asterisks
        value = value.replaceAll("^[\\s\\*]+|[\\s\\*]+$", "");
        // Fedora query does not accept "'" char and does not allow to escape special chars *, ?
        value = value.replaceAll("['*]", "?");
        if (!value.isEmpty() && !"*".equals(value)) {
            value = "'*" + value + "*'";
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

    public List<Item> find(String pid) throws FedoraClientException, IOException {
        return find(true, null, Arrays.asList(pid));
    }

    public List<Item> find(String... pids) throws FedoraClientException, IOException {
        return find(true, null, Arrays.asList(pids));
    }

    public List<Item> find(String model, String... pids) throws FedoraClientException, IOException {
        return find(true, model, Arrays.asList(pids));
    }

    public List<Item> find(boolean onlyActive, String... pids) throws FedoraClientException, IOException {
        return find(onlyActive, null, Arrays.asList(pids));
    }

    public List<Item> find(boolean onlyActive, String model, String... pids) throws FedoraClientException, IOException {
        return find(onlyActive, model, Arrays.asList(pids));
    }

    /**
     * Finds active descriptors of passed PIDs.
     *
     * @param pids PIDs of digital objects
     * @return list of descriptors
     * @throws FedoraClientException
     * @throws IOException
     */
    public List<Item> find(List<String> pids) throws FedoraClientException, IOException {
        return find(true, null, pids);
    }

    /**
     * Finds active descriptors of passed PIDs.
     *
     * @param pids PIDs of digital objects
     * @return list of descriptors
     * @throws FedoraClientException
     * @throws IOException
     */
    public List<Item> find(boolean onlyActive, List<String> pids) throws FedoraClientException, IOException {
        return find(onlyActive, null, pids);
    }

    /**
     * Finds descriptors of passed PIDs.
     *
     * @param pids PIDs of digital objects
     * @param onlyActive {@code true} includes only active objects
     * @return list of descriptors
     * @throws FedoraClientException
     * @throws IOException
     */
    public List<Item> find(boolean onlyActive, String model, List<String> pids) throws FedoraClientException, IOException {
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
            List<Item> membersFull = findImpl(pids, onlyActive, model);
            List<Item> members = new ArrayList<>();
            if (membersFull.size() > endOffset) {
                members = membersFull.subList(startOffset, endOffset);
            } else {
                members.addAll(membersFull);
            }
            repairItemsModel(members);
            startOffset = endOffset;
            result.addAll(members);
        }
        return result;
    }

    private void repairItemsModel(List<Item> members) {
        for (Item item : members) {
            if (item.getOrganization() != null) {
                item.setOrganization(item.getOrganization().substring(12));
            }
            if (item.getUser() != null) {
                item.setUser(item.getUser().substring(12));
            }
            if (item.getStatus() != null) {
                item.setStatus(item.getStatus().substring(12));
            }
        }
    }

    List<Item> findImpl(List<String> pids, boolean onlyActive, String model) throws FedoraClientException, IOException {
        if (pids.isEmpty()) {
            return Collections.emptyList();
        }
        String modelFilter = "";
        if (model != null && !model.isEmpty()) {
            modelFilter = String.format("and        $pid     <info:fedora/fedora-system:def/model#hasModel>        <info:fedora/%s>", model);
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
        query = query.replace("${pids.model}", modelFilter);

        String onlyActiveExpr = onlyActive
                ? "and        $pid     <info:fedora/fedora-system:def/model#state>"
                        + "           <info:fedora/fedora-system:def/model#Active>"
                : "";
        query = query.replace("${includeActive}", onlyActiveExpr);

        LOG.fine(query);
        RiSearch search = buildSearch(query);
        List<SearchView.Item> list = consumeSearch(search.execute(fedora));
        if (list.size() == 0) {
            String queryWithoutExtension = QUERY_FIND_PIDS_WITHOUT_EXTENSION.replace("${pids.expression}", expr);
            queryWithoutExtension = queryWithoutExtension.replace("${pids.model}", modelFilter);
            String onlyActiveExprWithoutExtension = onlyActive
                    ? "and        $pid     <info:fedora/fedora-system:def/model#state>"
                    + "           <info:fedora/fedora-system:def/model#Active>"
                    : "";
            queryWithoutExtension = queryWithoutExtension.replace("${includeActive}", onlyActiveExprWithoutExtension);

            LOG.fine(queryWithoutExtension);
            RiSearch searchWithoutExtension = buildSearch(queryWithoutExtension);
            list = consumeSearch(searchWithoutExtension.execute(fedora));
        }
        return list;

    }

    /**
     * Finds children of the passed remote object. The result list is sorted
     * using RELS-EXT stream.
     * 
     * @param parentPid PID of parent to query
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

    public List<Item> findLastCreated(int offset, String model, String user, Boolean filterWithoutExtension, String sort) throws FedoraClientException, IOException {
        return findLastCreated(offset, model, user, null, null, filterWithoutExtension, Integer.MAX_VALUE, sort);
    }
    
    public List<Item> findLastCreated(int offset, String model, String user, String organization, String username, Boolean filterWithoutExtension, int limit, String sort) throws FedoraClientException, IOException {
        return findLast(offset, model, user, organization, username, filterWithoutExtension, limit, "$created " + sort);
    }

    public List<Item> findAlphabetical(int offset, String model, String user, String organization, String userName, Boolean filterWithoutExtension, int limit, String sort) throws IOException, FedoraClientException {
        return findLast(offset, model, user, organization, userName, filterWithoutExtension, limit, "$label " + sort);
    }

    public List<Item> findLastModified(int offset, String model, String user, String organization, String username, Boolean filterWithoutExtension, int limit, String sort) throws FedoraClientException, IOException {
        return findLast(offset, model, user, organization, username, filterWithoutExtension, limit, "$modified " + sort);
    }

    public List<Item> findAdvancedSearchItems(String identifier, String label, String owner, String status, String organization, String processor, String model, String creator, Boolean allowAllForProcessor, Boolean filterWithoutExtension, String sortField, String sort, int offset, int limit) throws IOException, FedoraClientException {
        sortField = createSortField(sortField);
        sort = createSort(sort);
        if (label == null && identifier == null && owner == null && creator == null) {
            return findAdvancedObjects(null, status, organization, processor, model, allowAllForProcessor, filterWithoutExtension, sortField + " " + sort, offset, limit);
        }
        List<String> pids = findAdvancedPids(new Query().setLabel(label).setIdentifier(identifier).setModel(model).setCreator(creator).setOwner(owner));
        return findAdvancedObjects(pids, status, organization, processor, model, allowAllForProcessor, filterWithoutExtension,sortField + " " + sort, offset, limit);
    }


    public int findAdvancedSearchCount(String identifier, String label, String owner, String status, String organization, String processor, String model, String creator, Boolean allowAllForProcessor, Boolean filterWithoutExtension) throws FedoraClientException, IOException {
        if (label == null && identifier == null && owner == null && creator == null) {
            return findAdvancedCountObjects(null, status, organization, processor, model, allowAllForProcessor, filterWithoutExtension);
        }
        List<String> pids = findAdvancedPids(new Query().setLabel(label).setIdentifier(identifier).setModel(model).setCreator(creator).setOwner(owner));
        return findAdvancedCountObjects(pids, status, organization, processor, model, allowAllForProcessor, filterWithoutExtension);
    }

    private List<String> findAdvancedPids(Query q) throws FedoraClientException {
        List<String> objectsPid = new ArrayList<>();
        List<String> pids = new ArrayList<>();
        StringBuilder query = new StringBuilder();
        if (q.getModel() != null && !q.getModel().isEmpty()) {
            //query.append("type~").append(q.getModel());
        }
        buildQuery(query, "label", q.getLabel());
        buildQuery(query, "ownerId", q.getOwner());
        buildQuery(query, "creator", q.getCreator());
        buildQuery(query, "identifier", q.getIdentifier());

        final String queryString = query.toString().trim();
        LOG.fine(queryString);

        FindObjectsResponse response = FedoraClient.findObjects().query(queryString).resultFormat("xml").pid().maxResults(100).execute(fedora);
        pids.addAll(response.getPids());
        objectsPid.addAll(pids);

        while (!pids.isEmpty()) {
            String token = response.getToken();
            if (token == null) {
                break;
            }
            response =FedoraClient.findObjects().query(queryString).resultFormat("xml").pid().maxResults(100).sessionToken(token).execute(fedora);
            pids = response.getPids();
            objectsPid.addAll(pids);
        }
        return objectsPid;

    }


    private List<Item> findAdvancedObjects(List<String> pids, String status, String organization, String processor, String model, Boolean allowAllForProcessor, Boolean filterWithoutExtension, String orderBy, int offset, int limit) throws FedoraClientException, IOException {
        String statusFilter = "";
        String organizationFilter = "";
        String processorFilter = "";
        String pidsFilter = "";
        String modelFilter = "";

        StringBuilder expr = new StringBuilder();
        if (pids != null) {
            for (String pid : pids) {
                if (expr.length() > 0) {
                    expr.append("\n or ");
                }
                expr.append(String.format("$pid <http://mulgara.org/mulgara#is> <info:fedora/%s>", pid));
            }
            if (pids.size() > 0) {
                pidsFilter = "and (" + expr.toString() + ")";
            }
        }

        if (model != null && !model.isEmpty()) {
            modelFilter = String.format("and        $pid     <info:fedora/fedora-system:def/model#hasModel>        <info:fedora/%s>", model);
        }
        if (organization != null && !organization.isEmpty()) {
            organizationFilter = String.format("and       $pid      <http://proarc.lib.cas.cz/relations#organization>    <info:fedora/%s>", organization);
        }
        if (processor != null && !processor.isEmpty()) {
            processorFilter = String.format("and       ($pid      <http://proarc.lib.cas.cz/relations#user>    <info:fedora/%s>", processor);
            if (allowAllForProcessor) {
                processorFilter += "\n or        $pid      <http://proarc.lib.cas.cz/relations#user>    <info:fedora/all>)";
            } else {
                processorFilter +=")";
            }
        }
        if (status != null && !status.isEmpty()) {
            statusFilter = String.format("and   $pid    <http://proarc.lib.cas.cz/relations#status>     <info:fedora/%s>", status);
        }
        String query = QUERY_ADVANCED_SEARCH.replace("${OFFSET}", String.valueOf(offset));
        query = query.replace("${MODEL_FILTER}", modelFilter);
        query = query.replace("${ORGANIZATION_FILTER}", organizationFilter);
        query = query.replace("${PROCESSOR_FILTER}", processorFilter);
        query = query.replace("${STATUS_FILTER}", statusFilter);
        query = query.replace("${PIDS_FILTER}", pidsFilter);

        query = query.replace("${ORDERBY}", orderBy);

        LOG.fine(query);

        RiSearch search = buildSearch(query);
        if (limit > 0) {
            limit = Math.min(limit, maxLimit);
            search.limit(limit);
        }
        if (pids != null && pids.size() == 0) {
            return new ArrayList<Item>();
        } else {
            List<SearchView.Item> list = consumeSearch(search.execute(fedora));
            if (list.size() == 0 && !filterWithoutExtension) {
                String queryWithoutExtension = QUERY_ADVANCED_SEARCH_WITHOUT_EXTENSION.replace("${OFFSET}", String.valueOf(offset));
                queryWithoutExtension = queryWithoutExtension.replace("${MODEL_FILTER}", modelFilter);
                queryWithoutExtension = queryWithoutExtension.replace("${ORGANIZATION_FILTER}", organizationFilter);
                queryWithoutExtension = queryWithoutExtension.replace("${PROCESSOR_FILTER}", processorFilter);
                queryWithoutExtension = queryWithoutExtension.replace("${STATUS_FILTER}", statusFilter);
                queryWithoutExtension = queryWithoutExtension.replace("${PIDS_FILTER}", pidsFilter);
                queryWithoutExtension = queryWithoutExtension.replace("${ORDERBY}", orderBy);

                LOG.fine(queryWithoutExtension);

                RiSearch searchWithoutExtension = buildSearch(queryWithoutExtension);
                if (limit > 0) {
                    limit = Math.min(limit, maxLimit);
                    searchWithoutExtension.limit(limit);
                }
                list = consumeSearch(searchWithoutExtension.execute(fedora));
            }
            return list;
        }
    }

    private int findAdvancedCountObjects(List<String> pids, String status, String organization, String processor, String model, Boolean allowAllForProcessor, Boolean filterWithoutExtension) throws FedoraClientException, IOException {
        String statusFilter = "";
        String organizationFilter = "";
        String processorFilter = "";
        String pidsFilter = "";
        String modelFilter = "";

        StringBuilder expr = new StringBuilder();

        if (pids != null) {
            if (pids.size() < 1) {
                return 0;
            } else if (pids.size() > 0) {
                for (String pid : pids) {
                    if (expr.length() > 0) {
                        expr.append("\n or ");
                    }
                    expr.append(String.format("$pid <http://mulgara.org/mulgara#is> <info:fedora/%s>", pid));
                }
                if (pids.size() > 0) {
                    pidsFilter = "and (" + expr.toString() + ")";
                }
            }
        }

        if (model != null && !model.isEmpty()) {
            modelFilter = String.format("and        $pid     <info:fedora/fedora-system:def/model#hasModel>        <info:fedora/%s>", model);
        }
        if (organization != null && !organization.isEmpty()) {
            organizationFilter = String.format("and       $pid      <http://proarc.lib.cas.cz/relations#organization>    <info:fedora/%s>", organization);
        }
        if (processor != null && !processor.isEmpty()) {
            processorFilter = String.format("and       ($pid      <http://proarc.lib.cas.cz/relations#user>    <info:fedora/%s>", processor);
            if (allowAllForProcessor) {
                processorFilter += "\n or        $pid      <http://proarc.lib.cas.cz/relations#user>    <info:fedora/all>)";
            } else {
                processorFilter +=")";
            }
        }
        if (status != null && !status.isEmpty()) {
            statusFilter = String.format("and   $pid    <http://proarc.lib.cas.cz/relations#status>     <info:fedora/%s>", status);
        }
        String query = QUERY_ADVANCED_SEARCH_COUNT;
        query = query.replace("${MODEL_FILTER}", modelFilter);
        query = query.replace("${ORGANIZATION_FILTER}", organizationFilter);
        query = query.replace("${PROCESSOR_FILTER}", processorFilter);
        query = query.replace("${STATUS_FILTER}", statusFilter);
        query = query.replace("${PIDS_FILTER}", pidsFilter);


        LOG.fine(query);

        RiSearch search = buildSearch(query);
        List<Item> items = consumeSearch(search.execute(fedora));
        if (items.size() == 0 && !filterWithoutExtension) {
            String queryWithoutExtension = QUERY_ADVANCED_SEARCH_COUNT_WITHOUT_EXTENSION;
            queryWithoutExtension = queryWithoutExtension.replace("${MODEL_FILTER}", modelFilter);
            queryWithoutExtension = queryWithoutExtension.replace("${ORGANIZATION_FILTER}", organizationFilter);
            queryWithoutExtension = queryWithoutExtension.replace("${PROCESSOR_FILTER}", processorFilter);
            queryWithoutExtension = queryWithoutExtension.replace("${STATUS_FILTER}", statusFilter);
            queryWithoutExtension = queryWithoutExtension.replace("${PIDS_FILTER}", pidsFilter);

            LOG.fine(queryWithoutExtension);
            RiSearch searchWithoutExtension = buildSearch(queryWithoutExtension);
            items = consumeSearch(searchWithoutExtension.execute(fedora));
        }
        return items.size();
    }

    private String createSort(String sort) {
        if (sort != null && sort.equalsIgnoreCase("desc")) {
            return "desc";
        } else {
            return "asc";
        }
    }

    private String createSortField(String sortField) {
        if (sortField == null || sortField.isEmpty()) {
            return "$label";
        } else {
            switch(sortField) {
                case "created":
                    return "$created";
                case "label":
                    return "$label";
                case "pid":
                    return "$pid";
                case "model":
                    return "$model";
                case "owner":
                    return "$owner";
                case "state":
                    return "$state";
                case "organization":
                    return "$organization";
                case "user":
                    return "$user";
                case "status":
                    return "$status";
                case "modified":
                    return "$modified";
                default:
                    return "$label";
            }
        }
    }

    private List<Item> findLast(int offset, String model, String user, String organization, String username, Boolean filterWithoutExtension, int limit, String orderBy) throws FedoraClientException, IOException {
        String modelFilter = "";
        String ownerFilter = "";
        String organizationFilter = "";
        String userFilter = "";
        if (model != null && !model.isEmpty()) {
            modelFilter = String.format("and        $pid     <info:fedora/fedora-system:def/model#hasModel>        <info:fedora/%s>", model);
        }
        if (user != null) {
            ownerFilter = String.format("and        $pid     <http://proarc.lib.cas.cz/relations#hasOwner>        $group\n"
                    + "and        <info:fedora/%s>     <info:fedora/fedora-system:def/relations-external#isMemberOf>        $group", user);
        }
        if (organization != null) {
            organizationFilter = String.format("and       $pid      <http://proarc.lib.cas.cz/relations#organization>    <info:fedora/%s>", organization);
        }
        if (username != null) {
            userFilter = String.format("and       ($pid      <http://proarc.lib.cas.cz/relations#user>    <info:fedora/%s>\n"
                    + "or        $pid      <http://proarc.lib.cas.cz/relations#user>    <info:fedora/all>)", username);
        }
        String query = QUERY_LAST_CREATED.replace("${OFFSET}", String.valueOf(offset));
        query = query.replace("${MODEL_FILTER}", modelFilter);
        query = query.replace("${OWNER_FILTER}", ownerFilter);
        query = query.replace("${ORDERBY}", orderBy);
        query = query.replace("${ORGANIZATION}", organizationFilter);
        query = query.replace("${USERNAME}", userFilter);
        LOG.fine(query);
        RiSearch search = buildSearch(query);

        if (limit > 0) {
            limit = Math.min(limit, maxLimit);
            search.limit(limit);
        }
        List<SearchView.Item> items = consumeSearch(search.execute(fedora));
        if (items.size() == 0 && !filterWithoutExtension) {
            String queryWithoutExtension = QUERY_LAST_CREATED.replace("${OFFSET}", String.valueOf(offset));
            queryWithoutExtension = queryWithoutExtension.replace("${MODEL_FILTER}", modelFilter);
            queryWithoutExtension = queryWithoutExtension.replace("${OWNER_FILTER}", ownerFilter);
            queryWithoutExtension = queryWithoutExtension.replace("${ORDERBY}", orderBy);
            queryWithoutExtension = queryWithoutExtension.replace("${ORGANIZATION}", organizationFilter);
            queryWithoutExtension = queryWithoutExtension.replace("${USERNAME}", userFilter);
            LOG.fine(queryWithoutExtension);
            RiSearch searchWithoutExtension = buildSearch(queryWithoutExtension);

            if (limit > 0) {
                limit = Math.min(limit, maxLimit);
                searchWithoutExtension.limit(limit);
            }
            items = consumeSearch(searchWithoutExtension.execute(fedora));
        }
        return items;
    }

    public List<Item> countModels(String model, String user, String organization, String username, Boolean filterWithoutExtension) throws FedoraClientException, IOException {
        String modelFilter = "";
        String ownerFilter = "";
        String organizationFilter = "";
        String userFilter = "";
        if (model != null && !model.isEmpty()) {
            modelFilter = String.format("and        $pid     <info:fedora/fedora-system:def/model#hasModel>        <info:fedora/%s>", model);
        }
        if (user != null) {
            ownerFilter = String.format("and        $pid     <http://proarc.lib.cas.cz/relations#hasOwner>        $group\n"
                    + "and        <info:fedora/%s>     <info:fedora/fedora-system:def/relations-external#isMemberOf>        $group", user);
        }
        if (organization != null) {
            organizationFilter = String.format("and       $pid      <http://proarc.lib.cas.cz/relations#organization>    <info:fedora/%s>", organization);
        }
        if (username != null) {
            userFilter = String.format("and       ($pid      <http://proarc.lib.cas.cz/relations#user>    <info:fedora/%s>\n"
                    + "or        $pid      <http://proarc.lib.cas.cz/relations#user>    <info:fedora/all>)", username);
        }
        String query = QUERY_COUNT_MODELS;
        query = query.replace("${MODEL_FILTER}", modelFilter);
        query = query.replace("${OWNER_FILTER}", ownerFilter);
        query = query.replace("${ORGANIZATION}", organizationFilter);
        query = query.replace("${USERNAME}", userFilter);
        LOG.fine(query);
        RiSearch search = buildSearch(query);

        List<SearchView.Item> items = consumeSearch(search.execute(fedora));
        if (items.size() == 0 && !filterWithoutExtension) {
            String queryWithoutExtension = QUERY_COUNT_MODELS;
            queryWithoutExtension = queryWithoutExtension.replace("${MODEL_FILTER}", modelFilter);
            queryWithoutExtension = queryWithoutExtension.replace("${OWNER_FILTER}", ownerFilter);
            queryWithoutExtension = queryWithoutExtension.replace("${ORGANIZATION}", organizationFilter);
            queryWithoutExtension = queryWithoutExtension.replace("${USERNAME}", userFilter);
            LOG.fine(queryWithoutExtension);
            RiSearch searchWithoutExtension = buildSearch(queryWithoutExtension);
            items = consumeSearch(searchWithoutExtension.execute(fedora));
        }
        return items;
    }

    public List<Item> findAllObjects() throws FedoraClientException, IOException {
        String query = QUERY_FIND_ALL_OBJECTS;
        LOG.fine(query);
        RiSearch search = buildSearch(query);
        return consumeSearch(search.execute(fedora));
    }

    public List<Item> findReferrers(String pid) throws IOException, FedoraClientException {
        String query = QUERY_FIND_REFERRERS.replace("${PID}", pid);
        RiSearch search = buildSearch(query);
        return consumeSearch(search.execute(fedora));
    }

    public List<Item> findByModel(String modelId) throws  IOException, FedoraClientException {
        return findByModel(0, modelId);
    }

    public List<Item> findByModel(int offset, String modelId) throws IOException, FedoraClientException {
        return findByModel(offset, modelId, "$created desc");
    }

    public List<Item> findByModel(int offset, String modelId, String orderBy) throws IOException, FedoraClientException {
        String query = QUERY_FIND_BY_MODEL;
        query = query.replace("${metaModelPid}", modelId);
        query = query.replace("${OFFSET}", String.valueOf(offset));
        query = query.replace("${ORDERBY}", orderBy);
        RiSearch search = buildSearch(query);
        search.limit(1000);
        return consumeSearch(search.execute(fedora));
    }

    /**
     * Find objects that have the given model.
     * @param modelId1 model PID to query
     * @param modelId2 model PID to query
     * @return list of objects
     * @throws IOException
     * @throws FedoraClientException
     */
    public List<Item> findByModels(String modelId1, String modelId2) throws  IOException, FedoraClientException {
        return findByModel(0, modelId1, modelId2);
    }

    public List<Item> findByModels(int offset, String modelId1, String modelId2) throws IOException, FedoraClientException {
        return findByModels(offset, modelId1, modelId2, "$created desc");
    }

    public List<Item> findByModels(int offset, String modelId1, String modelId2, String orderBy) throws IOException, FedoraClientException {
        String query = QUERY_FIND_BY_MODELS;
        query = query.replace("${metaModelPid1}", modelId1);
        query = query.replace("${metaModelPid2}", modelId2);
        query = query.replace("${OFFSET}", String.valueOf(offset));
        query = query.replace("${ORDERBY}", orderBy);
        RiSearch search = buildSearch(query);
        search.limit(1000);
        return consumeSearch(search.execute(fedora));
    }

    /**
     * Is the device referred with {@code hasDevice} relation by any digital object?
     * @param deviceId device PID
     * @return {@code true} if it is connected
     * @throws IOException
     * @throws FedoraClientException
     */
    public boolean isDeviceInUse(String deviceId) throws IOException, FedoraClientException {
        String query = QUERY_FIND_DEVICE_REFERRERS.replace("${devicePid}", deviceId);
        RiSearch search = buildSearch(query);
        search.limit(1);
        search.stream(true);
        List<Item> result = consumeSearch(search.execute(fedora));
        return !result.isEmpty();
    }

    private List<Item> consumeSearch(RiSearchResponse response) throws IOException {
        String json = response.getEntity(String.class);
        Result result = readResponse(json);
        return consumeSearch(result.results);
    }

    Result readResponse(String json) throws IOException {
        if (mapper == null) {
            // requires mapper without mix in annotation of Item
            mapper = JsonUtils.createObjectMapper();
        }
        return  mapper.readValue(json, Result.class);
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
        // XXX implement a plugin cache
        MetaModel model = MetaModelRepository.getInstance().find(item.getModel());
        if (model == null) {
            // other than digital object model (device, ...)
            return ;
        }
        HasSearchViewHandler hasHandler = model.getPlugin().getHandlerProvider(HasSearchViewHandler.class);
        if (hasHandler != null) {
            String label = hasHandler.createSearchViewHandler().getObjectLabel(item, locale);
            item.setLabel(label);
        }
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

    /**
     * A plug-in capability.
     */
    public interface HasSearchViewHandler extends HasDataHandler {
        SearchViewHandler createSearchViewHandler();
    }

    /**
     * Implement to customize a result label of a search.
     */
    public interface SearchViewHandler {
        String getObjectLabel(Item item, Locale locale);
    }

    public static class Item {

        private String pid;
        private String model;
        private String owner;
        private String label;
        private String state;
        private String created;
        private String modified;
        /** Parent PID. Optional for some queries */
        private String parent;
        /** batch import ID. Optional for some queries */
        private Integer batchId;
        private String organization;
        private String user;
        private String status;
        /**
         * Synthetic name of count query. count(hasExport)
         * @see <a href='http://docs.mulgara.org/itqlcommands/select.html#o194'>
         *      Count Function</a>
         */
        private String k0;
        private String k1;
        private String k2;
        private String k3;
        private String k4;

        public Item() {
        }

        public Item(String pid) {
            this.pid = pid;
        }

        public String getOrganization() {
            return organization;
        }

        public void setOrganization(String organization) {
            this.organization = organization;
        }

        public String getUser() {
            return user;
        }

        public void setUser(String user) {
            this.user = user;
        }

        public String getStatus() {
            return status;
        }

        public void setStatus(String status) {
            this.status = status;
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

        public String getK0() {
            return k0;
        }

        public void setK0(String k0) {
            this.k0 = k0;
        }

        public Integer getHasExport() {
            if (k0 != null && !k0.isEmpty()) {
                try {
                    return Integer.parseInt(k0);
                } catch (NumberFormatException ex) {
                    // ignore
                }
            }
            return null;
        }

        public String getK1() {
            return k1;
        }

        public void setK1(String k1) {
            this.k1 = k1;
        }

        public Integer getHasNdkExport() {
            if (k1 != null && !k1.isEmpty()) {
                try {
                    return Integer.parseInt(k1);
                } catch (NumberFormatException ex) {
                    // ignore
                }
            }
            return null;
        }

        public String getK2() {
            return k2;
        }

        public void setK2(String k2) {
            this.k2 = k2;
        }

        public Integer getHasKrameriusExport() {
            if (k2 != null && !k2.isEmpty()) {
                try {
                    return Integer.parseInt(k2);
                } catch (NumberFormatException ex) {
                    // ignore
                }
            }
            return null;
        }

        public String getK3() {
            return k3;
        }

        public void setK3(String k3) {
            this.k3 = k3;
        }

        public Integer getHasArchiveExport() {
            if (k3 != null && !k3.isEmpty()) {
                try {
                    return Integer.parseInt(k3);
                } catch (NumberFormatException ex) {
                    // ignore
                }
            }
            return null;
        }

        public String getK4() {
            return k4;
        }

        public void setK4(String k4) {
            this.k4 = k4;
        }

        public Integer getHasCrossrefExport() {
            if (k4 != null && !k4.isEmpty()) {
                try {
                    return Integer.parseInt(k4);
                } catch (NumberFormatException ex) {
                    // ignore
                }
            }
            return null;
        }

    }

    static class Result {

        private List<Item> results;

        public List<Item> getResults() {
            return results;
        }

        public void setResults(List<Item> results) {
            this.results = results;
        }

    }

    public static class Query {

        private String title;
        private String creator;
        private String label;
        private String identifier;
        private String owner;
        private String model;
        private String status;
        private Collection<String> hasOwners;

        public String getOrganization() {
            return organization;
        }

        public Query setOrganization(String organization) {
            this.organization = organization;
            return this;
        }

        private String organization;

        public String getTitle() {
            return title;
        }

        public Query setTitle(String title) {
            this.title = title;
            return this;
        }

        public String getCreator() {
            return creator;
        }

        public Query setCreator(String creator) {
            this.creator = creator;
            return this;
        }

        public String getLabel() {
            return label;
        }

        public Query setLabel(String label) {
            this.label = label;
            return this;
        }

        public String getIdentifier() {
            return identifier;
        }

        public Query setIdentifier(String identifier) {
            this.identifier = identifier;
            return this;
        }

        public String getOwner() {
            return owner;
        }

        public Query setOwner(String owner) {
            this.owner = owner;
            return this;
        }

        public String getModel() {
            return model;
        }

        public Query setModel(String model) {
            this.model = model;
            return this;
        }

        public Collection<String> getHasOwners() {
            return hasOwners != null ? hasOwners : Collections.<String>emptyList();
        }

        public Query setHasOwners(Collection<String> hasOwners) {
            this.hasOwners = hasOwners;
            return this;
        }

        public String getStatus() {
            return status;
        }

        public Query setStatus(String status) {
            this.status = status;
            return this;
        }
    }

}
