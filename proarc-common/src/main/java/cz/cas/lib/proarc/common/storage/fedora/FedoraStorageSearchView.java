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
package cz.cas.lib.proarc.common.storage.fedora;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.client.request.RiSearch;
import com.yourmediashelf.fedora.client.response.FindObjectsResponse;
import com.yourmediashelf.fedora.client.response.RiSearchResponse;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.SearchViewQuery;
import cz.cas.lib.proarc.common.storage.akubra.SolrUtils;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage.RemoteObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.storage.relation.RelationResource;
import cz.cas.lib.proarc.common.json.JsonUtils;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
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

import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.isAudioPage;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.isPage;

/**
 * Implements search queries with ITQL.
 *
 * <p>It will require an interface to implement an alternative search that
 * does not support ITQL.
 *
 * @author Jan Pokorsky
 */
public final class FedoraStorageSearchView extends SearchView {

    private static final Logger LOG = Logger.getLogger(FedoraStorageSearchView.class.getName());

    private static final String QUERY_LAST_CREATED = readQuery("lastCreated.itql");
    private static final String QUERY_LAST_CREATED_WITHOUT_EXTENSION = readQuery("lastCreatedWithoutExtension.itql");
    private static final String QUERY_COUNT_MODELS = readQuery("countModels.itql");
    private static final String QUERY_COUNT_MODELS_WITHOUT_EXTENSION = readQuery("countModelsWithoutExtension.itql");
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
    private final FedoraStorage storage;
    private Locale locale = Locale.ENGLISH;
    private ObjectMapper mapper;

    FedoraStorageSearchView(FedoraStorage storage) {
        this(storage, Integer.MAX_VALUE);
    }

    FedoraStorageSearchView(FedoraStorage storage, int maxLimit) {
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

    @Override
    public List<SearchViewItem> findQuery(String title, String label, String identifier, String owner, String model, Collection<String> hasOwners)
            throws FedoraClientException, IOException {

        return findQuery(new SearchViewQuery().setTitle(title).setLabel(label)
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
    @Override
    public List<SearchViewItem> findQuery(SearchViewQuery q, String status)
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

        List<SearchViewItem> result = new ArrayList<SearchViewItem>();
        while (!pids.isEmpty()) {
            List<SearchViewItem> items = new ArrayList<>();
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
    @Override
    public List<SearchViewItem> findPhrase(String phrase, String status, String organization, String processor, String model, String sortField, String sort, int offset, int limit) throws IOException, FedoraClientException {
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
        List<SearchViewItem> result = new ArrayList<SearchViewItem>();
        sortField = createSortField(sortField);
        sort = createSort(sort);
        while (!pids.isEmpty()) {
            List<SearchViewItem> items = findAdvancedObjects(pids, status, organization, processor, model,sortField + " " + sort, offset, limit);
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

    @Override
    public List<SearchViewItem> find(String pid) throws FedoraClientException, IOException {
        return find(true, null, Arrays.asList(pid));
    }

    @Override
    public List<SearchViewItem> find(String... pids) throws FedoraClientException, IOException {
        return find(true, null, Arrays.asList(pids));
    }

    public List<SearchViewItem> find(String model, String... pids) throws FedoraClientException, IOException {
        return find(true, model, Arrays.asList(pids));
    }

    public List<SearchViewItem> find(boolean onlyActive, String... pids) throws FedoraClientException, IOException {
        return find(onlyActive, null, Arrays.asList(pids));
    }

    public List<SearchViewItem> find(boolean onlyActive, String model, String... pids) throws FedoraClientException, IOException {
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
    @Override
    public List<SearchViewItem> find(List<String> pids) throws FedoraClientException, IOException {
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
    @Override
    public List<SearchViewItem> find(boolean onlyActive, List<String> pids) throws FedoraClientException, IOException {
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
    public List<SearchViewItem> find(boolean onlyActive, String model, List<String> pids) throws FedoraClientException, IOException {
        // issue 85: reasonable count of PIDs per query to prevent StackOverflowError.
        // Greater query page sizes (>1000, <2000) are acceptable but Mulgara responses are really slow.
        // It does not make sence to add paging to API as load on demand of SmartGWT Tree
        // does not support paging and it is not expected to have monograph or
        // issue page counts grater than 10000.
        final int queryPageSize = 100;
        final int size = pids.size();
        ArrayList<SearchViewItem> result = new ArrayList<SearchViewItem>(size);
        for (int startOffset = 0; startOffset < size; ) {
            int endOffset = Math.min(size, startOffset + queryPageSize);
            List<SearchViewItem> membersFull = findImpl(pids, onlyActive, model);
            List<SearchViewItem> members = new ArrayList<>();
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

    private void repairItemsModel(List<SearchViewItem> members) {
        for (SearchViewItem item : members) {
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

    List<SearchViewItem> findImpl(List<String> pids, boolean onlyActive, String model) throws FedoraClientException, IOException {
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
        List<SearchViewItem> list = consumeSearch(search.execute(fedora));
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
    @Override
    public List<SearchViewItem> findSortedChildren(String parentPid)
            throws FedoraClientException, IOException, DigitalObjectException {
        
        RemoteObject parent = storage.find(parentPid);
        List<String> memberPids = new RelationEditor(parent).getMembers();
        List<SearchViewItem> items = find(memberPids);
        ArrayList<SearchViewItem> sortedItems = new ArrayList<SearchViewItem>(memberPids.size());
        for (String memberPid : memberPids) {
            for (Iterator<SearchViewItem> it = items.iterator(); it.hasNext();) {
                SearchViewItem item = it.next();
                if (memberPid.equals(item.getPid())) {
                    sortedItems.add(item);
                    it.remove();
                    break;
                }
            }
        }
        return sortedItems;
    }

    @Override
    public List<SearchViewItem> findSortedChildrenWithPagesFirst(String parentPid) throws FedoraClientException, IOException, DigitalObjectException {
        RemoteObject parent = storage.find(parentPid);
        List<String> memberPids = new RelationEditor(parent).getMembers();
        List<SearchViewItem> items = find(memberPids);
        ArrayList<SearchViewItem> sortedItems = new ArrayList<SearchViewItem>(memberPids.size());
        ArrayList<SearchViewItem> pageModelsList = new ArrayList<>();
        ArrayList<SearchViewItem> otherModelsList = new ArrayList<>();
        for (String memberPid : memberPids) {
            for (Iterator<SearchViewItem> it = items.iterator(); it.hasNext();) {
                SearchViewItem item = it.next();
                if (memberPid.equals(item.getPid())) {
                    if (NdkPlugin.MODEL_PAGE.equals(item.getModel()) || NdkPlugin.MODEL_NDK_PAGE.equals(item.getModel()) || OldPrintPlugin.MODEL_PAGE.equals(item.getModel())) {
                        pageModelsList.add(item);
                    } else {
                        otherModelsList.add(item);
                    }
                    it.remove();
                    break;
                }
            }
        }
        sortedItems.addAll(pageModelsList);
        sortedItems.addAll(otherModelsList);
        return sortedItems;
    }

    @Override
    public List<SearchViewItem> findChildren(String pid) throws FedoraClientException, IOException {
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
    @Override
    public List<SearchViewItem> findChildrenHierarchy(String pid) throws FedoraClientException, IOException {
        String query = QUERY_FIND_MEMBER_HIERARCHY.replace("${ROOT}", RelationResource.fromPid(pid).getResource());
        RiSearch search = buildSearch(query);
        return consumeSearch(search.execute(fedora));
    }

    @Override
    public List<SearchViewItem> findLastCreated(int offset, String model, String sort) throws FedoraClientException, IOException {
        return findLastCreated(offset, model, null, Integer.MAX_VALUE, sort);
    }

    @Override
    public List<SearchViewItem> findLastCreated(int offset, String model, String organization, int limit, String sort) throws FedoraClientException, IOException {
        return findLast(offset, model, organization, limit, "$created " + sort);
    }

    @Override
    public List<SearchViewItem> findAlphabetical(int offset, String model, String organization, int limit, String sort) throws IOException, FedoraClientException {
        return findLast(offset, model, organization, limit, "$label " + sort);
    }

    @Override
    public List<SearchViewItem> findLastModified(int offset, String model, String organization, int limit, String sort) throws FedoraClientException, IOException {
        return findLast(offset, model, organization, limit, "$modified " + sort);
    }

    @Override
    public List<SearchViewItem> findAdvancedSearchItems(String identifier, String label, String owner, String status, String organization, String processor, String model, String parentPid, String sortField, String sort, int offset, int limit) throws IOException, FedoraClientException {
        if (SolrUtils.PROPERTY_PARENTPID_NO_PARENT.equals(parentPid)) {
            return new ArrayList<>();
        }
        sortField = createSortField(sortField);
        sort = createSort(sort);
        if (label == null && identifier == null && owner == null) {
            return findAdvancedObjects(null, status, organization, processor, model,sortField + " " + sort, offset, limit);
        }
        List<String> pids = findAdvancedPids(new SearchViewQuery().setLabel(label).setIdentifier(identifier).setModel(model).setOwner(owner));
        return findAdvancedObjects(pids, status, organization, processor, model,sortField + " " + sort, offset, limit);
    }

    @Override
    public int findAdvancedSearchCount(String identifier, String label, String owner, String status, String organization, String processor, String model, String parentPid) throws FedoraClientException, IOException {
        if (SolrUtils.PROPERTY_PARENTPID_NO_PARENT.equals(parentPid)) {
            return 0;
        }
        if (label == null && identifier == null && owner == null) {
            return findAdvancedCountObjects(null, status, organization, processor, model);
        }
        List<String> pids = findAdvancedPids(new SearchViewQuery().setLabel(label).setIdentifier(identifier).setModel(model).setOwner(owner));
        return findAdvancedCountObjects(pids, status, organization, processor, model);
    }

    @Override
    public int countByOwner(String owner) throws IOException, FedoraClientException {
        List<String> pids = findAdvancedPids(new SearchViewQuery().setOwner(owner));
        return pids.size();
    }

    private List<String> findAdvancedPids(SearchViewQuery q) throws FedoraClientException {
        List<String> objectsPid = new ArrayList<>();
        if (q.getIdentifier() == null) {
            return getAdvancedPids(q, null);
        } else {
            for (String identifier : q.getIdentifier().split(",")) {
                objectsPid.addAll(getAdvancedPids(q, identifier));
            }
            return objectsPid;
        }
    }

    private List<String> getAdvancedPids(SearchViewQuery q, String identifier) throws FedoraClientException {
        List<String> objectsPid = new ArrayList<>();
        List<String> pids = new ArrayList<>();
        StringBuilder query = new StringBuilder();
        if (q.getModel() != null && !q.getModel().isEmpty()) {
            //query.append("type~").append(q.getModel());
        }
        buildQuery(query, "label", q.getLabel());
        buildQuery(query, "ownerId", q.getOwner());
        buildQuery(query, "creator", q.getProcessor());
        buildQuery(query, "identifier", identifier);

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


    private List<SearchViewItem> findAdvancedObjects(List<String> pids, String status, String organization, String processor, String model, String orderBy, int offset, int limit) throws FedoraClientException, IOException {
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
            processorFilter = String.format("and       $pid      <http://proarc.lib.cas.cz/relations#user>    <info:fedora/%s>", processor);
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
            return new ArrayList<SearchViewItem>();
        } else {
            return consumeSearch(search.execute(fedora));
        }
    }

    private int findAdvancedCountObjects(List<String> pids, String status, String organization, String processor, String model) throws FedoraClientException, IOException {
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
            processorFilter = String.format("and       $pid      <http://proarc.lib.cas.cz/relations#user>    <info:fedora/%s>", processor);
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
        return consumeCountSearch(search.execute(fedora));
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

    private List<SearchViewItem> findLast(int offset, String model, String organization, int limit, String orderBy) throws FedoraClientException, IOException {
        String modelFilter = "";
        String organizationFilter = "";
        if (model != null && !model.isEmpty()) {
            modelFilter = String.format("and        $pid     <info:fedora/fedora-system:def/model#hasModel>        <info:fedora/%s>", model);
        }
        if (organization != null) {
            organizationFilter = String.format("and       $pid      <http://proarc.lib.cas.cz/relations#organization>    <info:fedora/%s>", organization);
        }
        String query = QUERY_LAST_CREATED.replace("${OFFSET}", String.valueOf(offset));
        query = query.replace("${MODEL_FILTER}", modelFilter);
        query = query.replace("${ORDERBY}", orderBy);
        query = query.replace("${ORGANIZATION}", organizationFilter);
        LOG.fine(query);
        RiSearch search = buildSearch(query);

        if (limit > 0) {
            limit = Math.min(limit, maxLimit);
            search.limit(limit);
        }
        return consumeSearch(search.execute(fedora));
    }

    public int countModels(String model, String organization) throws FedoraClientException, IOException {
        String modelFilter = "";
        String ownerFilter = "";
        String organizationFilter = "";
        String userFilter = "";
        if (model != null && !model.isEmpty()) {
            modelFilter = String.format("and        $pid     <info:fedora/fedora-system:def/model#hasModel>        <info:fedora/%s>", model);
        }
        if (organization != null) {
            organizationFilter = String.format("and       $pid      <http://proarc.lib.cas.cz/relations#organization>    <info:fedora/%s>", organization);
        }
        String query = QUERY_COUNT_MODELS;
        query = query.replace("${MODEL_FILTER}", modelFilter);
        query = query.replace("${OWNER_FILTER}", ownerFilter);
        query = query.replace("${ORGANIZATION}", organizationFilter);
        query = query.replace("${USERNAME}", userFilter);
        LOG.fine(query);
        RiSearch search = buildSearch(query);

        return consumeCountSearch(search.execute(fedora));
    }

    public List<SearchViewItem> findAllObjects() throws FedoraClientException, IOException {
        String query = QUERY_FIND_ALL_OBJECTS;
        LOG.fine(query);
        RiSearch search = buildSearch(query);
        return consumeSearch(search.execute(fedora));
    }

    public List<SearchViewItem> findReferrers(String pid) throws IOException, FedoraClientException {
        String query = QUERY_FIND_REFERRERS.replace("${PID}", pid);
        RiSearch search = buildSearch(query);
        return consumeSearch(search.execute(fedora));
    }

    public List<SearchViewItem> findByModel(String modelId) throws  IOException, FedoraClientException {
        return findByModel(0, modelId);
    }

    public List<SearchViewItem> findByModel(int offset, String modelId) throws IOException, FedoraClientException {
        return findByModel(offset, modelId, "$created desc");
    }

    public List<SearchViewItem> findByModel(int offset, String modelId, String orderBy) throws IOException, FedoraClientException {
        String query = QUERY_FIND_BY_MODEL;
        query = query.replace("${metaModelPid}", modelId);
        query = query.replace("${OFFSET}", String.valueOf(offset));
        query = query.replace("${ORDERBY}", orderBy);
        RiSearch search = buildSearch(query);
        search.limit(1000);
        return consumeSearch(search.execute(fedora));
    }

    public List<SearchViewItem> findByModels(int offset, String modelId1, String modelId2) throws IOException, FedoraClientException {
        return findByModels(offset, modelId1, modelId2, "$created desc");
    }

    public List<SearchViewItem> findByModels(int offset, String... modelIds) throws IOException, FedoraClientException {
        List<SearchViewItem> items = new ArrayList<>();
        for (String modelId : modelIds) {
            items.addAll(findByModel(offset, modelId, "$created desc"));
        }
        return items;
    }

    public List<SearchViewItem> findByModels(int offset, String modelId1, String modelId2, String orderBy) throws IOException, FedoraClientException {
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
    @Override
    public boolean isDeviceInUse(String deviceId) throws IOException, FedoraClientException {
        String query = QUERY_FIND_DEVICE_REFERRERS.replace("${devicePid}", deviceId);
        RiSearch search = buildSearch(query);
        search.limit(1);
        search.stream(true);
        List<SearchViewItem> result = consumeSearch(search.execute(fedora));
        return !result.isEmpty();
    }

    @Override
    public boolean isSoftwareInUse(String softwareId) throws IOException, FedoraClientException {
        String query = QUERY_FIND_REFERRERS.replace("${PID}",softwareId);
        RiSearch search = buildSearch(query);
        search.limit(1);
        search.stream(true);
        List<SearchViewItem> result = consumeSearch(search.execute(fedora));
        return !result.isEmpty();
    }

    private List<SearchViewItem> consumeSearch(RiSearchResponse response) throws IOException {
        String json = response.getEntity(String.class);
        Result result = readResponse(json);
        return consumeSearch(result.results);
    }

    private int consumeCountSearch(RiSearchResponse response) throws IOException {
        String json = response.getEntity(String.class);
        Result result = readResponse(json);
        return result.results.size();
    }

    Result readResponse(String json) throws IOException {
        if (mapper == null) {
            // requires mapper without mix in annotation of Item
            mapper = JsonUtils.createObjectMapper();
        }
        return  mapper.readValue(json, Result.class);
    }

    private List<SearchViewItem> consumeSearch(List<SearchViewItem> items) {
        for (SearchViewItem item : items) {
            replaceUriWithPid(item);
            resolveObjectLabel(item);
            if (isPage(item.getModel())) {
                item.setPageIndex("-1");
                item.setPageNumber("-1");
                item.setPageType("-1");
                item.setPagePosition("-1");
            } else if (isAudioPage(item.getModel())) {
                item.setPageIndex("-1");
            }
        }
        return items;
    }

    private static String replaceUriWithPid(String uri) {
        return uri == null ? uri : RelationResource.toPid(uri);
    }

    private static SearchViewItem replaceUriWithPid(SearchViewItem item) {
        item.setPid(replaceUriWithPid(item.getPid()));
        item.setModel(replaceUriWithPid(item.getModel()));
        item.setState(replaceUriWithPid(item.getState()));
        return item;
    }
    void resolveObjectLabel(SearchViewItem item) {
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


    static class Result {

        private List<SearchViewItem> results;

        public List<SearchViewItem> getResults() {
            return results;
        }

        public void setResults(List<SearchViewItem> results) {
            this.results = results;
        }

    }

}
