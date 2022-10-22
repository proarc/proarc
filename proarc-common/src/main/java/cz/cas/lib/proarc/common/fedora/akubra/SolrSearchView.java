package cz.cas.lib.proarc.common.fedora.akubra;

import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.SearchViewItem;
import cz.cas.lib.proarc.common.fedora.SearchViewQuery;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraStorage.AkubraObject;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocument;

import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.appendAndValue;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.createItem;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.getModelQuery;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.getPidsQuery;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.getUserQuery;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.toList;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.transfromSort;

public class SolrSearchView extends SearchView {

    private final int maxLimit;
    private Locale locale = Locale.ENGLISH;
    private final AkubraStorage storage;
    private final SolrClient solrClient;

    public SolrSearchView(AkubraStorage storage, SolrClient solrClient) {
        this(storage, Integer.MAX_VALUE, solrClient);
    }

    public SolrSearchView(AkubraStorage storage, int maxValue, SolrClient solrClient) {
        this.storage = storage;
        this.maxLimit = maxValue;
        this.solrClient = solrClient;
    }

    public void setLocale(Locale locale) {
        if (locale == null) {
            throw new NullPointerException("Locale is null in " + this.getClass().getName() + ".");
        }
        this.locale = locale;
    }

    @Override
    public boolean isDeviceInUse(String deviceId) throws IOException, FedoraClientException {
        return super.isDeviceInUse(deviceId);
    }

    @Override
    public List<SearchViewItem> find(String... pids) throws FedoraClientException, IOException {
        return find(true, Arrays.asList(pids));
    }

    @Override
    public List<SearchViewItem> find(String pid) throws FedoraClientException, IOException {
        return find(true, Arrays.asList(pid));
    }

    @Override
    public List<SearchViewItem> find(List<String> pids) throws FedoraClientException, IOException {
        return find(true, pids);
    }

    @Override
    public List<SearchViewItem> find(boolean onlyActive, List<String> pids) throws FedoraClientException, IOException {
        return searchImplementation(0, null, null, null, onlyActive, null, pids);
    }

    @Override
    public List<SearchViewItem> findAllObjects() throws FedoraClientException, IOException {
        return searchImplementation(0, null, null, null, null, null, null);

    }

    @Override
    public List<SearchViewItem> findByModel(String modelId) throws IOException, FedoraClientException {
        return findByModel(0, modelId);
    }

    @Override
    public List<SearchViewItem> findByModel(int offset, String modelId) throws IOException, FedoraClientException {
        return searchImplementation(offset, 1000, "created", SolrUtils.SortOperation.DESC, null, Collections.singletonList(modelId), null);
    }

    @Override
    public List<SearchViewItem> findByModels(String modelId1, String modelId2) throws IOException, FedoraClientException {
        return findByModels(0, modelId1, modelId2);
    }

    @Override
    public List<SearchViewItem> findByModels(int offset, String modelId1, String modelId2) throws IOException, FedoraClientException {
        return searchImplementation(offset, 1000, "created", SolrUtils.SortOperation.DESC, null, toList(modelId1, modelId2), null);
    }

    @Override
    public List<SearchViewItem> findReferrers(String pid) throws IOException, FedoraClientException {
        return super.findReferrers(pid); //TODO
    }

    @Override
    public List<SearchViewItem> findSortedChildren(String parentPid) throws FedoraClientException, IOException, DigitalObjectException {
        AkubraObject parent = storage.find(parentPid);
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
    public List<SearchViewItem> findChildren(String pid) throws FedoraClientException, IOException {
        return super.findChildren(pid); //TODO
    }

    @Override
    public List<SearchViewItem> findChildrenHierarchy(String pid) throws FedoraClientException, IOException {
        return super.findChildrenHierarchy(pid); //TODO
    }

    @Override
    public List<SearchViewItem> findLastCreated(int offset, String model, String user, Boolean filterWithoutExtension, String sort) throws FedoraClientException, IOException {
        return findLastCreated(offset, model, user, null, null, filterWithoutExtension, this.maxLimit, sort);
    }

    @Override
    public List<SearchViewItem> findLastCreated(int offset, String model, String user, String organization, String username, Boolean filterWithoutExtension, int limit, String sort) throws FedoraClientException, IOException {
        return findLastImp(offset, model, user, organization, username, filterWithoutExtension, limit, "created", sort);
    }

    @Override
    public List<SearchViewItem> findAlphabetical(int offset, String model, String user, String organization, String username, Boolean filterWithoutExtension, int limit, String sort) throws IOException, FedoraClientException {
        return findLastImp(offset, model, user, organization, username, filterWithoutExtension, limit, "title", sort);
    }

    @Override
    public List<SearchViewItem> findLastModified(int offset, String model, String user, String organization, String username, Boolean filterWithoutExtension, int limit, String sort) throws FedoraClientException, IOException {
        return findLastImp(offset, model, user, organization, username, filterWithoutExtension, limit, "modified", sort);
    }

    @Override
    public List<SearchViewItem> findQuery(String title, String label, String identifier, String owner, String model, Collection<String> hasOwners) throws FedoraClientException, IOException {
        return findQuery(new SearchViewQuery().setTitle(title).setLabel(label).setIdentifier(identifier).setOwner(owner).setModel(model).setHasOwners(hasOwners), "active");
    }

    @Override
    public List<SearchViewItem> findQuery(SearchViewQuery q, String status) throws FedoraClientException, IOException {
        boolean onlyActive =  "active".equals(status) ? true : false;
        List<String> models = null;
        if (q.getModel() != null && !q.getModel().isEmpty()) {
            models = Collections.singletonList(q.getModel());
        }
        return searchImplementation(0, maxLimit, null, null, onlyActive, models, null, q.getOwner(), q.getOrganization(), q.getCreator(), q.getLabel(), q.getStatus(), false);
    }

    @Override
    public List<SearchViewItem> findPhrase(String phrase, String status, String organization, String processor, String model, Boolean allowAllForProcessor, Boolean filterWithoutExtension, String sortField, String sort, int offset, int limit) throws IOException, FedoraClientException {
        return super.findPhrase(phrase, status, organization, processor, model, allowAllForProcessor, filterWithoutExtension, sortField, sort, offset, limit);
    }

    @Override
    public int countModels(String model, String user, String organization, String username, Boolean filterWithoutExtension) throws FedoraClientException, IOException {
        return searchCountImplementation(0, this.maxLimit, true, Collections.singletonList(model), null, user, organization, username, null, null, true);
    }

    @Override
    public List<SearchViewItem> findAdvancedSearchItems(String identifier, String label, String owner, String status, String organization, String processor, String model, String creator, Boolean allowAllForProcessor, Boolean filterWithoutExtension, String sortField, String sort, int offset, int limit) throws IOException, FedoraClientException {
        return searchImplementation(offset, limit, sortField, transfromSort(sort), true, Collections.singletonList(model), Collections.singletonList(identifier), owner, organization, processor, label, status, allowAllForProcessor);
    }

    @Override
    public int findAdvancedSearchCount(String identifier, String label, String owner, String status, String organization, String processor, String model, String creator, Boolean allowAllForProcessor, Boolean filterWithoutExtension) throws FedoraClientException, IOException {
        return searchCountImplementation(0, this.maxLimit, true, Collections.singletonList(model), Collections.singletonList(identifier), owner, organization, processor, label, status, allowAllForProcessor);
    }

    private List<SearchViewItem> findLastImp(int offset, String model, String user, String organization, String username, Boolean filterWithoutExtension, int limit, String sortField, String sortOperation) throws IOException {
        return searchImplementation(offset, limit, sortField, transfromSort(sortOperation), true,  Collections.singletonList(model), null, user, organization, username, null, null, true);
    }

    private List<SearchViewItem> searchImplementation(Integer offset, Integer limit, String sortField, SolrUtils.SortOperation sortOperation, Boolean onlyActive, List<String> models, List<String> pids) throws IOException {
        return searchImplementation(offset, limit, sortField, sortOperation, onlyActive, models, pids, null, null, null, null, null, true);
    }

    private List<SearchViewItem> searchImplementation(Integer offset, Integer limit, String sortField, SolrUtils.SortOperation sortOperation, Boolean onlyActive, List<String> models, List<String> pids, String owner, String organization, String user, String label, String status, Boolean allowAllForUser) throws IOException {
        try {
            StringBuilder queryBuilder = createQuery(onlyActive, models, pids, owner, organization, user, label, status, allowAllForUser);
            SolrQuery solrQuery = createQueryWithParams(queryBuilder, offset, limit, sortOperation, sortField);

            List<SearchViewItem> items = new ArrayList<>();
            QueryResponse response = this.solrClient.query(solrQuery);

            for (SolrDocument solrDocument : response.getResults()) {
                items.add(createItem(solrDocument));
            }

            return items;
        } catch (SolrServerException ex) {
            ex.printStackTrace();
            throw new IOException(ex);
        }
    }

    private int searchCountImplementation(Integer offset, Integer limit, Boolean onlyActive, List<String> models, List<String> pids, String owner, String organization, String username, String label, String status, Boolean allowAllForUser) throws IOException {
        try {
            StringBuilder queryBuilder = createQuery(onlyActive, models, pids, owner, organization, username, label, status, allowAllForUser);
            SolrQuery solrQuery = createQueryWithParams(queryBuilder, offset, limit, null, null);

            QueryResponse response = this.solrClient.query(solrQuery);

            int total = response.getResults().size();
            return total;
        } catch (SolrServerException ex) {
            ex.printStackTrace();
            throw new IOException(ex);
        }
    }

    private SolrQuery createQueryWithParams(StringBuilder queryBuilder, Integer offset, Integer limit, SolrUtils.SortOperation sortOperation, String sortField) {
        SolrQuery solrQuery = new SolrQuery(queryBuilder.toString());
        if (offset != null && offset >= 0) {
            solrQuery.setStart(offset);
        }
        if (limit != null && limit >= 1) {
            solrQuery.setRows(limit);
        }
//        if (sortOperation != null && sortField != null) {
//            SolrQuery.SortClause sortClause = new SolrQuery.SortClause(sortField, sortOperation.name());
//            solrQuery.setSort(sortClause);
//        }
        return solrQuery;
    }

    private StringBuilder createQuery(Boolean onlyActive, List<String> models, List<String> pids, String owner, String organization, String user, String label, String status, Boolean allowAllForUser) {
        StringBuilder queryBuilder = new StringBuilder();
        if (onlyActive != null && onlyActive) {
            queryBuilder = appendAndValue(queryBuilder, "state:\"Active\"");
        } else if (onlyActive != null && !onlyActive){
            queryBuilder = appendAndValue(queryBuilder, "state:\"Deactive\"");
        }
        if (models != null && !models.isEmpty()) {
            queryBuilder = appendAndValue(queryBuilder, getModelQuery(models));
        }
        if (pids != null && !pids.isEmpty()) {
            queryBuilder = appendAndValue(queryBuilder, getPidsQuery(pids));
        }
        if (owner != null && !owner.isEmpty()) {
            queryBuilder = appendAndValue(queryBuilder, "owner:\"" + owner + "\"");
        }
        if (organization != null && !organization.isEmpty()) {
            queryBuilder = appendAndValue(queryBuilder, "organization:\"" + organization + "\"");
        }
        if (user != null && !user.isEmpty()) {
            queryBuilder = appendAndValue(queryBuilder, getUserQuery(Collections.singletonList(user), allowAllForUser));
        }
        if (label != null && !label.isEmpty()) {
            queryBuilder = appendAndValue(queryBuilder, "label:\"" + label + "\"");
        }
        if (status != null && !status.isEmpty()) {
            queryBuilder = appendAndValue(queryBuilder, "status:\"" + status + "\"");
        }
        return queryBuilder;
    }


}
