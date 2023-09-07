package cz.cas.lib.proarc.common.fedora.akubra;

import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.device.DeviceRepository;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.SearchViewItem;
import cz.cas.lib.proarc.common.fedora.SearchViewQuery;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraStorage.AkubraObject;
import cz.cas.lib.proarc.common.fedora.relation.RelationEditor;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
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

import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_CREATED;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_DEVICE;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_FULLTEXT;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_LABEL;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_MEMBERS;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_MODEL;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_MODIFIED;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_ORGANIZATION;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_OWNER;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_PID;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_STATE;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_STATUS;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.FIELD_USER;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.appendAndValue;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.createItem;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.getModelQuery;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.getPidsQuery;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.getUserQuery;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.toList;
import static cz.cas.lib.proarc.common.fedora.akubra.SolrUtils.transfromSort;

public class SolrSearchView extends SearchView {

    private static final Integer ROWS_DEFAULT_VALUE = 10000;
    private static final int DEFAULT_PIDS_SIZE = 25;
    private final int maxLimit;
    private Locale locale = Locale.ENGLISH;
    private final AkubraStorage storage;
    private final SolrClient solrClient;
    private boolean allowDevices;

    public SolrSearchView(AkubraStorage storage, SolrClient solrClient) {
        this(storage, Integer.MAX_VALUE, solrClient);
        allowDevices = false;
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

    public SolrSearchView setAllowDevices(boolean allowDevices) {
        this.allowDevices = allowDevices;
        return this;
    }

    @Override
    public boolean isDeviceInUse(String deviceId) throws IOException, FedoraClientException {
        try {
            String query = FIELD_DEVICE + ":\"" + deviceId + "\"";
            SolrQuery solrQuery = new SolrQuery(query);
            QueryResponse response = this.solrClient.query(solrQuery);
            int total = response.getResults().size();
            return total > 0;
        } catch (SolrServerException ex) {
            ex.printStackTrace();
            throw new IOException(ex);
        }
    }

    @Override
    public List<SearchViewItem> find(String... pids) throws IOException {
        return find(true, Arrays.asList(pids));
    }

    @Override
    public List<SearchViewItem> find(String pid) throws IOException {
        return find(true, Arrays.asList(pid));
    }

    @Override
    public List<SearchViewItem> find(List<String> pids) throws IOException {
        return find(true, pids);
    }

    @Override
    public List<SearchViewItem> find(boolean onlyActive, List<String> pids) throws IOException {
        return pids.isEmpty() ? Collections.EMPTY_LIST : searchImplementation(0, null, null, null, onlyActive, null, pids);
    }

    @Override
    public List<SearchViewItem> findAllObjects() throws IOException {
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
        try {
            StringBuilder queryBuilder = new StringBuilder();
            queryBuilder = appendAndValue(queryBuilder, FIELD_STATE + ":\"" + SolrUtils.PROPERTY_STATE_ACTIVE + "\"");
            queryBuilder = appendAndValue(queryBuilder, FIELD_MEMBERS + ":\"" + pid + "\"");
            SolrQuery solrQuery = new SolrQuery(queryBuilder.toString());

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

    @Override
    public List<SearchViewItem> findSortedChildren(String parentPid) throws IOException, DigitalObjectException {
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
    public List<SearchViewItem> findSortedChildrenWithPagesFirst(String parentPid) throws FedoraClientException, IOException, DigitalObjectException {
        AkubraObject parent = storage.find(parentPid);
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
    public List<SearchViewItem> findChildren(String parentPid) throws IOException, DigitalObjectException {
        AkubraObject parent = storage.find(parentPid);
        List<String> memberPids = new RelationEditor(parent).getMembers();
        if (memberPids.isEmpty()) {
            return Collections.EMPTY_LIST;
        }
        List<SearchViewItem> items = find(memberPids);
        return items;
    }

    @Override
    public List<SearchViewItem> findChildrenHierarchy(String parentPid) throws IOException, DigitalObjectException {
        List<SearchViewItem> items = new ArrayList<>();

        items = findChildrenHierarchy(items, parentPid);

        return items;
    }

    private List<SearchViewItem> findChildrenHierarchy(List<SearchViewItem> items, String parentPid) throws IOException, DigitalObjectException {
        if (items == null) {
            items = new ArrayList<>();
        }
        AkubraObject parent = storage.find(parentPid);
        List<String> memberPids = new RelationEditor(parent).getMembers();
        if (memberPids.isEmpty()) {
            return items;
        } else {
            items.addAll(find(memberPids));
            for (String memberPid : memberPids) {
                items = findChildrenHierarchy(items, memberPid);
            }
            return items;
        }
    }

    @Override
    public List<SearchViewItem> findLastCreated(int offset, String model, String user, Boolean filterWithoutExtension, String sort) throws IOException {
        return findLastCreated(offset, model, user, null, null, filterWithoutExtension, this.maxLimit, sort);
    }

    @Override
    public List<SearchViewItem> findLastCreated(int offset, String model, String user, String organization, String username, Boolean filterWithoutExtension, int limit, String sort) throws IOException {
        return findLastImp(offset, model, user, organization, username, filterWithoutExtension, limit, FIELD_CREATED, sort);
    }

    @Override
    public List<SearchViewItem> findAlphabetical(int offset, String model, String user, String organization, String username, Boolean filterWithoutExtension, int limit, String sort) throws IOException, FedoraClientException {
        return findLastImp(offset, model, user, organization, username, filterWithoutExtension, limit, FIELD_LABEL, sort);
    }

    @Override
    public List<SearchViewItem> findLastModified(int offset, String model, String user, String organization, String username, Boolean filterWithoutExtension, int limit, String sort) throws IOException {
        return findLastImp(offset, model, user, organization, username, filterWithoutExtension, limit, FIELD_MODIFIED, sort);
    }

    @Override
    public List<SearchViewItem> findQuery(String title, String label, String identifier, String owner, String model, Collection<String> hasOwners) throws IOException {
        return findQuery(new SearchViewQuery().setTitle(title).setLabel(label).setIdentifier(identifier).setOwner(owner).setModel(model).setHasOwners(hasOwners), "active");
    }

    @Override
    public List<SearchViewItem> findQuery(SearchViewQuery q, String status) throws IOException {
        boolean onlyActive =  "active".equals(status) ? true : false;
        List<String> models = null;
        if (q.getModel() != null && !q.getModel().isEmpty()) {
            models = Collections.singletonList(q.getModel());
        }
        List<String> pids = null;
        if (q.getIdentifier() != null && !q.getIdentifier().isEmpty()) {
            pids = Collections.singletonList(q.getIdentifier());
        }
        return searchImplementation(0, maxLimit, null, null, onlyActive, models, pids, q.getOwner(), q.getOrganization(), q.getCreator(), q.getLabel(), q.getStatus(), false);
    }

    @Override
    public List<SearchViewItem> findPhrase(String phrase, String status, String organization, String processor, String model, Boolean allowAllForProcessor, Boolean filterWithoutExtension, String sortField, String sort, int offset, int limit) throws IOException, FedoraClientException {
        return searchPhraseImplementation(phrase, status, organization, processor, model, allowAllForProcessor, filterWithoutExtension, sortField, sort, offset, limit);
    }

    @Override
    public int countModels(String model, String user, String organization, String username, Boolean filterWithoutExtension) throws IOException {
        return searchCountImplementation(0, this.maxLimit, true, Collections.singletonList(model), null, user, organization, username, null, null, true);
    }

    @Override
    public List<SearchViewItem> findAdvancedSearchItems(String identifier, String label, String owner, String status, String organization, String processor, String model, String creator, Boolean allowAllForProcessor, Boolean filterWithoutExtension, String sortField, String sort, int offset, int limit) throws IOException, FedoraClientException {
        return searchImplementation(offset, limit, sortField, transfromSort(sort), true, Collections.singletonList(model), Collections.singletonList(identifier), owner, organization, processor, label, status, allowAllForProcessor);
    }

    @Override
    public int findAdvancedSearchCount(String identifier, String label, String owner, String status, String organization, String processor, String model, String creator, Boolean allowAllForProcessor, Boolean filterWithoutExtension) throws IOException {
        return searchCountImplementation(0, this.maxLimit, true, Collections.singletonList(model), Collections.singletonList(identifier), owner, organization, processor, label, status, allowAllForProcessor);
    }

    private List<SearchViewItem> findLastImp(int offset, String model, String user, String organization, String username, Boolean filterWithoutExtension, int limit, String sortField, String sortOperation) throws IOException {
        return searchImplementation(offset, limit, sortField, transfromSort(sortOperation), true,  Collections.singletonList(model), null, user, organization, username, null, null, true);
    }

    private List<SearchViewItem> searchImplementation(Integer offset, Integer limit, String sortField, SolrUtils.SortOperation sortOperation, Boolean onlyActive, List<String> models, List<String> pids) throws IOException {
        return searchImplementation(offset, limit, sortField, sortOperation, onlyActive, models, pids, null, null, null, null, null, true);
    }


    private List<SearchViewItem> searchImplementation(Integer offset, Integer limit, String sortField, SolrUtils.SortOperation sortOperation, Boolean onlyActive, List<String> models, List<String> pids, String owner, String organization, String user, String label, String status, Boolean allowAllForUser) throws IOException {
        if (pids == null || pids.isEmpty()) {
            return searchImpl(offset, limit, sortField, sortOperation,  onlyActive, models, pids, owner, organization, user, label, status, allowAllForUser);
        } else {
            List<SearchViewItem> items = new ArrayList<>();
            List<String> tmpPids = new ArrayList<>();
            for (int index = 0; index < pids.size(); index++) {
                tmpPids.add(pids.get(index));
                if (index % DEFAULT_PIDS_SIZE == 0) {
                    items.addAll(searchImpl(offset, limit, sortField, sortOperation,  onlyActive, models, tmpPids, owner, organization, user, label, status, allowAllForUser));
                    tmpPids.clear();
                }
            }
            if (!tmpPids.isEmpty()) {
                items.addAll(searchImpl(offset, limit, sortField, sortOperation,  onlyActive, models, tmpPids, owner, organization, user, label, status, allowAllForUser));
                tmpPids.clear();
            }
            return items;
        }
    }

    private List<SearchViewItem> searchImpl(Integer offset, Integer limit, String sortField, SolrUtils.SortOperation sortOperation, Boolean onlyActive, List<String> models, List<String> pids, String owner, String organization, String user, String label, String status, Boolean allowAllForUser) throws IOException {
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

    private List<SearchViewItem> searchPhraseImplementation(String phrase, String status, String organization, String username, String model, Boolean allowAllForUser, Boolean filterWithoutExtension, String sortField, String sort, int offset, int limit) throws IOException {
        try {
            StringBuilder queryBuilder = createPhaseQueryWithParams(phrase, status, organization, username, model, allowAllForUser);
            SolrQuery solrQuery = createQueryWithParams(queryBuilder, offset, limit, transfromSort(sort), sortField);

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

    private StringBuilder createPhaseQueryWithParams(String phrase, String status, String organization, String username, String model, Boolean allowAllForUser) {
        List<String> models = null;
        if (model != null && !model.isEmpty()) {
            models = Collections.singletonList(model);
        }

        StringBuilder queryBuilder = createQuery(true, models, null, null, organization, username, null, status, allowAllForUser);
        if (phrase != null && !phrase.isEmpty()) {
            queryBuilder = appendAndValue(queryBuilder, FIELD_FULLTEXT + ":\"" + phrase + "\"");
        }
        return queryBuilder;
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
        } else {
            solrQuery.setRows(ROWS_DEFAULT_VALUE);
        }
        if (sortOperation != null && validField(sortField)) {
            SolrQuery.SortClause sortClause = new SolrQuery.SortClause(sortField, sortOperation.name().toLowerCase());
            solrQuery.setSort(sortClause);
        }
        return solrQuery;
    }

    private boolean validField(String sortField) {
        if (sortField == null || sortField.isEmpty()) {
            return false;
        } else {
            List<String> validFields = new ArrayList<>(Arrays.asList(FIELD_PID, FIELD_MODEL, FIELD_OWNER, FIELD_LABEL, FIELD_STATE, FIELD_CREATED, FIELD_MODIFIED, FIELD_ORGANIZATION, FIELD_USER, FIELD_STATUS));
            return validFields.contains(sortField);
        }
    }

    private StringBuilder createQuery(Boolean onlyActive, List<String> models, List<String> pids, String owner, String organization, String user, String label, String status, Boolean allowAllForUser) {
        StringBuilder queryBuilder = defaultQuery();
        if (onlyActive != null && onlyActive) {
            queryBuilder = appendAndValue(queryBuilder, FIELD_STATE + ":\"" + SolrUtils.PROPERTY_STATE_ACTIVE + "\"");
        } else if (onlyActive != null && !onlyActive){
            queryBuilder = appendAndValue(queryBuilder, FIELD_STATE + ":\"" + SolrUtils.PROPERTY_STATE_DEACTIVE + "\"");
        }
        if (models != null && !models.isEmpty() && hasValues(models)) {
            queryBuilder = appendAndValue(queryBuilder, getModelQuery(models));
        }
        if (pids != null && !pids.isEmpty() && hasValues(pids)) {
            queryBuilder = appendAndValue(queryBuilder, getPidsQuery(pids));
        }
        if (owner != null && !owner.isEmpty()) {
            queryBuilder = appendAndValue(queryBuilder, FIELD_OWNER + ":\"" + owner + "\"");
        }
        if (organization != null && !organization.isEmpty()) {
            queryBuilder = appendAndValue(queryBuilder, FIELD_ORGANIZATION + ":\"" + organization + "\"");
        }
        if (user != null && !user.isEmpty() && (hasValues(Collections.singletonList(user)) || allowAllForUser)) {
            queryBuilder = appendAndValue(queryBuilder, getUserQuery(Collections.singletonList(user), allowAllForUser));
        }
        if (label != null && !label.isEmpty()) {
            queryBuilder = appendAndValue(queryBuilder, FIELD_LABEL + ":\"" + label + "\"");
        }
        if (status != null && !status.isEmpty()) {
            queryBuilder = appendAndValue(queryBuilder, FIELD_STATUS + ":\"" + status + "\"");
        }
        return queryBuilder;
    }

    private StringBuilder defaultQuery() {
        StringBuilder queryBuilder = new StringBuilder();
        if (!allowDevices) {
            queryBuilder = appendAndValue(queryBuilder, "-" + FIELD_MODEL + ":\"" + DeviceRepository.METAMODEL_ID + "\"");
            queryBuilder = appendAndValue(queryBuilder, "-" + FIELD_MODEL + ":\"" + DeviceRepository.METAMODEL_AUDIODEVICE_ID + "\"");
        }
        return queryBuilder;
    }

    private boolean hasValues(List<String> values) {
        for (String value : values) {
            if (value != null && !value.isEmpty()) {
                return true;
            }
        }
        return false;
    }
}
