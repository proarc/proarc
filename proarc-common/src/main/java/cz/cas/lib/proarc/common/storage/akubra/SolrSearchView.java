package cz.cas.lib.proarc.common.storage.akubra;

import cz.cas.lib.proarc.common.device.DeviceRepository;
import cz.cas.lib.proarc.common.object.emods.BornDigitalModsPlugin;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.software.SoftwareRepository;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.SearchViewQuery;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage.AkubraObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
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
import org.apache.solr.client.solrj.util.ClientUtils;
import org.apache.solr.common.SolrDocument;

import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_CREATED;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_DEVICE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_FULLTEXT;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_LABEL;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_LABEL_SORT;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_MEMBERS;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_MODEL;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_MODIFIED;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_ORGANIZATION;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_OWNER;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_PARENT_PID;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_PID;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_SOFTWARE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_STATE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_STATUS;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_USER;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.appendAndValue;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.createItem;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.getIdentifiersQuery;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.getModelQuery;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.getPidsQuery;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.getUserQuery;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.toList;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.transfromSort;

public class SolrSearchView extends SearchView {

    private static final Integer ROWS_DEFAULT_VALUE = 10000;
    private static final int DEFAULT_PIDS_SIZE = 25;
    private final int maxLimit;
    private Locale locale = Locale.ENGLISH;
    private final AkubraStorage storage;
    private final SolrClient solrClient;
    private boolean allowDevicesAndSoftware;

    public SolrSearchView(AkubraStorage storage, SolrClient solrClient) {
        this(storage, Integer.MAX_VALUE, solrClient);
        allowDevicesAndSoftware = false;
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

    public SolrSearchView setAllowDevicesAndSoftware(boolean allowDevicesAndSoftware) {
        this.allowDevicesAndSoftware = allowDevicesAndSoftware;
        return this;
    }

    @Override
    public boolean isDeviceInUse(String deviceId) throws IOException {
        try {
            String query = FIELD_DEVICE + ":\"" + ClientUtils.escapeQueryChars(deviceId) + "\"";
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
    public boolean isSoftwareInUse(String softwareId) throws IOException {
        try {
            StringBuilder queryBuilder = new StringBuilder();
            queryBuilder = appendAndValue(queryBuilder, FIELD_STATE + ":\"" + SolrUtils.PROPERTY_STATE_ACTIVE + "\"");
            queryBuilder = appendAndValue(queryBuilder, FIELD_MEMBERS + ":\"" + ClientUtils.escapeQueryChars(softwareId) + "\"");
            SolrQuery solrQuery = new SolrQuery(queryBuilder.toString());
            QueryResponse response = this.solrClient.query(solrQuery);

            int total = response.getResults().size();
            if (total > 0) {
                return true;
            }

            String query = FIELD_SOFTWARE + ":\"" + ClientUtils.escapeQueryChars(softwareId) + "\"";
            solrQuery = new SolrQuery(query);
            response = this.solrClient.query(solrQuery);
            total = response.getResults().size();
            return  total > 0;
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
    public List<SearchViewItem> findByModel(String modelId) throws IOException {
        return findByModel(0, modelId);
    }

    @Override
    public List<SearchViewItem> findByModel(int offset, String modelId) throws IOException {
        return searchImplementation(offset, 1000, "created", SolrUtils.SortOperation.DESC, null, Collections.singletonList(modelId), null);
    }

    @Override
    public List<SearchViewItem> findByModels(int offset, String modelId1, String modelId2) throws IOException {
        return searchImplementation(offset, 1000, "created", SolrUtils.SortOperation.DESC, null, toList(modelId1, modelId2), null);
    }

    @Override
    public List<SearchViewItem> findByModels(int offset, String... modelIds) throws IOException {
        return searchImplementation(offset, 1000, "created", SolrUtils.SortOperation.DESC, null, Arrays.asList(modelIds), null);
    }

    @Override
    public List<SearchViewItem> findReferrers(String pid) throws IOException {
        try {
            StringBuilder queryBuilder = new StringBuilder();
            queryBuilder = appendAndValue(queryBuilder, FIELD_STATE + ":\"" + SolrUtils.PROPERTY_STATE_ACTIVE + "\"");
            queryBuilder = appendAndValue(queryBuilder, FIELD_MEMBERS + ":\"" + ClientUtils.escapeQueryChars(pid) + "\"");
            SolrQuery solrQuery = new SolrQuery(queryBuilder.toString());

            List<SearchViewItem> items = new ArrayList<>();
            QueryResponse response = this.solrClient.query(solrQuery);

            for (SolrDocument solrDocument : response.getResults()) {
                items.add(createItem(solrDocument, locale));
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
    public List<SearchViewItem> findSortedChildrenWithPagesFirst(String parentPid) throws IOException, DigitalObjectException {
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
        List<String> memberPids = null;
        try {
             memberPids = new RelationEditor(parent).getMembers();
        } catch (Exception ex) {
            memberPids = new ArrayList<>();
        }
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
    public List<SearchViewItem> findLastCreated(int offset, String model, String sort) throws IOException {
        return findLastCreated(offset, model, null, this.maxLimit, sort);
    }

    @Override
    public List<SearchViewItem> findLastCreated(int offset, String model, String organization, int limit, String sort) throws IOException {
        return findLastImp(offset, model, organization, limit, FIELD_CREATED, sort);
    }

    @Override
    public List<SearchViewItem> findAlphabetical(int offset, String model, String organization, int limit, String sort) throws IOException {
        return findLastImp(offset, model, organization, limit, FIELD_LABEL, sort);
    }

    @Override
    public List<SearchViewItem> findLastModified(int offset, String model, String organization, int limit, String sort) throws IOException {
        return findLastImp(offset, model, organization, limit, FIELD_MODIFIED, sort);
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
        String title = q.getTitle();
        if (title == null || title.isEmpty()) {
            title = q.getLabel();
        }
        return searchImplementation(0, maxLimit, null, null, onlyActive, models, pids, q.getOwner(), q.getOrganization(), q.getProcessor(), title, q.getStatus(), q.getParentPid());
    }

    @Override
    public List<SearchViewItem> findPhrase(String phrase, String status, String organization, String processor, String model, String sortField, String sort, int offset, int limit) throws IOException {
        return searchPhraseImplementation(phrase, status, organization, processor, model, sortField, sort, offset, limit);
    }

    @Override
    public int countModels(String model, String organization) throws IOException {
        return searchCountImplementation(0, this.maxLimit, true, Collections.singletonList(model), null, null, organization, null, null, null, null);
    }

    @Override
    public List<SearchViewItem> findAdvancedSearchItems(String identifier, String label, String owner, String status, String organization, String processor, String model, String parentPid, String sortField, String sort, int offset, int limit) throws IOException {
        List<String> queryPids = new ArrayList<>();
        if (identifier == null) {
            queryPids = null;
        } else if (identifier != null && identifier.contains(",")) {
            queryPids.addAll(Arrays.asList(identifier.split("\\s*,\\s*")));
        } else {
            queryPids.add(identifier);
        }
        return searchImplementation(offset, limit, sortField, transfromSort(sort), true, Collections.singletonList(model), queryPids, owner, organization, processor, label, status, parentPid);
    }

    @Override
    public int findAdvancedSearchCount(String identifier, String label, String owner, String status, String organization, String processor, String model, String parentPid) throws IOException {
        List<String> queryPids = new ArrayList<>();
        if (identifier != null && identifier.contains(",")) {
            queryPids.addAll(Arrays.asList(identifier.split("\\s*,\\s*")));
        } else {
            queryPids.add(identifier);
        }
        return searchCountImplementation(0, this.maxLimit, true, Collections.singletonList(model), queryPids, owner, organization, processor, label, status, parentPid);
    }

    @Override
    public int countByOwner(String owner) throws IOException {
        return searchCountImplementation(0, this.maxLimit, null, null, null, owner, null, null, null, null, null);
    }

    private List<SearchViewItem> findLastImp(int offset, String model, String organization, int limit, String sortField, String sortOperation) throws IOException {
        return searchImplementation(offset, limit, sortField, transfromSort(sortOperation), true,  Collections.singletonList(model), null, null, organization, null, null, null, null);
    }

    private List<SearchViewItem> searchImplementation(Integer offset, Integer limit, String sortField, SolrUtils.SortOperation sortOperation, Boolean onlyActive, List<String> models, List<String> pids) throws IOException {
        return searchImplementation(offset, limit, sortField, sortOperation, onlyActive, models, pids, null, null, null, null, null, null);
    }


    private List<SearchViewItem> searchImplementation(Integer offset, Integer limit, String sortField, SolrUtils.SortOperation sortOperation, Boolean onlyActive, List<String> models, List<String> pids, String owner, String organization, String processor, String label, String status, String parentPid) throws IOException {
        if (pids == null || pids.isEmpty()) {
            return searchImpl(offset, limit, sortField, sortOperation,  onlyActive, models, pids, owner, organization, processor, label, status, parentPid);
        } else {
            List<SearchViewItem> items = new ArrayList<>();
            List<String> tmpPids = new ArrayList<>();
            for (int index = 0; index < pids.size(); index++) {
                tmpPids.add(pids.get(index));
                if (index % DEFAULT_PIDS_SIZE == 0) {
                    items.addAll(searchImpl(offset, limit, sortField, sortOperation,  onlyActive, models, tmpPids, owner, organization, processor, label, status, parentPid));
                    tmpPids.clear();
                }
            }
            if (!tmpPids.isEmpty()) {
                items.addAll(searchImpl(offset, limit, sortField, sortOperation,  onlyActive, models, tmpPids, owner, organization, processor, label, status, parentPid));
                tmpPids.clear();
            }
            return items;
        }
    }

    private List<SearchViewItem> searchImpl(Integer offset, Integer limit, String sortField, SolrUtils.SortOperation sortOperation, Boolean onlyActive, List<String> models, List<String> pids, String owner, String organization, String processor, String label, String status, String parentPid) throws IOException {
        try {
            String query = createQuery(label);
            List<String> filterQueryList = createFilterQuery(onlyActive, models, pids, owner, organization, processor, status, parentPid);
            limit = updateLimit(limit, pids);
            SolrQuery solrQuery = createQueryWithParams(query, FIELD_LABEL, filterQueryList, offset, limit, sortOperation, sortField);

            List<SearchViewItem> items = new ArrayList<>();
            QueryResponse response = this.solrClient.query(solrQuery);

            for (SolrDocument solrDocument : response.getResults()) {
                items.add(createItem(solrDocument, locale));
            }

            return items;
        } catch (SolrServerException ex) {
            ex.printStackTrace();
            throw new IOException(ex);
        }
    }

    private List<String> createFilterQuery(Boolean onlyActive, List<String> models, List<String> pids, String owner, String organization, String processor, String status, String parentPid) {
        List<String> filterQueryList = defaultFilterQuery();
        if (onlyActive != null && onlyActive) {
            filterQueryList.add(FIELD_STATE + ":\"" + SolrUtils.PROPERTY_STATE_ACTIVE + "\"");
        } else if (onlyActive != null && !onlyActive){
            filterQueryList.add(FIELD_STATE + ":\"" + SolrUtils.PROPERTY_STATE_DEACTIVE + "\"");
        }
        if (models != null && !models.isEmpty() && hasValues(models)) {
            if (models.contains(MetaModel.MODELS_LEAF)) {
                filterQueryList.add(getModelQuery(Arrays.asList(new String[]{NdkPlugin.MODEL_PAGE, NdkPlugin.MODEL_NDK_PAGE, OldPrintPlugin.MODEL_PAGE, NdkAudioPlugin.MODEL_PAGE, BornDigitalModsPlugin.MODEL_ARTICLE, NdkEbornPlugin.MODEL_EARTICLE})));
            } else {
                filterQueryList.add(getModelQuery(models));
            }
        }
        if (pids != null && !pids.isEmpty() && hasValues(pids)) {
            String pidQuery = getPidsQuery(pids);
            if (pidQuery != null && !pidQuery.isEmpty()) {
                filterQueryList.add(pidQuery);
            }
            String identifiersQuery = getIdentifiersQuery(pids);
            if (identifiersQuery != null && !identifiersQuery.isEmpty()) {
                filterQueryList.add(identifiersQuery);
            }
        }
        if (owner != null && !owner.isEmpty()) {
            filterQueryList.add(FIELD_OWNER + ":\"" + owner + "\"");
        }
        if (organization != null && !organization.isEmpty()) {
            filterQueryList.add(FIELD_ORGANIZATION + ":\"" + organization + "\"");
        }
        if (processor != null && !processor.isEmpty() && (hasValues(Collections.singletonList(processor)))) {
            String userQuery = getUserQuery(Collections.singletonList(processor));
            if (userQuery != null && !userQuery.isEmpty()) {
                filterQueryList.add(userQuery);
            }
        }
        if (status != null && !status.isEmpty()) {
            filterQueryList.add(FIELD_STATUS + ":\"" + status + "\"");
        }
        if (parentPid != null && !parentPid.isEmpty()) {
            filterQueryList.add(FIELD_PARENT_PID + ":\"" + parentPid + "\"");
        }
        return filterQueryList;
    }

    private Integer updateLimit(Integer limit, List<String> pids) {
        if (limit != null && limit >= 1) {
            return limit;
        } else {
            if (pids != null && !pids.isEmpty()) {
                return pids.size();
            } else {
                return ROWS_DEFAULT_VALUE;
            }
        }
    }

    private List<SearchViewItem> searchPhraseImplementation(String phrase, String status, String organization, String processor, String model, String sortField, String sort, int offset, int limit) throws IOException {
        try {
            String query = createQuery(phrase);
            List<String> models = null;
            if (model != null && !model.isEmpty()) {
                models = Collections.singletonList(model);
            }
            List<String> filterQueryList = createFilterQuery(true, models, null, null, organization, processor, status, null);
            SolrQuery solrQuery = createQueryWithParams(query, FIELD_FULLTEXT, filterQueryList, offset, limit, transfromSort(sort), sortField);

            List<SearchViewItem> items = new ArrayList<>();
            QueryResponse response = this.solrClient.query(solrQuery);

            for (SolrDocument solrDocument : response.getResults()) {
                items.add(createItem(solrDocument, locale));
            }

            return items;
        } catch (SolrServerException ex) {
            ex.printStackTrace();
            throw new IOException(ex);
        }
    }

    private int searchCountImplementation(Integer offset, Integer limit, Boolean onlyActive, List<String> models, List<String> pids, String owner, String organization, String username, String label, String status, String parentPid) throws IOException {
        try {
            String query = createQuery(label);
            List<String> filterQueryList = createFilterQuery(onlyActive, models, pids, owner, organization, username, status, parentPid);
            SolrQuery solrQuery = createQueryWithParams(query, FIELD_LABEL, filterQueryList, offset, limit, null, null);

            QueryResponse response = this.solrClient.query(solrQuery);

            int total = response.getResults().size();
            return total;
        } catch (SolrServerException ex) {
            ex.printStackTrace();
            throw new IOException(ex);
        }
    }

    private SolrQuery createQueryWithParams(String query, String field, List<String> filterQueryList, Integer offset, Integer limit, SolrUtils.SortOperation sortOperation, String sortField) {
        SolrQuery solrQuery = new SolrQuery(query);
        solrQuery.set("q.op", "AND");
        if (query != null && !query.isEmpty() && !"*".equals(query)) {
            solrQuery.set("df", field);
        }
        for (String filterQuery : filterQueryList) {
            solrQuery.addFilterQuery(filterQuery);
        }
        if (offset != null && offset >= 0) {
            solrQuery.setStart(offset);
        }
        if (limit != null && limit >= 1) {
            solrQuery.setRows(limit);
        } else {
            solrQuery.setRows(ROWS_DEFAULT_VALUE);
        }
        sortField = fixLabelSortField(sortField);
        if (sortOperation != null && validField(sortField)) {
            SolrQuery.SortClause sortClause = new SolrQuery.SortClause(sortField, sortOperation.name().toLowerCase());
            solrQuery.setSort(sortClause);
        }
        return solrQuery;
    }

    private String fixLabelSortField(String sortField) {
        if (FIELD_LABEL.equals(sortField)) {
            return FIELD_LABEL_SORT;
        }
        return sortField;
    }

    private boolean validField(String sortField) {
        if (sortField == null || sortField.isEmpty()) {
            return false;
        } else {
            List<String> validFields = new ArrayList<>(Arrays.asList(FIELD_PID, FIELD_MODEL, FIELD_OWNER, FIELD_LABEL_SORT, FIELD_STATE, FIELD_CREATED, FIELD_MODIFIED, FIELD_ORGANIZATION, FIELD_USER, FIELD_STATUS));
            return validFields.contains(sortField);
        }
    }

    private String createQuery(String label) {
        if (label != null && !label.isEmpty()) {
            label = ClientUtils.escapeQueryChars(label);
            return label;
        }
        return "*";
    }

    private List<String> defaultFilterQuery() {
        List<String> defaultFilterQuery = new ArrayList<>();
        if (!allowDevicesAndSoftware) {
            defaultFilterQuery.add("-" + FIELD_MODEL + ":\"" + ClientUtils.escapeQueryChars(DeviceRepository.METAMODEL_ID) + "\"");
            defaultFilterQuery.add("-" + FIELD_MODEL + ":\"" + ClientUtils.escapeQueryChars(DeviceRepository.METAMODEL_AUDIODEVICE_ID) + "\"");
            defaultFilterQuery.add("-" + FIELD_MODEL + ":\"" + ClientUtils.escapeQueryChars(SoftwareRepository.METAMODEL_AGENT_ID) + "\"");
            defaultFilterQuery.add("-" + FIELD_MODEL + ":\"" + ClientUtils.escapeQueryChars(SoftwareRepository.METAMODEL_EVENT_ID) + "\"");
            defaultFilterQuery.add("-" + FIELD_MODEL + ":\"" + ClientUtils.escapeQueryChars(SoftwareRepository.METAMODEL_OBJECT_ID) + "\"");
            defaultFilterQuery.add("-" + FIELD_MODEL + ":\"" + ClientUtils.escapeQueryChars(SoftwareRepository.METAMODEL_SET_ID) + "\"");
        }
        return defaultFilterQuery;
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
