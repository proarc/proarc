package cz.cas.lib.proarc.common.storage;

import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.object.HasDataHandler;
import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.Locale;

public abstract class SearchView {

    public boolean isDeviceInUse(String deviceId) throws IOException, FedoraClientException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> find(String... pids) throws FedoraClientException, IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> find(String pid) throws FedoraClientException, IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> find(List<String> pids) throws FedoraClientException, IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> find(boolean onlyActive, List<String> pids) throws FedoraClientException, IOException {
        throw new IOException("Method is not implmeneted");
    }
    public List<SearchViewItem> findAllObjects() throws FedoraClientException, IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findReferrers(String pid) throws IOException, FedoraClientException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findByModel(String modelId) throws  IOException, FedoraClientException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findByModel(int offset, String modelId) throws IOException, FedoraClientException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findByModels(String modelId1, String modelId2) throws  IOException, FedoraClientException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findSortedChildren(String parentPid) throws FedoraClientException, IOException, DigitalObjectException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findSortedChildrenWithPagesFirst(String parentPid) throws FedoraClientException, IOException, DigitalObjectException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findChildren(String pid) throws FedoraClientException, IOException, DigitalObjectException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findChildrenHierarchy(String pid) throws FedoraClientException, IOException, DigitalObjectException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findLastCreated(int offset, String model, String user, Boolean filterWithoutExtension, String sort) throws FedoraClientException, IOException {
        throw new IOException("Method is not implmeneted");
    }


    public List<SearchViewItem> findByModels(int offset, String modelId1, String modelId2) throws IOException, FedoraClientException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findQuery(String title, String label, String identifier, String owner, String model, Collection<String> hasOwners) throws FedoraClientException, IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findQuery(SearchViewQuery q, String status) throws FedoraClientException, IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findPhrase(String phrase, String status, String organization, String processor, String model, Boolean allowAllForProcessor, Boolean filterWithoutExtension, String sortField, String sort, int offset, int limit) throws IOException, FedoraClientException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findAlphabetical(int offset, String model, String user, String organization, String userName, Boolean filterWithoutExtension, int limit, String sort) throws IOException, FedoraClientException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findLastCreated(int offset, String model, String user, String organization, String username, Boolean filterWithoutExtension, int limit, String sort) throws FedoraClientException, IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findLastModified(int offset, String model, String user, String organization, String username, Boolean filterWithoutExtension, int limit, String sort) throws FedoraClientException, IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findAdvancedSearchItems(String identifier, String label, String owner, String status, String organization, String processor, String model, String creator, Boolean allowAllForProcessor, Boolean filterWithoutExtension, String sortField, String sort, int offset, int limit) throws IOException, FedoraClientException {
        throw new IOException("Method is not implmeneted");
    }

    public int countModels(String model, String user, String organization, String username, Boolean filterWithoutExtension) throws FedoraClientException, IOException {
        throw new IOException("Method is not implmeneted");
    }

    public int findAdvancedSearchCount(String identifier, String label, String owner, String status, String organization, String processor, String model, String creator, Boolean allowAllForProcessor, Boolean filterWithoutExtension) throws FedoraClientException, IOException {
        throw new IOException("Method is not implmeneted");
    }

    public interface HasSearchViewHandler extends HasDataHandler {
        SearchViewHandler createSearchViewHandler();
    }

    /**
     * Implement to customize a result label of a search.
     */
    public interface SearchViewHandler {
        String getObjectLabel(SearchViewItem item, Locale locale);
    }


}
