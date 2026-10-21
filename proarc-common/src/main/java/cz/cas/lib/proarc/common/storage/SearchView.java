/*
 * Copyright (C) 2026 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.storage;

import cz.cas.lib.proarc.common.object.HasDataHandler;
import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public abstract class SearchView {

    public Map<String, Long> countObjectsByModel(String organization, boolean active) throws IOException {
        throw new IOException("Method is not implemented");
    }

    public boolean isDeviceInUse(String deviceId) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public boolean isSoftwareInUse(String softwareId) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> find(String... pids) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> find(String pid) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> find(List<String> pids) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> find(boolean onlyActive, List<String> pids) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public String findModel(String pid) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findAllObjects() throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findReferrers(String pid) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findByModel(String modelId) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findByModel(int offset, int limit, String modelId) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findSortedChildren(String parentPid) throws IOException, DigitalObjectException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findSortedChildrenWithPagesFirst(String parentPid) throws IOException, DigitalObjectException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findChildren(String pid) throws IOException, DigitalObjectException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findChildrenHierarchy(String pid) throws IOException, DigitalObjectException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findLastCreated(int offset, String model, String sort) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findByModels(int offset, int limit, String... modelIds) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findQuery(String title, String label, String identifier, String owner, String model, Collection<String> hasOwners) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findQuery(SearchViewQuery q, String status) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findPhrase(String phrase, String status, String organization, String processor, String model, String sortField, String sort, int offset, int limit) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findAlphabetical(int offset, String model, String organization, int limit, String sort) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findLastCreated(int offset, String model, String organization, int limit, String sort) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findLastModified(int offset, String model, String organization, int limit, String sort) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findAdvancedSearchItems(Boolean onlyActive, String identifier, String label, String owner, String status, String organization, String processor, String model, String parentPid, String sortField, String sort, int offset, int limit) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public int countByModels(String organization, String... modelIds) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public int findAdvancedSearchCount(Boolean onlyActive, String identifier, String label, String owner, String status, String organization, String processor, String model, String parentPid) throws IOException {
        throw new IOException("Method is not implmeneted");
    }

    public List<SearchViewItem> findByOwner(String owner) throws IOException {
        return findAdvancedSearchItems(true, null, null, owner, null, null, null, null, null, "label", "desc", 0, Integer.MAX_VALUE);
    }

    public List<SearchViewItem> findByProcessor(String processor) throws IOException {
        return findAdvancedSearchItems(true, null, null, null, null, null, processor, null, null, "label", "desc", 0, Integer.MAX_VALUE);
    }

    public int countByOwner(String owner) throws IOException {
        throw new IOException("Method is not implemented");
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
