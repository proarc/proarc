/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.object;

import com.yourmediashelf.fedora.client.FedoraClientException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.fedora.SearchViewItem;
import cz.cas.lib.proarc.common.object.DigitalObjectElement.Factory;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * It helps to crawl hierarchies of digital objects and provides access to the index and the storage.
 * It caches visited objects.
 *
 * @author Jan Pokorsky
 */
public class DigitalObjectCrawler {

    private final DigitalObjectManager dom;
    private final SearchView search;
    /** Maps PIDs to their parent elements. */
    private final Map<String, DigitalObjectElement> parents;
    /** Maps PIDs to elements. */
    private final Map<String, DigitalObjectElement> cache;
    private final DigitalObjectElement.Factory elmFactory;

    public DigitalObjectCrawler(DigitalObjectManager dom, SearchView search) {
        this(dom, search, null);
    }

    public DigitalObjectCrawler(DigitalObjectManager dom, SearchView search, DigitalObjectElement.Factory elmFactory) {
        this.dom = dom;
        this.search = search;
        this.parents = new HashMap<String, DigitalObjectElement>();
        this.cache = new HashMap<String, DigitalObjectElement>();
        this.elmFactory = elmFactory != null ? elmFactory : new Factory();
    }

    public DigitalObjectElement getEntry(SearchViewItem item) throws DigitalObjectException {
        String pid = item.getPid();
        return getEntry(pid);
    }

    public DigitalObjectElement getEntry(String pid) throws DigitalObjectException {
        DigitalObjectElement entry = cache.get(pid);
        if (entry == null) {
            SearchViewItem item = searchItem(pid);
            if (item != null) {
                entry = createEntry(item);
                cache.put(pid, entry);
            } else {
                throw new DigitalObjectNotFoundException(pid);
            }
        }
        return entry;
    }

    private DigitalObjectHandler createHandler(SearchViewItem item) throws DigitalObjectNotFoundException {
        FedoraObject fo = dom.find(item.getPid(), null);
        DigitalObjectHandler doHandler = dom.createHandler(fo);
        return doHandler;
    }

    private DigitalObjectElement createEntry(SearchViewItem item) throws DigitalObjectNotFoundException {
        DigitalObjectElement entry = elmFactory.create(item, createHandler(item));
        return entry;
    }

    /**
     * Gets parent.
     * @param pid child ID
     * @return the parent element or {@link DigitalObjectElement#NULL} in case of root.
     * @throws DigitalObjectNotFoundException failure
     */
    public DigitalObjectElement getParent(String pid) throws DigitalObjectNotFoundException {
        DigitalObjectElement parentEntry = parents.get(pid);
        if (parentEntry == null) {
            SearchViewItem parentItem = searchParentItem(pid);
            if (parentItem == null) {
                parentEntry = DigitalObjectElement.NULL;
            } else {
                parentEntry = cache.get(parentItem.getPid());
                if (parentEntry == null) {
                    parentEntry = createEntry(parentItem);
                    cache.put(parentItem.getPid(), parentEntry);
                }
            }
            parents.put(pid, parentEntry);
        }
        return parentEntry;
    }

    public List<DigitalObjectElement> getChildren(String pid) throws DigitalObjectException {
        List<SearchViewItem> children = searchChildren(pid);
        ArrayList<DigitalObjectElement> result = new ArrayList<DigitalObjectElement>();
        for (SearchViewItem item : children) {
            DigitalObjectElement childElement = cache.get(item.getPid());
            if (childElement == null) {
                childElement = createEntry(item);
                cache.put(item.getPid(), childElement);
            }
            DigitalObjectElement parentElm = cache.get(pid);
            if (parentElm != null) {
                parents.put(item.getPid(), parentElm);
            }
            result.add(childElement);
        }
        return result;
    }

    public List<DigitalObjectElement> getReversePath(String pid) throws DigitalObjectNotFoundException {
        List<DigitalObjectElement> path = getPath(pid);
        Collections.reverse(path);
        return path;
    }

    /**
     * Gets list of parents up to the root.
     * @param pid object ID to search parents
     * @return the list
     */
    public List<DigitalObjectElement> getPath(String pid) throws DigitalObjectNotFoundException {
        ArrayList<DigitalObjectElement> path = new ArrayList<DigitalObjectElement>();
        for (DigitalObjectElement entry = getParent(pid); entry != DigitalObjectElement.NULL; entry = getParent(pid)) {
            pid = entry.getItem().getPid();
            path.add(entry);
        }
        return path;
    }

    SearchViewItem searchParentItem(String pid) {
        List<SearchViewItem> issueParents;
        try {
            issueParents = search.findReferrers(pid);
        } catch (Exception ex) {
            throw new IllegalStateException(pid, ex);
        }
        if (issueParents.isEmpty()) {
            return null;
        }
        return issueParents.get(0);
    }

    SearchViewItem searchItem(String pid) throws DigitalObjectException {
        try {
            List<SearchViewItem> items = search.find(pid);
            if (items.isEmpty()) {
                return null;
            }
            return items.get(0);
        } catch (FedoraClientException ex) {
            throw new DigitalObjectException(pid, ex);
        } catch (IOException ex) {
            throw new DigitalObjectException(pid, ex);
        }
    }

    List<SearchViewItem> searchChildren(String pid) throws DigitalObjectException {
        try {
            List<SearchViewItem> children = search.findSortedChildren(pid);
            return children;
        } catch (FedoraClientException ex) {
            throw new DigitalObjectException(pid, ex);
        } catch (IOException ex) {
            throw new DigitalObjectException(pid, ex);
        }
    }

}
