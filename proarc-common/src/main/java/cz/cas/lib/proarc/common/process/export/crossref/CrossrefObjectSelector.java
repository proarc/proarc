/*
 * Copyright (C) 2016 Jan Pokorsky
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
package cz.cas.lib.proarc.common.process.export.crossref;

import cz.cas.lib.proarc.common.process.export.cejsh.CejshStatusHandler;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.emods.BornDigitalModsPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

/**
 * Finds exportable objects and possible set of their articles that should
 * be included in the resulting export.
 *
 * @author Jan Pokorsky
 */
class CrossrefObjectSelector {

    private final DigitalObjectCrawler crawler;
    private final CejshStatusHandler statusHandler;
    private List<CrossrefPackage> selections;

    public CrossrefObjectSelector(DigitalObjectCrawler crawler, CejshStatusHandler statusHandler) {
        this.crawler = crawler;
        this.statusHandler = statusHandler;
    }

    /**
     * Selects Crossref packages for the passed PIDs.
     */
    public List<CrossrefPackage> select(List<String> pids) {
        LinkedHashMap<DigitalObjectElement, Set<DigitalObjectElement>> articalParents = find(pids);
        this.selections = new ArrayList<CrossrefPackage>();
        for (Entry<DigitalObjectElement, Set<DigitalObjectElement>> entry : articalParents.entrySet()) {
            DigitalObjectElement articalParent = entry.getKey();
            Set<DigitalObjectElement> articleFilter = entry.getValue();

            try {
                List<DigitalObjectElement> entryPath = crawler.getPath(articalParent.getPid());
                entryPath.add(0, articalParent);
                searchPath(entryPath, articleFilter);
            } catch (DigitalObjectException ex) {
                statusHandler.error(articalParent, null, ex);
            }
        }
        return selections;
    }

    /**
     * Selects articles for the passed parent (periodical issue or volume) and the article's filter.
     *
     * @param parent a parent object to query for articles
     * @param filter {@code null} or empty set stands for all articles to include
     */
    public List<DigitalObjectElement> selectArticles(
            DigitalObjectElement parent, Set<DigitalObjectElement> filter
    ) throws DigitalObjectException {
        List<DigitalObjectElement> children = crawler.getChildren(parent.getPid());
        ArrayList<DigitalObjectElement> articles = new ArrayList<DigitalObjectElement>(children.size());
        for (DigitalObjectElement child : children) {
            if (BornDigitalModsPlugin.MODEL_ARTICLE.equals(child.getModelId())
                    && (filter == null || filter.isEmpty() || filter.contains(child))) {
                articles.add(child);
            }
        }
        return articles;
    }

    /**
     * Finds exportable object and possible set of their articles that should
     * be included in the resulting export.
     *
     * @param pids          a list of PIDs to search for
     * @return
     */
    private LinkedHashMap<DigitalObjectElement, Set<DigitalObjectElement>> find(
            List<String> pids
    ) {
        LinkedHashMap<DigitalObjectElement, Set<DigitalObjectElement>> dobjs =
                new LinkedHashMap<DigitalObjectElement, Set<DigitalObjectElement>>(pids.size());
        for (String pid : pids) {
            try {
                DigitalObjectElement elm = crawler.getEntry(pid);
                if (BornDigitalModsPlugin.MODEL_ARTICLE.equals(elm.getModelId())) {
                    // add article as inlude filter
                    DigitalObjectElement parent = crawler.getParent(pid);
                    if (parent == DigitalObjectElement.NULL) {
                        statusHandler.error(elm, "No parent!", null, null);
                        break;
                    }
                    Set<DigitalObjectElement> children = dobjs.get(parent);
                    if (children == null) {
                        children = new HashSet<DigitalObjectElement>();
                        dobjs.put(parent, children);
                    }
                    children.add(elm);
                } else {
                    if (!dobjs.containsKey(elm)) {
                        dobjs.put(elm, null);
                    }
                }
            } catch (DigitalObjectException ex) {
                statusHandler.error(pid, "No parent!", ex);
            }
        }
        return dobjs;
    }

    private void searchPath(List<DigitalObjectElement> entryPath,
                            Set<DigitalObjectElement> articleFilter
    ) throws DigitalObjectException {
        DigitalObjectElement entry = entryPath.get(0);
        String modelId = entry.getModelId();
        if (NdkPlugin.MODEL_PERIODICALISSUE.equals(modelId)
        ) {
            addSelection(entryPath, articleFilter);
        } else if (NdkPlugin.MODEL_PERIODICALVOLUME.equals(modelId)) {
            List<DigitalObjectElement> children = crawler.getChildren(entry.getPid());
            if (!children.isEmpty()
                    && BornDigitalModsPlugin.MODEL_ARTICLE.equals(children.get(0).getModelId())) {
                addSelection(entryPath, articleFilter);
            } else {
                searchChildren(entry, entryPath, children);
            }
        } else if (NdkPlugin.MODEL_PERIODICAL.equals(modelId)) {
            searchChildren(entry, entryPath);
        } else if (NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(modelId)) {
            DigitalObjectElement parent = entryPath.get(1);
            if (NdkPlugin.MODEL_PERIODICALISSUE.equals(parent.getModelId())) {
                // select the parent issue
                searchPath(entryPath.subList(1, entryPath.size()), articleFilter);
            } else {
                addSelection(entryPath, articleFilter);
            }
        }
    }

    private void searchChildren(DigitalObjectElement entry, List<DigitalObjectElement> entryPath) throws DigitalObjectException {
        List<DigitalObjectElement> children = crawler.getChildren(entry.getPid());
        searchChildren(entry, entryPath, children);
    }

    private void searchChildren(DigitalObjectElement entry, List<DigitalObjectElement> entryPath,
                                List<DigitalObjectElement> children) throws DigitalObjectException {

        for (DigitalObjectElement child : children) {
            List<DigitalObjectElement> childPath = new ArrayList<DigitalObjectElement>(entryPath.size() + 1);
            childPath.add(child);
            childPath.addAll(entryPath);
            searchPath(childPath, null);
        }
    }

    private void addSelection(List<DigitalObjectElement> entryPath, Set<DigitalObjectElement> articleFilter) {
        CrossrefPackage pkg = new CrossrefPackage();
        pkg.setArticleFilter(articleFilter);
        pkg.setPath(entryPath);
        selections.add(pkg);
    }

}
