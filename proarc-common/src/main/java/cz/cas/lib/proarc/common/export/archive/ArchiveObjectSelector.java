/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.export.archive;

import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.object.DigitalObjectCrawler;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import cz.cas.lib.proarc.common.object.emods.BornDigitalModsPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * It selects digital object to archive.
 *
 * @author Jan Pokorsky
 */
public class ArchiveObjectSelector {
    
    protected final DigitalObjectCrawler crawler;
    protected final List<List<DigitalObjectElement>> selectedObjects = new ArrayList<>();
    protected final Set<String> processedPids = new HashSet<>();

    public ArchiveObjectSelector(DigitalObjectCrawler crawler) {
        this.crawler = crawler;
    }

    /**
     * Gets a list of leaf-root paths of selected objects.
     * @return the list.
     */
    public List<List<DigitalObjectElement>> getSelectedObjects() {
        return Collections.unmodifiableList(selectedObjects);
    }

    /**
     * Computes the selection.
     * @param pids a list of PIDs to search
     * @throws DigitalObjectException failure
     */
    public void select(List<String> pids) throws DigitalObjectException, MetsExportException {
        for (String pid : pids) {
            select(pid);
        }
    }

    private void select(String pid) throws DigitalObjectException, MetsExportException {
        DigitalObjectElement entry = crawler.getEntry(pid);
        List<DigitalObjectElement> entryPath = crawler.getPath(pid);
        entryPath.add(0, entry);
        searchPath(entryPath);
    }

    protected void searchPath(List<DigitalObjectElement> entryPath) throws DigitalObjectException, MetsExportException {
        DigitalObjectElement entry = entryPath.get(0);
        if (processedPids.contains(entry.getPid())) {
            return ;
        }
        processedPids.add(entry.getPid());
        String modelId = entry.getModelId();
        if (NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(modelId)
                || NdkPlugin.MODEL_MONOGRAPHUNIT.equals(modelId)
                || NdkPlugin.MODEL_CARTOGRAPHIC.equals(modelId)
                || NdkPlugin.MODEL_PERIODICALISSUE.equals(modelId)
                || NdkPlugin.MODEL_SHEETMUSIC.equals(modelId)
                || NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME.equals(modelId)
                || NdkEbornPlugin.MODEL_EPERIODICALISSUE.equals(modelId)
                ) {
            if (NdkPlugin.MODEL_MONOGRAPHVOLUME.equals(modelId) && entryPath.size() > 0 && entryPath.get(1) != null &&
                    NdkPlugin.MODEL_MONOGRAPHTITLE.equals(entryPath.get(1).getModelId())) {
                throw new MetsExportException("Nepovolená vazba - Ndk Svazek monografie pod Ndk Vícedílnou monografii.");
            }
            addSelection(entryPath);
        } else if (NdkPlugin.MODEL_MONOGRAPHTITLE.equals(modelId)
                || NdkPlugin.MODEL_PERIODICAL.equals(modelId)
                || NdkPlugin.MODEL_PERIODICALVOLUME.equals(modelId)
                || NdkEbornPlugin.MODEL_EMONOGRAPHTITLE.equals(modelId)
                || NdkEbornPlugin.MODEL_EPERIODICAL.equals(modelId)
                || NdkEbornPlugin.MODEL_EPERIODICALVOLUME.equals(modelId)) {
            searchChildren(entry, entryPath);
        } else if (NdkPlugin.MODEL_PERIODICALSUPPLEMENT.equals(modelId)) {
            DigitalObjectElement parent = entryPath.get(1);
            if (NdkPlugin.MODEL_PERIODICALISSUE.equals(parent.getModelId())) {
                // select the parent issue
                searchPath(entryPath.subList(1, entryPath.size()));
            } else {
                addSelection(entryPath);
            }
        } else if (NdkPlugin.MODEL_ARTICLE.equals(modelId)
                || NdkPlugin.MODEL_CHAPTER.equals(modelId)
                || NdkPlugin.MODEL_PAGE.equals(modelId)
                || NdkPlugin.MODEL_PICTURE.equals(modelId)
                || BornDigitalModsPlugin.MODEL_ARTICLE.equals(modelId)
                || NdkEbornPlugin.MODEL_EARTICLE.equals(modelId)
                ) {
            if (entryPath.size() == 1) {
                throw new DigitalObjectException(entry.getPid(), "Unexpected hierarchy: " + entryPath);
            } else {
                searchPath(entryPath.subList(1, entryPath.size()));
            }
        } else {
            throw new DigitalObjectException(entry.getPid(), "Unexpected model: " + entry);
        }
    }

    protected void searchChildren(DigitalObjectElement entry, List<DigitalObjectElement> entryPath) throws DigitalObjectException, MetsExportException {
        List<DigitalObjectElement> children = crawler.getChildren(entry.getPid());
        for (DigitalObjectElement child : children) {
            List<DigitalObjectElement> childPath = new ArrayList<>(entryPath.size() + 1);
            childPath.add(child);
            childPath.addAll(entryPath);
            searchPath(childPath);
        }
    }

    protected void addSelection(List<DigitalObjectElement> entryPath) {
        selectedObjects.add(entryPath);
    }

}
