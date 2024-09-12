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
package cz.cas.lib.proarc.common.storage;

import cz.cas.lib.proarc.common.dao.BatchItem.ObjectState;
import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import cz.cas.lib.proarc.common.process.BatchManager.BatchItemObject;
import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * A view over batch items that are usually pages. Each plugin that wants to
 * handle batch imports has to provide {@link MetadataHandler} that implements
 * {@link PageViewHandler}.
 *
 * @author Jan Pokorsky
 */
public final class PageView {

    private LocalStorage storage = new LocalStorage();

    public List<Item> list(int batchId, Collection<BatchItemObject> imports, Locale locale) throws DigitalObjectException {
        ArrayList<Item> result = new ArrayList<>(imports.size());
        for (BatchItemObject imp : imports) {
            ObjectState objectState = imp.getState();
            if (objectState == ObjectState.LOADING || objectState == ObjectState.LOADING_FAILED) {
                // issue 245: it is unsafe to touch FOXML file if the object
                // has not been loaded yet or it is broken
                result.add(new Item(batchId, imp.getItem().getFile(), imp.getPid()));
                continue;
            }
            result.add(createItem(imp, locale));
        }
        return result;
    }

    private Item createItem(BatchItemObject imp, Locale locale) throws DigitalObjectException {
        Integer batchId = imp.getBatchId();
        File foxml = imp.getFile();
        LocalObject local = storage.load(imp.getPid(), foxml);
        DigitalObjectHandler doHandler = DigitalObjectManager.getDefault().createHandler(local);
        MetadataHandler<?> metadataHandler = doHandler.metadata();

        DescriptionMetadata<?> metadata = metadataHandler.getMetadata();
        RelationEditor relsExt = doHandler.relations();
        String model = relsExt.getModel();
        String filename = relsExt.getImportFile();
        Item item = new Item(batchId, filename, imp.getPid(),
                model, metadata.getTimestamp(), local.getOwner(), local.getLabel());

        if (metadataHandler instanceof PageViewHandler) {
            PageViewHandler pvh = (PageViewHandler) metadataHandler;
            PageViewItem pvItem = pvh.createPageViewItem(locale);
            updateItem(item, pvItem);
        } else {
            throw new DigitalObjectException(imp.getPid(), batchId, null, "Model '" + model + "' unsuported by any plug-in!", null);
        }
        return item;
    }

    private Item updateItem(Item item, PageViewItem pvItem) {
        item.pageIndex = pvItem.getPageIndex();
        item.pageNumber = pvItem.getPageNumber();
        item.pageType = pvItem.getPageType();
        item.pageTypeLabel = pvItem.getPageTypeLabel();
        item.pagePosition = pvItem.getPagePosition();
        item.pageRepre = pvItem.getPageRepre();
        return item;
    }
    /**
     * Gets localized label of fedora object containing page.
     * It relies on page label syntax: {@code <pageLabel>, <pageType>}
     *
     * @param label label of fedora object
     * @param pageTypeTitles bundle of page type titles in target locale
     * @return localized label
     *
     * @see ModsUtils#getLabel
     */
    public static String resolveFedoraObjectLabel(String label, ResourceBundle pageTypeTitles) {
        int typeIndex = label.lastIndexOf(", ") + 2;
        if (typeIndex - 2 >= 0 && typeIndex < label.length()) {
            String typeCode = label.substring(typeIndex);
            try {
                if (!typeCode.isEmpty()) {
                    String typeName = pageTypeTitles.getString(typeCode);
                    label = label.substring(0, typeIndex) + typeName;
                }
            } catch (MissingResourceException e) {
                // ignore
            }
        }
        return label;
    }

    public interface PageViewHandler {

        PageViewItem createPageViewItem(Locale locale) throws DigitalObjectException;

        /** Rewrites metadata with page. */
        void setPage(PageViewItem page, String message) throws DigitalObjectException;

    }

    public static class PageViewItem {

        private String pageIndex;
        private String pageNumber;
        private String pageType;
        private String pagePosition;
        private String pageTypeLabel;
        private String pageRepre;

        public String getPageIndex() {
            return pageIndex;
        }

        public void setPageIndex(String pageIndex) {
            this.pageIndex = pageIndex;
        }

        public String getPageNumber() {
            return pageNumber;
        }

        public void setPageNumber(String pageNumber) {
            this.pageNumber = pageNumber;
        }

        public String getPageType() {
            return pageType;
        }

        public void setPageType(String pageType) {
            this.pageType = pageType;
        }

        public String getPageTypeLabel() {
            return pageTypeLabel;
        }

        public void setPageTypeLabel(String pageTypeLabel) {
            this.pageTypeLabel = pageTypeLabel;
        }

        public String getPagePosition() {
            return pagePosition;
        }

        public void setPagePosition(String pagePosition) {
            this.pagePosition = pagePosition;
        }

        public String getPageRepre() {
            return pageRepre;
        }

        public void setPageRepre(String pageRepre) {
            this.pageRepre = pageRepre;
        }
    }

    public static class Item {

        private Integer batchId;
        private String filename;
        private String pid;
        private String model;
        private String pageIndex;
        private String pageNumber;
        private String pageType;
        private String pageTypeLabel;
        private String pagePosition;
        private String pageRepre;
        private long timestamp;
        private String user;
        private String label;

        public Item(Integer batchId, String pid) {
            this(batchId, null, pid);
        }

        public Item(Integer batchId, String filename, String pid) {
            this(batchId, filename, pid, null, -1, null, null);
        }

        public Item(Integer batchId, String filename, String pid, String model, long timestamp, String user, String label) {
            this(batchId, filename, pid, model, null, null, null, null, null, timestamp, user, label);
        }

        public Item(Integer batchId, String filename, String pid, String model,
                String pageIndex, String pageNumber, String pageType, String pagePosition, String pageRepre,
                long timestamp, String user, String label) {
            this.batchId = batchId;
            this.filename = filename;
            this.pid = pid;
            this.model = model;
            this.pageIndex = pageIndex;
            this.pageNumber = pageNumber;
            this.pageType = pageType;
            this.pagePosition = pagePosition;
            this.pageRepre = pageRepre;
            this.timestamp = timestamp;
            this.user = user;
            this.label = label;
        }

        public Item() {
        }

        public Integer getBatchId() {
            return batchId;
        }

        public String getFilename() {
            return filename;
        }

        public String getPid() {
            return pid;
        }

        public String getModel() {
            return model;
        }

        public String getPageIndex() {
            return pageIndex;
        }

        public String getPageNumber() {
            return pageNumber;
        }

        public String getPageType() {
            return pageType;
        }

        public String getPageTypeLabel() {
            return pageTypeLabel;
        }

        public long getTimestamp() {
            return timestamp;
        }

        public String getUser() {
            return user;
        }

        public String getLabel() {
            return label;
        }

        public String getPagePosition() {
            return pagePosition;
        }

        public String getPageRepre() {
            return pageRepre;
        }
    }

}
