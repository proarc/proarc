/*
 * Copyright (C) 2011 Jan Pokorsky
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
package cz.incad.pas.editor.server.imports;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 *
 * @author Jan Pokorsky
 */
public class ImportBatchManager {

    private static final Logger LOG = Logger.getLogger(ImportBatchManager.class.getName());
    private static final ImportBatchManager INSTANCE = new ImportBatchManager();

    /** memory storage for now */
    private final Map<Integer, ImportBatch> map = new HashMap<Integer, ImportBatch>();

    private int temp_itemSequence;

    public static ImportBatchManager getInstance() {
        return INSTANCE;
    }

    public ImportBatchManager() {
        try {
            ImportBatch ib1 = new ImportBatch(1, "path/to/first_import",
                    new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ").parse("2010-05-20T15:50:48"), 1);
            ib1.addItem(new ImportItem(++temp_itemSequence, "file1", "uuid:1"));
            ib1.addItem(new ImportItem(++temp_itemSequence, "file2", "uuid:2"));
            ib1.addItem(new ImportItem(++temp_itemSequence, "file2", "uuid:3"));

            map.put(1, ib1);
        } catch (ParseException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

//    public Collection<ImportBatch> findAll() {
//        return findAll(false);
//    }
    public Collection<ImportBatch> findAll(Integer user, boolean withItems) {
        synchronized(map) {
            ArrayList<ImportBatch> result = new ArrayList<ImportBatch>(map.values());
            if (withItems) {
                result = new ArrayList<ImportBatch>(map.values());
            } else {
                result = new ArrayList<ImportBatch>(map.size());
                for (ImportBatch b : map.values()) {
                    result.add(new ImportBatch(b.getId(), b.getFolderPath(), b.getTimeStamp(), b.getUser()));
                }
            }
            return result;
        }
    }

    public ImportBatch add(String path, int user) {
        synchronized(map) {
            int id = Collections.max(map.keySet()) + 1;
            ImportBatch batch = new ImportBatch(id, path, new Date(), user);
            map.put(id, batch);
            return batch;
        }
    }

    public ImportItem addItem(int batchId, ImportItem item) {
        synchronized(map) {
            ImportBatch batch = map.get(batchId);
            if (batch == null) {
                throw new IllegalStateException("Unknown batch: " + batchId);
            }
            item.id = ++temp_itemSequence;
            batch.addItem(item);
            return item;
        }
    }

    @XmlRootElement(name="batch")
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class ImportBatch {
        @XmlElement(required=true)
        private int id;
        private String folderPath;
        private Date timeStamp;
        private int user;
        private List<ImportItem> items;

        public ImportBatch() {
        }

        public ImportBatch(Integer id, String folderPath, Date timeStamp, int user) {
            this.id = id;
            this.folderPath = folderPath;
            this.timeStamp = timeStamp;
            this.user = user;
        }

        public void addItem(ImportItem item) {
            List<ImportItem> l = getItems();
            l.add(item);
        }

        public List<ImportItem> getItems() {
            if (items == null) {
                items = new ArrayList<ImportItem>();
            }
            return items;
        }

        public String getFolderPath() {
            return folderPath;
        }

        public int getId() {
            return id;
        }

        public Date getTimeStamp() {
            return timeStamp;
        }

        public int getUser() {
            return user;
        }

    }

    /**
     * Describes imported digital object.
     * In case the special RELS-EXT relation (importBatch->batchId) will exist
     * it could be read from triple store index. Advantage of this solution
     * would be always live content. Otherwise we should update imports table
     * on Digital Object modifications.
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class ImportItem {
        @XmlElement(required=true)
        private Integer id;
        private String filename;
        private String pid;
        // XXX get from digital object?
        private String model;

        private ImportItem() {
        }

        public ImportItem(String filename, String pid) {
            this.filename = filename;
            this.pid = pid;
        }

        public ImportItem(int id, String filename, String pid) {
            this(filename, pid);
            this.id = id;
        }

        public String getFilename() {
            return filename;
        }

    }

}
