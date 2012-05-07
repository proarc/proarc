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
package cz.incad.pas.editor.server.fedora;

import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.incad.pas.editor.server.dublincore.DcStreamEditor;
import cz.incad.pas.editor.server.fedora.LocalStorage.LocalObject;
import cz.incad.pas.editor.server.imports.ImportBatchManager.ImportItem;
import cz.incad.pas.editor.server.mods.ModsStreamEditor;
import cz.incad.pas.editor.server.mods.custom.PageMapper;
import cz.incad.pas.editor.server.mods.custom.PageMapper.Page;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 *
 * @author Jan Pokorsky
 */
public final class PageView {

    public ImportBatchItemList list(int batchId, Collection<ImportItem> imports) {
        ArrayList<Item> result = new ArrayList<Item>(imports.size());
        LocalStorage storage = new LocalStorage();
//        Mapping mapping = new Mapping();
        PageMapper mapper = new PageMapper();
        for (ImportItem imp : imports) {
            File foxml = imp.getFoxmlAsFile();
            LocalObject local = storage.load(imp.getPid(), foxml);
            ModsStreamEditor editor = new ModsStreamEditor(local);
            Page page = mapper.map(editor.read());
//            Object custom = mapping.read(record.getMods(), MetaModelDataSource.EDITOR_PAGE);
            result.add(new Item(batchId, imp.getFilename(), imp.getPid(),
                    "model:page", page.getIndex(), page.getNumber(), page.getType(),
                    editor.getLastModified(), "XXX-user"));
        }
        return new ImportBatchItemList(result);
    }

    public ImportBatchItemList updateItem(int batchId, ImportItem item, long timestamp, String pageIndex, String pageNumber, String pageType)
            throws IOException {
        
        LocalStorage storage = new LocalStorage();
//        PageMapper mapper = new PageMapper();
        LocalObject local = storage.load(item.getPid(), item.getFoxmlAsFile());

        // MODS
        ModsStreamEditor editor = new ModsStreamEditor(local);
        ModsType mods = editor.read();
        editor.updatePage(mods, pageIndex, pageNumber, pageType);
        editor.write(mods, timestamp);

        // DC
        DcStreamEditor dcEditor = new DcStreamEditor(local);
        dcEditor.write(mods, "model:page", dcEditor.getLastModified());

        local.flush();
        Item update = new Item(batchId, item.getFilename(), item.getPid(), "XXX-model",
                pageIndex, pageNumber, pageType,
                editor.getLastModified(), "XXX-user");
        return new ImportBatchItemList(Arrays.asList(update));
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Item {

        private Integer batchId;
        private String filename;
        private String pid;
        private String model;
        private String pageIndex;
        private String pageNumber;
        private String pageType;
        private long timestamp;
        private String user;

        public Item(Integer batchId, String filename, String pid, String model,
                String pageIndex, String pageNumber, String pageType,
                long timestamp, String user) {
            this.batchId = batchId;
            this.filename = filename;
            this.pid = pid;
            this.model = model;
            this.pageIndex = pageIndex;
            this.pageNumber = pageNumber;
            this.pageType = pageType;
            this.timestamp = timestamp;
            this.user = user;
        }

        public Item() {
        }

    }

    @XmlRootElement(name="items")
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class ImportBatchItemList {

        @XmlElement(name="item")
        private List<Item> records;

        public ImportBatchItemList() {}

        public ImportBatchItemList(List<Item> objects) {
            this.records = objects;
        }

    }

}
