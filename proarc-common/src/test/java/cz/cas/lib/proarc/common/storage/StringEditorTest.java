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

import cz.cas.lib.proarc.common.storage.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.storage.StringEditor.StringRecord;
import java.io.File;
import static org.junit.Assert.*;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 *
 * @author Jan Pokorsky
 */
public class StringEditorTest {

    @Rule
    public TemporaryFolder tmp = new TemporaryFolder();

    @Test
    public void testWrite() throws Exception {
        LocalStorage storage = new LocalStorage();
        LocalObject local = storage.create();
        StringEditor instance = StringEditor.ocr(local);
        final String content = "ocr";
        instance.write(content, 0, null);
        local.flush();

        StringRecord result = instance.readRecord();
        assertNotNull(result);
        assertEquals(content, result.getContent());
        assertEquals(local.getPid(), result.getPid());

        instance.write("update", result.getTimestamp(), null);
    }

    @Test
    public void testReadWrite() throws Exception {
        LocalStorage storage = new LocalStorage();
        File foxml = tmp.newFile();
        LocalObject local = storage.create(foxml);
        StringEditor instance = StringEditor.ocr(local);
        final String content = "ocr";
        instance.write(content, 0, null);
        local.flush();

        local = storage.load(local.getPid(), foxml);
        instance = StringEditor.ocr(local);
        StringRecord result = instance.readRecord();

        local = storage.load(local.getPid(), foxml);
        instance = StringEditor.ocr(local);
        instance.write("update", result.getTimestamp(), null);
        local.flush();

        local = storage.load(local.getPid(), foxml);
        instance = StringEditor.ocr(local);
        StringRecord result2 = instance.readRecord();
        assertNotNull(result2);
        assertEquals(local.getPid(), result2.getPid());
        assertEquals("update", result2.getContent());
    }

}
