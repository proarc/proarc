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

import cz.incad.pas.editor.server.imports.ImportFileScanner.Folder;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class ImportFileScannerTest {

    public ImportFileScannerTest() {
    }

    @Rule
    public TemporaryFolder tmpFolder = new TemporaryFolder();

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() throws Exception {
    }

    @After
    public void tearDown() {

    }

    @Test
    public void testScan() throws Exception {
        tmpFolder.newFolder("A");
        tmpFolder.newFolder("B");
        tmpFolder.newFile("B/" + ImportFileScanner.IMPORT_STATE_FILENAME);
        tmpFolder.newFolder("C");
        tmpFolder.newFile("irrelevant.file");

        File folder = tmpFolder.getRoot();
        ImportFileScanner instance = new ImportFileScanner();
        List<Folder> result = instance.findSubfolders(folder);
        assertNotNull(result);
        assertEquals("found folders", 3, result.size());
        assertEquals("A", result.get(0).getHandle().getName());
        assertEquals("B", result.get(1).getHandle().getName());
        assertEquals("C", result.get(2).getHandle().getName());
        assertEquals(ImportFileScanner.State.NEW, result.get(0).getStatus());
        assertEquals(ImportFileScanner.State.IMPORTED, result.get(1).getStatus());
        assertEquals(ImportFileScanner.State.NEW, result.get(2).getStatus());
    }

    @Test
    public void testScanFileNotFound() throws Exception {
        File folder = new File(tmpFolder.getRoot(), "A");
        ImportFileScanner instance = new ImportFileScanner();
        try {
            List<Folder> result = instance.findSubfolders(folder);
            fail("exception expected");
        } catch (FileNotFoundException ex) {
            // expected
        }
    }

    @Test
    public void testScanFileAsParameter() throws Exception {
        File file = tmpFolder.newFile("illegal.param");
        ImportFileScanner instance = new ImportFileScanner();
        try {
            List<Folder> result = instance.findSubfolders(file);
            fail("exception expected");
        } catch (IllegalArgumentException ex) {
            // expected
        }
    }
}
