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
package cz.cas.lib.proarc.common.process.imports;

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.process.imports.FileSet;
import cz.cas.lib.proarc.common.process.imports.FileSet.FileEntry;
import cz.cas.lib.proarc.common.process.imports.FileSetImport;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.imports.ImportFileScanner;
import cz.cas.lib.proarc.common.process.imports.ImportFileScanner.Folder;
import cz.cas.lib.proarc.common.process.imports.ImportProcess;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import org.easymock.EasyMock;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class ImportFileScannerTest {

    public ImportFileScannerTest() {
    }

    @Rule
    public CustomTemporaryFolder tmpFolder = new CustomTemporaryFolder();

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() throws Exception {
        AppConfiguration config = AppConfigurationFactory.getInstance().create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, tmpFolder.getRoot().getPath());
        }});
        BatchManager.setInstance(config, EasyMock.createMock(DaoFactory.class));
    }

    @After
    public void tearDown() {

    }

    @Test
    public void testScan() throws Exception {
        tmpFolder.newFolder("A");
        tmpFolder.newFolder("B");
        tmpFolder.newFile("B/" + ImportFileScanner.IMPORT_STATE_FILENAME);
        File folderC = tmpFolder.newFolder("C");
        File tiff = new File(folderC, "scan1.tiff");
        tiff.createNewFile();
        tmpFolder.newFile("irrelevant.file");

        File folder = tmpFolder.getRoot();
        ImportFileScanner instance = new ImportFileScanner();
        List<Folder> result = instance.findSubfolders(folder, new FileSetImport());
        assertNotNull(result);
        assertEquals("found folders", 3, result.size());
        assertEquals("A", result.get(0).getHandle().getName());
        assertEquals("B", result.get(1).getHandle().getName());
        assertEquals("C", result.get(2).getHandle().getName());
        assertEquals(ImportFileScanner.State.EMPTY, result.get(0).getStatus());
        assertEquals(ImportFileScanner.State.IMPORTED, result.get(1).getStatus());
        assertEquals(ImportFileScanner.State.NEW, result.get(2).getStatus());
    }

    @Test(expected = FileNotFoundException.class)
    public void testScanFileNotFound() throws Exception {
        File folder = new File(tmpFolder.getRoot(), "A");
        ImportFileScanner instance = new ImportFileScanner();
        List<Folder> result = instance.findSubfolders(folder, new FileSetImport());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testScanFileAsParameter() throws Exception {
        File file = tmpFolder.newFile("illegal.param");
        ImportFileScanner instance = new ImportFileScanner();
        List<Folder> result = instance.findSubfolders(file, new FileSetImport());
    }

    private boolean isWindows() {
        return System.getProperty("os.name").toLowerCase().contains("win");
    }

    @Test
    public void testFolderSort() throws Exception {
//        tmpFolder.setDeleteOnExit(false);
        if (!isWindows()) {
            tmpFolder.newFolder("B");
            tmpFolder.newFolder("Na Návrší");
        }
        tmpFolder.newFolder("AAA");
        tmpFolder.newFolder("A");
        tmpFolder.newFile("AA");
        tmpFolder.newFolder("C");
        tmpFolder.newFolder("CH");
        tmpFolder.newFolder("H");
        tmpFolder.newFolder("b");
        tmpFolder.newFolder("BB");
        tmpFolder.newFolder("23");
        tmpFolder.newFolder("1");
        tmpFolder.newFolder("Č");
        tmpFolder.newFolder("Na");
        tmpFolder.newFolder("Na návrší");

        tmpFolder.newFolder("Nad návrším");

        File folder = tmpFolder.getRoot();
        ImportFileScanner instance = new ImportFileScanner();
        List<Folder> result = instance.findSubfolders(folder, new FileSetImport());
        assertNotNull(result);

        String[] resultAsArray = new String[result.size()];
        for (int i = 0; i < resultAsArray.length; i++) {
            resultAsArray[i] = result.get(i).getHandle().getName();
        }
        if (isWindows()) {
            String[]  expectedOrder = {"1", "23", "A", "AAA", "b", "BB", "C", "Č",
                "H", "CH", "Na", "Na návrší", "Nad návrším"};
            assertArrayEquals(Arrays.toString(resultAsArray), expectedOrder, resultAsArray);
        } else {
            String[]  expectedOrder = {"1", "23", "A", "AAA", "b", "B", "BB", "C", "Č",
                    "H", "CH", "Na", "Na návrší", "Na Návrší", "Nad návrším"};
                assertArrayEquals(Arrays.toString(resultAsArray), expectedOrder, resultAsArray);
        }
    }

    @Test
    public void testFindDigitalContent() throws Exception {
        File f1 = tmpFolder.newFile("f1.ext");
        File f2 = tmpFolder.newFile("f2.ext");
        tmpFolder.newFile(ImportFileScanner.IMPORT_STATE_FILENAME);
        tmpFolder.newFolder(ImportProcess.TMP_DIR_NAME);
        ImportFileScanner instance = new ImportFileScanner();
        List<File> result = instance.findDigitalContent(tmpFolder.getRoot());
        assertNotNull(result);
        assertArrayEquals(new Object[] {f1, f2}, result.toArray());
    }

    @Test
    public void testGetFileSets() throws Exception {
        File f2 = tmpFolder.newFile("f2.ext");
        File f1ext1 = tmpFolder.newFile("f1.ds.ext1");
        File f1ext2 = tmpFolder.newFile("f1.ext2");
        ImportFileScanner instance = new ImportFileScanner();
        List<File> files = instance.findDigitalContent(tmpFolder.getRoot());
        assertEquals(3, files.size());
        List<FileSet> fileSets = ImportFileScanner.getFileSets(files);
        assertNotNull(fileSets);
        assertEquals(2, fileSets.size());

        FileSet fs1 = fileSets.get(0);
        assertEquals("f1", fs1.getName());
        assertArrayEquals(new Object[] {f1ext1, f1ext2}, asFiles(fs1.getFiles()));

        FileSet fs2 = fileSets.get(1);
        assertEquals("f2", fs2.getName());
        assertArrayEquals(new Object[] {f2}, asFiles(fs2.getFiles()));
    }

    @Test
    public void testRepairFilename() throws Exception {
        File co1 = tmpFolder.newFile("CO_007.ext1");
        File co2 = tmpFolder.newFile("CO_007.ext2");
        File co3 = tmpFolder.newFile("CO_007.ext3");
        File audio1 = tmpFolder.newFile("MCA_007.ext1");
        File audio2 = tmpFolder.newFile("SA_007.ext2");
        File audio3 = tmpFolder.newFile("uca_007.ext3");
        ImportFileScanner instance = new ImportFileScanner();
        List<File> files = instance.findDigitalContent(tmpFolder.getRoot());
        assertEquals(6, files.size());
        List<FileSet> fileSets = ImportFileScanner.getFileSets(files);
        assertNotNull(fileSets);
        assertEquals(2, fileSets.size());

        FileSet co = fileSets.get(0);
        assertEquals("CO_007", co.getName());
        assertArrayEquals(new Object[] {co1, co2, co3}, asFiles(co.getFiles()));

        FileSet audio = fileSets.get(1);
        assertEquals("SA_007", audio.getName());
        assertArrayEquals(new Object[] {audio1, audio2, audio3}, asFiles(audio.getFiles()));
    }

    private static File[] asFiles(List<FileEntry> entries) {
        if (entries == null) {
            return null;
        }
        File[] files = new File[entries.size()];
        for (int i = 0; i < files.length; i++) {
            files[i] = entries.get(i).getFile();
        }
        return files;
    }
}
