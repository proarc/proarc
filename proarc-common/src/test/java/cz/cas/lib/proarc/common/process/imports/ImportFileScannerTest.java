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

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.imports.FileSet.FileEntry;
import cz.cas.lib.proarc.common.process.imports.ImportFileScanner.Folder;
import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import org.easymock.EasyMock;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 *
 * @author Jan Pokorsky
 */
public class ImportFileScannerTest {

    public ImportFileScannerTest() {
    }

    @TempDir
    File tempDir;

    @BeforeAll
    public static void setUpClass() throws Exception {
    }

    @AfterAll
    public static void tearDownClass() throws Exception {
    }

    @BeforeEach
    public void setUp() throws Exception {
        AppConfiguration config = AppConfigurationFactory.getInstance().create(new HashMap<String, String>() {{
            put(AppConfiguration.PROPERTY_APP_HOME, tempDir.getPath());
        }});
        BatchManager.setInstance(config, EasyMock.createMock(DaoFactory.class));
    }

    @AfterEach
    public void tearDown() {

    }

    @Test
    public void testScan() throws Exception {
        File tempFolderA = new File(tempDir, "A");
        tempFolderA.mkdir();

        File tempFolderB = new File(tempDir, "B");
        tempFolderB.mkdir();

        File tempFileC = new File(tempFolderB, ImportFileScanner.IMPORT_STATE_FILENAME);
        tempFileC.createNewFile();

        File tiff = new File(tempFileC, "scan1.tiff");

        tiff.createNewFile();

        File tempIrrelevantFile = new File(tempDir, "irrelevant.file");
        tempIrrelevantFile.createNewFile();

        File folder = tempDir;
        ImportFileScanner instance = new ImportFileScanner();
        List<Folder> result = instance.findSubfolders(folder, new FileSetImport());
        assertNotNull(result);
        assertEquals(3, result.size(), "found folders");
        assertEquals("A", result.get(0).getHandle().getName());
        assertEquals("B", result.get(1).getHandle().getName());
        assertEquals("C", result.get(2).getHandle().getName());
        assertEquals(ImportFileScanner.State.EMPTY, result.get(0).getStatus());
        assertEquals(ImportFileScanner.State.IMPORTED, result.get(1).getStatus());
        assertEquals(ImportFileScanner.State.NEW, result.get(2).getStatus());
    }

    @Test
    public void testScanFileNotFound() throws Exception {
        File folder = new File(tempDir, "A");
        ImportFileScanner instance = new ImportFileScanner();
        List<Folder> result = instance.findSubfolders(folder, new FileSetImport());
    }

    @Test
    public void testScanFileAsParameter() throws Exception {
        File file = new File(tempDir, "illegal.param");
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
            File tempFolderB = new File(tempDir, "B");
            tempFolderB.mkdir();

            File tempFolderNaNavrsi = new File(tempDir, "Na Návrší");
            tempFolderNaNavrsi.mkdir();
        }

        File tempFolderA = new File(tempDir, "A");
        tempFolderA.mkdir();

        File tempFolderAA = new File(tempDir, "AA");
        tempFolderAA.mkdir();

        File tempFolderAAA = new File(tempDir, "AAA");
        tempFolderAAA.mkdir();

        File folder = tempDir;
        ImportFileScanner instance = new ImportFileScanner();
        List<Folder> result = instance.findSubfolders(folder, new FileSetImport());
        assertNotNull(result);

        String[] resultAsArray = new String[result.size()];
        for (int i = 0; i < resultAsArray.length; i++) {
            resultAsArray[i] = result.get(i).getHandle().getName();
        }
        if (isWindows()) {
            String[] expectedOrder = {"A", "AA", "AAA", "B", "Na Návrší"};
            assertArrayEquals(expectedOrder, resultAsArray, () -> Arrays.toString(resultAsArray));
        } else {
            String[] expectedOrder = {"A", "AA", "AAA"};
            assertArrayEquals(expectedOrder, resultAsArray, () -> Arrays.toString(resultAsArray));
        }
    }

    @Test
    public void testFindDigitalContent() throws Exception {
        File f1 = new File(tempDir, "f1.ext");
        f1.createNewFile();

        File f2 = new File(tempDir, "f2.ext");
        f2.createNewFile();

        File importState = new File(tempDir, ImportFileScanner.IMPORT_STATE_FILENAME);
        importState.createNewFile();

        File dirName = new File(importState, ImportProcess.TMP_DIR_NAME);
        dirName.mkdir();

        ImportFileScanner instance = new ImportFileScanner();
        List<File> result = instance.findDigitalContent(tempDir);
        assertNotNull(result);
        assertArrayEquals(new Object[]{f1, f2}, result.toArray());
    }

    @Test
    public void testGetFileSets() throws Exception {
        File f2 = new File(tempDir, "f2.ext");
        f2.createNewFile();

        File f1ext1 = new File(tempDir, "f1.ds.ext1");
        f1ext1.createNewFile();

        File f1ext2 = new File(tempDir, "f1.ext2");
        f1ext2.createNewFile();


        ImportFileScanner instance = new ImportFileScanner();
        List<File> files = instance.findDigitalContent(tempDir);

        assertEquals(3, files.size());
        List<FileSet> fileSets = ImportFileScanner.getFileSets(files);
        assertNotNull(fileSets);
        assertEquals(2, fileSets.size());

        FileSet fs1 = fileSets.get(0);
        assertEquals("f1", fs1.getName());
        assertArrayEquals(new Object[]{f1ext1, f1ext2}, asFiles(fs1.getFiles()));

        FileSet fs2 = fileSets.get(1);
        assertEquals("f2", fs2.getName());
        assertArrayEquals(new Object[]{f2}, asFiles(fs2.getFiles()));
    }

    @Test
    public void testRepairFilename() throws Exception {
        File co1 = new File(tempDir, "CO_007.ext1");
        co1.createNewFile();

        File co2 = new File(tempDir, "CO_007.ext2");
        co2.createNewFile();

        File co3 = new File(tempDir, "CO_007.ext3");
        co3.createNewFile();

        File audio1 = new File(tempDir, "MCA_007.ext1");
        audio1.createNewFile();

        File audio2 = new File(tempDir, "SA_007.ext2");
        audio2.createNewFile();

        File audio3 = new File(tempDir, "uca_007.ext3");
        audio3.createNewFile();

        ImportFileScanner instance = new ImportFileScanner();
        List<File> files = instance.findDigitalContent(tempDir);
        assertEquals(6, files.size());
        List<FileSet> fileSets = ImportFileScanner.getFileSets(files);
        assertNotNull(fileSets);
        assertEquals(2, fileSets.size());

        FileSet co = fileSets.get(0);
        assertEquals("CO_007", co.getName());
        assertArrayEquals(new Object[]{co1, co2, co3}, asFiles(co.getFiles()));

        FileSet audio = fileSets.get(1);
        assertEquals("SA_007", audio.getName());
        assertArrayEquals(new Object[]{audio1, audio2, audio3}, asFiles(audio.getFiles()));
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
