/*
 * Copyright (C) 2013 Robert Simonovsky
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

package cz.cas.lib.proarc.common.process.export.desa;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.process.export.desa.DesaContext;
import cz.cas.lib.proarc.common.process.export.desa.structure.DesaElement;
import cz.cas.lib.proarc.common.process.export.desa.structure.DesaElementVisitor;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.fedora.Storage;
import cz.cas.lib.proarc.mets.FileType;
import cz.cas.lib.proarc.mets.Mets;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import net.lingala.zip4j.ZipFile;

import static org.junit.Assert.assertEquals;

public class DesaExportTest {
    private static final Map<String, String> testParentMap = new HashMap<String, String>();

    static {
        testParentMap.put("uuid:619dbb60-fada-4abd-b2ce-3144c086327d", "uuid:d18329c6-2f2a-48b3-b315-0da2a896ba3e");
        testParentMap.put("uuid:00134047-30f3-4967-9966-2cdb606c8ac9", "uuid:619dbb60-fada-4abd-b2ce-3144c086327d");
    }

    @Rule
    public TemporaryFolder tmp = new TemporaryFolder();

    @Before
    public void setUp() {
    }

    /**
     * Copies the files for a test from jar file to a temporary file system
     */
    private void copyFiles(String directory, String zipName) {
        File destination = null;
        try {
            destination = tmp.newFolder(directory);
        } catch (IOException ex) {
            throw new RuntimeException("Unable to create folder: " + directory);
        }
        InputStream is = this.getClass().getResourceAsStream(zipName);
        String zipFileLocation = tmp.getRoot().getAbsolutePath() + File.separator + zipName;
        File zipFile = new File(zipFileLocation);
        try {
            FileOutputStream fos = new FileOutputStream(zipFile);
            MetsUtils.copyStream(is, fos);
            ZipFile zip = new ZipFile(zipFile);
            zip.extractAll(destination.getAbsolutePath());
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    private String extractFile(String fileName, String testName) throws Exception {
        File fileTest = new File(tmp.getRoot().getAbsolutePath() + File.separator + testName);
        fileTest.mkdir();
        ZipFile zipFile = new ZipFile(new File(fileName));
        zipFile.extractAll(fileTest.getAbsolutePath());
        return fileTest.getAbsolutePath();
    }

    private Mets readMets(String fileName) throws Exception {
        JAXBContext jaxbContext = JAXBContext.newInstance(Mets.class);
        Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
        Mets mets = (Mets) unmarshaller.unmarshal(new File(fileName));
        return mets;
    }

    @Test
    public void testSaveCorrectDesa() throws Exception {
        String directory = "correctDesa";
        copyFiles("correctDesa", "correctDesa.zip");
        String sourceDirPath = tmp.getRoot().getAbsolutePath() + File.separator + directory + File.separator;
        File resultDir = tmp.newFolder("result" + directory);
        String path = sourceDirPath + "d18329c6-2f2a-48b3-b315-0da2a896ba3e.xml";
        DesaContext desaContext = new DesaContext();
        desaContext.setTypeOfStorage(Storage.LOCAL);
        desaContext.setFsParentMap(testParentMap);
        desaContext.setOutputPath(resultDir.getAbsolutePath());
        desaContext.setPackageID("SAMPLEDESA");
        desaContext.setPath(sourceDirPath);
        try {
            DigitalObject object = MetsUtils.readFoXML(path);
            DesaElement.getElement(object, null, desaContext, true);
            DesaElementVisitor desaVisitor = new DesaElementVisitor();
            desaContext.getRootElement().accept(desaVisitor);
        } catch (MetsExportException ex) {
            assertEquals(0, ex.getExceptions().size());
        }
        String outputDir = extractFile(resultDir + File.separator + "A-37_FILE.zip", "correctDesaTest");
        Mets mets = readMets(outputDir + File.separator + "mets.xml");
        String DMDID = mets.getDmdSec().get(0).getID();
        assertEquals("DM_0001", DMDID);
        String structType = mets.getStructMap().get(0).getDiv().getTYPE();
        assertEquals("file", structType);
        String innerStructType = mets.getStructMap().get(0).getDiv().getDiv().get(0).getTYPE();
        assertEquals("record", innerStructType);
        String href = mets.getStructMap().get(0).getDiv().getDiv().get(0).getMptr().get(0).getHref();
        assertEquals("A-37_01", href);
        outputDir = extractFile(resultDir + File.separator + "A-37_01_0001.zip", "noFileNameDesaTest");
        mets = readMets(outputDir + File.separator + "mets.xml");
        FileType fileType = mets.getFileSec().getFileGrp().get(0).getFile().get(0);
        assertEquals("b5100365bd0c93d1596e29fb8e8969a0", fileType.getCHECKSUM());
        assertEquals("kniha_text.tif", fileType.getFLocat().get(0).getHref());
        File file = new File(outputDir + File.separator + fileType.getFLocat().get(0).getHref());
        assertEquals(true, file.exists());
    }

    @Test
    public void testSaveCorrectDesafromFile() throws Exception {
        String directory = "correctDesa";
        copyFiles("correctDesa", "correctDesa.zip");
        String sourceDirPath = tmp.getRoot().getAbsolutePath() + File.separator + directory + File.separator;
        File resultDir = tmp.newFolder("result" + directory);
        String path = sourceDirPath + "00134047-30f3-4967-9966-2cdb606c8ac9.xml";
        DesaContext desaContext = new DesaContext();
        desaContext.setFsParentMap(testParentMap);
        desaContext.setOutputPath(resultDir.getAbsolutePath());
        desaContext.setPackageID("SAMPLEDESA");
        desaContext.setPath(sourceDirPath);
        try {
            DigitalObject object = MetsUtils.readFoXML(path);
            DesaElement desaElement = DesaElement.getElement(object, null, desaContext, true);
            DesaElementVisitor desaVisitor = new DesaElementVisitor();
            desaElement.accept(desaVisitor);
        } catch (MetsExportException ex) {
            assertEquals(0, ex.getExceptions().size());
        }
        // String outputDir = extractFile(resultDir + File.separator +
        // "A-37_FILE.zip", "correctDesaTest");
        // Mets mets = readMets(outputDir + File.separator + "mets.xml");
        // String DMDID = mets.getDmdSec().get(0).getID();
        // assertEquals("DM_0001", DMDID);
        // String structType = mets.getStructMap().get(0).getDiv().getTYPE();
        // assertEquals("file", structType);
        // String innerStructType =
        // mets.getStructMap().get(0).getDiv().getDiv().get(0).getTYPE();
        // assertEquals("record", innerStructType);
        // String href =
        // mets.getStructMap().get(0).getDiv().getDiv().get(0).getMptr().get(0).getHref();
        // assertEquals("A-37_01", href);
        String outputDir = extractFile(resultDir + File.separator + "A-37_01_0001.zip", "noFileNameDesaTest");
        Mets mets = readMets(outputDir + File.separator + "mets.xml");
        FileType fileType = mets.getFileSec().getFileGrp().get(0).getFile().get(0);
        assertEquals("b5100365bd0c93d1596e29fb8e8969a0", fileType.getCHECKSUM());
        assertEquals("kniha_text.tif", fileType.getFLocat().get(0).getHref());
        File file = new File(outputDir + File.separator + fileType.getFLocat().get(0).getHref());
        assertEquals(true, file.exists());
    }

    @Test
    public void testSaveDesaNoFileName() throws Exception {
        String directory = "noFileNameDesa";
        copyFiles(directory, "desaNoFileName.zip");
        String sourceDirPath = tmp.getRoot().getAbsolutePath() + File.separator + directory + File.separator;
        File resultDir = tmp.newFolder("result" + directory);
        String path = sourceDirPath + "d18329c6-2f2a-48b3-b315-0da2a896ba3e.xml";
        DesaContext desaContext = new DesaContext();
        desaContext.setFsParentMap(testParentMap);
        desaContext.setOutputPath(resultDir.getAbsolutePath());
        desaContext.setPackageID("SAMPLEDESA2");
        desaContext.setPath(sourceDirPath);
        try {
            DigitalObject object = MetsUtils.readFoXML(path);
            DesaElement.getElement(object, null, desaContext, true);
            DesaElementVisitor desaVisitor = new DesaElementVisitor();
            desaContext.getRootElement().accept(desaVisitor);
        } catch (MetsExportException ex) {
            assertEquals(0, ex.getExceptions().size());
        }
        String outputDir = extractFile(resultDir + File.separator + "A-37_FILE.zip", "noFileNameDesaTest");
        Mets mets = readMets(outputDir + File.separator + "mets.xml");
        String DMDID = mets.getDmdSec().get(0).getID();
        assertEquals("DM_0001", DMDID);
        String structType = mets.getStructMap().get(0).getDiv().getTYPE();
        assertEquals("file", structType);
        String innerStructType = mets.getStructMap().get(0).getDiv().getDiv().get(0).getTYPE();
        assertEquals("record", innerStructType);
        String href = mets.getStructMap().get(0).getDiv().getDiv().get(0).getMptr().get(0).getHref();
        assertEquals("A-37_01", href);
        outputDir = extractFile(resultDir + File.separator + "A-37_01_0001.zip", "noFileNameDesaTest");
        mets = readMets(outputDir + File.separator + "mets.xml");
        FileType fileType = mets.getFileSec().getFileGrp().get(0).getFile().get(0);
        assertEquals("b5100365bd0c93d1596e29fb8e8969a0", fileType.getCHECKSUM());
        assertEquals("file_0001.tif", fileType.getFLocat().get(0).getHref());
        File file = new File(outputDir + File.separator + fileType.getFLocat().get(0).getHref());
        assertEquals(true, file.exists());
    }
}
