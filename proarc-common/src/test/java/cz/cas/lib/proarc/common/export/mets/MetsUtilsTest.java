package cz.cas.lib.proarc.common.export.mets;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import com.yourmediashelf.fedora.generated.foxml.DigitalObject;

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.export.mets.structure.MetsEntity;
import cz.cas.lib.proarc.common.export.mets.structure.MetsInfo;
import cz.cas.lib.proarc.info.Info;
import cz.cas.lib.proarc.mets.Mets;

public class MetsUtilsTest {
    private static final Logger LOG = Logger.getLogger(MetsUtilsTest.class.getName());
    private final List<MetsExportTestElement> testElements = new ArrayList<MetsExportTestElement>();

    /**
     * Inits the elements to test - documents which are in repository
     * 
     */
    private void initTestElements() {
        List<String> monografieFiles = new ArrayList<String>();
        monografieFiles.add("44589055-9fad-4a9f-b6a8-75be399f332d.xml");
        monografieFiles.add("2ff2dd0c-d438-4d95-940f-690ee0f44a4a.xml");
        monografieFiles.add("1ccbf6c5-b22c-4d89-b42e-8cd14101a737.xml");
        MetsExportTestElement monografieTestElement = new MetsExportTestElement(monografieFiles, "monograph", 6, 647, 5, "monograph", "1ccbf6c5-b22c-4d89-b42e-8cd14101a737.xml");
        this.testElements.add(monografieTestElement);
    }

    @Rule
    public CustomTemporaryFolder tmp = new CustomTemporaryFolder();

    @Before
    public void setUp() {
        initTestElements();
    }

    /**
     * 
     * Simple test for ModName conversion
     * 
     * @throws Exception
     */
    @Test
    public void getModNameTest() throws Exception {
        String modName = MetsUtils.getModName(Const.PERIODICAL_VOLUME);
        assertEquals(modName, "VOLUME");
    }

    /**
     * Copies the files for a test from jar file to a temporary file system
     * 
     * @param testElement
     */
    private void copyFiles(MetsExportTestElement testElement) {
        File destination = null;
        try {
            destination = tmp.newFolder(testElement.getDirectory());
        } catch (IOException ex) {
            throw new RuntimeException("Unable to create folder: " + testElement.getDirectory());
        }
        for (String fileName : testElement.getFileList()) {
            try {
                InputStream is = this.getClass().getResourceAsStream(testElement.getDirectory() + "/" + fileName);
                File fileCopy = new File(destination.getAbsolutePath() + File.separator + fileName);
                LOG.log(Level.INFO, "Copying file to:" + fileCopy.getAbsolutePath());
                FileOutputStream fos = new FileOutputStream(fileCopy);
                MetsUtils.copyStream(is, fos);
                fos.close();
            } catch (IOException ex) {
                throw new RuntimeException("Unable to copy file:" + fileName);
            }
        }

    }

    /**
     * Reads and FoXML document from file system and gets its PID
     * 
     * @throws Exception
     */
    @Test
    public void readFoXMLTest() throws Exception {
        for (MetsExportTestElement element : testElements) {
            copyFiles(element);
            String path = tmp.getRoot().getAbsolutePath() + File.separator + element.getDirectory() + File.separator + element.getInitialDocument();
            DigitalObject dbObj = MetsUtils.readFoXML(path);
            LOG.log(Level.INFO, dbObj.getPID());
            String fileName = element.getInitialDocument();
            assertEquals(dbObj.getPID().substring(dbObj.getPID().indexOf(":") + 1), fileName.substring(0, fileName.indexOf(".")));
        }
    }

    /**
     * 
     * Saves a mets document and test it for different parameters (size, number
     * of files, ...)
     * 
     * @throws Exception
     */
    @Test
    public void saveMetsTest() throws Exception {
        for (MetsExportTestElement testElement : testElements) {
            copyFiles(testElement);
            String sourceDirPath = tmp.getRoot().getAbsolutePath() + File.separator + testElement.getDirectory() + File.separator;
            File resultDir = tmp.newFolder("result" + testElement.getDirectory());
            String path = sourceDirPath + testElement.getInitialDocument();
            DigitalObject dbObj = MetsUtils.readFoXML(path);
            MetsInfo metsInfo = new MetsEntity(dbObj, sourceDirPath, "SAMPLE");
            metsInfo.insertIntoMets(resultDir.getAbsolutePath(), true);
            metsInfo.save();
            File infoFile = new File(resultDir.getAbsolutePath() + File.separator + "info.xml");
            JAXBContext jaxbContext = JAXBContext.newInstance(Info.class);
            Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
            Info info = (Info) unmarshaller.unmarshal(infoFile);
            assertEquals(info.getItemlist().getItemtotal().intValue(), testElement.getTotalItems());
            assertEquals(info.getSize(), testElement.getSize());
            File metsFile = new File(resultDir.getAbsolutePath() + File.separator + "METS_SAMPLE.xml");
            JAXBContext jaxbContextMets = JAXBContext.newInstance(Mets.class);
            Unmarshaller unmarshallerMets = jaxbContextMets.createUnmarshaller();
            Mets mets = (Mets) unmarshallerMets.unmarshal(metsFile);
            assertEquals(mets.getFileSec().getFileGrp().size(), testElement.getNumberOfFiles());
            assertEquals(mets.getTYPE(), testElement.getType());
        }
    }
}
