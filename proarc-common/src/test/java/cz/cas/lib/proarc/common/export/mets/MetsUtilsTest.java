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

package cz.cas.lib.proarc.common.export.mets;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.export.mets.structure.MetsElementVisitor;
import cz.cas.lib.proarc.common.export.mockrepository.MockFedoraClient;
import cz.cas.lib.proarc.common.export.mockrepository.MockSearchView;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.Storage;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.mets.FileType;
import cz.cas.lib.proarc.mets.Mets;
import cz.cas.lib.proarc.mets.MetsType.FileSec.FileGrp;
import cz.cas.lib.proarc.mets.info.Info;
import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import mockit.Mocked;

import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertEquals;

public class MetsUtilsTest {
    private static final Logger LOG = Logger.getLogger(MetsUtilsTest.class.getName());
    private final List<MetsExportTestElement> testElements = new ArrayList<>();

    @Mocked
    private FedoraClient client;

    /**
     * Inits the elements to test - documents which are in repository
     *
     */
    private void initTestElements() {
        MetsExportTestElement monografieTestElement = new MetsExportTestElement("monograph", "monograph", 6, 32, 4, "Monograph", "1ccbf6c5-b22c-4d89-b42e-8cd14101a737.xml");
        this.testElements.add(monografieTestElement);
        MetsExportTestElement periodikumTestElement = new MetsExportTestElement("periodikum", "periodikum", 42, 323, 5, "Periodical", "3733b6e3-61ab-42fc-a437-964d143acc45.xml");
        this.testElements.add(periodikumTestElement);
        MetsExportTestElement periodikumPageTestElement = new MetsExportTestElement("periodikumPage", "periodikum", 7, 39, 5, "Periodical", "b46aff0e-26af-11e3-88e3-001b63bd97ba.xml");
        this.testElements.add(periodikumPageTestElement);
    }

    @Rule
    public TemporaryFolder tmp = new TemporaryFolder();

    @Before
    public void setUp() {
        initTestElements();
        MetaModelRepository.setInstance();
    }

    /**
     *
     * Simple test for ModName conversion
     *
     * @throws Exception
     */
    @Test
    public void getModNameTest() throws Exception {
        String modName = Const.typeNameMap.get(Const.PERIODICAL_VOLUME);
        assertEquals(modName, Const.VOLUME);
    }

    /**
     * Returns the source path for input documents
     *
     * @return
     */
    private String getTargetPath() {
        URL res = this.getClass().getResource(this.getClass().getSimpleName() + ".class");
        File fileName = new File(res.getFile());
        return fileName.getParent();
    }

    /**
     * Reads and FoXML document from file system and gets its PID
     *
     * @throws Exception
     */
    @Test
    public void readFoXMLTest() throws Exception {
        for (MetsExportTestElement element : testElements) {
            // copyFiles(element);
            String path = getTargetPath() + File.separator + element.getDirectory() + File.separator + element.getInitialDocument();
            DigitalObject dbObj = MetsUtils.readFoXML(path);
            LOG.log(Level.INFO, dbObj.getPID());
            String fileName = element.getInitialDocument();
            assertEquals(dbObj.getPID().substring(dbObj.getPID().indexOf(":") + 1), fileName.substring(0, fileName.indexOf(".")));
        }
    }

    /**
     * Tests if the exception is thrown for invalid mets
     *
     */
    @Test(expected = MetsExportException.class)
    public void saveInvalidMods() throws Exception {
        String sourceDirPath = getTargetPath() + File.separator +
                "monographInvalid" + File.separator;
        File resultDir = tmp.newFolder("result" + "monographInvalidMods");
        String path = sourceDirPath + "1ccbf6c5-b22c-4d89-b42e-8cd14101a737.xml";
        DigitalObject dbObj = MetsUtils.readFoXML(path);
        MetsContext context = new MetsContext();
        context.setTypeOfStorage(Storage.LOCAL);
        context.setPath(sourceDirPath);
        context.setFsParentMap(TestConst.parents);
        context.setOutputPath(resultDir.getAbsolutePath());
        context.setAllowNonCompleteStreams(true);
        context.setAllowMissingURNNBN(true);
        MetsElement metsElement = MetsElement.getElement(dbObj, null, context, true);
        MetsElementVisitor visitor = new MetsElementVisitor();
        metsElement.accept(visitor);
    }

    /**
     * Tests if the exception is thrown for invalid mets
     *
     */
    @Test(expected = MetsExportException.class)
    public void saveInvalidDC() throws Exception {
        String sourceDirPath = getTargetPath() + File.separator +
                "monographInvalid" + File.separator;
        File resultDir = tmp.newFolder("result" + "monographInvalidDC");
        String path = sourceDirPath + "1ccbf6c5-b22c-4d89-b42e-8cd14101a737.xml";
        DigitalObject dbObj = MetsUtils.readFoXML(path);
        MetsContext context = new MetsContext();
        context.setTypeOfStorage(Storage.LOCAL);
        context.setPath(sourceDirPath);
        context.setFsParentMap(TestConst.parents);
        context.setOutputPath(resultDir.getAbsolutePath());
        context.setAllowNonCompleteStreams(true);
        context.setAllowMissingURNNBN(true);
        MetsElement metsElement = MetsElement.getElement(dbObj, null, context, true);
        MetsElementVisitor visitor = new MetsElementVisitor();
        metsElement.accept(visitor);
    }

    /**
     * Tests if all filegoups are created
     *
     * @throws Exception
     */
    @Test
    public void initGroupsTest() throws Exception {
        HashMap<String, FileGrp> fileGroups = MetsUtils.initFileGroups();
        assertEquals(5, fileGroups.keySet().size());
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
            // copyFiles(testElement);
            String sourceDirPath = getTargetPath() + File.separator +
                    testElement.getDirectory() + File.separator;
            File resultDir = tmp.newFolder("result" + testElement.getResultFolder());
            String path = sourceDirPath + testElement.getInitialDocument();
            DigitalObject dbObj = MetsUtils.readFoXML(path);
            Configuration config = new BaseConfiguration();
            config.addProperty(NdkExportOptions.PROP_NDK_AGENT_ARCHIVIST, "Archivist");
            config.addProperty(NdkExportOptions.PROP_NDK_AGENT_CREATOR, "Creator");
            MetsContext context = new MetsContext();
            context.setTypeOfStorage(Storage.LOCAL);
            context.setPath(sourceDirPath);
            context.setFsParentMap(TestConst.parents);
            context.setOutputPath(resultDir.getAbsolutePath());
            context.setAllowNonCompleteStreams(true);
            context.setAllowMissingURNNBN(true);
            context.setConfig(NdkExportOptions.getOptions(config));
            MetsElement metsElement = MetsElement.getElement(dbObj, null, context, true);
            MetsElementVisitor visitor = new MetsElementVisitor();
            metsElement.accept(visitor);
            String packageId = context.getGeneratedPSP().get(0);
            File infoFile = new File(resultDir.getAbsolutePath() + File.separator + packageId + File.separator +
                    "info_" + packageId + ".xml");
            JAXBContext jaxbContext = JAXBContext.newInstance(Info.class);
            Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
            Info info = (Info) unmarshaller.unmarshal(infoFile);
            assertEquals(1 + testElement.getTotalItems(),
                    info.getItemlist().getItemtotal().intValue());
            if (System.getProperty("os.name").toLowerCase().contains("win")) {
                // this is an aproximation as the precompute sizes ignore win EOLs
                assertTrue(info.getSize() != 0 && testElement.getSize() <= info.getSize());
            } else {
                assertEquals(testElement.getSize(), info.getSize());
            }
            File metsFile = new File(resultDir.getAbsolutePath() + File.separator + packageId + File.separator +
                    "mets_" + packageId + ".xml");
            JAXBContext jaxbContextMets = JAXBContext.newInstance(Mets.class);
            Unmarshaller unmarshallerMets = jaxbContextMets.createUnmarshaller();
            Mets mets = (Mets) unmarshallerMets.unmarshal(metsFile);
            assertEquals(testElement.getNumberOfFiles(),
                    mets.getFileSec().getFileGrp().size());
            assertEquals(testElement.getType(), mets.getTYPE());

            String expectedFileName = "txt_" + packageId + "_0001";
            String actualFileName = ((FileType)(mets.getStructMap().get(1).getDiv().getDiv().get(0).getFptr().get(0).getFILEID())).getID();
            assertEquals(expectedFileName, actualFileName);

            String expectedFileHref = "txt/" + expectedFileName + ".txt";
            String actualFileHref = mets.getFileSec().getFileGrp().get(0).getFile().get(0).getFLocat().get(0).getHref();
            assertEquals(expectedFileHref, actualFileHref);
        }
    }

    /**
     * Tests if all roles are fill
     */
    @Test
    public void missingRole() throws Exception {
        for (MetsExportTestElement testElement : testElements) {
            // copyFiles(testElement);
            String sourceDirPath = getTargetPath() + File.separator +
                    testElement.getDirectory() + File.separator;
            File resultDir = tmp.newFolder("result" + testElement.getResultFolder());
            String path = sourceDirPath + testElement.getInitialDocument();
            DigitalObject dbObj = MetsUtils.readFoXML(path);
            Configuration config = new BaseConfiguration();
            config.addProperty(NdkExportOptions.PROP_NDK_AGENT_ARCHIVIST, "Archivist");
            config.addProperty(NdkExportOptions.PROP_NDK_AGENT_CREATOR, "");
            MetsContext context = new MetsContext();
            context.setTypeOfStorage(Storage.LOCAL);
            context.setPath(sourceDirPath);
            context.setFsParentMap(TestConst.parents);
            context.setOutputPath(resultDir.getAbsolutePath());
            context.setAllowNonCompleteStreams(true);
            context.setAllowMissingURNNBN(true);
            context.setConfig(NdkExportOptions.getOptions(config));
            MetsElement metsElement = MetsElement.getElement(dbObj, null, context, true);
            MetsElementVisitor visitor = new MetsElementVisitor();
            try {
                metsElement.accept(visitor);
                Assert.fail("The validation error expected.");
            } catch (MetsExportException ex) {
                String message = "Error - missing role. Please insert value in proarc.cfg into export.ndk.agent.creator and export.ndk.agent.archivist";
                assertEquals(message, ex.getMessage());
            }
        }
    }

    @Test
    public void missingTitle()throws Exception{
        MetsExportTestElement testElement = new MetsExportTestElement("periodikum", "periodikum", 7, 39, 5, "Periodical", "2ad73397-ef9d-429a-b3a5-65083fa4c333.xml");
        String sourceDirPath = getTargetPath() + File.separator +
                testElement.getDirectory() + File.separator;
        File resultDir = tmp.newFolder("result" + testElement.getResultFolder());
        String path = sourceDirPath + testElement.getInitialDocument();
        DigitalObject dbObj = MetsUtils.readFoXML(path);
        Configuration config = new BaseConfiguration();
        config.addProperty(NdkExportOptions.PROP_NDK_AGENT_ARCHIVIST, "Archivist");
        config.addProperty(NdkExportOptions.PROP_NDK_AGENT_CREATOR, "Creator");
        MetsContext context = new MetsContext();
        context.setTypeOfStorage(Storage.LOCAL);
        context.setPath(sourceDirPath);
        context.setFsParentMap(TestConst.parents);
        context.setOutputPath(resultDir.getAbsolutePath());
        context.setAllowNonCompleteStreams(true);
        context.setAllowMissingURNNBN(true);
        context.setConfig(NdkExportOptions.getOptions(config));
        MetsElement metsElement = MetsElement.getElement(dbObj, null, context, true);
        MetsElementVisitor visitor = new MetsElementVisitor();
        try{
            metsElement.accept(visitor);
            Assert.fail("The missing title expected");
        } catch (MetsExportException ex){
            String message =  "Error - missing title. Please insert title.";
            assertEquals(message, ex.getMessage());
        }
    }

    @Test
    public void findPSPPIDsTest() throws MetsExportException {
        new MockFedoraClient();
        new MockSearchView();

        MetaModelRepository.setInstance("ndk", "ndkEborn");
        RemoteStorage remoteStorage = new RemoteStorage(client);
        MetsContext ctx = new MetsContext();
        ctx.setTypeOfStorage(Storage.FEDORA);
        ctx.setRemoteStorage(remoteStorage);
        ctx.setFedoraClient(remoteStorage.getClient());

        List<String> pspIDs = MetsUtils.findPSPPIDs("uuid:26342028-12c8-4446-9217-d3c9f249bd13", ctx, true);
        Set<String> setPspIDs = new HashSet<>(pspIDs);
        assertTrue("pspIds aren't unique", pspIDs.size() == setPspIDs.size());

        List<String> pspIdPeriodical = MetsUtils.findPSPPIDs("uuid:8548cc82-3601-45a6-8eb0-df6538db4de6", ctx, true);
        assertTrue(pspIdPeriodical.size() > 0);
    }
}
