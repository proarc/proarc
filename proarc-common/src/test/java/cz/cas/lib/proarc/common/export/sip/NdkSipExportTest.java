/*
 * Copyright (C) 2018 Martin Rumanek
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cas.lib.proarc.common.export.sip;

import com.mchange.util.AssertException;
import com.yourmediashelf.fedora.client.FedoraClient;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.export.ExportUtils;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.export.mets.NdkExport;
import cz.cas.lib.proarc.common.export.mockrepository.MockFedoraClient;
import cz.cas.lib.proarc.common.export.mockrepository.MockSearchView;
import cz.cas.lib.proarc.common.fedora.RemoteStorage;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.mets.info.Info;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Collections;
import java.util.List;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import mockit.Mock;
import mockit.MockUp;
import mockit.Mocked;
import org.apache.commons.lang.StringUtils;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ErrorCollector;
import org.junit.rules.TemporaryFolder;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertTrue;

public class NdkSipExportTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Rule
    public ErrorCollector collector = new ErrorCollector();

    @Mocked
    private FedoraClient client;

    @Mocked
    private SearchView searchView;

    private RemoteStorage remoteStorage;

    private final AppConfiguration appConfig = AppConfigurationFactory.getInstance().defaultInstance();

    public NdkSipExportTest() throws Exception {
    }

    @Before
    public void setUp() {
        new MockFedoraClient();
        new MockSearchView();

        remoteStorage = new RemoteStorage(client);
//
        DigitalObjectManager.setDefault(new DigitalObjectManager(
                appConfig,
                null,
                remoteStorage,
                null,
                null));

        new MockUp<ExportUtils>() {
            @SuppressWarnings("EmptyMethod")
            @Mock
            void storeObjectExportResult(String pid, String target, String log) {
            }
        };

        //new MockFedoraClient();

        MetaModelRepository.setInstance("ndk", "ndkEborn");
    }

    @Test
    public void export() throws Exception {
        new MockFedoraClient();
        NdkExport export = new NdkSipExport(remoteStorage, appConfig.getNdkExportOptions());

        String pid = "uuid:acd66301-4e75-4d12-9d98-b323ff5beee9";

        assertTrue("Junit didn't create a temporary folder", folder.getRoot().exists());

        List<NdkExport.Result> resultsList = export.export(folder.getRoot(), Collections.singletonList(pid),
                true, true, null);

        for (NdkExport.Result result: resultsList) {
            if (result.getValidationError() != null) {
                throw result.getValidationError();
            }
        }

        String packageId = "123";
        Path sip = folder.getRoot().toPath().resolve(StringUtils.removeStart(pid, "uuid:")).resolve(packageId);
        validatePackage(sip);

        Files.walkFileTree(sip, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                System.out.println(file);
                return FileVisitResult.CONTINUE;
            }
        });
    }

    @Test
    //TODO-MR test multipart with multiple chapter as well
    //TODO-MR test missing root MODS
    public void exportMultipartMonograph() throws Exception {
        NdkExport export = new NdkSipExport(remoteStorage, appConfig.getNdkExportOptions());
        String pid = "uuid:26342028-12c8-4446-9217-d3c9f249bd13";

        List<NdkExport.Result> resultsList = export.export(folder.getRoot(), Collections.singletonList(pid),
                true, true, null);

        resultsList.stream().filter(result -> result.getValidationError() != null).flatMap(result -> result.getValidationError().getExceptions().stream())
                .forEach(exception -> collector.addError(exception.getEx() != null ? exception.getEx() : new AssertException(exception.getMessage())));

        String packageId = "123";
        Path sip = folder.getRoot().toPath().resolve(StringUtils.removeStart(pid, "uuid:")).resolve(packageId);

        Files.walkFileTree(sip, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                System.out.println(file);
                return FileVisitResult.CONTINUE;
            }
        });

        validatePackage(sip);
    }

    private void validatePackage(Path sip) throws Exception {
        assertTrue("No SIP package", Files.isDirectory(sip));

        String identifier = sip.getFileName().toString();

        assertTrue("No original files", Files.list(sip.resolve("original")).count() > 0);
        assertTrue("No metadata files", Files.list(sip.resolve("metadata")).count() > 0);
        assertTrue("No info.xml", Files.exists(sip.resolve("info_" + identifier + ".xml")));
        assertTrue("No pdf file", Files.exists(sip.resolve("original/oc_" + identifier + ".pdf")));
        assertTrue("Empty pdf file", Files.size(sip.resolve("original/oc_" + identifier + ".pdf")) > 0);
        assertTrue("No mods file", Files.exists(sip.resolve("metadata/mods_volume.xml")));


        List<String> errors = MetsUtils.validateAgainstXSD(sip.resolve("info_" + identifier + ".xml").toFile(), Info.class.getResourceAsStream("info.xsd"));
        assertTrue(errors.toString(), errors.isEmpty());

        JAXBContext jContext = JAXBContext.newInstance(Info.class);
        Unmarshaller unmarshallerObj = jContext.createUnmarshaller();
        Info info = (Info) unmarshallerObj.unmarshal(sip.resolve("info_" + identifier + ".xml").toFile());
        assertTrue(info.getMetadataversion() >= NdkSipExport.PACKAGE_VERSION);
        assertEquals(info.getPackageid(), identifier);
        // assertEquals(info.getMainmets(), ""); //??? https://github.com/NLCR/Standard_NDK/issues/60

        assertTrue(!info.getTitleid().isEmpty());
        // assertTrue(!info.getCreator().isEmpty()); On Travis nullpointerexception

        assertTrue(info.getItemlist().getItem().size() > 1);
    }
}