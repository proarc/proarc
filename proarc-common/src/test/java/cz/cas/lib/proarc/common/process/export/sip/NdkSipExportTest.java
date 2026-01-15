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

package cz.cas.lib.proarc.common.process.export.sip;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.model.MetaModelRepository;
import cz.cas.lib.proarc.common.process.export.ExportUtils;
import cz.cas.lib.proarc.common.process.export.mets.MetsContext;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.process.export.mets.NdkExport;
import cz.cas.lib.proarc.common.process.export.mets.structure.MetsElement;
import cz.cas.lib.proarc.common.process.export.mockrepository.MockFedoraClient;
import cz.cas.lib.proarc.common.process.export.mockrepository.MockSearchView;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.storage.fedora.FedoraStorage;
import cz.cas.lib.proarc.mets.info.Info;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.Unmarshaller;
import java.io.File;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Collections;
import java.util.List;
import mockit.Mock;
import mockit.MockUp;
import mockit.Mocked;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static cz.cas.lib.proarc.common.kramerius.KrameriusOptions.KRAMERIUS_INSTANCE_LOCAL;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

public class NdkSipExportTest {

    @TempDir
    File tempDir;

    @Mocked
    private FedoraClient client;

    @Mocked
    private SearchView searchView;

    private FedoraStorage fedoraStorage;

    private final AppConfiguration appConfig = AppConfigurationFactory.getInstance().defaultInstance();
    private AkubraConfiguration akubraConfiguration = null;

    public NdkSipExportTest() throws Exception {
    }

    @BeforeEach
    public void setUp() throws Exception {
        if (Storage.AKUBRA.equals(appConfig.getTypeOfStorage())) {
            this.akubraConfiguration = AkubraConfigurationFactory.getInstance().defaultInstance(appConfig.getConfigHome());
        } else {
            this.akubraConfiguration = null;
        }
        new MockFedoraClient();
        new MockSearchView();

        fedoraStorage = new FedoraStorage(client);
        DigitalObjectManager.setDefault(new DigitalObjectManager(
                appConfig, akubraConfiguration,
                null,
                null,
                null));

        new MockUp<ExportUtils>() {
            @SuppressWarnings("EmptyMethod")
            @Mock
            void storeObjectExportResult(String pid, String target, String log) {
            }
        };

        MetaModelRepository.setInstance("ndk", "ndkEborn");
    }

    @Test
    public void testCreateMetsElement() throws MetsExportException {
        DigitalObject dobj = MetsUtils.readFoXML("uuid:b0ebac65-e9fe-417d-a71b-58e74fe707a4", client);
        MetsContext mc = new MetsContext();
        mc.setTypeOfStorage(Storage.FEDORA);
        mc.setFedoraClient(client);
        mc.setRemoteStorage(fedoraStorage);

        MetsElement mElm = MetsElement.getElement(dobj, null, mc, true);
        assertNotNull(mElm.getParent(), () -> "missing parent for " + mElm.getOriginalPid() + " (" + mElm.getElementType() + ")");
    }

    @Test
    public void exportPeriodical() throws Exception {
        NdkExport export = new NdkSipExport(fedoraStorage, appConfig, akubraConfiguration);
        String pid = "uuid:8548cc82-3601-45a6-8eb0-df6538db4de6";

        List<NdkExport.Result> resultsList = export.export(tempDir, Collections.singletonList(pid),
                true, true, null, false, null, KRAMERIUS_INSTANCE_LOCAL, "public", null, null);

        assertAll(
                resultsList.stream()
                        .filter(result -> result.getValidationError() != null)
                        .flatMap(result -> result.getValidationError().getExceptions().stream())
                        .map(exception -> (org.junit.jupiter.api.function.Executable) () -> {
                            if (exception.getEx() != null) {
                                fail(exception.getEx());
                            } else {
                                fail(exception.getMessage());
                            }
                        })
        );

        String sipIdentifier = "123";
        Path sip = resultsList.get(0).getTargetFolder().toPath().resolve(sipIdentifier);
        Files.walkFileTree(sip, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                System.out.println(file);
                return FileVisitResult.CONTINUE;
            }
        });

        validatePackage(sip, 4);

    }

    /**
     * Test export of multipart monograph, 1 eVolume, 2 eChapter
     */
    @Test
    public void exportMultipartMonograph() throws Exception {
        NdkExport export = new NdkSipExport(fedoraStorage, appConfig, akubraConfiguration);
        String pid = "uuid:26342028-12c8-4446-9217-d3c9f249bd13";

        List<NdkExport.Result> resultsList = export.export(tempDir, Collections.singletonList(pid),
                true, true, null, false, null, KRAMERIUS_INSTANCE_LOCAL, "public", null, null);

        assertAll(
                resultsList.stream()
                        .filter(result -> result.getValidationError() != null)
                        .flatMap(result -> result.getValidationError().getExceptions().stream())
                        .map(exception -> (org.junit.jupiter.api.function.Executable) () -> {
                            if (exception.getEx() != null) {
                                fail(exception.getEx());
                            } else {
                                fail(exception.getMessage());
                            }
                        })
        );

        String packageId = "123";
        Path sip = tempDir.toPath().resolve(StringUtils.removeStart(pid, "uuid:")).resolve(packageId);

        Files.walkFileTree(sip, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                System.out.println(file);
                return FileVisitResult.CONTINUE;
            }
        });

        validatePackage(sip, 4);
    }

    private void validatePackage(Path sip, int metadatacount) throws Exception {
        assertTrue(Files.isDirectory(sip), "No SIP package");

        String identifier = sip.getFileName().toString();

        assertTrue(Files.list(sip.resolve("original")).count() > 0, "No original files");
        assertEquals(Files.list(sip.resolve("metadata")).count(), metadatacount, "Wrong count of metadata files");
        assertTrue(Files.exists(sip.resolve("info_" + identifier + ".xml")), "No info.xml");
        assertTrue(Files.exists(sip.resolve("original/oc_" + identifier + ".pdf")), "No pdf file");
        assertTrue(Files.size(sip.resolve("original/oc_" + identifier + ".pdf")) > 0, "Empty pdf file");
        assertTrue(Files.exists(sip.resolve("metadata/mods_volume.xml")), "No mods file");


        List<String> errors = MetsUtils.validateAgainstXSD(sip.resolve("info_" + identifier + ".xml").toFile(), Info.class.getResourceAsStream("info.xsd"));
        assertTrue(errors.isEmpty(), () -> errors.toString());

        JAXBContext jContext = JAXBContext.newInstance(Info.class);
        Unmarshaller unmarshallerObj = jContext.createUnmarshaller();
        Info info = (Info) unmarshallerObj.unmarshal(sip.resolve("info_" + identifier + ".xml").toFile());
        assertTrue(info.getMetadataversion() >= 2.2f);
        assertEquals(info.getPackageid(), identifier);
        // assertEquals(info.getMainmets(), ""); //??? https://github.com/NLCR/Standard_NDK/issues/60

        assertTrue(!info.getTitleid().isEmpty());
        // assertTrue(!info.getCreator().isEmpty()); On Travis nullpointerexception

        assertTrue(info.getItemlist().getItem().size() > 1);
    }
}
