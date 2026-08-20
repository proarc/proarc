package cz.cas.lib.proarc.common.process.imports.metacheck;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.process.export.Kramerius4ExportOptions;
import cz.cas.lib.proarc.common.process.imports.ImportProcess;
import cz.cas.lib.proarc.common.process.imports.ImportProfile;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Collections;
import mockit.Expectations;
import mockit.Mocked;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class MetaCheckImportTest {

    private static final String ROOT_PID = "uuid:root";
    private static final String UNIT_PID = "uuid:unit";
    private static final String PAGE_PID = "uuid:page";

    @TempDir
    File tempDir;

    @Test
    public void createPackageInfoWritesSelectedObjectHierarchyWithoutPages(
            @Mocked ImportProfile profile,
            @Mocked AppConfiguration config,
            @Mocked DigitalObjectManager dom,
            @Mocked ProArcObject rootObject,
            @Mocked ProArcObject unitObject,
            @Mocked ProArcObject pageObject,
            @Mocked DigitalObjectHandler rootHandler,
            @Mocked DigitalObjectHandler unitHandler,
            @Mocked DigitalObjectHandler pageHandler,
            @Mocked RelationEditor rootRelations,
            @Mocked RelationEditor unitRelations,
            @Mocked RelationEditor pageRelations,
            @Mocked MetadataHandler<String> rootMetadataHandler,
            @Mocked MetadataHandler<String> unitMetadataHandler
    ) throws Exception {
        Batch batch = new Batch();
        batch.setId(1);
        batch.setParamsFromObject(new BatchParams(Collections.singletonList(ROOT_PID)));

        ImportProcess.ImportOptions options = new ImportProcess.ImportOptions(
                tempDir, null, null, false, null, profile, Batch.PRIORITY_MEDIUM);
        options.setBatch(batch);

        DescriptionMetadata<String> rootMetadata = new DescriptionMetadata<>();
        rootMetadata.setData("<mods>root</mods>");
        DescriptionMetadata<String> unitMetadata = new DescriptionMetadata<>();
        unitMetadata.setData("<mods>unit</mods>");

        new Expectations() {{
            dom.find(ROOT_PID, null);
            result = rootObject;
            minTimes = 0;
            dom.find(UNIT_PID, null);
            result = unitObject;
            minTimes = 0;
            dom.find(PAGE_PID, null);
            result = pageObject;
            minTimes = 0;

            dom.createHandler(rootObject);
            result = rootHandler;
            minTimes = 0;
            dom.createHandler(unitObject);
            result = unitHandler;
            minTimes = 0;
            dom.createHandler(pageObject);
            result = pageHandler;
            minTimes = 0;

            rootHandler.relations();
            result = rootRelations;
            minTimes = 0;
            unitHandler.relations();
            result = unitRelations;
            minTimes = 0;
            pageHandler.relations();
            result = pageRelations;
            minTimes = 0;

            rootRelations.getModel();
            result = NdkPlugin.MODEL_MONOGRAPHTITLE;
            minTimes = 0;
            rootRelations.getMembers();
            result = Collections.singletonList(UNIT_PID);
            minTimes = 0;
            unitRelations.getModel();
            result = NdkPlugin.MODEL_MONOGRAPHUNIT;
            minTimes = 0;
            unitRelations.getMembers();
            result = Collections.singletonList(PAGE_PID);
            minTimes = 0;
            pageRelations.getModel();
            result = NdkPlugin.MODEL_NDK_PAGE;
            minTimes = 0;
            pageRelations.getMembers();
            result = Collections.emptyList();
            minTimes = 0;

            rootHandler.metadata();
            result = rootMetadataHandler;
            minTimes = 0;
            unitHandler.metadata();
            result = unitMetadataHandler;
            minTimes = 0;
            rootMetadataHandler.getMetadataAsXml();
            result = rootMetadata;
            minTimes = 0;
            unitMetadataHandler.getMetadataAsXml();
            result = unitMetadata;
            minTimes = 0;

            config.getKramerius4Export();
            result = new Kramerius4ExportOptions();
            minTimes = 0;
        }};

        TestableMetaCheckImport importer = new TestableMetaCheckImport(dom);
        importer.createPackageInfo(options, config);

        JSONObject packageInfo = new JSONObject(new String(
                Files.readAllBytes(new File(tempDir, "packageInfo.json").toPath()),
                StandardCharsets.UTF_8));

        assertEquals("monograph", packageInfo.getString("type"));
        JSONArray objects = packageInfo.getJSONArray("objects");
        assertEquals(2, objects.length());
        assertPackageObject(objects.getJSONObject(0), ROOT_PID, "monograph", "<mods>root</mods>");
        assertPackageObject(objects.getJSONObject(1), UNIT_PID, "monographunit", "<mods>unit</mods>");
    }

    @Test
    public void createPackageInfoSkipsFileWhenNoPids(
            @Mocked ImportProfile profile,
            @Mocked AppConfiguration config,
            @Mocked DigitalObjectManager dom
    ) throws Exception {
        Batch batch = new Batch();
        batch.setId(1);
        batch.setParamsFromObject(new BatchParams(Collections.emptyList()));

        ImportProcess.ImportOptions options = new ImportProcess.ImportOptions(
                tempDir, null, null, false, null, profile, Batch.PRIORITY_MEDIUM);
        options.setBatch(batch);

        TestableMetaCheckImport importer = new TestableMetaCheckImport(dom);
        importer.createPackageInfo(options, config);

        assertFalse(new File(tempDir, "packageInfo.json").exists());
    }

    @Test
    public void getMetaCheckFolderReturnsPathFromUsersFolder() {
        MetaCheckImport importer = new MetaCheckImport();

        String folder = importer.getMetaCheckFolder(
                new File("/data/.proarc/users/proarc/import/Knav/Periodikum/1"),
                new File("/data/.proarc"));

        assertEquals("users/proarc/import/Knav/Periodikum/1", folder);
    }

    @Test
    public void getMetaCheckFolderReturnsRelativeConfigHomePathWhenUsersFolderIsMissing() {
        MetaCheckImport importer = new MetaCheckImport();

        String folder = importer.getMetaCheckFolder(
                new File("/data/.proarc/import/Knav/Periodikum/1"),
                new File("/data/.proarc"));

        assertEquals("import/Knav/Periodikum/1", folder);
    }

    private static void assertPackageObject(JSONObject object, String pid, String model, String metadata) {
        assertEquals(pid, object.getString("pid"));
        assertEquals(model, object.getString("model"));
        assertEquals(metadata, object.getString("metadata"));
    }

    private static final class TestableMetaCheckImport extends MetaCheckImport {

        private final DigitalObjectManager dom;

        private TestableMetaCheckImport(DigitalObjectManager dom) {
            this.dom = dom;
        }

        @Override
        DigitalObjectManager getDigitalObjectManager() {
            return dom;
        }
    }
}
