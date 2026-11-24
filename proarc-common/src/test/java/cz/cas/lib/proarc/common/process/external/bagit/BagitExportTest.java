package cz.cas.lib.proarc.common.process.external.bagit;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.zip.ZipFile;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.*;

class BagitExportTest {
    @TempDir
    Path temp;

    @Test
    void defaultsToJavaAndCompletesExportPipeline() throws Exception {
        AppConfiguration config = configuration(Map.of("processor.bagit.exec", "nonexistent-bagit-executable"));
        assertEquals("java", config.getBagitMode());
        // Deliberately unusable external command: default export must not call it.
        Path root = Files.createDirectory(temp.resolve("export"));
        Files.writeString(root.resolve("scan.tif"), "scan");
        Bagit export = new Bagit(config, root.toFile());
        export.prepare();
        export.bagit();
        export.zip();
        export.moveToBagitFolder();
        export.createMd5File();

        Path zip = temp.resolve("bagit_archive_export/archive_export.zip");
        try (ZipFile archive = new ZipFile(zip.toFile())) {
            assertNotNull(archive.getEntry("archive_export/bagit.txt"));
            assertNotNull(archive.getEntry("archive_export/data/scan.tif"));
            assertNotNull(archive.getEntry("archive_export/tagmanifest-md5.txt"));
        }
        assertEquals("MD5 " + BagitJava.md5(zip),
                Files.readString(zip.resolveSibling("archive_export.sums")));
    }

    @Test
    void retainsLegacyScriptSelectionAndAllowsExplicitJavaOverride() throws Exception {
        Path script = temp.resolve("missing.sh");
        AppConfiguration config = configuration(Map.of(AppConfiguration.EXPORT_BAGIT_SCRIPT_PATH, script.toString()));
        assertEquals("script", config.getBagitMode());
        Path root = Files.createDirectory(temp.resolve("export"));
        Files.writeString(root.resolve("original"), "keep");
        IOException error = assertThrows(IOException.class, () -> new Bagit(config, root.toFile()).bagit());
        assertTrue(error.getMessage().contains(script.toString()));
        assertEquals("keep", Files.readString(root.resolve("original")));
        AppConfiguration javaConfig = configuration(Map.of(AppConfiguration.EXPORT_BAGIT_MODE, "java",
                AppConfiguration.EXPORT_BAGIT_SCRIPT_PATH, script.toString()));
        new Bagit(javaConfig, root.toFile()).bagit();
        assertEquals("keep", Files.readString(root.resolve("data/original")));
    }

    @Test
    void explicitExternalModeRunsConfiguredCommand() throws Exception {
        String classpath = Path.of(Executable.class.getProtectionDomain().getCodeSource().getLocation().toURI())
                + File.pathSeparator + Path.of(BagitJava.class.getProtectionDomain().getCodeSource().getLocation().toURI());
        AppConfiguration config = configuration(Map.of(AppConfiguration.EXPORT_BAGIT_MODE, "external",
                "processor.bagit.exec", Path.of(System.getProperty("java.home"), "bin", "java").toString(),
                "processor.bagit.arg", "-cp," + classpath.replace('\\', '/') + "," + Executable.class.getName()));
        Path root = Files.createDirectory(temp.resolve("export with spaces"));
        new Bagit(config, root.toFile()).bagit();
        assertEquals("external", Files.readString(root.resolve("data/external-marker")));
    }

    @Test
    void invalidModeAndMissingScriptFailBeforeChangingExport() throws Exception {
        Path root = Files.createDirectory(temp.resolve("export"));
        Files.writeString(root.resolve("original"), "keep");
        for (String mode : List.of("unknown", "script")) {
            AppConfiguration config = configuration(Map.of(AppConfiguration.EXPORT_BAGIT_MODE, mode));
            assertThrows(IOException.class, () -> new Bagit(config, root.toFile()).bagit());
            assertEquals("keep", Files.readString(root.resolve("original")));
            assertFalse(Files.exists(root.resolve("data")));
        }
    }

    private AppConfiguration configuration(Map<String, String> values) throws Exception {
        Path home = Files.createTempDirectory(temp, "config");
        Properties properties = new Properties();
        properties.putAll(values);
        try (var output = Files.newOutputStream(home.resolve(AppConfiguration.CONFIG_FILE_NAME))) {
            properties.store(output, null);
        }
        Map<String, String> defaults = new HashMap<>();
        defaults.put(AppConfiguration.PROPERTY_APP_HOME, home.toString());
        return AppConfigurationFactory.getInstance().create(defaults);
    }

    /** Small real external command used to verify command dispatch without requiring Python. */
    public static class Executable {
        public static void main(String[] args) throws Exception {
            Path root = Path.of(args[0]);
            Files.writeString(root.resolve("external-marker"), "external");
            new BagitJava().create(root);
        }
    }
}
