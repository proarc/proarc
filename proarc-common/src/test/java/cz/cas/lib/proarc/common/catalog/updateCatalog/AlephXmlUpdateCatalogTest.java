package cz.cas.lib.proarc.common.catalog.updateCatalog;

import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.apache.commons.configuration2.BaseConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import static org.junit.jupiter.api.Assertions.*;

class AlephXmlUpdateCatalogTest {
    @TempDir Path directory;

    @Test
    void identicalExistingLinkSucceedsWithoutCreatingAnotherFile() throws Exception {
        AlephXmlUpdateCatalog adapter = adapter(List.of("https://library/uuid/uuid:one"));
        assertTrue(adapter.process(configuration(), "123456789", "uuid:one"));
        assertFalse(adapter.getResult().warning());
        assertFalse(Files.exists(directory.resolve("one.csv")));
    }

    @Test
    void differentLinkIsWrittenAndBothValuesAreLoggedForReview() throws Exception {
        AlephXmlUpdateCatalog adapter = adapter(List.of("https://library/view/uuid:old"));
        assertTrue(adapter.process(configuration(), "123456789", "uuid:new"));
        assertEquals("123456789 @ https://library/view/uuid:new", Files.readString(directory.resolve("new.csv")));
        assertTrue(adapter.getResult().warning());
        assertTrue(adapter.getResult().message().contains("https://library/view/uuid:old"));
        assertTrue(adapter.getResult().message().contains("https://library/view/uuid:new"));
    }

    @Test
    void pendingAlephFilesAreComparedBeforeCatalogHasConsumedThem() throws Exception {
        assertTrue(adapter(List.of()).process(configuration(), "123456789", "uuid:one"));
        AlephXmlUpdateCatalog second = adapter(List.of());
        assertTrue(second.process(configuration(), "123456789", "uuid:two"));
        assertTrue(second.getResult().warning());
        assertTrue(second.getResult().message().contains("uuid:one"));
        AlephXmlUpdateCatalog repeated = adapter(List.of());
        assertTrue(repeated.process(configuration(), "123456789", "uuid:one"));
        assertFalse(repeated.getResult().warning());
    }

    private AlephXmlUpdateCatalog adapter(List<String> links) {
        return new AlephXmlUpdateCatalog(null, null) {
            @Override protected List<String> readCatalogLinks(CatalogConfiguration config, String identifier) {
                return links;
            }
        };
    }

    private CatalogConfiguration configuration() {
        BaseConfiguration properties = new BaseConfiguration();
        properties.addProperty(CatalogConfiguration.PROPERTY_CATALOG_DIRECTORY, directory.toString());
        properties.addProperty(CatalogConfiguration.PROPERTY_CATALOG_URL_LINK, "https://library/view/");
        properties.addProperty(CatalogConfiguration.PROPERTY_FIELD001_BASE_LENGHT, 0);
        properties.addProperty(CatalogConfiguration.PROPERTY_FIELD001_BASE_DEFAULT, "");
        properties.addProperty(CatalogConfiguration.PROPERTY_FIELD001_SYSNO_LENGHT, 9);
        return new CatalogConfiguration("test", "catalog.test", properties);
    }
}
