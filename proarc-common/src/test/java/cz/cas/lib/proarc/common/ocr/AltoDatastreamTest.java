/*
 * Copyright (C) 2026 The ProArc Authors
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.ocr;

import cz.cas.lib.proarc.common.process.imports.ImportProfile;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import javax.xml.validation.Schema;
import org.apache.commons.configuration2.BaseConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.xml.sax.SAXException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AltoDatastreamTest {

    private static final List<String> SUPPORTED_VERSIONS = List.of(
            "2.0", "2.1", "3.0", "3.1", "4.0", "4.1", "4.2", "4.3", "4.4");

    @TempDir
    Path tempDir;

    @Test
    void loadsAllSchemasWhenVersionIsNotConfigured() throws Exception {
        new AltoDatastream(profile(null));

        List<Schema> schemas = AltoDatastream.getSchemas();

        assertEquals(SUPPORTED_VERSIONS, AltoDatastream.SUPPORTED_VERSIONS);
        assertEquals(SUPPORTED_VERSIONS.size(), schemas.size());

        new AltoDatastream(profile(""));
        assertEquals(SUPPORTED_VERSIONS.size(), AltoDatastream.getSchemas().size());
    }

    @Test
    void loadsOnlyExplicitlyConfiguredSchema() throws Exception {
        for (String version : SUPPORTED_VERSIONS) {
            new AltoDatastream(profile(version));

            assertEquals(1, AltoDatastream.getSchemas().size(), version);
        }
    }

    @Test
    void rejectsUnsupportedExplicitVersion() {
        new AltoDatastream(profile("5.0"));

        SAXException exception = assertThrows(SAXException.class, AltoDatastream::getSchemas);

        assertTrue(exception.getMessage().contains("Unsupported ALTO version '5.0'"));
    }

    @Test
    void acceptsDocumentWhenAnySupportedSchemaMatches() throws Exception {
        Path alto = createAlto4Document();
        new AltoDatastream(profile(null));

        assertTrue(AltoDatastream.isAlto(alto.toUri()));
    }

    @Test
    void validatesOnlyAgainstExplicitVersion() throws Exception {
        Path alto = createAlto4Document();
        new AltoDatastream(profile("3.1"));

        assertFalse(AltoDatastream.isAlto(alto.toUri()));
    }

    private ImportProfile profile(String version) {
        BaseConfiguration configuration = new BaseConfiguration();
        if (version != null) {
            configuration.setProperty(ImportProfile.ALTO_VERSION, version);
        }
        return new ImportProfile(configuration);
    }

    private Path createAlto4Document() throws Exception {
        Path alto = tempDir.resolve("alto.xml");
        Files.writeString(alto, """
                <?xml version="1.0" encoding="UTF-8"?>
                <alto xmlns="http://www.loc.gov/standards/alto/ns-v4#">
                    <Layout>
                        <Page ID="page1" PHYSICAL_IMG_NR="1"/>
                    </Layout>
                </alto>
                """);
        return alto;
    }
}
