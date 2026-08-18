/*
 * Copyright (C) 2026 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.process.imports.kramerius;

import java.io.File;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;

class FileReaderTest {

    @TempDir
    Path tempDir;

    @Test
    void findsMasterCopyNamedAfterParentUrlSegment() throws Exception {
        File foxml = Files.createFile(tempDir.resolve("uuid.xml")).toFile();
        File masterCopy = Files.createFile(tempDir.resolve("HB_720_041a.JP2")).toFile();

        File result = FileReader.findSiblingMasterCopy(foxml,
                "http://imageserver.mlp.cz/imageserver/Archiv/TITULY/D/das_dritte_hundertjahrige/"
                + "HB_720/d_book/UC/JP2/HB_720_041a/big.jpg");

        assertEquals(masterCopy, result);
    }

    @Test
    void ignoresLocalContentLocation() throws Exception {
        File foxml = Files.createFile(tempDir.resolve("uuid.xml")).toFile();
        Files.createFile(tempDir.resolve("HB_720_041a.jp2"));

        assertNull(FileReader.findSiblingMasterCopy(foxml, "C:/images/HB_720_041a/big.jpg"));
    }

    @Test
    void createsValidFileUri() {
        File file = new File("C:\\Users\\Lukas Sykora\\HB_720_001a.full.jpg");

        String result = FileReader.toFileUri(file);

        assertEquals(file.toURI(), URI.create(result));
        assertFalse(result.contains("\\"));
    }
}
