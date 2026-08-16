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
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;

public class FileReaderTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void findsMasterCopyNamedAfterParentUrlSegment() throws Exception {
        File foxml = folder.newFile("uuid.xml");
        File masterCopy = folder.newFile("HB_720_041a.JP2");

        File result = FileReader.findSiblingMasterCopy(foxml,
                "http://imageserver.mlp.cz/imageserver/Archiv/TITULY/D/das_dritte_hundertjahrige/"
                + "HB_720/d_book/UC/JP2/HB_720_041a/big.jpg");

        assertEquals(masterCopy, result);
    }

    @Test
    public void ignoresLocalContentLocation() throws Exception {
        File foxml = folder.newFile("uuid.xml");
        folder.newFile("HB_720_041a.jp2");

        assertNull(FileReader.findSiblingMasterCopy(foxml, "C:/images/HB_720_041a/big.jpg"));
    }

    @Test
    public void createsValidFileUri() {
        File file = new File("C:\\Users\\Lukas Sykora\\HB_720_001a.full.jpg");

        String result = FileReader.toFileUri(file);

        assertEquals(file.toURI(), URI.create(result));
        assertFalse(result.contains("\\"));
    }
}
