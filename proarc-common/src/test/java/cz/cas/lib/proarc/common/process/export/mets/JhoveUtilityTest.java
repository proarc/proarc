/*
 * Copyright (C) 2014 Jan Pokorsky
 *
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
package cz.cas.lib.proarc.common.process.export.mets;

import cz.cas.lib.proarc.common.process.imports.TiffImporterTest;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mix.MixUtils;
import java.io.File;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 *
 * @author Jan Pokorsky
 */
public class JhoveUtilityTest {

    @TempDir
    File tempDir;

    public JhoveUtilityTest() {
    }

    @BeforeAll
    public static void setUpClass() {
    }

    @AfterAll
    public static void tearDownClass() {
    }

    @BeforeEach
    public void setUp() {
    }

    @AfterEach
    public void tearDown() {
    }

    @Test
    public void testCreateContext() throws Exception {
        File root = tempDir;
        JhoveContext ctx = JhoveUtility.createContext(root);
        assertNotNull(ctx);
        assertTrue(new File(root, JhoveUtility.JHOVE_CONFIG_NAME).exists());
        ctx.destroy();
        assertFalse(root.exists(), () -> root.toString());
    }

    @Test
    public void testGetMix() throws Exception {
        File root = tempDir;
        File imageFile = new File(root, "test.tif");
//        FileUtils.copyFile(new File("/tmp/test.jp2"),
//                imageFile, true);
        FileUtils.copyURLToFile(TiffImporterTest.class.getResource("testscan.tiff"), imageFile);
        JHoveOutput output = JhoveUtility.getMix(imageFile, root, null,
                MetsUtils.getCurrentDate(), "testscan.tiff");
        assertNotNull(output);
        Mix mix = output.getMix();
        assertNotNull(mix);

        String toXml = MixUtils.toXml(mix, true);
//        System.out.println(toXml);
        assertEquals(toXml, "image/tiff", mix.getBasicDigitalObjectInformation()
                .getFormatDesignation().getFormatName().getValue());
    }
}