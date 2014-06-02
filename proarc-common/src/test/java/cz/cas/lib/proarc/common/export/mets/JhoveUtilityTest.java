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
package cz.cas.lib.proarc.common.export.mets;

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.imports.TiffImporterTest;
import cz.cas.lib.proarc.mix.Mix;
import cz.cas.lib.proarc.mix.MixUtils;
import java.io.File;
import org.apache.commons.io.FileUtils;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.Rule;

/**
 *
 * @author Jan Pokorsky
 */
public class JhoveUtilityTest {

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder();

    public JhoveUtilityTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testCreateContext() throws Exception {
        File root = temp.newFolder();
        JhoveContext ctx = JhoveUtility.createContext(root);
        assertNotNull(ctx);
        assertTrue(new File(root, JhoveUtility.JHOVE_CONFIG_NAME).exists());
        ctx.destroy();
        assertFalse(root.toString(), root.exists());
    }

    @Test
    public void testGetMix() throws Exception {
        File root = temp.getRoot();
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