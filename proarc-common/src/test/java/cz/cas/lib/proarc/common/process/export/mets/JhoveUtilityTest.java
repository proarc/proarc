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
import javax.xml.parsers.DocumentBuilderFactory;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
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
        assertEquals("image/tiff", mix.getBasicDigitalObjectInformation()
                .getFormatDesignation().getFormatName().getValue(), toXml);
    }

    @Test
    public void testRemoveIccProfileAndEmptyColorProfile() throws Exception {
        Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
        Node mix = document.appendChild(document.createElementNS("http://www.loc.gov/mix/v20", "mix:mix"));
        Node photometric = mix.appendChild(document.createElementNS("http://www.loc.gov/mix/v20", "mix:PhotometricInterpretation"));
        Node colorProfile = photometric.appendChild(document.createElementNS("http://www.loc.gov/mix/v20", "mix:ColorProfile"));
        colorProfile.appendChild(document.createElementNS("http://www.loc.gov/mix/v20", "mix:IccProfile"));

        JhoveUtility.removeIccProfile(mix);

        assertNull(JhoveUtility.getNodeRecursive(mix, "IccProfile"));
        assertNull(JhoveUtility.getNodeRecursive(mix, "ColorProfile"));
    }

    @Test
    public void testRemoveIccProfileKeepsOtherColorProfileMetadata() throws Exception {
        Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
        Node mix = document.appendChild(document.createElementNS("http://www.loc.gov/mix/v20", "mix:mix"));
        Node colorProfile = mix.appendChild(document.createElementNS("http://www.loc.gov/mix/v20", "mix:ColorProfile"));
        colorProfile.appendChild(document.createElementNS("http://www.loc.gov/mix/v20", "mix:IccProfile"));
        colorProfile.appendChild(document.createElementNS("http://www.loc.gov/mix/v20", "mix:LocalProfile"));

        JhoveUtility.removeIccProfile(mix);

        assertNull(JhoveUtility.getNodeRecursive(mix, "IccProfile"));
        assertNotNull(JhoveUtility.getNodeRecursive(mix, "ColorProfile"));
        assertNotNull(JhoveUtility.getNodeRecursive(mix, "LocalProfile"));
    }
}
