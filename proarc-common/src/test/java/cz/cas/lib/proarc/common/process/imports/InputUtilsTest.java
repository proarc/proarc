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
package cz.cas.lib.proarc.common.process.imports;

import cz.cas.lib.proarc.common.CustomTemporaryFolder;
import cz.cas.lib.proarc.common.process.imports.audio.WaveImporterTest;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import org.apache.commons.io.FileUtils;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.Rule;

/**
 *
 * @author Jan Pokorsky
 */
public class InputUtilsTest {

    @Rule
    public CustomTemporaryFolder temp = new CustomTemporaryFolder();

    public InputUtilsTest() {
    }

    @Test
    public void testIsJp2000() throws Exception {
        String templatePath = "testscan.uc.jp2";
        File result = createTestFile(templatePath, new File(temp.getRoot(), "test.jp2"));
        assertTrue(templatePath, InputUtils.isJp2000(result));
    }

    @Test
    public void testIsJpeg() throws Exception {
        String templatePath = "testscan.jpg";
        File result = createTestFile(templatePath, new File(temp.getRoot(), "test.jpg"));
        assertTrue(templatePath, InputUtils.isJpeg(result));
    }

    @Test
    public void testIsPdf() throws Exception {
        String templatePath = "pdfa_test.pdf";
        File result = createTestFile(templatePath, new File(temp.getRoot(), "test.pdf"));
        assertTrue(templatePath, InputUtils.isPdf(result));
    }

    @Test
    public void testIsTiff() throws Exception {
        String templatePath = "testscan-lzw.tiff";
        File result = createTestFile(templatePath, new File(temp.getRoot(), "test.tiff"));
        assertTrue(templatePath, InputUtils.isTiff(result));
    }

    @Test
    public void testIsWav() throws Exception {
        String templatePath = "test_wav.mc.wav";
        File result = createAudioTestFile(templatePath, new File(temp.getRoot(), "test.wav"));
        assertTrue(templatePath, InputUtils.isWave(result));
    }

    @Test
    public void testIsMp3() throws Exception {
        String templatePath = "test_mp3.uc.mp3";
        File result = createAudioTestFile(templatePath, new File(temp.getRoot(), "test.wav"));
        assertTrue(templatePath, InputUtils.isMp3(result));
    }

    @Test
    public void testIsTiffInvalid() throws Exception {
        String templatePath = "testscan.jpg";
        File result = createTestFile(templatePath, new File(temp.getRoot(), "test.tiff"));
        assertFalse(templatePath, InputUtils.isTiff(result));
    }

    @Test(expected = FileNotFoundException.class)
    public void testIsTiffMissingFile() throws Exception {
        File result = new File(temp.getRoot(), "missing.tif");
        InputUtils.isTiff(result);
    }

    @Test
    public void testIsTiffEmptyFile() throws Exception {
        File result = temp.newFile("empty.tif");
        assertFalse(result.getName(), InputUtils.isTiff(result));
    }

    private File createTestFile(String templatePath, File file) throws IOException {
        URL resource = TiffImporterTest.class.getResource(templatePath);
        assertNotNull(resource);
        FileUtils.copyURLToFile(resource, file);
        return file;
    }

    private File createAudioTestFile(String templatePath, File file) throws IOException {
        URL resource = WaveImporterTest.class.getResource(templatePath);
        assertNotNull(resource);
        FileUtils.copyURLToFile(resource, file);
        return file;
    }

}
