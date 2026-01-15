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

import cz.cas.lib.proarc.common.process.imports.audio.WaveImporterTest;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 *
 * @author Jan Pokorsky
 */
public class InputUtilsTest {

    @TempDir
    File tempDir;

    public InputUtilsTest() {
    }

    @Test
    public void testIsJp2000() throws Exception {
        String templatePath = "testscan.uc.jp2";
        File result = createTestFile(templatePath, new File(tempDir, "test.jp2"));
        assertTrue(InputUtils.isJp2000(result), () -> templatePath);
    }

    @Test
    public void testIsJpeg() throws Exception {
        String templatePath = "testscan.jpg";
        File result = createTestFile(templatePath, new File(tempDir, "test.jpg"));
        assertTrue(InputUtils.isJpeg(result), () -> templatePath);
    }

    @Test
    public void testIsPdf() throws Exception {
        String templatePath = "pdfa_test.pdf";
        File result = createTestFile(templatePath, new File(tempDir, "test.pdf"));
        assertTrue(InputUtils.isPdf(result), () -> templatePath);
    }

    @Test
    public void testIsTiff() throws Exception {
        String templatePath = "testscan-lzw.tiff";
        File result = createTestFile(templatePath, new File(tempDir, "test.tiff"));
        assertTrue(InputUtils.isTiff(result), () -> templatePath);
    }

    @Test
    public void testIsWav() throws Exception {
        String templatePath = "test_wav.mc.wav";
        File result = createAudioTestFile(templatePath, new File(tempDir, "test.wav"));
        assertTrue(InputUtils.isWave(result), () -> templatePath);
    }

    @Test
    public void testIsMp3() throws Exception {
        String templatePath = "test_mp3.uc.mp3";
        File result = createAudioTestFile(templatePath, new File(tempDir, "test.wav"));
        assertTrue(InputUtils.isMp3(result), () -> templatePath);
    }

    @Test
    public void testIsTiffInvalid() throws Exception {
        String templatePath = "testscan.jpg";
        File result = createTestFile(templatePath, new File(tempDir, "test.tiff"));
        assertFalse(InputUtils.isTiff(result), () -> templatePath);
    }

    @Test
    public void testIsTiffMissingFile() throws Exception {
        File result = new File(tempDir, "missing.tif");
        assertThrows(FileNotFoundException.class, () -> {
            InputUtils.isTiff(result);
        });

    }

    @Test
    public void testIsTiffEmptyFile() throws Exception {
        File result = new File(tempDir, "empty.tif");
        assertFalse(InputUtils.isTiff(result), () -> result.getName());
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
