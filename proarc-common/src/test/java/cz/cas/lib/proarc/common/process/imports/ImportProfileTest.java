/*
 * Copyright (C) 2013 Jan Pokorsky
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

import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.incad.imgsupport.ImageSupport.ScalingMethod;
import org.apache.commons.configuration2.BaseConfiguration;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;

/**
 *
 * @author Jan Pokorsky
 */
public class ImportProfileTest {

    private BaseConfiguration conf;

    public ImportProfileTest() {
    }

    @BeforeEach
    public void setUp() {
        conf = new BaseConfiguration();
    }

    @AfterEach
    public void tearDown() {
    }

    /**
     * Test of getPlainOcrCharset method, of class ImportProfile.
     */
    @Test
    public void testGetPlainOcrCharset() {
        ImportProfile instance = new ImportProfile(conf);
        String expResult = "UTF-8";
        String result = instance.getPlainOcrCharset();
        assertEquals(expResult, result);

        conf.addProperty(ImportProfile.PLAIN_OCR_CHARSET, "");
        result = instance.getPlainOcrCharset();
        assertEquals(expResult, result);

        expResult = "testCharset";
        conf.setProperty(ImportProfile.PLAIN_OCR_CHARSET, expResult);
        result = instance.getPlainOcrCharset();
        assertEquals(expResult, result);
    }

    /**
     * Test of getPreviewMaxHeight method, of class ImportProfile.
     */
    @Test
    public void testGetPreviewMaxHeight() {
        ImportProfile instance = new ImportProfile(conf);
        assertNull(instance.getPreviewMaxHeight());

        conf.setProperty(ImportProfile.PREVIEW_MAX_HEIGHT, "100");
        assertEquals(Integer.valueOf(100), instance.getPreviewMaxHeight());

        try {
            conf.setProperty(ImportProfile.PREVIEW_MAX_HEIGHT, "nonsense");
            instance.getPreviewMaxHeight();
            fail();
        } catch (Exception e) {
        }
        try {
            conf.setProperty(ImportProfile.PREVIEW_MAX_HEIGHT, -100);
            instance.getPreviewMaxHeight();
            fail();
        } catch (Exception e) {
        }
    }

    @Test
    public void testGetThumbnailScaling() {
        ImportProfile instance = new ImportProfile(conf);
        // test default
        assertEquals(ScalingMethod.BICUBIC_STEPPED, instance.getThumbnailScaling());
        // test invalid
        conf.setProperty(ImportProfile.THUMBNAIL_JAVA_SCALING, "nonsense");
        try {
            fail(String.valueOf(instance.getThumbnailScaling()));
        } catch (Exception e) {
        }
    }

    @Test
    public void testCheckThumbnailScaleParams() throws Exception {
        ImportProfile instance = new ImportProfile(conf);
        try {
            instance.checkThumbnailScaleParams();
            fail();
        } catch (AppConfigurationException ex) {
        }
        conf.setProperty(ImportProfile.THUMBNAIL_MAX_HEIGHT, 1);
        instance.checkThumbnailScaleParams();
    }
}