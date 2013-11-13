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
package cz.cas.lib.proarc.common.imports;

import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.incad.imgsupport.ImageSupport.ScalingMethod;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConversionException;

/**
 * Import configuration.
 *
 * <p>Later there could be predefined profiles for TIFF, PDF, ...
 *
 * @author Jan Pokorsky
 */
public final class ImportProfile {

    public static final String PREVIEW_MAX_HEIGHT = "import.image.preview.maxHeight";
    public static final String PREVIEW_MAX_WIDTH = "import.image.preview.maxWidth";
    public static final String PREVIEW_JAVA_SCALING = "import.image.preview.java.scalingMethod";
    public static final String THUMBNAIL_MAX_HEIGHT = "import.image.thumbnail.maxHeight";
    public static final String THUMBNAIL_MAX_WIDTH = "import.image.thumbnail.maxWidth";
    public static final String THUMBNAIL_JAVA_SCALING = "import.image.thumbnail.java.scalingMethod";
    public static final String PLAIN_OCR_CHARSET = "import.text_ocr.file.charset";
    public static final String PLAIN_OCR_SUFFIX = "import.text_ocr.file.suffix";

    private final Configuration config;

    public ImportProfile(Configuration config) {
        this.config = config;
    }

    public String getPlainOcrCharset() {
        String val = config.getString(PLAIN_OCR_CHARSET);
        return val == null || val.isEmpty() ? "UTF-8" : val;
    }

    public String getPlainOcrFileSuffix() {
        String suffix = config.getString(PLAIN_OCR_SUFFIX, ".ocr.txt");
        return suffix.toLowerCase();
    }

    public Integer getPreviewMaxHeight() {
        return getPositiveInteger(PREVIEW_MAX_HEIGHT);
    }

    public Integer getPreviewMaxWidth() {
        return getPositiveInteger(PREVIEW_MAX_WIDTH);
    }

    public ScalingMethod getPreviewScaling() {
        return getJavaScaling(PREVIEW_JAVA_SCALING);
    }

    public Integer getThumbnailMaxHeight() {
        return getPositiveInteger(THUMBNAIL_MAX_HEIGHT);
    }

    public Integer getThumbnailMaxWidth() {
        return config.getInteger(THUMBNAIL_MAX_WIDTH, null);
    }

    public ScalingMethod getThumbnailScaling() {
        return getJavaScaling(THUMBNAIL_JAVA_SCALING);
    }

    public void checkPreviewScaleParams() throws AppConfigurationException {
        Integer maxHeight = getPreviewMaxHeight();
        Integer maxWidth = getPreviewMaxWidth();
        if (maxHeight == null && maxHeight == maxWidth) {
            throw new AppConfigurationException(String.format("%s and %s cannot be null!",
                    PREVIEW_MAX_HEIGHT, PREVIEW_MAX_WIDTH));
        }
    }

    public void checkThumbnailScaleParams() throws AppConfigurationException {
        Integer maxHeight = getThumbnailMaxHeight();
        Integer maxWidth = getThumbnailMaxWidth();
        if (maxHeight == null && maxHeight == maxWidth) {
            throw new AppConfigurationException(String.format("%s and %s cannot be null!",
                    THUMBNAIL_MAX_HEIGHT, THUMBNAIL_MAX_WIDTH));
        }
    }

    private ScalingMethod getJavaScaling(String key) {
        String val = config.getString(key);
        if (val == null || val.isEmpty()) {
            return ScalingMethod.BICUBIC_STEPPED;
        }
        try {
            ScalingMethod hint = ScalingMethod.valueOf(val);
            return hint;
        } catch (Exception e) {
            throw new ConversionException(key, e);
        }
    }

    private Integer getPositiveInteger(String key) {
        Integer val = config.getInteger(key, null);
        if (val != null && val <= 0) {
            throw new ConversionException(key + " expects positive integer!");
        }
        return val;
    }

}
