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
import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.common.config.Profiles;
import cz.cas.lib.proarc.common.export.archive.ArchiveImport;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.incad.imgsupport.ImageSupport.ScalingMethod;
import java.util.List;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConversionException;

/**
 * The import configuration. There can be several versions declared
 * with {@link #PROFILES} in {@code proarc.cfg}. They are available with {@link Profiles}.
 *
 * @author Jan Pokorsky
 */
public final class ImportProfile {

    /** The configuration key to register import configuration profiles. */
    public static final String PROFILES = "import.profiles";

    public static final String ALTO_SUFFIX = "import.alto.file.suffix";
    public static final String MODEL_ID = "import.page.modelId";
    public static final String NDK_ARCHIVAL_PROCESSOR = "import.ndk_archival.processor";
    public static final String NDK_ARCHIVAL_SUFFIX = "import.ndk_archival.file.suffix";
    public static final String NDK_USER_PROCESSOR = "import.ndk_user.processor";
    public static final String NDK_USER_SUFFIX = "import.ndk_user.file.suffix";
    public static final String PLAIN_OCR_CHARSET = "import.text_ocr.file.charset";
    public static final String PLAIN_OCR_SUFFIX = "import.text_ocr.file.suffix";
    public static final String PREVIEW_JAVA_SCALING = "import.image.preview.java.scalingMethod";
    public static final String PREVIEW_MAX_HEIGHT = "import.image.preview.maxHeight";
    public static final String PREVIEW_MAX_WIDTH = "import.image.preview.maxWidth";
    public static final String PROCESSOR = "processor";
    public static final String REQUIRED_DATASTREAM = "import.requiredDatastreamId";
    public static final String THUMBNAIL_JAVA_SCALING = "import.image.thumbnail.java.scalingMethod";
    public static final String THUMBNAIL_MAX_HEIGHT = "import.image.thumbnail.maxHeight";
    public static final String THUMBNAIL_MAX_WIDTH = "import.image.thumbnail.maxWidth";
    public static final String THUMBNAIL_PROCESSOR = "import.thumbnail.processor";
    public static final String CONVERTOR_JPG_SMALL_PROCESSOR = "import.jpg_small_convertor.processor";
    public static final String CONVERTOR_JPG_LARGE_PROCESSOR = "import.jpg_large_convertor.processor";
    public static final String CONVERTOR_JP2_PROCESSOR = "import.jp2_convertor.processor";


    private final Configuration config;
    private final String profileId;

    public ImportProfile(Configuration config) {
        this(config, ConfigurationProfile.DEFAULT);
    }

    public ImportProfile(Configuration config, String profileId) {
        this.config = config;
        this.profileId = profileId;
    }

    public String getProfileId() {
        return profileId;
    }

    public ImportHandler createImporter() {
        if (ConfigurationProfile.DEFAULT_ARCHIVE_IMPORT.equals(getProfileId())) {
            return new ArchiveImport();
        } else {
            return new FileSetImport();
        }
    }

    public String getModelId() {
        String val = config.getString(MODEL_ID, NdkPlugin.MODEL_PAGE);
        return val;
    }

    public String getPlainOcrCharset() {
        String val = config.getString(PLAIN_OCR_CHARSET);
        return val == null || val.isEmpty() ? "UTF-8" : val;
    }

    public String getPlainOcrFileSuffix() {
        String suffix = config.getString(PLAIN_OCR_SUFFIX, ".ocr.txt");
        return suffix.toLowerCase();
    }

    public String getAltoFileSuffix() {
        String suffix = config.getString(ALTO_SUFFIX, ".ocr.xml");
        return suffix.toLowerCase();
    }

    public Configuration getNdkUserProcessor() {
        String processor = config.getString(NDK_USER_PROCESSOR, "-");
        return config.subset(PROCESSOR + "." + processor);
    }

    public Configuration getNdkArchivalProcessor() {
        String processor = config.getString(NDK_ARCHIVAL_PROCESSOR, "-");
        return config.subset(PROCESSOR + "." + processor);
    }

    public String getNdkArchivalFileSuffix() {
        String suffix = config.getString(NDK_ARCHIVAL_SUFFIX, ".ac.jp2");
        return suffix.toLowerCase();
    }

    public String getNdkUserFileSuffix() {
        String suffix = config.getString(NDK_USER_SUFFIX, ".uc.jp2");
        return suffix.toLowerCase();
    }

    public Configuration getConvertorJpgSmallProcessor() {
        String processor = config.getString(CONVERTOR_JPG_SMALL_PROCESSOR, "-");
        return config.subset(PROCESSOR + "." + processor);
    }

    public Configuration getConvertorJpgLargeProcessor() {
        String processor = config.getString(CONVERTOR_JPG_LARGE_PROCESSOR, "-");
        return config.subset(PROCESSOR + "." + processor);
    }

    public Configuration getConvertorJp2Processor () {
        String processor = config.getString(CONVERTOR_JP2_PROCESSOR, "-");
        return config.subset(PROCESSOR + "." + processor);
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

    public List<Object> getRequiredDatastreamId() {
        return config.getList(REQUIRED_DATASTREAM);
    }

    public Configuration getThumbnailProcessor() {
        String processor = config.getString(THUMBNAIL_PROCESSOR, "-");
        String confId = PROCESSOR + "." + processor;
        Configuration subset = config.subset(confId);
        if (!subset.isEmpty() && !subset.containsKey("id")) {
            subset.addProperty("id", confId);
        }
        return subset;
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
