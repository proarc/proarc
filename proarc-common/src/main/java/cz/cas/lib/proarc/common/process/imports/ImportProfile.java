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
import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.common.config.Profiles;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.process.imports.archive.ArchiveImport;
import cz.cas.lib.proarc.common.process.imports.audio.SoundRecordingImport;
import cz.cas.lib.proarc.common.process.imports.kramerius.FileReader;
import cz.cas.lib.proarc.common.process.imports.kramerius.KrameriusImport;
import cz.cas.lib.proarc.common.process.imports.ndk.NdkImport;
import cz.cas.lib.proarc.common.process.imports.replaceStream.ReplaceStreamImport;
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

    public static final String RAW_SUFFIX = "import.raw.file.suffix";
    public static final String ALTO_SUFFIX = "import.alto.file.suffix";
    public static final String ALTO_FILE_PATH = "import.alto.file.path";
    public static final String ALTO_VERSION = "import.alto.file.version";
    public static final String MODEL_ID = "import.page.modelId";
    public static final String MODEL_AUDIO_ID="import.ndkaudiopage.modelId";
    public static final String PAGE_PATH="import.page.path";
    public static final String NDK_ARCHIVAL_PROCESSOR = "import.ndk_archival.processor";
    public static final String JP2_TO_TIFF_PROCESSOR = "import.jp2ToTiff.processor";
    public static final String NDK_ARCHIVAL_SUFFIX = "import.ndk_archival.file.suffix";
    public static final String NDK_USER_PROCESSOR = "import.ndk_user.processor";
    public static final String NDK_USER_SUFFIX = "import.ndk_user.file.suffix";
    public static final String PLAIN_OCR_CHARSET = "import.text_ocr.file.charset";
    public static final String PLAIN_OCR_FILE_PATH = "import.text_ocr.file.path";
    public static final String OCRALTO_LAST_FOLDER_PATH = "import.ocralto.last.folder.path";
    public static final String PLAIN_OCR_SUFFIX = "import.text_ocr.file.suffix";
    public static final String PREVIEW_JAVA_SCALING = "import.image.preview.java.scalingMethod";
    public static final String PREVIEW_MAX_HEIGHT = "import.image.preview.maxHeight";
    public static final String PREVIEW_MAX_WIDTH = "import.image.preview.maxWidth";
    public static final String PREVIEW_SUFFIX = "import.preview.file.suffix";
    public static final String PROCESSOR = "processor";
    public static final String REQUIRED_DATASTREAM = "import.requiredDatastreamId";
    public static final String SKIPPED_DATASTREAM = "import.skippedDatastreamId";
    public static final String THUMBNAIL_JAVA_SCALING = "import.image.thumbnail.java.scalingMethod";
    public static final String THUMBNAIL_MAX_HEIGHT = "import.image.thumbnail.maxHeight";
    public static final String THUMBNAIL_MAX_WIDTH = "import.image.thumbnail.maxWidth";
    public static final String THUMBNAIL_PROCESSOR = "import.thumbnail.processor";
    public static final String THUMBNAIL_SUFFIX = "import.thumbnail.file.suffix";
    public static final String FULL_SUFFIX = "import.full.file.suffix";
    public static final String CONVERTOR_JPG_SMALL_PROCESSOR = "import.jpg_small_convertor.processor";
    public static final String CONVERTOR_JPG_LARGE_PROCESSOR = "import.jpg_large_convertor.processor";
    public static final String CONVERTOR_JP2_PROCESSOR = "import.jp2_convertor.processor";
    public static final String OCR_GEN_PROCESSOR = "import.ocr_generator.processor";
    public static final String CONVERTOR_TIFF_JPG_PROCESSOR = "import.tiff_to_jpg_convertor.processor";
    public static final String NDK_SOURCE_AUDIO_SUFFIX = "import.ndk_audio_source.file.suffix";
    public static final String NDK_ARCHIVAL_AUDIO_SUFFIX = "import.ndk_audio_archival.file.suffix";
    public static final String NDK_USER_AUDIO_SUFFIX = "import.ndk_audio_user.file.suffix";
    public static final String CREATE_MODELS_HIERARCHY = "import.create.models_hierarchy";
    public static final String DEFAULT_PROCESSOR = "import.create.defaultProcessor";
    public static final String DEFAULT_ALTO = "import.default_alto.file";
    public static final String DEFAULT_OCR = "import.default_ocr.file";
    public static final String DEFAULT_CATALOG = "import.catalog.file";
    public static final String DEFAULT_ARCHIVE_CATALOG = "import.archive.catalog.file";
    public static final String DEFAULT_ALTO_AND_OCR = "import.default_alto_and_ocr";
    public static final String DELETE_PACKAGE_IMPORT = "import.delete_package";
    public static final String DEFAULT_IMPORT_FOLDER = "import.folder.default";
    public static final String IMPORT_FOLDER_PATH = "import.folder.path";
    public static final String IMPORT_FOXML_IMAGE_SERVER_PATH = "import.foxml.imageServer.path";
    public static final String IMPORT_FOXML_FOLDER_PATH = "import.foxml.folder.path";


    public static final String[] FILE_EXTENSIONS = new String[]{".xml", ".jp2", ".txt", ".jpg", ".jpeg", ".mp3", ".ogg", ".wav", ".flac", ".tif"};

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
        switch(getProfileId()) {
            case ConfigurationProfile.DEFAULT_ARCHIVE_IMPORT:
                return new ArchiveImport();
            case ConfigurationProfile.DEFAULT_NDK_IMPORT:
                return new NdkImport();
            case ConfigurationProfile.DEFAULT_KRAMERIUS_IMPORT:
                return new KrameriusImport(FileReader.K4_MAP);
            case ConfigurationProfile.NDK_MONOGRAPH_KRAMERIUS_IMPORT:
                return new KrameriusImport(FileReader.NDK_MONOGRAPH_MAP);
            case ConfigurationProfile.NDK_PERIODICAL_KRAMERIUS_IMPORT:
                return new KrameriusImport(FileReader.NDK_PERIODICAL_MAP);
            case ConfigurationProfile.NDK_EMONOGRAPH_KRAMERIUS_IMPORT:
                return new KrameriusImport(FileReader.NDK_EMONOGRAPH_MAP);
            case ConfigurationProfile.NDK_EPERIODICAL_KRAMERIUS_IMPORT:
                return new KrameriusImport(FileReader.NDK_EPERIODICAL_MAP);
            case ConfigurationProfile.STT_KRAMERIUS_IMPORT:
                return new KrameriusImport(FileReader.STT_MAP);
            case ConfigurationProfile.DEFAULT_SOUNDRECORDING_IMPORT:
                return new SoundRecordingImport();
            case ConfigurationProfile.IMPORT_WITH_CREATION_PARENT:
                return new FileSetImportWithParentCreated();
            case ConfigurationProfile.GENERATE_ALTO_OCR:
                return new GeneratorAltoOcr();
            case ConfigurationProfile.REPLACE_STREAM_IMPORT:
                return new ReplaceStreamImport();
            default:
                return new FileSetImport();
        }
        /*
        if (ConfigurationProfile.DEFAULT_ARCHIVE_IMPORT.equals(getProfileId())) {
            return new ArchiveImport();
        } else if(ConfigurationProfile.DEFAULT_KRAMERIUS_IMPORT.equals(getProfileId())) {
            return new KrameriusImport();
        } else {
            return new FileSetImport();
        }
        */
    }

    public String getModelId() {
        String val = config.getString(MODEL_ID, NdkPlugin.MODEL_PAGE);
        return val;
    }

    public String getAudioModelID() {
        String val = config.getString(MODEL_AUDIO_ID, NdkAudioPlugin.MODEL_PAGE);
        return val;
    }

    public boolean isPagePath() {
        String val = config.getString(PAGE_PATH, "false");
        return Boolean.parseBoolean(val);
    }

    public String getRawFileSuffix() {
        String suffix = config.getString(RAW_SUFFIX, ".tif");
        return suffix.toLowerCase();
    }

    public String getPlainOcrCharset() {
        String val = config.getString(PLAIN_OCR_CHARSET);
        return val == null || val.isEmpty() ? "UTF-8" : val;
    }

    public String getPlainOcrFileSuffix() {
        String suffix = config.getString(PLAIN_OCR_SUFFIX, ".ocr.txt");
        return suffix.toLowerCase();
    }

    public String getPlainOcrFilePath() {
        String path = config.getString(PLAIN_OCR_FILE_PATH, "null");
        return path.toLowerCase();
    }

    public int getOcrAltoFolderPath() {
        int value = config.getInt(OCRALTO_LAST_FOLDER_PATH, 2);
        return value;
    }

    public String getAltoFileSuffix() {
        String suffix = config.getString(ALTO_SUFFIX, ".ocr.xml");
        return suffix.toLowerCase();
    }

    public String getAltoFilePath() {
        String suffix = config.getString(ALTO_FILE_PATH, "null");
        return suffix.toLowerCase();
    }

    public String getAltoFileVersion() {
        String version = config.getString(ALTO_VERSION, null);
        return version.toLowerCase();
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

    public List<Object> getNdkSourceAudioFileSuffix() {
        List<Object> suffix = config.getList(NDK_SOURCE_AUDIO_SUFFIX);
        return suffix;
    }

    public List<Object> getNdkArchivalAudioFileSuffix() {
        List<Object>  suffix = config.getList(NDK_ARCHIVAL_AUDIO_SUFFIX);
        if (suffix == null || suffix.isEmpty()) {
            suffix.add("ac.wav");
        }
        return suffix;
    }

    public String getNdkUserFileSuffix() {
        String suffix = config.getString(NDK_USER_SUFFIX, ".uc.jp2");
        return suffix.toLowerCase();
    }

    public List<Object> getNdkUserAudioFileSuffix() {
        List<Object> suffix = config.getList(NDK_USER_AUDIO_SUFFIX);
        if (suffix == null || suffix.isEmpty()) {
            suffix.add(".uc.mp3");
        }
        return suffix;
    }

    public String getNdkFullFileSuffix() {
        String suffix = config.getString(FULL_SUFFIX, ".full.jpg");
        return suffix.toLowerCase();
    }

    public String getNdkPreviewFileSuffix() {
        String suffix = config.getString(PREVIEW_SUFFIX, ".preview.jpg");
        return suffix.toLowerCase();
    }

    public String getNdkThumbnailFileSuffix() {
        String suffix = config.getString(THUMBNAIL_SUFFIX, ".thumbnail.jpg");
        return suffix.toLowerCase();
    }

    public String getDefaultOcr() {
        String path = config.getString(DEFAULT_OCR);
        return path.toLowerCase();
    }

    public String getDefaultAlto() {
        String path = config.getString(DEFAULT_ALTO);
        return path.toLowerCase();
    }

    public String getDefaultCatalog() {
        String path = config.getString(DEFAULT_CATALOG);
        return path.toLowerCase();
    }

    public String getArchiveCatalog() {
        String path = config.getString(DEFAULT_ARCHIVE_CATALOG);
        return path.toLowerCase();
    }

    public boolean getDefaultAltoAndOcr() {
        String value = config.getString(DEFAULT_ALTO_AND_OCR);
        return "true".equals(value);
    }

    public Configuration getOcrGenProcessor() {
        String processor = config.getString(OCR_GEN_PROCESSOR, "-");
        return config.subset(PROCESSOR + "." + processor);
    }

    public boolean getDeletePackageImport() {
        String value = config.getString(DELETE_PACKAGE_IMPORT);
        return "true".equals(value);
    }

    public boolean getDefaultImportFolder() {
        String value = config.getString(DEFAULT_IMPORT_FOLDER);
        if (!(value == null || value.isEmpty() || "true".equals(value))) {
            return false;
        } else {
            return true;
        }
    }

    public String getImportFolderPath() {
        String value = config.getString(IMPORT_FOLDER_PATH);
        return value;
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

    public Configuration getConvertorTiffToJpgProcessor () {
        String processor = config.getString(CONVERTOR_TIFF_JPG_PROCESSOR, "-");
        return config.subset(PROCESSOR + "." + processor);
    }

    public Boolean getCreateModelsHierarchy() {
        String createHierarchy = config.getString(CREATE_MODELS_HIERARCHY, "false");
        return  Boolean.parseBoolean(createHierarchy);
    }

    public String getFoxmlImageServerPath() {
        String imageServerPath = config.getString(IMPORT_FOXML_IMAGE_SERVER_PATH);
        return imageServerPath;
    }

    public String getFoxmlFolderPath() {
        String folderPath = config.getString(IMPORT_FOXML_FOLDER_PATH);
        return folderPath;
    }

    public String getDefaultProcessor() {
        String processor = config.getString(DEFAULT_PROCESSOR, "all");
        return  processor;
    }

    public boolean isTiffToJpgDefined() {
        return !config.getString(CONVERTOR_TIFF_JPG_PROCESSOR, "-").equals("-");
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

    public List<Object> getSkippedDatastreamId() {
        return config.getList(SKIPPED_DATASTREAM);
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
