/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.config;

import java.io.File;

/**
 * The configuration profile. It is a descriptor of an alternative configuration
 * file.
 *
 * @author Jan Pokorsky
 */
public class ConfigurationProfile {

    /** The ID of {@code proarc.cfg}. */
    public static final String DEFAULT = "profile.default";
    /** The ID of the default import with creation a parent and setting parents metadata that is based on {@code proarc.cfg}. */
    public static final String IMPORT_WITH_CREATION_PARENT = "profile.createObjectWithMetadata_import";
    /** The ID of the default archive import profile that is based on {@code proarc.cfg}. */
    public static final String DEFAULT_ARCHIVE_IMPORT = "profile.default_archive_import";
    /** The ID of the default ndk import profile that is based on {@code proarc.cfg}. */
    public static final String DEFAULT_NDK_IMPORT = "profile.default_ndk_import";
    /** The ID of the default soundrecording import profile that is based on (@code proarc.cfg]. */
    public static final String DEFAULT_SOUNDRECORDING_IMPORT = "profile.soundrecording_import";
    /** The ID of the default kramerius import profile that is based on (@code proarc.cfg]. */
    public static final String DEFAULT_KRAMERIUS_IMPORT = "profile.default_kramerius_import";
    /** The ID of the ndk monograph kramerius import profile that is based on (@code proarc.cfg]. */
    public static final String STT_KRAMERIUS_IMPORT = "profile.stt_kramerius_import";
    /** The ID of the ndk monograph kramerius import profile that is based on (@code proarc.cfg]. */
    public static final String NDK_MONOGRAPH_KRAMERIUS_IMPORT = "profile.ndk_monograph_kramerius_import";
    /** The ID of the ndk monograph title kramerius import profile that is based on (@code proarc.cfg]. */
    public static final String NDK_MONOGRAPH_TITLE_KRAMERIUS_IMPORT = "profile.ndk_monograph_title_kramerius_import";
    /** The ID of the ndk periodical kramerius import profile that is based on (@code proarc.cfg]. */
    public static final String NDK_PERIODICAL_KRAMERIUS_IMPORT = "profile.ndk_periodical_kramerius_import";
    /** The ID of the ndk eMonograph kramerius import profile that is based on (@code proarc.cfg]. */
    public static final String NDK_EMONOGRAPH_KRAMERIUS_IMPORT = "profile.ndk_emonograph_kramerius_import";
    /** The ID of the ndk eMonograph title kramerius import profile that is based on (@code proarc.cfg]. */
    public static final String NDK_EMONOGRAPH_TITLE_KRAMERIUS_IMPORT = "profile.ndk_emonograph_title_kramerius_import";
    /** The ID of the ndk ePeriodical kramerius import profile that is based on (@code proarc.cfg]. */
    public static final String NDK_EPERIODICAL_KRAMERIUS_IMPORT = "profile.ndk_eperiodical_kramerius_import";
    /** The ID of profile that only generates ALTO and OCR. */
    public static final String GENERATE_ALTO_OCR = "profile.generate";
    /** The ID of profile that replace stream according to file name. */
    public static final String REPLACE_STREAM_IMPORT = "profile.replace_stream_import";
    private final String id;
    private File file;
    private String label;
    private String description;
    private String error;

    public ConfigurationProfile(String id) {
        if (id == null) {
            throw new NullPointerException();
        }
        this.id = id;
    }

    public String getId() {
        return id;
    }

    public File getFile() {
        return file;
    }

    public void setFile(File file) {
        this.file = file;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getError() {
        return error;
    }

    public void setError(String error) {
        this.error = error;
    }

    @Override
    public String toString() {
        return "ConfigurationProfile{" + "id=" + id + ", file=" + file + '}';
    }

    @Override
    public int hashCode() {
        int hash = 3;
        hash = 11 * hash + (this.id != null ? this.id.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final ConfigurationProfile other = (ConfigurationProfile) obj;
        return this.id.equals(other.id);
    }

}
