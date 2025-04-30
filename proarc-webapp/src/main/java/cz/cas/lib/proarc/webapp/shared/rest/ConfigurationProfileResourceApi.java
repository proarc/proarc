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
package cz.cas.lib.proarc.webapp.shared.rest;

import cz.cas.lib.proarc.common.config.ConfigurationProfile;
import cz.cas.lib.proarc.common.process.imports.ImportProfile;

/**
 * The resource {@code /profile} API.
 *
 * @author Jan Pokorsky
 */
public interface ConfigurationProfileResourceApi {

    // resource /profile
    public static final String PATH = "profile";
    /**
     * @see ProfileGroup
     */
    public static final String PROFILE_GROUP_PARAM = "profileGroup";
    public static final String PROFILE_ID = "id";
    public static final String PROFILE_LABEL = "label";
    public static final String PROFILE_DESCRIPTION = "description";
    public static final String PROFILE_ERROR = "error";
    public static final String ARCHIVE_ID = ConfigurationProfile.DEFAULT_ARCHIVE_IMPORT;
    public static final String NDK_ID = ConfigurationProfile.DEFAULT_NDK_IMPORT;
    public static final String KRAMERIUS_DEFAULT_ID = ConfigurationProfile.DEFAULT_KRAMERIUS_IMPORT;
    public static final String KRAMERIUS_NDK_MONOGRAPH_ID = ConfigurationProfile.NDK_MONOGRAPH_KRAMERIUS_IMPORT;
    public static final String KRAMERIUS_NDK_MONOGRAPH_TITLE_ID = ConfigurationProfile.NDK_MONOGRAPH_TITLE_KRAMERIUS_IMPORT;
    public static final String KRAMERIUS_NDK_PERIODICAL_ID = ConfigurationProfile.NDK_PERIODICAL_KRAMERIUS_IMPORT;
    public static final String KRAMERIUS_NDK_EMONOGRAPH_ID = ConfigurationProfile.NDK_EMONOGRAPH_KRAMERIUS_IMPORT;
    public static final String KRAMERIUS_NDK_EPERIODICAL_ID = ConfigurationProfile.NDK_EPERIODICAL_KRAMERIUS_IMPORT;
    public static final String KRAMERIUS_STT_ID = ConfigurationProfile.STT_KRAMERIUS_IMPORT;
    public static final String REPLACE_STREAM_ID = ConfigurationProfile.REPLACE_STREAM_IMPORT;

    /**
     * @see #PROFILE_GROUP_PARAM
     */
    public enum ProfileGroup {

        IMPORTS(ImportProfile.PROFILES);

        private String id;

        private ProfileGroup(String id) {
            this.id = id;
        }

        public String getId() {
            return id;
        }

        public static ProfileGroup fromString(String s) {
            for (ProfileGroup value : values()) {
                if (value.getId().equals(s)) {
                    return value;
                }
            }
            return null;
        }

    }
}
