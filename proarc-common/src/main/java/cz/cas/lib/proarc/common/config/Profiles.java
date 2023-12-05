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

import cz.cas.lib.proarc.common.process.imports.ImportProfile;
import java.io.File;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;

/**
 * This manages the configuration profiles.
 *
 * @author Jan Pokorsky
 */
public class Profiles {

    private static final Logger LOG = Logger.getLogger(Profiles.class.getName());
    private final Configuration config;
    private final URI configHomeUri;

    public Profiles(Configuration config, File configHome) {
        this.config = config;
        this.configHomeUri = configHome.toURI();
    }

    /**
     * Finds a profile ID among registered profiles of a given type.
     * Profiles with invalid file path are considered as not found.
     * @param profileGroup e.g. {@link ImportProfile#PROFILES}
     * @param profileId ID to search
     * @return the found profile or {@code null}
     */
    public ConfigurationProfile getProfile(String profileGroup, String profileId) {
        if (profileGroup == null) {
            throw new NullPointerException("profileGroup");
        }
        for (String registeredId : getGroupIds(profileGroup)) {
            if (registeredId.equals(profileId)) {
                return getProfile(profileId);
            }
        }
        return null;
    }

    /**
     * Gets a profile. The profile may not be part of any group.
     * Profiles with invalid file path are considered as not found.
     * @param profileId ID to search
     * @return the found profile or {@code null}
     */
    public ConfigurationProfile getProfile(String profileId) {
        if (profileId == null) {
            return null;
        }
        String label = config.getString(profileId + ".label", profileId);
        String dsc = config.getString(profileId + ".description", label);
        String filePath = config.getString(profileId + ".file", null);
        ConfigurationProfile profile = new ConfigurationProfile(profileId);
        File file = getProfileFile(profile, filePath);
        profile.setDescription(dsc);
        profile.setFile(file);
        profile.setLabel(label);
        return profile;
    }

    private File getProfileFile(ConfigurationProfile profile, String filePath) {
        if (filePath == null) {
            if (ConfigurationProfile.DEFAULT.equals(profile.getId())
                    || ConfigurationProfile.DEFAULT_ARCHIVE_IMPORT.equals(profile.getId())
                    || ConfigurationProfile.DEFAULT_SOUNDRECORDING_IMPORT.equals(profile.getId())) {
                // default profile
                return null;
            }
            filePath = profile.getId() + ".cfg";
        }
        try {
            URI profileUri = configHomeUri.resolve(filePath);
            return new File(profileUri);
        } catch (Exception e) {
            String err = String.format("Cannot resolve path '%s'!",filePath);
            LOG.log(Level.SEVERE, "Profile {0}. Cannot resolve path {1}!",
                    new Object[]{profile.getId(), filePath});
            profile.setError(err);
            return null;
        }
    }

    public List<ConfigurationProfile> getProfiles(String profileGroup) {
        String[] ids = getGroupIds(profileGroup);
        ArrayList<ConfigurationProfile> profiles = new ArrayList<ConfigurationProfile>(ids.length);
        for (String id : ids) {
            ConfigurationProfile profile = getProfile(id);
            if (profile != null) {
                profiles.add(profile);
            }
        }
        return profiles;
    }

    private String[] getGroupIds(String profileGroup) {
        String[] ids = config.getStringArray(profileGroup);
        return ids;
    }

}
