/*
 * Copyright (C) 2017 Lukas Sykora
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
package cz.cas.lib.proarc.common.export.mets;

import org.apache.commons.configuration.Configuration;

/**
 * Settings for NDK export
 *
 * @author lsykora
 */
public class NdkExportOptions {
    static final String PROP_NDK_AGENT_ARCHIVIST = "export.ndk.agent.archivist";
    static final String PROP_NDK_AGENT_CREATOR = "export.ndk.agent.creator";
    static final String PROP_PROARC_VERSION = "proarc.version";
    static final String PROP_PROARC_REVISION = "proarc.build.revision";
    static final String PROP_PROARC_TIMESTAMP = "proarc.build.timestamp";
    private String archivist;
    private String creator;
    private String version;
    private String timestamp;
    private String revision;

    public static NdkExportOptions getOptions(Configuration config) {
        NdkExportOptions options = new NdkExportOptions();

        String creator = config.getString(PROP_NDK_AGENT_CREATOR);
        if (creator != null && !creator.isEmpty()) {
            options.setCreator(creator);
        }

        String archivist = config.getString(PROP_NDK_AGENT_ARCHIVIST);
        if (archivist != null && !archivist.isEmpty()) {
            options.setArchivist(archivist);
        }

        String version = config.getString(PROP_PROARC_VERSION);
        if (version != null && !version.isEmpty()) {
            options.setVersion(version);
        }

        String timestamp = config.getString(PROP_PROARC_TIMESTAMP);
        if (timestamp != null && !timestamp.isEmpty()) {
            options.setTimestamp(timestamp);
        }

        String revision = config.getString(PROP_PROARC_REVISION);
        if (revision != null && !revision.isEmpty()) {
            options.setRevision(revision);
        }

        return options;
    }

    /** Returns the creator organization - used for mets header */
    public String getCreator() {
        return creator;
    }

    /** Sets the creator organization*/
    public void setCreator(String creator) {
        this.creator = creator;
    }

    /** Returns the archivist organization - used for mets header */
    public String getArchivist() {
        return archivist;
    }

    /** Sets the archivist organization */
    public void setArchivist(String archivist) {
        this.archivist = archivist;
    }

    /** Returns value of version*/
    public String getVersion() {
        return version;
    }

    /** Sets value of Proarc version */
    public void setVersion(String version) {
        this.version = version;
    }

    public String getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(String timestamp) {
        this.timestamp = timestamp;
    }

    public String getRevision() {
        return revision;
    }

    public void setRevision(String revision) {
        this.revision = revision;
    }
}
