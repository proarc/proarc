/*
 * Copyright (C) 2021 Lukas Sykora
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
package cz.cas.lib.proarc.common.info;

import cz.cas.lib.proarc.common.config.AppConfiguration;

public class ApplicationInfo {

    String version;
    String timestamp;
    String revision;

    public void initValues(AppConfiguration config) {
        this.version = config.getNdkExportOptions().getVersion();
        this.timestamp = config.getNdkExportOptions().getTimestamp();
        this.revision = config.getNdkExportOptions().getRevision();
    }

    public String getVersion() {
        return version;
    }

    public String getTimestamp() {
        return timestamp;
    }

    public String getRevision() {
        return revision;
    }

    @Override
    public String toString() {
        return "Info{" + "version=" + version + ", revision=" + revision + ", timestamp=" + timestamp + '}';
    }
}
