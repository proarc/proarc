/*
 * Copyright (C) 2022 Lukas Sykora
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
package cz.cas.lib.proarc.common.export;

import java.io.File;
import org.apache.commons.configuration.Configuration;

/**
 * Settings for KWIS export.
 *
 * @author Lukas Sykora
 */
public final class KwisExportOptions {

    static final String PROP_KWIS_EXPORT_PATH = "export.kwis.path";
    private String kwisPath;

    public static KwisExportOptions from(Configuration config) {
        KwisExportOptions options = new KwisExportOptions();

        String kwisExportPath = config.getString(PROP_KWIS_EXPORT_PATH);
        options.setKwisPath(kwisExportPath);

        return options;
    }

    public String getKwisPath() {
        if (kwisPath != null) {
            File file = new File(kwisPath);
            if (!file.exists()) {
                file.mkdir();
            }
            return file.getPath();
        }
        return null;
    }

    public void setKwisPath(String kwisPath) {
        this.kwisPath = kwisPath;
    }
}
