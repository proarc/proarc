/*
 * Copyright (C) 2018 Lukas Sykora
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
package cz.cas.lib.proarc.common.process.export.mets;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.process.export.mets.structure.IMetsElementVisitor;
import cz.cas.lib.proarc.common.process.export.mets.structure.SttElementVisitor;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import java.io.File;

/**
 * Exports digital object and transforms its data streams to NDK format.
 *
 * @author Lukas Sykora
 * @see <a href='http://ndk.cz/digitalizace/nove-standardy-digitalizace-od-roku-2011'>NDK</a>
 */
public class NdkSttExport extends NdkExport {

    public NdkSttExport(AppConfiguration appConfiguration, AkubraConfiguration akubraConfiguration) {
        super(appConfiguration, akubraConfiguration);
    }

    @Override
    protected IMetsElementVisitor createMetsVisitor() {
        return new SttElementVisitor();
    }


    /**
     * ALTO and TXT are unnecessary, if they are empty, they will be deleted
     */
    @Override
    protected void deleteUnnecessaryFolder(File folder) {
        File[] files = folder.listFiles();
        if (files != null) {
            for (File f : files) {
                if (f.isDirectory()) {
                    deleteUnnecessaryFolder(f);
                }
            }
        }
        if (isEmptyFolder(folder) && isName(folder)) {
            folder.delete();
        }
    }

    private boolean isName(File folder) {
        return "alto".equals(folder.getName()) || "txt".equals(folder.getName());
    }

    private boolean isEmptyFolder(File folder) {
        return folder.listFiles().length == 0;
    }
}
