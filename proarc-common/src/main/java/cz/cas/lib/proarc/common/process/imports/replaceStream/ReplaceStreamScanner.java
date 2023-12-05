/*
 * Copyright (C) 2023 Lukas Sykora
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
package cz.cas.lib.proarc.common.process.imports.replaceStream;


import cz.cas.lib.proarc.common.process.imports.ImportProfile;
import cz.cas.lib.proarc.common.process.imports.ImportFileScanner;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static cz.cas.lib.proarc.common.process.export.ExportUtils.PROARC_EXPORT_STATUSLOG;

/**
 * The helper to recognize the replace stream on a filesystem.
 *
 * @author Lukas Sykora
 */
public class ReplaceStreamScanner {

    public static List<File> findFile(File folder) {
        List<File> files = new ArrayList<>();
        for (File file : folder.listFiles()) {
            if (file.isFile()) {
                if (!(ImportFileScanner.IMPORT_STATE_FILENAME.equals(file.getName()) || PROARC_EXPORT_STATUSLOG.equals(file.getName()))) {
                    files.add(file);
                }
            }
        }
        return files;
    }

    public static boolean isReplaceStreamFolder(File folder) {
        int fileCounter = 0;
        if (folder.isDirectory()) {
            for (File file : folder.listFiles()) {
                if (file.isDirectory()) {
                    return false;
                }
//                if (!(IMPORT_STATE_FILENAME.equals(file.getName()) || PROARC_EXPORT_STATUSLOG.equals(file.getName()))) {
//                    fileCounter++;
//                }
                if (checkIfFileHasExtension(file.getName(), ImportProfile.FILE_EXTENSIONS)) {
                    fileCounter++;
                }
            }
        }
        return fileCounter > 0;
    }

    public static boolean isImportable(File folder) {
        return isReplaceStreamFolder(folder);
    }

    protected static boolean checkIfFileHasExtension(String filename, String... extension) {
        return checkIfFileHasExtension(filename, Arrays.stream(extension).toArray());
    }
    protected static boolean checkIfFileHasExtension(String filename, Object[] extensions) {
        return Arrays.stream(extensions).anyMatch(entry -> filename.endsWith((String) entry));
    }
}
