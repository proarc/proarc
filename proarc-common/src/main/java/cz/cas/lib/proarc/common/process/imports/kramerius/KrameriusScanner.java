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
package cz.cas.lib.proarc.common.process.imports.kramerius;

import cz.cas.lib.proarc.common.process.export.ExportUtils;
import cz.cas.lib.proarc.common.process.imports.ImportFileScanner;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * The helper to recognize the kramerius folder on a filesystem.
 *
 * @author Lukas Sykora
 */
public class KrameriusScanner {

    /**
     * Finds all files ending with {@code .xml}.
     * @param folder the folder to search
     * @return the list of files ending with .xml
     */
    public static List<File> findImportableFiles(File folder) {
        List<File> importableFiles = new ArrayList<>();
        for (File file : folder.listFiles()) {
            if (isAcceptableFile(file)) {
                importableFiles.add(file);
            }
        }
        return importableFiles;
    }

    /**
     * Contains at least one xml file?
     */
    public static boolean isImportable(File folder) {
        String[] fileNames = folder.list();
        for (String fileName : fileNames) {
            if (ImportFileScanner.IMPORT_STATE_FILENAME.equals(fileName)) {
                return false;
            }
            File file = new File(folder, fileName);
            if (file.isDirectory() || (file.isFile() && file.canRead() && !isAcceptableFiles(file))) {
                return false;
            }
        }
        return true;
    }

    private static boolean isAcceptableFiles(File fileName) {
        return isAcceptableFile(fileName) || fileName.getName().endsWith(ExportUtils.PROARC_EXPORT_STATUSLOG);
    }

    private static boolean isAcceptableFile(File fileName) {
        return fileName.getName().endsWith(".xml");
    }

}
