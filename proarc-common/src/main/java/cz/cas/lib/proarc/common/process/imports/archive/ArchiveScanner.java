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
package cz.cas.lib.proarc.common.process.imports.archive;

import cz.cas.lib.proarc.common.process.export.archive.PackageBuilder;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * The helper to recognize the proarc archive on a filesystem.
 *
 * @author Jan Pokorsky
 */
public class ArchiveScanner {

    /**
     * Finds {@code mets.xml} files in a folder hierarchy.
     * @param folder the folder to search
     * @return the list of METs
     */
    public static List<File> findMets(File folder) {
        List<File> metsFiles = new ArrayList<File>();
        File mets = new File(folder, PackageBuilder.METS_FILENAME);
        if (mets.exists()) {
            metsFiles.add(mets);
            return metsFiles;
        }
        for (File f : folder.listFiles()) {
            if (f.isDirectory()) {
                metsFiles.addAll(findMets(f));
            }
        }
        return metsFiles;
    }

    /**
     * Is the folder an archive folder?
     */
    public static boolean isArchiveFolder(File folder) {
        if (folder.isDirectory()) {
            File mets = new File(folder, PackageBuilder.METS_FILENAME);
            if (mets.exists()) {
                return true;
            }
        }
        return false;
    }

    /**
     * Contains at least one archive? It checks the passed folder and its direct subfolders .
     */
    public static boolean isImportable(File folder) {
        if (isArchiveFolder(folder)) {
            return true;
        } else {
            File[] listFiles = folder.listFiles();
            for (File f : listFiles) {
                if (isArchiveFolder(f)) {
                    return true;
                }
            }
        }
        return false;
    }

}
