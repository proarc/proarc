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
package cz.cas.lib.proarc.common.process.imports.ndk;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * The helper to recognize the ndk on a filesystem.
 *
 * @author Lukas Sykora
 */
public class NdkScanner {

    /**
     * Finds {@code mets.xml} files in a folder hierarchy.
     *
     * @param folder the folder to search
     * @return the list of METs
     */
    public static List<File> findMets(File folder) {
        List<File> metsFile = new ArrayList<>();
        File[] files = folder.listFiles(new FileFilter() {
            @Override
            public boolean accept(File file) {
                String fileName = file.getName().toLowerCase();
                if (fileName.startsWith("mets") && fileName.endsWith(".xml")) {
                    return true;
                } else {
                    return false;
                }
            }
        });
        if (files.length > 0) {
            metsFile.addAll(Arrays.asList(files));
            return metsFile;
        }
        for (File file : folder.listFiles()) {
            if (file.isDirectory()) {
                metsFile.addAll(findMets(file));
            }
        }

        return metsFile;
    }

    /**
     * Is the folder a ndk folder?
     */
    public static boolean isNdkFolder(File folder) {
        if (folder.isDirectory()) {
            File[] files = folder.listFiles(new FileFilter() {
                @Override
                public boolean accept(File file) {
                    String fileName = file.getName().toLowerCase();
                    if (fileName.startsWith("mets") && fileName.endsWith(".xml")) {
                        return true;
                    } else {
                        return false;
                    }
                }
            });
            return files.length > 0;
        }
        return false;
    }

    /**
     * Contains at least one ndk? It checks the passed folder and its direct subfolders .
     */
    public static boolean isImportable(File folder) {
        if (isNdkFolder(folder)) {
            return true;
        } else {
            File[] listFiles = folder.listFiles();
            for (File f : listFiles) {
                if (isNdkFolder(f)) {
                    return true;
                }
            }
        }
        return false;
    }

}
