/*
 * Copyright (C) 2011 Jan Pokorsky
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.server.imports;

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.text.Collator;
import java.text.ParseException;
import java.text.RuleBasedCollator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;

/**
 *
 * @author Jan Pokorsky
 */
public final class ImportFileScanner {
    
    public enum State {
        IMPORTED, NEW, EMPTY;
    }

    public static final String IMPORT_STATE_FILENAME = "proarc_import_status.log";

    private static final FileFilter FOLDER_FILTER = new FileFilter() {
        @Override
        public boolean accept(File f) {
            return f.isDirectory() && f.canRead() && f.canWrite() && !ImportProcess.TMP_DIR_NAME.equals(f.getName());
        }
    };

    /**
     * File name comparator. It delegates to extended Czech collator implementation.
     * @see <a href='http://www.docjar.com/html/api/sun/text/resources/CollationData_cs.java.html'>CollationData_cs.java</a>
     * @see java.text.CollationRules
     */
    private static final Comparator<File> FILE_COMPARATOR = new Comparator<File>() {
        
        private final Comparator<Object> czech;

        {
            RuleBasedCollator czechDefault = (RuleBasedCollator) Collator.getInstance(new Locale("cs"));
            try {
                czech = new RuleBasedCollator(
                        // Space before 0 results to "on", "on board", "online"
                        //   instead of "on", "online", "on board"
                        // '&' to reset definition does not work for space
                        "'\u0020' < 0"
                        + czechDefault.getRules());
            } catch (ParseException ex) {
                throw new IllegalStateException(ex);
            }
        }

        @Override
        public int compare(File o1, File o2) {
            return czech.compare(o1.getName(), o2.getName());
        }
    };

    /**
     * Finds subfolders in folder. No recursive search.
     *
     * @param folder folder to scan
     * @return list of direct subfolders
     */
    public List<Folder> findSubfolders(File folder) throws FileNotFoundException, IllegalArgumentException {
        validateImportFolder(folder);

        File[] listFiles = folder.listFiles(FOLDER_FILTER);
        Arrays.sort(listFiles, FILE_COMPARATOR);
        List<Folder> content = new ArrayList<Folder>(listFiles.length);
        for (File file : listFiles) {
            content.add(new Folder(file));
        }
        return content;
    }
    
    public List<File> findDigitalContent(File folder) throws IllegalArgumentException, FileNotFoundException {
        validateImportFolder(folder);

        File[] files = folder.listFiles();
        List<File> contents = new ArrayList<File>(files.length);
        for (File file : files) {
            if (!file.isDirectory() || file.canRead()) {
                contents.add(file);
            }
        }
        Collections.sort(contents, FILE_COMPARATOR);
        return contents;
    }


    static void validateImportFolder(File folder) throws FileNotFoundException, IllegalArgumentException {
        if (!folder.exists()) {
            throw new FileNotFoundException(folder.toString());
        }
        if (!folder.isDirectory()) {
            throw new IllegalArgumentException("FILE_IS_NOT_DIRECTORY");
        }
        if (!(folder.canRead() && folder.canWrite())) {
            throw new IllegalArgumentException("FILE_INSUFFICIENT_ACCESS_PERMISSIONS");
        }
    }

    static State folderImportState(File folder) {
        File stateFile = new File(folder, IMPORT_STATE_FILENAME);
        State state = stateFile.exists() ? State.IMPORTED : State.NEW;
        // check file content for more details
        if (state == State.NEW) {
            state = isImportable(folder) ? State.NEW : State.EMPTY;
        }
        return state;
    }

    static boolean isImported(File folder) {
        File stateFile = new File(folder, IMPORT_STATE_FILENAME);
        return stateFile.exists();
    }

    private static boolean isImportable(File folder) {
        String[] fileNames = folder.list();
        for (String fileName : fileNames) {
            if (ImportFileScanner.IMPORT_STATE_FILENAME.equals(fileName)) {
                continue;
            }
            File file = new File(folder, fileName);
            if (file.isFile() && file.canRead()) {
                if (ImportProcess.canImport(file)) {
                    return true;
                }
            }
        }
        return false;
    }

    static void rollback(File folder) {
        File stateFile = new File(folder, IMPORT_STATE_FILENAME);
        stateFile.delete();
    }

    public static final class Folder {
        private File handle;
        private State status;

        private Folder(File handle) {
            this.handle = handle;
        }

        public File getHandle() {
            return handle;
        }

        public State getStatus() {
            if (status == null) {
                status = folderImportState(handle);
            }
            return status;
        }
    }

}
