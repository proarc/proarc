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
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Jan Pokorsky
 */
public class ImportFileScanner {
    
    public enum State {
        IMPORTED, NEW, IMPORT_RUNNING;
    }

    public static final String IMPORT_STATE_FILENAME = "das_import_status.log";

    private static final FileFilter FOLDER_FILTER = new FileFilter() {
        @Override
        public boolean accept(File f) {
            return f.isDirectory() && f.canRead() && f.canWrite();
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
        List<Folder> content = new ArrayList<Folder>(listFiles.length);
        for (File file : listFiles) {
            content.add(new Folder(file));
        }
        return content;
    }
    
    public List<File> findDigitalContent(File folder) throws IllegalArgumentException, FileNotFoundException {
        validateImportFolder(folder);

        File[] files = folder.listFiles();
        List<File> consumed = new ArrayList<File>(files.length);
        for (File file : files) {
            if (file.isDirectory()) {
                continue;
            }

//            if (consumeFile(file)) {
//                consumed.add(file);
//            } else {
//                // log unknown file
//            }
        }
        return consumed;
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
        return state;
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

    public interface FileConsumer extends FileFilter {

        public boolean consume(File f);
    }

}
