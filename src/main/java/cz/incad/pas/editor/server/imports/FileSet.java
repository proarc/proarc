/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.incad.pas.editor.server.imports;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * A collection of files to import as single digital object.
 * <p>{@code page1.tiff, page1.ocr.txt, ...}
 *
 * @author Jan Pokorsky
 */
public final class FileSet {

    private final String name;
    private List<FileEntry> files;

    public FileSet(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public List<FileEntry> getFiles() {
        if (files == null) {
            files = new ArrayList<FileEntry>();
        }
        return files;
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 31 * hash + (this.name != null ? this.name.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final FileSet other = (FileSet) obj;
        if ((this.name == null) ? (other.name != null) : !this.name.equals(other.name)) {
            return false;
        }
        return true;
    }

    public static final class FileEntry {

        private final File file;
        private String mimetype;

        public FileEntry(File file) {
            this.file = file;
        }

        public String getMimetype() {
            if (mimetype == null) {
                mimetype = ImportProcess.findMimeType(file);
            }
            return mimetype;
        }

        public void setMimetype(String mimetype) {
            this.mimetype = mimetype;
        }

        public File getFile() {
            return file;
        }

    }
}
