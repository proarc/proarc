/*
 * Copyright (C) 2013 Robert Simonovsky
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

package cz.cas.lib.proarc.common.export.mets;


public class MetsExportTestElement {
    private final String resultFolder;
    private final String directory;
    private final int totalItems;
    private final int size;
    private final int numberOfFiles;
    private final String type;
    private final String initialDocument;

    public String getInitialDocument() {
        return initialDocument;
    }

    public String getResultFolder() {
        return resultFolder;
    }

    public String getDirectory() {
        return directory;
    }

    public int getTotalItems() {
        return totalItems;
    }

    public int getSize() {
        return size;
    }

    public int getNumberOfFiles() {
        return numberOfFiles;
    }

    public String getType() {
        return type;
    }

    public MetsExportTestElement(String resultFolder, String directory, int totalItems, int size, int numberOfFiles, String type, String initialDocument) {
        super();
        this.resultFolder = resultFolder;
        this.directory = directory;
        this.totalItems = totalItems;
        this.size = size;
        this.numberOfFiles = numberOfFiles;
        this.type = type;
        this.initialDocument = initialDocument;
    }
}
