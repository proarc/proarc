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
package cz.cas.lib.proarc.common.export.archive;

import org.apache.commons.configuration.Configuration;

/**
 * Settings for Archive export
 *
 * @author Lukas Sykora
 */
public class ArchiveExportOptions {

    static final String PROP_ARCHIVE_PACKAGE_EXTENDED = "export.archive.extended";
    static final String PROP_ARCHIVAL_COPY_FOLDER_NAME = "export.archive.archivalCopy.folderName";
    static final String PROP_NO_TIFF_AVAILABLE_FILE_NAME = "export.archive.noTif.fileName";
    static final String PROP_NO_TIFF_AVAILABLE_PATH = "export.archive.noTif.path";
    static final String PROP_ADDITIONAL_INFO_FILE_NAME = "export.archive.addInfo.fileName";
    private boolean extendedPackage;
    private String archivalCopyFolderName;
    private String noTifAvailableFileName;
    private String noTifAvailablePath;
    private String additionalInfoFileName;

    public static ArchiveExportOptions getOptions(Configuration config) {
        ArchiveExportOptions options = new ArchiveExportOptions();

        boolean extendedPackage = config.getBoolean(PROP_ARCHIVE_PACKAGE_EXTENDED, false);
        options.setExtendedPackage(extendedPackage);


        String archivalCopyFolderName = config.getString(PROP_ARCHIVAL_COPY_FOLDER_NAME, "Original_Tif");
        if (archivalCopyFolderName != null && !archivalCopyFolderName.isEmpty()) {
            options.setArchivalCopyFolderName(archivalCopyFolderName);
        }

        String noTiffAvailableFileName = config.getString(PROP_NO_TIFF_AVAILABLE_FILE_NAME, "No_Tif_available.txt");
        if (noTiffAvailableFileName != null && !noTiffAvailableFileName.isEmpty()) {
            options.setNoTifAvailableFileName(noTiffAvailableFileName);
        }

        String additionalInfoFileName = config.getString(PROP_ADDITIONAL_INFO_FILE_NAME, "Addition_Info.txt");
        if (additionalInfoFileName != null && !additionalInfoFileName.isEmpty()) {
            options.setAdditionalInfoFileName(additionalInfoFileName);
        }

        String noTiffAvailablePath = config.getString(PROP_NO_TIFF_AVAILABLE_PATH, "${proarc.home}/noTifAvailable.tif");
        if (noTiffAvailablePath != null && !noTiffAvailablePath.isEmpty()) {
            options.setNoTifAvailablePath(noTiffAvailablePath);
        }

        return options;
    }

    public boolean isExtendedPackage() {
        return extendedPackage;
    }

    public void setExtendedPackage(boolean extendedPackage) {
        this.extendedPackage = extendedPackage;
    }

    public String getArchivalCopyFolderName() {
        return archivalCopyFolderName;
    }

    public void setArchivalCopyFolderName(String archivalCopyFolderName) {
        this.archivalCopyFolderName = archivalCopyFolderName;
    }

    public String getNoTifAvailableFileName() {
        return noTifAvailableFileName;
    }

    public void setNoTifAvailableFileName(String noTiffAvailableFileName) {
        this.noTifAvailableFileName = noTiffAvailableFileName;
    }

    public String getAdditionalInfoFileName() {
        return additionalInfoFileName;
    }

    public void setAdditionalInfoFileName(String additionalInfoFileName) {
        this.additionalInfoFileName = additionalInfoFileName;
    }

    public String getNoTifAvailablePath() {
        return noTifAvailablePath;
    }

    public void setNoTifAvailablePath(String noTiffAvailablePath) {
        this.noTifAvailablePath = noTiffAvailablePath;
    }
}
