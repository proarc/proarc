/*
 * Copyright (C) 2017 Jakub Kremlacek
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

package cz.cas.lib.proarc.common.imports;

import cz.cas.lib.proarc.common.imports.ImportBatchManager.BatchItemObject;

/**
 * Represents processor for main image at input.
 *
 * @author Jakub Kremlacek
 */
public interface ImageImporter {

    /**
     * checks whether current ImageImporter is capable of importing specified fileset
     *
     * @param fileSet FileSet containing imagefile to be imported
     * @return true if FileSet contains importable imagetype
     */
    boolean accept(FileSet fileSet);

    /**
     * processes specified fileset with options defined in ImportOptions
     *
     * @param fileSet FileSet to be consumed and processed
     * @param ctx setup options for importer
     * @return returns object with processed images from fileSet
     */
    BatchItemObject consume(FileSet fileSet, ImportProcess.ImportOptions ctx);
}
