/*
 * Copyright (C) 2016 Jan Pokorsky
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

import cz.cas.lib.proarc.common.imports.ImportProcess.ImportOptions;
import java.io.File;
import java.io.IOException;

/**
 * The import handler called by the {@link ImportProcess}. Subclasses should
 * implement custom processing of the imported contents.
 *
 * @author Jan Pokorsky
 */
public interface ImportHandler {

    /**
     * Estimates the number of items to import.
     * @param importConfig the settings
     * @return the number
     * @throws IOException failure
     */
    int estimateItemNumber(ImportOptions importConfig) throws IOException;

    /**
     * Does the folder contain stuff that can be imported?
     * @param folder the folder
     * @return yes or no
     */
    boolean isImportable(File folder);

    /**
     * The import implementation. It can just load items or load and ingest
     * items in one step.
     * @param importConfig the settings
     * @param batchManager
     * @throws Exception failure
     */
    void start(ImportOptions importConfig, ImportBatchManager batchManager) throws Exception;

}
