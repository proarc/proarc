/*
 * Copyright (C) 2014 Robert Simonovsky
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

package cz.cas.lib.proarc.common.export.mets.structure;

import cz.cas.lib.proarc.common.export.mets.MetsExportException;

/**
 * Interface for Desa Element Visitor - inserting into mets
 *
 * @author eskymo
 *
 */
public interface IMetsElementVisitor {
    /**
     * Inserts an Element into mets structure and saves it. The output is stored
     * in the directory specified in MetsContext (outputPath).
     *
     * @param metsElement This represents the element of Mets export
     * @throws MetsExportException Contains errors from export with PID
     */
    void insertIntoMets(IMetsElement metsElement) throws MetsExportException;
}
