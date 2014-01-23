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

package cz.cas.lib.proarc.common.export.desa.structure;

import java.util.HashMap;

import cz.cas.lib.proarc.common.export.mets.MetsExportException;

/**
 * Interface for Desa Element Visitor - inserting into mets
 * 
 * @author eskymo
 * 
 */
public interface IDesaElementVisitor {
    /**
     * Inserts an Element into mets structure and saves it. The output is stored
     * in the directory specified in DesaContext (outputPath). The result is a
     * set of ZIP files - 1 main descriptor ([PACKAGE_ID]_FILE.ZIP]) and other
     * documents ([PACKAGE_ID]_XXXX.ZIP)
     * 
     * If exportToDesa parameter is set, ZIP files are sent to DESA. The result
     * of the export is stored in the directory specified in DesaContext
     * (desaResultPath)
     * 
     * @param desaElement
     * @throws MetsExportException
     */
    public void insertIntoMets(IDesaElement desaElement, HashMap<String, String> desaProps) throws MetsExportException;
}
