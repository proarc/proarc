/*
 * Copyright (C) 2012 Jan Pokorsky
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
package cz.cas.lib.proarc.common.export;

import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.bind.JAXB;

/**
 *
 * @author Jan Pokorsky
 */
final class ExportUtils {

    private static final Logger LOG = Logger.getLogger(ExportUtils.class.getName());

    /**
     * Creates new folder. If name already exists it finds similar free name.
     * @param parent target folder
     * @param name name of the new folder
     * @return the new folder
     */
    public static File createFolder(File parent, String name) {
        if (name == null || name.contains(":")) {
            throw new IllegalArgumentException(name);
        }
        File folder = new File(parent, name);
        for (int i = 1; !folder.mkdir(); i++) {
            folder = new File(parent, name + '_' + i);
        }
        return folder;
    }

    /**
     * Creates XML file instance for given PID.
     * <p>It does not use special characters as ':' to avoid platform particularities.
     *
     * @param output target folder
     * @param pid PID of digital object
     * @return file
     */
    public static File pidAsXmlFile(File output, String pid) {
        String uuid = FoxmlUtils.pidAsUuid(pid);
        File foxml = new File(output, uuid + ".xml");
        return foxml;
    }

    /**
     * Writes an export result in XML.
     */
    public static void writeExportResult(File targetFolder, ExportResultLog result) {
        if (result.getEnd() == null) {
            result.setEnd(new Date());
        }
        if (result.getExports().size() == 1) {
            result.setBegin(null);
            result.setEnd(null);
        }
        File resultFile = new File(targetFolder, "proarc_export_status.log");
        try {
            JAXB.marshal(result, resultFile);
        } catch (Exception e) {
            LOG.log(Level.SEVERE, targetFolder.toString(), e);
        }
    }

    public static String toString(Throwable ex) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        ex.printStackTrace(pw);
        pw.close();
        return sw.toString();
    }

}
