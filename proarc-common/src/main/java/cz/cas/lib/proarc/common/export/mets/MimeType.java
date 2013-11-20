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

import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * 
 * Mime type extension mapping
 * 
 * @author Robert Simonovsky
 * 
 */
public class MimeType {
    private static HashMap<String, String> mimeMap = new HashMap<String, String>();
    private static Logger LOG = Logger.getLogger(MimeType.class.getName());

    static {
        mimeMap.put("text/xml", "xml");
        mimeMap.put("image/jp2", "jp2");
        mimeMap.put("text/plain", "txt");
        mimeMap.put("image/tiff", "tif");
        mimeMap.put("image/jpeg", "jpg");
    }

    /**
     * 
     * Returns an extension for the mime type
     * 
     * @param mime
     * @return
     */
    public static String getExtension(String mime) throws MetsExportException {
        String result = mimeMap.get(mime);
        if (result == null) {
            LOG.log(Level.SEVERE, "Unknown mime:" + mime);
            throw new MetsExportException("Unknown mime:" + mime);
        }
        return result;
    }
}