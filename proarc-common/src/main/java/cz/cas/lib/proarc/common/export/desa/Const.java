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

package cz.cas.lib.proarc.common.export.desa;

import cz.cas.lib.proarc.common.object.DerDesaPlugin;
import cz.cas.lib.proarc.common.object.DesDesaPlugin;
import java.util.HashMap;
import java.util.Map;

/**
 * 
 * Constants for DesaMetsExport
 * 
 * @author Robert Simonovsky
 * 
 */
public class Const {
    public final static String DERFOLDER = DerDesaPlugin.MODEL_FOLDER;
    public final static String DERDOCUMENT = DerDesaPlugin.MODEL_DOCUMENT;
    public final static String DERFILE = DerDesaPlugin.MODEL_FILE;
    public final static String DESFILE = DesDesaPlugin.MODEL_FILE;
    public final static String DESFOLDER = DesDesaPlugin.MODEL_FOLDER;
    public final static String DESOWNDOCUMENT = DesDesaPlugin.MODEL_INTERNAL_RECORD;
    public final static String DESSENTDOCUMENT = DesDesaPlugin.MODEL_EXTERNAL_RECORD;
    public final static String FEDORAPREFIX = "info:fedora/";
    public final static String FOLDER = "FOLDER";
    public final static String DOCUMENT = "DOCUMENT";
    public final static String FILE = "FILE";

    public final static String ORIGINAL = "Original";
    public final static String INPUT = "Input";
    public final static String DIGITIZED = "Digitized";
    public final static String PREVIEW = "Preview";
    public final static String MIGRATED = "Migrated";

    public final static Map<String, String> typeMap = new HashMap<String, String>();
    public final static Map<String, String> fileGrpMap = new HashMap<String, String>();

    static {
        typeMap.put(FEDORAPREFIX + DERFOLDER, FOLDER);
        typeMap.put(FEDORAPREFIX + DERDOCUMENT, DOCUMENT);
        typeMap.put(FEDORAPREFIX + DERFILE, FILE);
        typeMap.put(FEDORAPREFIX + DESFOLDER, FOLDER);
        typeMap.put(FEDORAPREFIX + DESOWNDOCUMENT, DOCUMENT);
        typeMap.put(FEDORAPREFIX + DESSENTDOCUMENT, DOCUMENT);
        typeMap.put(FEDORAPREFIX + DESFILE, FILE);

        fileGrpMap.put("ADM", Const.INPUT);
        fileGrpMap.put("PS", Const.DIGITIZED);
        fileGrpMap.put("MC", Const.ORIGINAL);
        fileGrpMap.put("UC", Const.PREVIEW);
        // TODO Mapping to migrated is missing
    }
}
