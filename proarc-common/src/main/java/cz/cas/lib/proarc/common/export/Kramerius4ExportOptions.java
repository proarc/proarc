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
package cz.cas.lib.proarc.common.export;

import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.object.K4Plugin;
import cz.cas.lib.proarc.common.object.emods.BornDigitalModsPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import org.apache.commons.configuration.Configuration;

/**
 * Settings for Kramerius4 export.
 *
 * @author Jan Pokorsky
 */
public final class Kramerius4ExportOptions {
    
    static final String PROP_EXCLUDE_DATASTREAM_ID = "export.kramerius4.excludeDatastreamId";
    static final String PROP_POLICY = "export.kramerius4.policy";
    static final String PROP_RENAME_PREFIX = "export.kramerius4.rename";

    public static Kramerius4ExportOptions from(Configuration config) {
        Kramerius4ExportOptions options = new Kramerius4ExportOptions();

        String[] excludeIds = config.getStringArray(PROP_EXCLUDE_DATASTREAM_ID);
        options.setExcludeDatastreams(new HashSet<String>(Arrays.asList(excludeIds)));

        Configuration renames = config.subset(PROP_RENAME_PREFIX);
        HashMap<String, String> dsIdMap = new HashMap<String, String>();
        // use RAW if FULL ds is not available
        dsIdMap.put(BinaryEditor.RAW_ID, "IMG_FULL");
        for (Iterator<String> it = renames.getKeys(); it.hasNext();) {
            String dsId = it.next();
            String newDsId = renames.getString(dsId);
            dsIdMap.put(dsId, newDsId);
        }
        options.setDsIdMap(dsIdMap);

        String policy = config.getString(PROP_POLICY);
        if (policy != null && !policy.isEmpty()) {
            options.setPolicy(policy);
        }
        return options;
    }

    private Set<String> excludeDatastreams = Collections.emptySet();
    private Map<String, String> dsIdMap = Collections.emptyMap();
    private String policy;
    // config options; it should go to proarc.properties
    private Map<String, String> relationMap = new HashMap<String, String>() {
        {
            put(NdkPlugin.MODEL_PAGE, "hasPage");
            put(K4Plugin.MODEL_MONOGRAPHUNIT, "hasUnit");
            put(K4Plugin.MODEL_PERIODICALVOLUME, "hasVolume");
            put(K4Plugin.MODEL_PERIODICALITEM, "hasItem");
            put(NdkPlugin.MODEL_ARTICLE, "hasIntCompPart");
            put(NdkPlugin.MODEL_CARTOGRAPHIC, "hasUnit");
            put(NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, "hasIntCompPart");
            put(NdkPlugin.MODEL_MONOGRAPHVOLUME, "hasUnit");
            put(NdkPlugin.MODEL_PERIODICALISSUE, "hasItem");
            put(NdkPlugin.MODEL_PERIODICALSUPPLEMENT, "hasIntCompPart");
            put(NdkPlugin.MODEL_PERIODICALVOLUME, "hasVolume");
            put(NdkPlugin.MODEL_PICTURE, "hasIntCompPart");
            put(NdkPlugin.MODEL_SHEETMUSIC, "hasUnit");
            put(BornDigitalModsPlugin.MODEL_ARTICLE, "hasIntCompPart");
            put(OldPrintPlugin.MODEL_PAGE, "hasPage");
            put(OldPrintPlugin.MODEL_SUPPLEMENT, "hasIntCompPart");
            put(OldPrintPlugin.MODEL_VOLUME, "hasUnit");
        }
    };

    // NDK to K4 model mapping
    private Map<String, String> modelMap = new HashMap<String, String>() {
        {
            put(NdkPlugin.MODEL_ARTICLE, "model:article");
            put(NdkPlugin.MODEL_CARTOGRAPHIC, "model:map");
            put(NdkPlugin.MODEL_MONOGRAPHTITLE, "model:monograph");
            put(NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, "model:supplement");
            // XXX should be model:monographunit in case the parent is monograph
            put(NdkPlugin.MODEL_MONOGRAPHVOLUME, "model:monograph");
            put(NdkPlugin.MODEL_PERIODICAL, "model:periodical");
            put(NdkPlugin.MODEL_PERIODICALISSUE, "model:periodicalitem");
            put(NdkPlugin.MODEL_PERIODICALSUPPLEMENT, "model:supplement");
            put(NdkPlugin.MODEL_PERIODICALVOLUME, "model:periodicalvolume");
            put(NdkPlugin.MODEL_PICTURE, "model:picture");
            put(NdkPlugin.MODEL_SHEETMUSIC, "model:sheetmusic");
            put(BornDigitalModsPlugin.MODEL_ARTICLE, "model:article");
            put(OldPrintPlugin.MODEL_VOLUME, "model:monograph");
            put(OldPrintPlugin.MODEL_SUPPLEMENT, "model:supplement");
            put(OldPrintPlugin.MODEL_PAGE, "model:page");
            put(OldPrintPlugin.MODEL_MONOGRAPHTITLE, "model:monograph");
        }
    };

    public Set<String> getExcludeDatastreams() {
        return excludeDatastreams;
    }

    public void setExcludeDatastreams(Set<String> excludeDatastreams) {
        this.excludeDatastreams = excludeDatastreams;
    }

    public Map<String, String> getDsIdMap() {
        return dsIdMap;
    }

    public void setDsIdMap(Map<String, String> dsIdMap) {
        this.dsIdMap = dsIdMap;
    }

    public String getPolicy() {
        return policy;
    }

    public void setPolicy(String policy) {
        this.policy = policy;
    }

    public Map<String, String> getRelationMap() {
        return relationMap;
    }

    public void setRelationMap(Map<String, String> relationMap) {
        this.relationMap = relationMap;
    }

    public Map<String, String> getModelMap() {
        return modelMap;
    }

}
