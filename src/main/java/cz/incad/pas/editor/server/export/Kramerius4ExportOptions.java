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
package cz.incad.pas.editor.server.export;

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
    static final String PROP_RENAME_PREFIX = "export.kramerius4.rename";

    public static Kramerius4ExportOptions from(Configuration config) {
        Kramerius4ExportOptions options = new Kramerius4ExportOptions();

        String[] excludeIds = config.getStringArray(PROP_EXCLUDE_DATASTREAM_ID);
        options.setExcludeDatastreams(new HashSet<String>(Arrays.asList(excludeIds)));

        Configuration renames = config.subset(PROP_RENAME_PREFIX);
        HashMap<String, String> dsIdMap = new HashMap<String, String>();
        for (Iterator<String> it = renames.getKeys(); it.hasNext();) {
            String dsId = it.next();
            String newDsId = renames.getString(dsId);
            dsIdMap.put(dsId, newDsId);
        }
        options.setDsIdMap(dsIdMap);
        return options;
    }

    private Set<String> excludeDatastreams = Collections.emptySet();
    private Map<String, String> dsIdMap = Collections.emptyMap();
    // config options; it should go to proarc.properties
    private Map<String, String> relationMap = new HashMap<String, String>() {
        {
            put("model:page", "hasPage");
            put("model:monographunit", "hasUnit");
            put("model:periodicalvolume", "hasVolume");
            put("model:periodicalitem", "hasItem");
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

    public Map<String, String> getRelationMap() {
        return relationMap;
    }

    public void setRelationMap(Map<String, String> relationMap) {
        this.relationMap = relationMap;
    }

}
