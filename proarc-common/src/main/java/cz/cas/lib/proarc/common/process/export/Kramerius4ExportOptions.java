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
package cz.cas.lib.proarc.common.process.export;

import cz.cas.lib.proarc.common.object.K4Plugin;
import cz.cas.lib.proarc.common.object.collectionOfClippings.CollectionOfClippingsPlugin;
import cz.cas.lib.proarc.common.object.emods.BornDigitalModsPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkClippingPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.common.storage.BinaryEditor;
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
    static final String PROP_REPLACE_OWNER_ID_NEW_VALUED = "export.kramerius4.owner.newValue";


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

        String newOwnerId = config.getString(PROP_REPLACE_OWNER_ID_NEW_VALUED);
        if (newOwnerId != null && !newOwnerId.isEmpty()) {
            options.setNewOwnerId(newOwnerId);
        }
        return options;
    }

    private Set<String> excludeDatastreams = Collections.emptySet();
    private Map<String, String> dsIdMap = Collections.emptyMap();
    private String policy;
    private String newOwnerId;
    // config options; it should go to proarc.properties
    private Map<String, String> relationMap = new HashMap<String, String>() {
        {
            put(NdkPlugin.MODEL_PAGE, "hasPage");
            put(NdkPlugin.MODEL_NDK_PAGE, "hasPage");
            put(K4Plugin.MODEL_MONOGRAPHUNIT, "hasUnit");
            put(K4Plugin.MODEL_PERIODICALVOLUME, "hasVolume");
            put(K4Plugin.MODEL_PERIODICALITEM, "hasItem");
            put(NdkPlugin.MODEL_ARTICLE, "hasIntCompPart");
            put(NdkPlugin.MODEL_CHAPTER, "hasIntCompPart");
            put(NdkPlugin.MODEL_CARTOGRAPHIC, "hasUnit");
            put(NdkPlugin.MODEL_GRAPHIC, "hasUnit");
            put(NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, "hasIntCompPart");
            put(NdkPlugin.MODEL_MONOGRAPHVOLUME, "hasUnit");
            put(NdkPlugin.MODEL_MONOGRAPHUNIT, "hasUnit");
            put(NdkPlugin.MODEL_PERIODICALISSUE, "hasItem");
            put(NdkPlugin.MODEL_PERIODICALSUPPLEMENT, "hasIntCompPart");
            put(NdkPlugin.MODEL_PERIODICALVOLUME, "hasVolume");
            put(NdkPlugin.MODEL_PICTURE, "hasIntCompPart");
            put(NdkPlugin.MODEL_SHEETMUSIC, "hasUnit");
            put(BornDigitalModsPlugin.MODEL_ARTICLE, "hasIntCompPart");
            put(OldPrintPlugin.MODEL_PAGE, "hasPage");
            put(OldPrintPlugin.MODEL_SUPPLEMENT, "hasIntCompPart");
            put(OldPrintPlugin.MODEL_MONOGRAPHVOLUME, "hasUnit");
            put(OldPrintPlugin.MODEL_MONOGRAPHUNIT, "hasUnit");
            put(OldPrintPlugin.MODEL_CHAPTER, "hasIntCompPart");
            put(OldPrintPlugin.MODEL_GRAPHICS, "hasUnit");
            put(OldPrintPlugin.MODEL_CARTOGRAPHIC, "hasUnit");
            put(OldPrintPlugin.MODEL_SHEETMUSIC, "hasUnit");
            put(CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_VOLUME, "hasUnit");
            put(NdkEbornPlugin.MODEL_EARTICLE, "hasIntCompPart");
            put(NdkEbornPlugin.MODEL_ECHAPTER, "hasIntCompPart");
            put(NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME, "hasUnit");
            put(NdkEbornPlugin.MODEL_EMONOGRAPHUNIT, "hasUnit");
            put(NdkEbornPlugin.MODEL_EMONOGRAPHSUPPLEMENT, "hasIntCompPart");
            put(NdkEbornPlugin.MODEL_EPERIODICALISSUE, "hasItem");
            put(NdkEbornPlugin.MODEL_EPERIODICALVOLUME, "hasVolume");
            put(NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT, "hasIntCompPart");
            put(NdkAudioPlugin.MODEL_SONG, "hasSoundUnit");
            put(NdkAudioPlugin.MODEL_TRACK, "containsTrack");
            put(NdkClippingPlugin.MODEL_CLIPPING_DIRECTORY, "hasVolume");
            put(NdkClippingPlugin.MODEL_CLIPPING_UNIT, "hasUnit");
        }
    };

    // NDK to K4 model mapping
    private Map<String, String> modelMap = new HashMap<String, String>() {
        {
            put(NdkPlugin.MODEL_NDK_PAGE, K4Plugin.MODEL_PAGE);
            put(NdkPlugin.MODEL_ARTICLE, "model:article");
            put(NdkEbornPlugin.MODEL_EARTICLE, "model:article");
            put(NdkPlugin.MODEL_CARTOGRAPHIC, "model:map");
            put(NdkPlugin.MODEL_GRAPHIC, "model:graphic");
            put(NdkPlugin.MODEL_MONOGRAPHTITLE, K4Plugin.MODEL_MONOGRAPH);
            put(NdkEbornPlugin.MODEL_EMONOGRAPHTITLE, K4Plugin.MODEL_MONOGRAPH);
            put(NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, "model:supplement");
            put(NdkEbornPlugin.MODEL_EMONOGRAPHSUPPLEMENT, "model:supplement");
            put(NdkPlugin.MODEL_MONOGRAPHUNIT, K4Plugin.MODEL_MONOGRAPHUNIT);
            put(NdkEbornPlugin.MODEL_EMONOGRAPHUNIT, "model:monographunit");
            put(NdkPlugin.MODEL_MONOGRAPHVOLUME, K4Plugin.MODEL_MONOGRAPH);
            put(NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME, K4Plugin.MODEL_MONOGRAPH);
            put(NdkPlugin.MODEL_PERIODICAL, K4Plugin.MODEL_PERIODICAL);
            put(NdkEbornPlugin.MODEL_EPERIODICAL, K4Plugin.MODEL_PERIODICAL);
            put(NdkPlugin.MODEL_PERIODICALISSUE, K4Plugin.MODEL_PERIODICALITEM);
            put(NdkEbornPlugin.MODEL_EPERIODICALISSUE, K4Plugin.MODEL_PERIODICALITEM);
            put(NdkPlugin.MODEL_PERIODICALSUPPLEMENT, "model:supplement");
            put(NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT, "model:supplement");
            put(NdkPlugin.MODEL_PERIODICALVOLUME, K4Plugin.MODEL_PERIODICALVOLUME);
            put(NdkEbornPlugin.MODEL_EPERIODICALVOLUME, K4Plugin.MODEL_PERIODICALVOLUME);
            put(NdkPlugin.MODEL_PICTURE, "model:picture");
            put(NdkPlugin.MODEL_SHEETMUSIC, "model:sheetmusic");
            put(NdkPlugin.MODEL_CHAPTER, "model:internalpart");
            put(BornDigitalModsPlugin.MODEL_ARTICLE, "model:article");
            put(OldPrintPlugin.MODEL_MONOGRAPHVOLUME, K4Plugin.MODEL_MONOGRAPH);
            put(OldPrintPlugin.MODEL_SUPPLEMENT, "model:supplement");
            put(OldPrintPlugin.MODEL_PAGE, K4Plugin.MODEL_PAGE);
            put(OldPrintPlugin.MODEL_MONOGRAPHTITLE, K4Plugin.MODEL_MONOGRAPH);
            put(OldPrintPlugin.MODEL_MONOGRAPHUNIT, K4Plugin.MODEL_MONOGRAPHUNIT);
            put(OldPrintPlugin.MODEL_CHAPTER, "model:internalpart");
            put(OldPrintPlugin.MODEL_GRAPHICS, "model:graphic");
            put(OldPrintPlugin.MODEL_CARTOGRAPHIC, "model:map");
            put(OldPrintPlugin.MODEL_SHEETMUSIC, "model:sheetmusic");
            put(OldPrintPlugin.MODEL_CONVOLUTTE, "model:convolute");
            put(CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_TITLE, "model:convolute");
            put(NdkAudioPlugin.MODEL_MUSICDOCUMENT, "model:soundrecording");
            put(NdkAudioPlugin.MODEL_PHONOGRAPH, "model:soundrecording");
            put(NdkAudioPlugin.MODEL_SONG, "model:soundunit");
            put(NdkAudioPlugin.MODEL_TRACK, "model:track");
            put(NdkClippingPlugin.MODEL_CLIPPING_COLLECTION, "model:clippingcollection");
            put(NdkClippingPlugin.MODEL_CLIPPING_DIRECTORY,  "model:clippingdirectory");
            put(NdkClippingPlugin.MODEL_CLIPPING_UNIT, "model:clipping");
        }
    };

    // K4 to NDK model mapping
    private Map<String, String> reverseModelMap = new HashMap<String, String>() {
        {
            put(K4Plugin.MODEL_PAGE, NdkPlugin.MODEL_PAGE);
            put("model:article", NdkPlugin.MODEL_ARTICLE);
            put("model:map", NdkPlugin.MODEL_CARTOGRAPHIC);
            put("model:supplement", NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT);
            put(K4Plugin.MODEL_MONOGRAPH, NdkPlugin.MODEL_MONOGRAPHVOLUME);
            put(K4Plugin.MODEL_MONOGRAPHUNIT, NdkPlugin.MODEL_MONOGRAPHUNIT);
            put(K4Plugin.MODEL_PERIODICAL, NdkPlugin.MODEL_PERIODICAL);
            put(K4Plugin.MODEL_PERIODICALITEM, NdkPlugin.MODEL_PERIODICALISSUE);
            put(K4Plugin.MODEL_PERIODICALVOLUME, NdkPlugin.MODEL_PERIODICALVOLUME);
            put("model:picture", NdkPlugin.MODEL_PICTURE);
            put("model:sheetmusic", NdkPlugin.MODEL_SHEETMUSIC);
            put("model:internalpart", NdkPlugin.MODEL_CHAPTER);
            put("model:graphic", NdkPlugin.MODEL_GRAPHIC);
            put("model:convolute", OldPrintPlugin.MODEL_CONVOLUTTE);
            put("model:soundrecording", NdkAudioPlugin.MODEL_MUSICDOCUMENT);
            put("model:soundrecording", NdkAudioPlugin.MODEL_PHONOGRAPH);
            put("model:soundunit", NdkAudioPlugin.MODEL_SONG);
            put("model:track", NdkAudioPlugin.MODEL_TRACK);
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

    public String getNewOwnerId() {
        return newOwnerId;
    }

    public void setNewOwnerId(String newOwnerId) {
        this.newOwnerId = newOwnerId;
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

    public Map<String, String> getReverseModelMap() {
        return reverseModelMap;
    }

}
