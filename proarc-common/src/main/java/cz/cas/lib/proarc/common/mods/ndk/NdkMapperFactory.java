/*
 * Copyright (C) 2018 Martin Rumanek
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
package cz.cas.lib.proarc.common.mods.ndk;

import cz.cas.lib.proarc.common.mods.custom.ModsCutomEditorType;
import cz.cas.lib.proarc.common.mods.ndk.eborn.NdkEArticleMapper;
import cz.cas.lib.proarc.common.mods.ndk.eborn.NdkEChapterMapper;
import cz.cas.lib.proarc.common.mods.ndk.eborn.NdkEMonographTitleMapper;
import cz.cas.lib.proarc.common.mods.ndk.eborn.NdkEMonographUnitMapper;
import cz.cas.lib.proarc.common.mods.ndk.eborn.NdkEMonographVolumeMapper;
import cz.cas.lib.proarc.common.mods.ndk.eborn.NdkEMonographSupplementMapper;
import cz.cas.lib.proarc.common.mods.ndk.eborn.NdkEPeriodicalIssueMapper;
import cz.cas.lib.proarc.common.mods.ndk.eborn.NdkEPeriodicalMapper;
import cz.cas.lib.proarc.common.mods.ndk.eborn.NdkEPeriodicalVolumeMapper;
import cz.cas.lib.proarc.common.mods.ndk.eborn.NdkEPeriodicalSupplementMapper;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPageMapper;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkClippingPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * Creates default NDK mappers. Subclasses can implement own mappings.
 *
 * @author Martin Rumanek
 */
public class NdkMapperFactory {

    private static final Map<String, Supplier<NdkMapper>> mappers = new HashMap<>();

    static {
        mappers.put(NdkPlugin.MODEL_PAGE, NdkPageMapper::new);
        mappers.put(ModsCutomEditorType.EDITOR_PAGE, NdkPageMapper::new);
        mappers.put(NdkPlugin.MODEL_NDK_PAGE, NdkNewPageMapper::new);
        mappers.put(NdkPlugin.MODEL_PERIODICAL, NdkPeriodicalMapper::new);
        mappers.put(NdkPlugin.MODEL_PERIODICALVOLUME, NdkPeriodicalVolumeMapper::new);
        mappers.put(NdkPlugin.MODEL_PERIODICALISSUE, NdkPeriodicalIssueMapper::new);
        mappers.put(NdkPlugin.MODEL_PERIODICALSUPPLEMENT, NdkPeriodicalSupplementMapper::new);
        mappers.put(NdkPlugin.MODEL_ARTICLE, NdkArticleMapper::new);
        mappers.put(NdkPlugin.MODEL_PICTURE, NdkPictureMapper::new);
        mappers.put(NdkPlugin.MODEL_MONOGRAPHTITLE, NdkMonographTitleMapper::new);
        mappers.put(NdkPlugin.MODEL_MONOGRAPHUNIT, NdkMonographUnitMapper::new);
        mappers.put(NdkPlugin.MODEL_MONOGRAPHVOLUME, NdkMonographVolumeMapper::new);
        mappers.put(NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, NdkMonographSupplementMapper::new);
        mappers.put(NdkPlugin.MODEL_CHAPTER, NdkChapterMapper::new);
        mappers.put(NdkPlugin.MODEL_CARTOGRAPHIC, NdkCartographicMapper::new);
        mappers.put(NdkPlugin.MODEL_GRAPHIC, NdkGraphicMapper::new);
        mappers.put(NdkPlugin.MODEL_SHEETMUSIC, NdkSheetMusicMapper::new);
        mappers.put(NdkAudioPlugin.MODEL_MUSICDOCUMENT, NdkSoundCollectionMapper::new);
        mappers.put(NdkAudioPlugin.MODEL_PHONOGRAPH, NdkSoundPhonographMapper::new);
        mappers.put(NdkAudioPlugin.MODEL_SONG, NdkSoundRecordingMapper::new);
        mappers.put(NdkAudioPlugin.MODEL_TRACK, NdkSoundPartMapper::new);
        mappers.put(NdkAudioPlugin.MODEL_PAGE, NdkAudioPageMapper::new);
        mappers.put(NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME, NdkEMonographVolumeMapper::new);
        mappers.put(NdkEbornPlugin.MODEL_EMONOGRAPHTITLE, NdkEMonographTitleMapper::new);
        mappers.put(NdkEbornPlugin.MODEL_EMONOGRAPHUNIT, NdkEMonographUnitMapper::new);
        mappers.put(NdkEbornPlugin.MODEL_EMONOGRAPHSUPPLEMENT, NdkEMonographSupplementMapper::new);
        mappers.put(NdkEbornPlugin.MODEL_ECHAPTER, NdkEChapterMapper::new);
        mappers.put(NdkEbornPlugin.MODEL_EPERIODICAL, NdkEPeriodicalMapper::new);
        mappers.put(NdkEbornPlugin.MODEL_EPERIODICALISSUE, NdkEPeriodicalIssueMapper::new);
        mappers.put(NdkEbornPlugin.MODEL_EPERIODICALVOLUME, NdkEPeriodicalVolumeMapper::new);
        mappers.put(NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT, NdkEPeriodicalSupplementMapper::new);
        mappers.put(NdkEbornPlugin.MODEL_EARTICLE, NdkEArticleMapper::new);
        mappers.put(NdkClippingPlugin.MODEL_CLIPPING_COLLECTION, NdkClippingCollectionMapper::new);
        mappers.put(NdkClippingPlugin.MODEL_CLIPPING_DIRECTORY, NdkClippingDirectoryMapper::new);
        mappers.put(NdkClippingPlugin.MODEL_CLIPPING_UNIT, NdkClippingUnitMapper::new);
    }

    /**
     * Gets a NDK mapper for the given model ID.
     *
     * @param modelId model ID
     * @return the mapper
     */
    public NdkMapper get(String modelId) {
        Optional<Supplier<NdkMapper>> ndkMapper = Optional.ofNullable(mappers.get(modelId));
        return ndkMapper.map(s -> s.get()).orElseThrow(() -> new IllegalStateException("Unsupported model: " + modelId));
    }

}
