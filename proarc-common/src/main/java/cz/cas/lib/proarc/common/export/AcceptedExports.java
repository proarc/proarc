/*
 * Copyright (C) 2023 Lukas Sykora
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

import cz.cas.lib.proarc.common.object.K4Plugin;
import cz.cas.lib.proarc.common.object.chronicle.ChroniclePlugin;
import cz.cas.lib.proarc.common.object.collectionOfClippings.CollectionOfClippingsPlugin;
import cz.cas.lib.proarc.common.object.emods.BornDigitalModsPlugin;
import cz.cas.lib.proarc.common.object.graphic.GraphicPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


/**
 * List of accepted exports items according to models.
 *
 * @author Lukas Sykora
 */
public class AcceptedExports {

    private final String BAGIT_SUFFIX = "_bagit";
    private final String LTP_UPLOAD_SUFFIX = "_upload_cesnet";

    private List<String> ALL_MODELS = Arrays.asList(
            // pages//
            NdkPlugin.MODEL_PAGE, NdkPlugin.MODEL_NDK_PAGE, OldPrintPlugin.MODEL_PAGE, NdkAudioPlugin.MODEL_PAGE,
            // ndk //
            NdkPlugin.MODEL_PERIODICAL, NdkPlugin.MODEL_PERIODICALVOLUME, NdkPlugin.MODEL_PERIODICALISSUE, NdkPlugin.MODEL_PERIODICALSUPPLEMENT,
            NdkPlugin.MODEL_MONOGRAPHTITLE, NdkPlugin.MODEL_MONOGRAPHUNIT, NdkPlugin.MODEL_MONOGRAPHVOLUME, NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, NdkPlugin.MODEL_CARTOGRAPHIC,
            NdkPlugin.MODEL_SHEETMUSIC, NdkPlugin.MODEL_ARTICLE, NdkPlugin.MODEL_CHAPTER, NdkPlugin.MODEL_PICTURE,
            // ndkAudio //
            NdkAudioPlugin.MODEL_MUSICDOCUMENT, NdkAudioPlugin.MODEL_SONG, NdkAudioPlugin.MODEL_TRACK, NdkAudioPlugin.MODEL_PHONOGRAPH,
            // ndkEPlugion //
            NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME, NdkEbornPlugin.MODEL_EMONOGRAPHTITLE, NdkEbornPlugin.MODEL_ECHAPTER, NdkEbornPlugin.MODEL_EMONOGRAPHSUPPLEMENT,
            NdkEbornPlugin.MODEL_EPERIODICALISSUE, NdkEbornPlugin.MODEL_EPERIODICALVOLUME, NdkEbornPlugin.MODEL_EPERIODICAL, NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT,
            NdkEbornPlugin.MODEL_EARTICLE,
            // chronicle //
            ChroniclePlugin.MODEL_CHRONICLETITLE, ChroniclePlugin.MODEL_CHRONICLEVOLUME, ChroniclePlugin.MODEL_CHRONICLESUPPLEMENT,
            // oldPrint //
            OldPrintPlugin.MODEL_VOLUME, OldPrintPlugin.MODEL_SUPPLEMENT, OldPrintPlugin.MODEL_PAGE, OldPrintPlugin.MODEL_MONOGRAPHTITLE,
            OldPrintPlugin.MODEL_CHAPTER, OldPrintPlugin.MODEL_CONVOLUTTE, OldPrintPlugin.MODEL_GRAPHICS, OldPrintPlugin.MODEL_CARTOGRAPHIC,
            OldPrintPlugin.MODEL_SHEETMUSIC,
            // CollectionOfClippings //
            CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_VOLUME, CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_TITLE,
            // BornDigital //
            BornDigitalModsPlugin.MODEL_ARTICLE,
            //  Graphic //
            GraphicPlugin.MODEL_GRAPHIC,
            // K4 //
            K4Plugin.MODEL_MONOGRAPH, K4Plugin.MODEL_MONOGRAPHUNIT, K4Plugin.MODEL_PERIODICAL, K4Plugin.MODEL_PERIODICALITEM, K4Plugin.MODEL_PERIODICALVOLUME
    );


    private final String EXPORT_ARCHIVE = "archive";
    private final Set<String> EXPORT_ARCHIVE_MODELS = new HashSet<>(Arrays.asList(
            NdkPlugin.MODEL_PERIODICAL, NdkPlugin.MODEL_PERIODICALVOLUME, NdkPlugin.MODEL_PERIODICALISSUE, NdkPlugin.MODEL_PERIODICALSUPPLEMENT,
            NdkPlugin.MODEL_MONOGRAPHTITLE, NdkPlugin.MODEL_MONOGRAPHUNIT, NdkPlugin.MODEL_MONOGRAPHVOLUME, NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, NdkPlugin.MODEL_CARTOGRAPHIC,
            NdkPlugin.MODEL_SHEETMUSIC, NdkPlugin.MODEL_ARTICLE, NdkPlugin.MODEL_CHAPTER, NdkPlugin.MODEL_PICTURE));

    private final String EXPORT_ARCHIVE_STT = "archive_stt";
    private final Set<String> EXPORT_ARCHIVE_STT_MODELS = new HashSet<>(Arrays.asList(
            OldPrintPlugin.MODEL_MONOGRAPHTITLE, OldPrintPlugin.MODEL_VOLUME, OldPrintPlugin.MODEL_SUPPLEMENT, OldPrintPlugin.MODEL_CHAPTER,
            OldPrintPlugin.MODEL_SHEETMUSIC, OldPrintPlugin.MODEL_CARTOGRAPHIC, OldPrintPlugin.MODEL_GRAPHICS));

    private final String EXPORT_KRAMERIUS = "kramerius";
    private final Set<String> EXPORT_KRAMERIUS_MODELS = new HashSet<>(ALL_MODELS);

    private final String EXPORT_KWIS = "kwis";
    private final Set<String> EXPORT_KWIS_MODELS = new HashSet<>(ALL_MODELS);

    private final String EXPORT_NDK_PSP = "ndk_psp";
    private final Set<String> EXPORT_NDK_PSP_MODELS = new HashSet<>(Arrays.asList(
            NdkPlugin.MODEL_PERIODICAL, NdkPlugin.MODEL_PERIODICALVOLUME, NdkPlugin.MODEL_PERIODICALISSUE, NdkPlugin.MODEL_PERIODICALSUPPLEMENT,
            NdkPlugin.MODEL_MONOGRAPHTITLE, NdkPlugin.MODEL_MONOGRAPHUNIT, NdkPlugin.MODEL_MONOGRAPHVOLUME, NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, NdkPlugin.MODEL_CARTOGRAPHIC,
            NdkPlugin.MODEL_SHEETMUSIC, NdkPlugin.MODEL_ARTICLE, NdkPlugin.MODEL_CHAPTER, NdkPlugin.MODEL_PICTURE, NdkAudioPlugin.MODEL_MUSICDOCUMENT
    ));

    private final String EXPORT_NDK_OLDPRINT = "ndk_oldprint";
    private final Set<String> EXPORT_NDK_OLDPRINT_MODELS = new HashSet<>(Arrays.asList(
            OldPrintPlugin.MODEL_MONOGRAPHTITLE, OldPrintPlugin.MODEL_VOLUME, OldPrintPlugin.MODEL_SUPPLEMENT, OldPrintPlugin.MODEL_CHAPTER
    ));

    private final Set<String> LTP_UPLOAD_MODELS = new HashSet<>(Arrays.asList(
            NdkPlugin.MODEL_PERIODICALISSUE, NdkPlugin.MODEL_PERIODICALSUPPLEMENT, NdkPlugin.MODEL_MONOGRAPHUNIT, NdkPlugin.MODEL_MONOGRAPHVOLUME, NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT,
            NdkPlugin.MODEL_CARTOGRAPHIC, NdkPlugin.MODEL_SHEETMUSIC, NdkPlugin.MODEL_PICTURE, NdkAudioPlugin.MODEL_MUSICDOCUMENT,
            OldPrintPlugin.MODEL_VOLUME, OldPrintPlugin.MODEL_SUPPLEMENT
    ));

    private final String EXPORT_NDK_SIP = "ndk_sip";
    private final Set<String> EXPORT_NDK_SIP_MODELS = new HashSet<>(Arrays.asList(
            NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME, NdkEbornPlugin.MODEL_EMONOGRAPHTITLE, NdkEbornPlugin.MODEL_ECHAPTER, NdkEbornPlugin.MODEL_EMONOGRAPHSUPPLEMENT,
            NdkEbornPlugin.MODEL_EPERIODICALISSUE, NdkEbornPlugin.MODEL_EPERIODICALVOLUME, NdkEbornPlugin.MODEL_EPERIODICAL, NdkEbornPlugin.MODEL_EPERIODICALSUPPLEMENT,
            NdkEbornPlugin.MODEL_EARTICLE
    ));

    private final String EXPORT_NDK_CHRONICLE = "ndk_chronicle";
    private final Set<String> EXPORT_NDK_CHRONICLE_MODELS = new HashSet<>(Arrays.asList(
            ChroniclePlugin.MODEL_CHRONICLETITLE, ChroniclePlugin.MODEL_CHRONICLEVOLUME, ChroniclePlugin.MODEL_CHRONICLESUPPLEMENT));

    private final String EXPORT_DATASTREAM_FULL = "datastream_full";
    private final Set<String> EXPORT_DATASTREAM_FULL_MODELS = new HashSet<>(ALL_MODELS);

    private final String EXPORT_DATASTREAM_RAW = "datastream_raw";
    private final Set<String> EXPORT_DATASTREAM_RAW_MODELS = new HashSet<>(ALL_MODELS);

    private final String EXPORT_DATASTREAM_NDKUSER = "datastream_ndkUser";
    private final Set<String> EXPORT_DATASTREAM_NDKUSER_MODELS = new HashSet<>(ALL_MODELS);

    private final String EXPORT_CEJSH = "cejsh";
    private final Set<String> EXPORT_CEJSH_MODELS = new HashSet<>(Arrays.asList(
            NdkPlugin.MODEL_PERIODICAL, NdkPlugin.MODEL_PERIODICALVOLUME, NdkPlugin.MODEL_PERIODICALISSUE, NdkPlugin.MODEL_PERIODICALSUPPLEMENT,
            NdkEbornPlugin.MODEL_EPERIODICALISSUE, NdkEbornPlugin.MODEL_EPERIODICALVOLUME, NdkEbornPlugin.MODEL_EPERIODICAL,
            NdkEbornPlugin.MODEL_EARTICLE, BornDigitalModsPlugin.MODEL_ARTICLE));

    private final String EXPORT_CROSSREFF = "crossref";
    private final Set<String> EXPORT_CROSSREFF_MODELS = new HashSet<>(Arrays.asList(
            NdkPlugin.MODEL_PERIODICAL, NdkPlugin.MODEL_PERIODICALVOLUME, NdkPlugin.MODEL_PERIODICALISSUE, NdkPlugin.MODEL_PERIODICALSUPPLEMENT,
            NdkEbornPlugin.MODEL_EPERIODICALISSUE, NdkEbornPlugin.MODEL_EPERIODICALVOLUME, NdkEbornPlugin.MODEL_EPERIODICAL,
            NdkEbornPlugin.MODEL_EARTICLE, BornDigitalModsPlugin.MODEL_ARTICLE));

    private String modelId;

    public AcceptedExports(String modelId) {
        this.modelId = modelId;
    }

    public List<String> getList() {
        List<String> acceptedItems = new ArrayList<>();
        if (EXPORT_ARCHIVE_MODELS.contains(this.modelId)) {
            acceptedItems.add(EXPORT_ARCHIVE);
            acceptedItems.add(EXPORT_ARCHIVE + BAGIT_SUFFIX);
        }
        if (EXPORT_ARCHIVE_STT_MODELS.contains(this.modelId)) {
            acceptedItems.add(EXPORT_ARCHIVE_STT);
            acceptedItems.add(EXPORT_ARCHIVE_STT + BAGIT_SUFFIX);
        }
        if (EXPORT_KRAMERIUS_MODELS.contains(this.modelId)) {
            acceptedItems.add(EXPORT_KRAMERIUS);
            acceptedItems.add(EXPORT_KRAMERIUS + BAGIT_SUFFIX);
        }
        if (EXPORT_KWIS_MODELS.contains(this.modelId)) {
            acceptedItems.add(EXPORT_KWIS);
        }
        if (EXPORT_NDK_PSP_MODELS.contains(this.modelId)) {
            acceptedItems.add(EXPORT_NDK_PSP);
            acceptedItems.add(EXPORT_NDK_PSP + BAGIT_SUFFIX);
            if (LTP_UPLOAD_MODELS.contains(this.modelId)) {
                acceptedItems.add(EXPORT_NDK_PSP + LTP_UPLOAD_SUFFIX);
            }
        }
        if (EXPORT_NDK_OLDPRINT_MODELS.contains(this.modelId)) {
            acceptedItems.add(EXPORT_NDK_OLDPRINT);
            acceptedItems.add(EXPORT_NDK_OLDPRINT + BAGIT_SUFFIX);
            if (LTP_UPLOAD_MODELS.contains(this.modelId)) {
                acceptedItems.add(EXPORT_NDK_OLDPRINT + LTP_UPLOAD_SUFFIX);
            }
        }
        if (EXPORT_NDK_SIP_MODELS.contains(this.modelId)) {
            acceptedItems.add(EXPORT_NDK_SIP);
            acceptedItems.add(EXPORT_NDK_SIP + BAGIT_SUFFIX);
        }
        if (EXPORT_NDK_CHRONICLE_MODELS.contains(this.modelId)) {
            acceptedItems.add(EXPORT_NDK_CHRONICLE);
        }
        if (EXPORT_DATASTREAM_FULL_MODELS.contains(this.modelId)) {
            acceptedItems.add(EXPORT_DATASTREAM_FULL);
        }
        if (EXPORT_DATASTREAM_RAW_MODELS.contains(this.modelId)) {
            acceptedItems.add(EXPORT_DATASTREAM_RAW);
        }
        if (EXPORT_DATASTREAM_NDKUSER_MODELS.contains(this.modelId)) {
            acceptedItems.add(EXPORT_DATASTREAM_NDKUSER);
        }
        if (EXPORT_CEJSH_MODELS.contains(this.modelId)) {
            acceptedItems.add(EXPORT_CEJSH);
        }
        if (EXPORT_CROSSREFF_MODELS.contains(this.modelId)) {
            acceptedItems.add(EXPORT_CROSSREFF);
        }
        return acceptedItems;
    }
}
