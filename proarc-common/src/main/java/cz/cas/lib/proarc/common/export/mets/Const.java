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

package cz.cas.lib.proarc.common.export.mets;

import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.StringEditor;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 *
 * Constants for MetsMetsExport
 *
 * @author Robert Simonovsky
 *
 */
public class Const {
    public final static String FEDORAPREFIX = "info:fedora/";
    public static HashMap<String, List<String>> streamMapping;
    public static HashMap<String, List<String>> audioStremMapping;
    public static List<String> allowedNdkIdentifiers = new ArrayList<String>();
    public static List<String> allowedNdkEbornIdentifiers = new ArrayList<String>();
    public static ArrayList<String> mandatoryStreams = new ArrayList<String>();
    public static ArrayList<String> audioMandatoryStreams = new ArrayList<String>();
    public static ArrayList<String> canContainPage = new ArrayList<String>();
    public static HashMap<String, String> streamMappingFile = new HashMap<String, String>();
    public static HashMap<String, String> streamMappingPrefix = new HashMap<String, String>();
    public static final List<String> sttMandatoryStreams = new ArrayList<>();
    public static final String FEDORA_CREATEDATE = "info:fedora/fedora-system:def/model#createdDate";
    public static final String FEDORA_LASTMODIFIED = "info:fedora/fedora-system:def/view#lastModifiedDate";
    public static final String FEDORA_LABEL = "info:fedora/fedora-system:def/model#label";
    public static final String DIV_PHYSICAL_ID = "PHYSICAL";
    public static final String DIV_PHYSICAL_LABEL = "Physical_structure";
    public static final String DIV_LOGICAL_ID = "LOGICAL";
    public static final String DIV_LOGICAL_LABEL = "Logical_structure";

    public final static String PERIODICAL_ITEM_MODEL = NdkPlugin.MODEL_PERIODICALISSUE;
    public final static String PERIODICAL_MODEL = NdkPlugin.MODEL_PERIODICAL;
    public final static String PERIODICAL_VOLUME_MODEL = NdkPlugin.MODEL_PERIODICALVOLUME;
    public final static String PAGE_MODEL = NdkPlugin.MODEL_PAGE;
    public final static String PAGE_NDK_MODEL = NdkPlugin.MODEL_NDK_PAGE;
    public final static String MONOGRAPH_TITLE_MODEL = NdkPlugin.MODEL_MONOGRAPHTITLE;
    public final static String MONOGRAPH_MODEL = NdkPlugin.MODEL_MONOGRAPHVOLUME;
    public final static String PICTURE_MODEL = NdkPlugin.MODEL_PICTURE;
    public final static String ARTICLE_MODEL = NdkPlugin.MODEL_ARTICLE;
    public final static String CHAPTER_MODEL = NdkPlugin.MODEL_CHAPTER;
    public final static String SUPPLEMENT_MODEL = "model:supplement";
    public final static String SOUND_COLLECTION_MODEL = NdkAudioPlugin.MODEL_MUSICDOCUMENT;
    public final static String SOUND_FONOGRAPH_MODEL = NdkAudioPlugin.MODEL_PHONOGRAPH;
    public final static String SOUND_RECORDING_MODEL = NdkAudioPlugin.MODEL_SONG;
    public final static String SOUND_PART_MODEL = NdkAudioPlugin.MODEL_TRACK;
    public final static String SOUND_PAGE_MODEL = NdkAudioPlugin.MODEL_PAGE;

    public final static String NDK_EXPORT_STRUCTMAP_SOUNDCOLLECTION = "sound collection";
    public final static String NDK_EXPORT_STRUCTMAP_SOUNDRECORDING = "sound";
    public final static String NDK_EXPORT_STRUCTMAP_SOUNDPART = "soundpart";

    public final static String GENRE_ARTICLE = "article";
    public final static String GENRE_CARTOGRAPHIC= "cartographic";
    public final static String GENRE_CHAPTER = "chapter";
    public final static String GENRE_ISSUE = "issue";
    public final static String GENRE_PICTURE = "picture";
    public final static String GENRE_SUPPLEMENT = "supplement";
    public final static String GENRE_TITLE = "title";
    public final static String GENRE_VOLUME = "volume";

    public static final String NDK_EBORN_MODELS_IDENTIFIER = "ndke";
    public final static String GENRE_EARTICLE = "electronic article";
    public final static String GENRE_ECHAPTER = "electronic chapter";
    public final static String GENRE_EISSUE = "electronic issue";
    public final static String GENRE_ETITLE = "electronic title";
    public final static String GENRE_EVOLUME = "electronic volume";

    public static final String VOLUME = "VOLUME";
    public static final String TITLE = "TITLE";

    public final static String DC_URI = "http://www.openarchives.org/OAI/2.0/oai_dc/";
    public final static String NSESSS_URI = "http://www.mvcr.cz/nsesss/v2";
    public final static String URNNBN = "urnnbn";
    public final static String UUID = "uuid";
    public final static String CCNB = "ccnb";
    public final static String ISSN = "issn";
    public final static String ISBN = "isbn";

    public final static String ISSUE = "ISSUE";
    public final static String PERIODICAL_VOLUME = "PERIODICAL_VOLUME";
    public final static String PERIODICAL_TITLE = "PERIODICAL_TITLE";
    public final static String PAGE = "PAGE";
    public final static String MONOGRAPH = "MONOGRAPH";
    public final static String MONOGRAPH_MULTIPART = "MONOGRAPH_MULTIPART";
    public final static String MONOGRAPH_UNIT = "MONOGRAPH_UNIT";
    public final static String PICTURE = "PICTURE";
    public final static String ARTICLE = "ARTICLE";
    public final static String SUPPLEMENT = "SUPPLEMENT";
    public final static String CHAPTER = "CHAPTER";
    public final static String SOUND_COLLECTION = "SOUNDCOLLECTION";
    public final static String SOUND_RECORDING = "SOUNDRECORDING";
    public final static String SOUND_PART = "SOUNDPART";
    public final static String SOUND_PAGE = "SOUNDPAGE";

    public final static String MODS_ARTICLE = "ART";
    public final static String MODS_PICTURE = "PICT";
    public final static String MODS_SUPPLEMENT = "SUPPL";
    public final static String MODS_CHAPTER = "CHAP";

    public static final String HASPAGE = "kramerius:hasPage";
    public static final String HASMODEL = "fedora-model:hasModel";
    public static final String HASVOLUME = "kramerius:hasVolume";
    public static final String HASISSUE = "kramerius:hasItem";
    public static final String HASUNIT = "kramerius:hasUnit";
    public static final String HASINTCOMPPART = "kramerius:hasIntCompPart";
    public static final String HASMEMBER = "fedora-rels-ext:hasMember";
    public static final String ISONPAGE = "kramerius:isOnPage";

    public static final String RAW_GRP_ID = "RAW";
    public static final String MC_GRP_ID = "MC_IMGGRP";
    public static final String ALTO_GRP_ID = "ALTOGRP";
    public static final String UC_GRP_ID = "UC_IMGGRP";
    public static final String TXT_GRP_ID = "TXTGRP";
    public static final String AUDIO_RAW_GRP_ID = "SA_AUDIOGRP";
    public static final String AUDIO_MC_GRP_ID = "MC_AUDIOGRP";
    public static final String AUDIO_MC_GRP_ID_FLAC = "MC_AUDIOGRP_FLAC";
    public static final String AUDIO_UC_GRP_ID = "UC_AUDIOGRP";
    public static final String AUDIO_UC_GRP_ID_OGG = "UC_AUDIOGRP_OGG";
    public static final String OC_GRP_ID = "OC_EBGRP";
    public static final String OC_GRP_ID_CREATION = "OC_EBGRP_CREATION";
    public static final String OC_GRP_ID_VALIDATION = "OC_EBGRP_VALIDATION";
    //public
    public static final String TECHMDGRP = "TECHMDGRP";

    public static final String MIX001 = "MIX_001";
    public static final String MIX002 = "MIX_002";
    public static final String AES001 = "AES_001";
    public static final String AES002 = "AES_002";
    public static final String CODINGHISTORY001 = "CODINGHISTORY_001";
    public static final String CODINGHISTORY002 = "CODINGHISTORY_002";

    public static final List<String> PSPElements = new ArrayList<>();

    public final static Map<String, String> typeMap = new HashMap<>();
    public final static Map<String, String> typeNameMap = new HashMap<>();
    public final static Map<String, String> mimeToFmtMap = new HashMap<>();
    public final static Map<String, String> mimeToExtensionMap = new HashMap<>();
    // Tech MD creation constants
    public final static Map<String, String> dataStreamToModel = new HashMap<>();
    public final static Map<String, String> dataStreamToEvent = new HashMap<>();

    private Const() {
    }


    static {
        allowedNdkIdentifiers.add("isbn");
        allowedNdkIdentifiers.add("issn");
        allowedNdkIdentifiers.add("ccnb");
        allowedNdkIdentifiers.add("urnnbn");
        allowedNdkEbornIdentifiers.addAll(allowedNdkIdentifiers);
        allowedNdkEbornIdentifiers.add("uuid");
        mimeToFmtMap.put("image/tiff", "fmt/353");
        mimeToFmtMap.put("image/jp2", "x-fmt/392");
        mimeToFmtMap.put("text/xml", "fmt/101");
        mimeToFmtMap.put("text/plain", "fmt/611");
        mimeToFmtMap.put("application/pdf", "fmt/18");
        mimeToFmtMap.put("image/jpeg", "fmt/44");
        mimeToFmtMap.put("audio/wav", "fmt/141");
        mimeToFmtMap.put("audio/mpeg", "fmt/134");

        mimeToExtensionMap.put("image/tiff", ".tif");
        mimeToExtensionMap.put("image/jp2", ".jp2");
        mimeToExtensionMap.put("text/xml", ".xml");
        mimeToExtensionMap.put("text/plain", ".txt");
        mimeToExtensionMap.put("application/pdf", ".pdf");
        mimeToExtensionMap.put("image/jpeg", ".jpeq");
        mimeToExtensionMap.put("audio/wav", ".wav");
        mimeToExtensionMap.put("audio/flac", ".flac");
        mimeToExtensionMap.put("audio/mpeg", ".mp3");
        mimeToExtensionMap.put("audio/ogg", ".ogg");

        PSPElements.add(Const.MONOGRAPH_UNIT);
        PSPElements.add(Const.ISSUE);
        PSPElements.add(Const.SUPPLEMENT);
        PSPElements.add(Const.SOUND_RECORDING);


        typeMap.put(FEDORAPREFIX + PERIODICAL_ITEM_MODEL, ISSUE);
        typeMap.put(FEDORAPREFIX + PERIODICAL_MODEL, PERIODICAL_TITLE);
        typeMap.put(FEDORAPREFIX + PERIODICAL_VOLUME_MODEL, PERIODICAL_VOLUME);
        typeMap.put(FEDORAPREFIX + PAGE_MODEL, PAGE);
        typeMap.put(FEDORAPREFIX + PAGE_NDK_MODEL, PAGE);
        typeMap.put(FEDORAPREFIX + MONOGRAPH_MODEL, MONOGRAPH_UNIT);
        typeMap.put(FEDORAPREFIX + MONOGRAPH_TITLE_MODEL, MONOGRAPH_MULTIPART);
        typeMap.put(FEDORAPREFIX + PICTURE_MODEL, PICTURE);
        typeMap.put(FEDORAPREFIX + ARTICLE_MODEL, ARTICLE);
        typeMap.put(FEDORAPREFIX + SUPPLEMENT_MODEL, SUPPLEMENT);
        typeMap.put(FEDORAPREFIX + NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, SUPPLEMENT);
        typeMap.put(FEDORAPREFIX + NdkPlugin.MODEL_PERIODICALSUPPLEMENT, SUPPLEMENT);
        typeMap.put(FEDORAPREFIX + NdkPlugin.MODEL_CARTOGRAPHIC, MONOGRAPH_UNIT);
        typeMap.put(FEDORAPREFIX + NdkPlugin.MODEL_SHEETMUSIC, MONOGRAPH_UNIT);
        typeMap.put(FEDORAPREFIX + SOUND_COLLECTION_MODEL, SOUND_COLLECTION);
        typeMap.put(FEDORAPREFIX + SOUND_FONOGRAPH_MODEL, SOUND_COLLECTION);
        typeMap.put(FEDORAPREFIX + SOUND_RECORDING_MODEL, SOUND_RECORDING);
        typeMap.put(FEDORAPREFIX + SOUND_PART_MODEL, SOUND_PART);
        typeMap.put(FEDORAPREFIX + SOUND_PAGE_MODEL, SOUND_PAGE);


        typeMap.put(FEDORAPREFIX + CHAPTER_MODEL, CHAPTER);

        typeNameMap.put(ISSUE, ISSUE);
        typeNameMap.put(PERIODICAL_VOLUME, VOLUME);
        typeNameMap.put(PERIODICAL_TITLE, TITLE);
        typeNameMap.put(PAGE, PAGE);
        typeNameMap.put(MONOGRAPH_UNIT, VOLUME);
        typeNameMap.put(PICTURE, MODS_PICTURE);
        typeNameMap.put(ARTICLE, MODS_ARTICLE);
        typeNameMap.put(SUPPLEMENT, MODS_SUPPLEMENT);
        typeNameMap.put(CHAPTER, MODS_CHAPTER);
        typeNameMap.put(MONOGRAPH_MULTIPART, TITLE);
        typeNameMap.put(SOUND_COLLECTION, SOUND_COLLECTION);
        typeNameMap.put(SOUND_RECORDING, SOUND_RECORDING);
        typeNameMap.put(SOUND_PART, SOUND_PART);
        typeNameMap.put(SOUND_PAGE, SOUND_PAGE);


        sttMandatoryStreams.add(MC_GRP_ID);
        sttMandatoryStreams.add(UC_GRP_ID);

        mandatoryStreams.addAll(sttMandatoryStreams);
        mandatoryStreams.add(ALTO_GRP_ID);
        mandatoryStreams.add(TXT_GRP_ID);

        audioMandatoryStreams.add(AUDIO_RAW_GRP_ID);
        audioMandatoryStreams.add(AUDIO_UC_GRP_ID);

        audioStremMapping = new HashMap<String, List<String>>();
        audioStremMapping.put(AUDIO_RAW_GRP_ID, new ArrayList<String>());
        audioStremMapping.get(AUDIO_RAW_GRP_ID).add(BinaryEditor.RAW_AUDIO_ID);

        audioStremMapping.put(AUDIO_MC_GRP_ID, new ArrayList<String>());
        audioStremMapping.get(AUDIO_MC_GRP_ID).add(BinaryEditor.NDK_AUDIO_ARCHIVAL_ID);
        audioStremMapping.put(AUDIO_MC_GRP_ID_FLAC, new ArrayList<String>());
        audioStremMapping.get(AUDIO_MC_GRP_ID_FLAC).add(BinaryEditor.NDK_AUDIO_ARCHIVAL_FLAC_ID);

        audioStremMapping.put(AUDIO_UC_GRP_ID, new ArrayList<String>());
        audioStremMapping.get(AUDIO_UC_GRP_ID).add(BinaryEditor.NDK_AUDIO_USER_ID);
        audioStremMapping.put(AUDIO_UC_GRP_ID_OGG, new ArrayList<String>());
        audioStremMapping.get(AUDIO_UC_GRP_ID_OGG).add(BinaryEditor.NDK_AUDIO_USER_OGG_ID);

        streamMapping = new HashMap<String, List<String>>();
        streamMapping.put(MC_GRP_ID, new ArrayList<String>());
        streamMapping.get(MC_GRP_ID).add(BinaryEditor.NDK_ARCHIVAL_ID);
        // streamMapping.get("MC_IMGGRP").add("RAW");

        streamMapping.put(UC_GRP_ID, new ArrayList<>());
        streamMapping.get(UC_GRP_ID).add(BinaryEditor.NDK_USER_ID);
        // streamMapping.get("UC_IMGGRP").add("FULL");

        streamMapping.put(ALTO_GRP_ID, new ArrayList<>());
        streamMapping.get(ALTO_GRP_ID).add(AltoDatastream.ALTO_ID);

        streamMapping.put(TXT_GRP_ID, new ArrayList<>());
        streamMapping.get(TXT_GRP_ID).add(StringEditor.OCR_ID);

        streamMapping.put(TECHMDGRP, new ArrayList<>());
        streamMapping.get(TECHMDGRP).add("FULL_AMD");

        streamMappingPrefix.put(MC_GRP_ID, "mc");
        streamMappingPrefix.put(UC_GRP_ID, "uc");
        streamMappingPrefix.put(ALTO_GRP_ID, "alto");
        streamMappingPrefix.put(TXT_GRP_ID, "txt");
        streamMappingPrefix.put(TECHMDGRP, "amd_mets");
        streamMappingPrefix.put(AUDIO_MC_GRP_ID, "mca");
        streamMappingPrefix.put(AUDIO_MC_GRP_ID_FLAC, "mca");
        streamMappingPrefix.put(AUDIO_UC_GRP_ID, "uca");
        streamMappingPrefix.put(AUDIO_UC_GRP_ID_OGG, "uca");
        streamMappingPrefix.put(AUDIO_RAW_GRP_ID, "sa");


        streamMappingFile.put(MC_GRP_ID, "mastercopy");
        streamMappingFile.put(UC_GRP_ID, "usercopy");
        streamMappingFile.put(ALTO_GRP_ID, "alto");
        streamMappingFile.put(TXT_GRP_ID, "txt");
        streamMappingFile.put(TECHMDGRP, "amdsec");
        streamMappingFile.put(AUDIO_MC_GRP_ID, "mastercopy_audio");
        streamMappingFile.put(AUDIO_MC_GRP_ID_FLAC, "mastercopy_audio");
        streamMappingFile.put(AUDIO_UC_GRP_ID, "usercopy_audio");
        streamMappingFile.put(AUDIO_UC_GRP_ID_OGG, "usercopy_audio");
        streamMappingFile.put(AUDIO_RAW_GRP_ID, "sourceaudio");

        canContainPage.add(Const.ISSUE);
        canContainPage.add(Const.MONOGRAPH_MULTIPART);
        canContainPage.add(Const.MONOGRAPH_UNIT);
        canContainPage.add(Const.SUPPLEMENT);
        canContainPage.add(Const.PERIODICAL_VOLUME);

        dataStreamToModel.put(RAW_GRP_ID, BinaryEditor.RAW_ID);
        dataStreamToModel.put(MC_GRP_ID, BinaryEditor.NDK_ARCHIVAL_ID);
        dataStreamToModel.put(ALTO_GRP_ID, AltoDatastream.ALTO_ID);
        dataStreamToModel.put(UC_GRP_ID, BinaryEditor.NDK_USER_ID);
        dataStreamToModel.put(TXT_GRP_ID, StringEditor.OCR_ID);
        dataStreamToModel.put(OC_GRP_ID_CREATION, "PDF");
        dataStreamToModel.put(OC_GRP_ID_VALIDATION, "PDF");

        dataStreamToEvent.put(RAW_GRP_ID, "digitization_001");
        dataStreamToEvent.put(MC_GRP_ID, "MC_creation_001");
        dataStreamToEvent.put(ALTO_GRP_ID, "XML_creation_001");
        dataStreamToEvent.put(UC_GRP_ID, "UC_creation_001");
        dataStreamToEvent.put(TXT_GRP_ID, "TXT_creation_001");
        dataStreamToEvent.put(OC_GRP_ID_VALIDATION, "PDF_validation_001");
        dataStreamToEvent.put(OC_GRP_ID_CREATION, "PDF_Creation_001");

    }
}
