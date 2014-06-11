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
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;

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
    public static ArrayList<String> mandatoryStreams = new ArrayList<String>();
    public static HashMap<String, String> streamMappingFile = new HashMap<String, String>();
    public static HashMap<String, String> streamMappingPrefix = new HashMap<String, String>();
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
    public final static String MONOGRAPH_TITLE_MODEL = NdkPlugin.MODEL_MONOGRAPHTITLE;
    public final static String MONOGRAPH_MODEL = NdkPlugin.MODEL_MONOGRAPHVOLUME;
    public final static String PICTURE_MODEL = NdkPlugin.MODEL_PICTURE;
    public final static String ARTICLE_MODEL = NdkPlugin.MODEL_ARTICLE;
    public final static String CHAPTER_MODEL = NdkPlugin.MODEL_CHAPTER;
    public final static String SUPPLEMENT_MODEL = "model:supplement";

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

    public static final String HASPAGE = "kramerius:hasPage";
    public static final String HASMODEL = "fedora-model:hasModel";
    public static final String HASVOLUME = "kramerius:hasVolume";
    public static final String HASISSUE = "kramerius:hasItem";
    public static final String HASUNIT = "kramerius:hasUnit";
    public static final String HASINTCOMPPART = "kramerius:hasIntCompPart";
    public static final String HASMEMBER = "fedora-rels-ext:hasMember";
    public static final String ISONPAGE = "kramerius:isOnPage";

    public static final List<String> PSPElements = new ArrayList<String>();

    public final static Map<String, String> typeMap = new HashMap<String, String>();
    public final static Map<String, String> typeNameMap = new HashMap<String, String>();
    public final static Map<String, String> mimeToFmtMap = new HashMap<String, String>();

    static {
        mimeToFmtMap.put("image/tiff", "fmt/353");
        mimeToFmtMap.put("image/jp2", "fmt/151");
        mimeToFmtMap.put("text/xml", "fmt/101");
        mimeToFmtMap.put("application/pdf", "fmt/18");
        mimeToFmtMap.put("image/jpeg", "fmt/44");
        mimeToFmtMap.put("audio/wav", "fmt/141");
        mimeToFmtMap.put("audio/mpeg", "fmt/134");

        PSPElements.add(Const.MONOGRAPH_UNIT);
        PSPElements.add(Const.ISSUE);
        PSPElements.add(Const.SUPPLEMENT);

        typeMap.put(FEDORAPREFIX + PERIODICAL_ITEM_MODEL, ISSUE);
        typeMap.put(FEDORAPREFIX + PERIODICAL_MODEL, PERIODICAL_TITLE);
        typeMap.put(FEDORAPREFIX + PERIODICAL_VOLUME_MODEL, PERIODICAL_VOLUME);
        typeMap.put(FEDORAPREFIX + PAGE_MODEL, PAGE);
        typeMap.put(FEDORAPREFIX + MONOGRAPH_MODEL, MONOGRAPH_UNIT);
        typeMap.put(FEDORAPREFIX + MONOGRAPH_TITLE_MODEL, MONOGRAPH_MULTIPART);
        typeMap.put(FEDORAPREFIX + PICTURE_MODEL, PICTURE);
        typeMap.put(FEDORAPREFIX + ARTICLE_MODEL, ARTICLE);
        typeMap.put(FEDORAPREFIX + SUPPLEMENT_MODEL, SUPPLEMENT);
        typeMap.put(FEDORAPREFIX + NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, SUPPLEMENT);
        typeMap.put(FEDORAPREFIX + NdkPlugin.MODEL_PERIODICALSUPPLEMENT, SUPPLEMENT);
        typeMap.put(FEDORAPREFIX + NdkPlugin.MODEL_CARTOGRAPHIC, MONOGRAPH_UNIT);
        typeMap.put(FEDORAPREFIX + NdkPlugin.MODEL_SHEETMUSIC, MONOGRAPH_UNIT);

        typeMap.put(FEDORAPREFIX + CHAPTER_MODEL, CHAPTER);

        typeNameMap.put(ISSUE, ISSUE);
        typeNameMap.put(PERIODICAL_VOLUME, VOLUME);
        typeNameMap.put(PERIODICAL_TITLE, TITLE);
        typeNameMap.put(PAGE, PAGE);
        typeNameMap.put(MONOGRAPH_UNIT, VOLUME);
        typeNameMap.put(PICTURE, PICTURE);
        typeNameMap.put(ARTICLE, "ART");
        typeNameMap.put(SUPPLEMENT, "SUPPLEMENT");
        typeNameMap.put(CHAPTER, CHAPTER);
        typeNameMap.put(MONOGRAPH_MULTIPART, MONOGRAPH);

        mandatoryStreams.add("MC_IMGGRP");
        mandatoryStreams.add("UC_IMGGRP");

        streamMapping = new HashMap<String, List<String>>();
        streamMapping.put("MC_IMGGRP", new ArrayList<String>());
        streamMapping.get("MC_IMGGRP").add(BinaryEditor.NDK_ARCHIVAL_ID);
        streamMapping.get("MC_IMGGRP").add("RAW");

        streamMapping.put("UC_IMGGRP", new ArrayList<String>());
        streamMapping.get("UC_IMGGRP").add(BinaryEditor.NDK_USER_ID);
        streamMapping.get("UC_IMGGRP").add("FULL");

        streamMapping.put("ALTOGRP", new ArrayList<String>());
        streamMapping.get("ALTOGRP").add("ALTO");

        streamMapping.put("TXTGRP", new ArrayList<String>());
        streamMapping.get("TXTGRP").add("TEXT_OCR");

        streamMapping.put("TECHMDGRP", new ArrayList<String>());
        streamMapping.get("TECHMDGRP").add("FULL_AMD");

        streamMappingPrefix.put("MC_IMGGRP", "MC");
        streamMappingPrefix.put("UC_IMGGRP", "UC");
        streamMappingPrefix.put("ALTOGRP", "ALTO");
        streamMappingPrefix.put("TXTGRP", "TXT");
        streamMappingPrefix.put("TECHMDGRP", "AMD_METS");

        streamMappingFile.put("MC_IMGGRP", "masterCopy");
        streamMappingFile.put("UC_IMGGRP", "userCopy");
        streamMappingFile.put("ALTOGRP", "ALTO");
        streamMappingFile.put("TXTGRP", "TXT");
        streamMappingFile.put("TECHMDGRP", "amdSec");
    }
}
