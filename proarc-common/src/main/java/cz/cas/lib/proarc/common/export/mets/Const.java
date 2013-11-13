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

import org.apache.log4j.Logger;

/**
 * 
 * Constants
 * 
 * @author Robert Simonovsky
 * 
 */
public class Const {
    public static final String FEDORA_CREATEDATE = "info:fedora/fedora-system:def/model#createdDate";
    public static final String FEDORA_LASTMODIFIED = "info:fedora/fedora-system:def/view#lastModifiedDate";
    public static final String FEDORA_LABEL = "info:fedora/fedora-system:def/model#label";

    public static final String hasPAGE = "kramerius:hasPage";
    public static final String hasMODEL = "fedora-model:hasModel";
    public static final String hasVOLUME = "kramerius:hasVolume";
    public static final String hasISSUE = "kramerius:hasItem";
    public static final String hasUNIT = "kramerius:hasUnit";
    public static final String hasINTCOMPPART = "kramerius:hasIntCompPart";
    public static final String isONPAGE = "kramerius:isOnPage";
    public static final String hasMEMBER = "fedora-rels-ext:hasMember";

    public static final String DIV_PHYSICAL_ID = "PHYSICAL";
    public static final String DIV_PHYSICAL_LABEL = "Physical_structure";
    public static final String DIV_LOGICAL_ID = "LOGICAL";
    public static final String DIV_LOGICAL_LABEL = "Logical_structure";

    public static final String MONOGRAPH = "monograph";
    public static final String PERIODICAL = "periodical";

    public static final String ARTICLE = "ARTICLE";
    public static final String PERIODICAL_VOLUME = "PERIODICAL_VOLUME";
    public static final String PAGE = "PAGE";
    public static final String PERIODICAL_TITLE = "PERIODICAL_TITLE";
    public static final String VOLUME = "VOLUME";
    public static final String PICTURE = "PICTURE";
    public static final String ISSUE = "ISSUE";
    public static final String CHAPTER = "CHAPTER";
    public static final String MONOGRAPHUNIT = "MONOGRAPHUNIT";
    @SuppressWarnings("unused")
    private static Logger logger = Logger.getLogger(Const.class);
}
