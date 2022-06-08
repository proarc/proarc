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
package cz.cas.lib.proarc.common.mods.custom;

/**
 * Constants to share names of JAXB elements.
 *
 * @author Jan Pokorsky
 */
public interface ModsConstants {

    /** Namespace {@code "http://www.loc.gov/mods/v3"}. */
    public static final String NS = "http://www.loc.gov/mods/v3";

    // identifier
    public static final String FIELD_IDENTIFIER_VALUE = "value";
    public static final String FIELD_IDENTIFIER_TYPE = "type";
    //page
    public static final String FIELD_STRING_VALUE = "value";
    public static final String FIELD_PAGE_TYPE = "pageType";
    public static final String FIELD_PAGE_INDEX = "pageIndex";
    public static final String FIELD_PAGE_NUMBER = "pageNumber";
    public static final String FIELD_PAGE_NUMBER_SPLIT = "page number";
    public static final String FIELD_PAGE_TITLE = "pageTitle";
    public static final String FIELD_PAGE_SUBTITLE = "pageSubTitle";
    public static final String FIELD_PAGE_EXTENT = "pageExtent";
    public static final String FIELD_PAGE_NOTE = "pageNote";
    public static final String FIELD_PAGE_GENRE = "pageGenre";
    public static final String FIELD_PAGE_TYPEOFRESOURCE = "pageTypeOfResource";
    public static final String FIELD_IDENTIFIERS = "identifiers";
    public static final String FIELD_NOTE = "note";
    //periodical
    public static final String FIELD_PERIODICITIES = "periodicities";
    public static final String FIELD_SIGLA = "sigla";
    public static final String FIELD_SHELF_LOCATORS = "shelfLocators";
    public static final String FIELD_AUTHORS = "authors";
    public static final String FIELD_CONTRIBUTORS = "contributors";
    public static final String FIELD_NAME_FAMILY = "family";
    public static final String FIELD_NAME_GIVEN = "given";
    public static final String FIELD_PUBLISHERS = "publishers";
    public static final String FIELD_PRINTER_PUBLISHER_NAME = "publisherName";
    public static final String FIELD_PRINTER_PUBLISHER_DATE = "publisherDate";
    public static final String FIELD_PRINTER_PUBLISHER_PLACE = "publisherPlace";
    public static final String FIELD_PRINTERS = "printers";
    public static final String FIELD_TITLES = "titles";
    public static final String FIELD_SUBTITLES = "subtitles";
    public static final String FIELD_ALTERNATIVE_TITLES = "alternativeTitles";
    public static final String FIELD_KEY_TITLES = "keyTitles";
    public static final String FIELD_KEYWORDS = "keywords";
    public static final String FIELD_LANGUAGES = "languages";
    public static final String FIELD_LANGUAGE_CODE = "languageCode";
    public static final String FIELD_CLASSIFICATIONS = "classifications";
    public static final String FIELD_CLASSIFICATION_UDC = "classificationsUDC";
    public static final String FIELD_CLASSIFICATION_DDC = "classificationsDDC";
    public static final String FIELD_PHYSICAL_DESCRIPTIONS = "physicalDescriptions";
    public static final String FIELD_PHYSICAL_DESCRIPTIONS_EXTENT = "physicalDescriptionsExtent";
    public static final String FIELD_PHYSICAL_DESCRIPTIONS_SIZE = "physicalDescriptionsSize";
    public static final String FIELD_RECORD_ORIGIN = "recordOrigin";
    // periodical volume
    public static final String FIELD_PER_VOLUME_NUMBER = "periodicalVolumeNumber";
    public static final String FIELD_PER_VOLUME_YEAR = "periodicalVolumeYear";
    // periodical issue
    public static final String FIELD_PER_ISSUE_NUMBER = "PeriodicalItemNumber";
    public static final String FIELD_PER_ISSUE_NUMBER_SORTING = "PeriodicalItemNumberSorting";
    public static final String FIELD_PER_ISSUE_DATE = "periodicalItemDate";
    // monograph
    public static final String FIELD_PRESERVATION_TREATMENT = "preservationTreatment";
    public static final String FIELD_PRESERVATION_STATEOFART = "preservationStateOfArt";
    // monograph unit
    public static final String FIELD_MONOGRAPHUNIT_NUMBER = "monographUnitNumber";

    public static final String VALUE_ORIGININFO_EVENTTYPE_PUBLICATION = "publication";
    public static final String VALUE_ORIGININFO_EVENTTYPE_PRODUCTION = "production";
    public static final String VALUE_ORIGININFO_EVENTTYPE_DISTRIBUTION = "distribution";
    public static final String VALUE_ORIGININFO_EVENTTYPE_MANUFACTURE = "manufacture";
    public static final String VALUE_ORIGININFO_EVENTTYPE_COPYRIGHT = "copyright";

    public static final String FIELD_ORIGININFO_DATE_ISSUED = "dateIssued";
    public static final String FIELD_ORIGININFO_DATE_OTHER = "dateOther";
    public static final String FIELD_ORIGININFO_DATE_COPYRIGHT = "copyrightDate";

    public static final String VALUE_PHYSICALDESCRIPTION_FORM_RDAMEDIA = "rdamedia";
    public static final String VALUE_PHYSICALDESCRIPTION_FORM_MEDIA = "media";
    public static final String VALUE_PHYSICALDESCRIPTION_FORM_RDACARRIER = "rdacarrier";
    public static final String VALUE_PHYSICALDESCRIPTION_FORM_CARRIER = "carrier";
    public static final String VALUE_PHYSICALDESCRIPTION_FORM_MARCFORM = "marcform";
    public static final String VALUE_PHYSICALDESCRIPTION_FORM_MARCCATEGORY= "marccategory";
    public static final String VALUE_PHYSICALDESCRIPTION_FORM_MARCSMD= "marcsmd";
    public static final String VALUE_PHYSICALDESCRIPTION_FORM_GMD = "gmd";

    public static final String VALUE_DESCRIPTIONSTANDARD_RDA = "rda";
    public static final String VALUE_DESCRIPTIONSTANDARD_AACR = "aacr";

    public static final String VALUE_PAGE_NOTE_LEFT = "left";
    public static final String VALUE_PAGE_NOTE_RIGHT = "right";
    public static final String VALUE_PAGE_NOTE_SINGLE_PAGE = "singlePage";

}
