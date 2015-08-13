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
package cz.cas.lib.proarc.common.i18n;

/**
 * Localization bundle names.
 *
 * @author Jan Pokorsky
 */
public enum BundleName {

    /**
     * @see <a href='https://docs.google.com/document/d/1zSriHPdnUY5d_tKv0M8a6nEym560DKh2H6XZ24tGAEw/edit?pli=1#'>Page Types</a>
     * @see <a href='http://digit.nkp.cz/DigitizedPeriodicals/DTD/2.10/Periodical.xsd'>PeriodicalPage[@Type]</a>
     */
    MODS_PAGE_TYPES("cz.cas.lib.proarc.common.config.modsPageTypes", "ndk.mods.pageTypes"),
    MODS_IDENTIFIER_TYPES("cz.cas.lib.proarc.common.config.modsIdentifierTypes", "ndk.mods.IdentifierTypes"),
    /**
     * <a href='http://www.loc.gov/standards/iso639-2/php/English_list.php'>ISO 639-2 Languages</a>
     */
    LANGUAGES_ISO639_2("cz.cas.lib.proarc.common.config.languagesIso639-2", "ndk.mods.languageTerms");

    private String bundleName;
    private String valueMapId;

    private BundleName(String bundleName, String valueMapId) {
        this.bundleName = bundleName;
        this.valueMapId = valueMapId;
    }

    @Override
    public String toString() {
        return bundleName;
    }

    public String getValueMapId() {
        return valueMapId;
    }

    public static BundleName fromString(String bundleName) {
        for (BundleName bundle : values()) {
            if (bundle.bundleName.equals(bundleName)) {
                return bundle;
            }
        }
        return null;
    }
}
