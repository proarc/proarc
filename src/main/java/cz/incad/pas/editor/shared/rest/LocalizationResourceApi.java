/*
 * Copyright (C) 2012 Jan Pokorsky
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
package cz.incad.pas.editor.shared.rest;

/**
 *
 * @author Jan Pokorsky
 */
public final class LocalizationResourceApi {

    public static final String PATH = "localization";
    public static final String GETBUNDLE_BUNDLENAME_PARAM = "bundleName";
    public static final String GETBUNDLE_LOCALE_PARAM = "locale";
    public static final String GETBUNDLE_SORTED_PARAM = "sorted";
    public static final String ITEM_KEY = "code";
    public static final String ITEM_VALUE = "value";

    public enum BundleName {

        /**
         * @see <a href='https://docs.google.com/document/d/1zSriHPdnUY5d_tKv0M8a6nEym560DKh2H6XZ24tGAEw/edit?pli=1#'>Page Types</a>
         * @see <a href='http://digit.nkp.cz/DigitizedPeriodicals/DTD/2.10/Periodical.xsd'>PeriodicalPage[@Type]</a>
         */
        MODS_PAGE_TYPES("cz.incad.pas.editor.server.config.modsPageTypes");

        private String bundleName;

        private BundleName(String bundleName) {
            this.bundleName = bundleName;
        }

        @Override
        public String toString() {
            return bundleName;
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

}
