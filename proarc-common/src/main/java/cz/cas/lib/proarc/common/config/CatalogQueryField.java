/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.config;

import org.apache.commons.configuration.Configuration;

/**
 * The field descriptor for catalog queries.
 *
 * @author Jan Pokorsky
 */
public class CatalogQueryField {

    public static final String PROPERTY_FIELD_TITLE = "title";

    private String name;
    private Configuration properties;

    public CatalogQueryField(String name, Configuration properties) {
        this.name = name;
        this.properties = properties;
    }

    public String getName() {
        return name;
    }

    public String getTitle() {
        return properties.getString(PROPERTY_FIELD_TITLE, getFieldNameDefault(getName()));
    }

    private String getFieldNameDefault(String name) {
        switch (name) {
            case "issn": return "ISSN";
            case "isbn": return "ISBN";
            case "barcode": return "Čárový kód";
            case "ean": return "Čárový kód";
            case "signature": return "Signatura";
            case "ccnb": return "čČNB";
            case "sys": return "SYS";
            case "id": return "ID";
            case "author": return "Autor";
            case "title": return "Název";
            default: return name;
        }
    }

    public Configuration getProperties() {
        return properties;
    }

}
