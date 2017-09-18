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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.config;

import cz.cas.lib.proarc.common.catalog.AlephXServer;
import cz.cas.lib.proarc.common.catalog.DigitizationRegistryCatalog;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationUtils;

/**
 * Bibliographic catalog configuration.
 *
 * @author Jan Pokorsky
 */
public final class CatalogConfiguration {

    private static final Logger LOG = Logger.getLogger(CatalogConfiguration.class.getName());

    public static final String PROPERTY_DEBUG = "debug";
    public static final String PROPERTY_NAME = "name";
    public static final String PROPERTY_USER = "user";
    public static final String PROPERTY_PASSWD = "password";
    public static final String PROPERTY_URL = "url";
    public static final String PROPERTY_TYPE = "type";
    /** The configuration property name to list field IDs. */
    public static final String PROPERTY_FIELDS = "fields";
    /** The configuration property prefix of field's properties. */
    public static final String FIELD_PREFIX = "field";

    private final String id;
    private final String prefix;
    private final Configuration properties;

    public CatalogConfiguration(String id, String prefix, Configuration properties) {
        this.id = id;
        this.properties = properties;
        this.prefix = prefix;
    }

    public String getId() {
        return id;
    }

    public boolean getDebug() {
        return properties.getBoolean(PROPERTY_DEBUG, false);
    }

    public String getName() {
        return properties.getString(PROPERTY_NAME);
    }

    public String getType() {
        return properties.getString(PROPERTY_TYPE);
    }

    public String getUrl() {
        return fixUrlProtocol(properties.getString(PROPERTY_URL));
    }

    public List<CatalogQueryField> getQueryFields() {
        String[] fieldIds = properties.getStringArray(PROPERTY_FIELDS);
        if (fieldIds.length == 0) {
            return getDefaultQueryFields();
        }
        ArrayList<CatalogQueryField> fields = new ArrayList<CatalogQueryField>(fieldIds.length);
        for (String fieldId : fieldIds) {
            CatalogQueryField field = new CatalogQueryField(fieldId, properties.subset(FIELD_PREFIX + '.' + fieldId));
            fields.add(field);
        }
        return fields;
    }

    /**
     * Fills fields for existing configurations to ensure backward compatibility.
     */
    private List<CatalogQueryField> getDefaultQueryFields() {
        String type = getType();

        ArrayList<CatalogQueryField> fields = new ArrayList<>();
        if (AlephXServer.TYPE.equals(type)) {
            fields.add(new CatalogQueryField("barcode",
                    new BaseConfiguration() {{addProperty(AlephXServer.PROPERTY_FIELD_QUERY, "bar");}}));
            fields.add(new CatalogQueryField("ccnb",
                    new BaseConfiguration() {{addProperty(AlephXServer.PROPERTY_FIELD_QUERY, "cnb");}}));
            fields.add(new CatalogQueryField("isbn",
                    new BaseConfiguration() {{addProperty(AlephXServer.PROPERTY_FIELD_QUERY, "sbn");}}));
            fields.add(new CatalogQueryField("issn",
                    new BaseConfiguration() {{addProperty(AlephXServer.PROPERTY_FIELD_QUERY, "ssn");}}));
            fields.add(new CatalogQueryField("signature",
                    new BaseConfiguration() {{addProperty(AlephXServer.PROPERTY_FIELD_QUERY, "sg");}}));
        } else if (DigitizationRegistryCatalog.TYPE.equals(type)) {
            BaseConfiguration emptyConfiguration = new BaseConfiguration();
            fields.add(new CatalogQueryField("barcode", emptyConfiguration));
            fields.add(new CatalogQueryField("ccnb", emptyConfiguration));
            fields.add(new CatalogQueryField("isbn", emptyConfiguration));
            fields.add(new CatalogQueryField("issn", emptyConfiguration));
            fields.add(new CatalogQueryField("signature", emptyConfiguration));
            fields.add(new CatalogQueryField("title", emptyConfiguration));
        }
        return fields;
    }

    public Configuration getProperties() {
        return properties;
    }

    public String getProperty(String name) {
        return properties.getString(name);
    }

    public String getProperty(String name, String defaultValue) {
        return properties.getString(name, defaultValue);
    }

    @Override
    public String toString() {
        StringWriter dump = new StringWriter();
        PrintWriter printWriter = new PrintWriter(dump);
        ConfigurationUtils.dump(properties, printWriter);
        printWriter.flush();
        return "CatalogConfiguration{id=" + id + ", prefix=" + prefix + ", properties:" + dump + '}';
    }

    /** Adds missing protocol that is required by URL/URI classes. */
    private static String fixUrlProtocol(String url) {
        if (url == null) {
            return url;
        }
        if (url.contains("://")) {
            return url;
        } else {
            return "http://" + url;
        }
    }

}
