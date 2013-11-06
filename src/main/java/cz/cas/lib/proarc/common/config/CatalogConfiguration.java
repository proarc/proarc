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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationUtils;

/**
 * Bibliographic catalog configuration.
 *
 * @author Jan Pokorsky
 */
public final class CatalogConfiguration {

    private static final Logger LOG = Logger.getLogger(CatalogConfiguration.class.getName());

    public static final String PROPERTY_NAME = "name";
    public static final String PROPERTY_USER = "user";
    public static final String PROPERTY_PASSWD = "password";
    public static final String PROPERTY_URL = "url";
    public static final String PROPERTY_TYPE = "type";

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

    public String getName() {
        return properties.getString(PROPERTY_NAME);
    }

    public String getType() {
        return properties.getString(PROPERTY_TYPE);
    }

    public String getUrl() {
        return fixUrlProtocol(properties.getString(PROPERTY_URL));
    }

    public Configuration getProperties() {
        return properties;
    }

    public String getProperty(String name) {
        return properties.getString(name);
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
