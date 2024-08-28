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
import cz.cas.lib.proarc.common.catalog.updateCatalog.AlephXmlUpdateCatalog;
import cz.cas.lib.proarc.common.catalog.updateCatalog.VerbisUpdateCatalog;
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
    public static final String PROPERTY_DEFAULT_SEARCH_FIELD = "defaultSearchField";

    /* konfigurace jen pro editaci zaznamu v katalogu */
    public static final String PROPERTY_UPDATE_TYPE = "updateType";

    /* parametry pro upravu verbisu */
    public static final String PROPERTY_AUTHORIZATION_CATALOG_URL = "authorizationUrl";
    public static final String PROPERTY_UPDATE_CATALOG_URL = "updateRecordUrl";
    public static final String PROPERTY_LOGIN_USERNAME = "username";
    public static final String PROPERTY_LOGIN_PASSWORD = "password";
    public static final String PROPERTY_UPDATE_FIELD = "recordField";
    public static final String PROPERTY_UPDATE_SUBFIELD_APP = "recordSubfieldAppIdentifier";
    public static final String PROPERTY_UPDATE_SUBFIELD_OBJECT = "recordSubfieldObjectIdentifier";
    public static final String PROPERTY_UPDATE_SUBFIELD_DIGITALIZED = "recordSubfieldDigitalizated";

    /* parametry pro upravu aleph xml */
    public static final String PROPERTY_CATALOG_DIRECTORY = "catalogDirectory";
    public static final String PROPERTY_CATALOG_URL_LINK = "catalogUrlLink";
    public static final String PROPERTY_FIELD001_BASE_LENGHT = "baseLenght";
    public static final String PROPERTY_FIELD001_BASE_DEFAULT = "baseDefault";
    public static final String PROPERTY_FIELD001_SYSNO_LENGHT = "sysnoLenght";

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

    public String getPrefix() {
        return prefix;
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

    public String getDefaultSearchField() {
        return properties.getString(PROPERTY_DEFAULT_SEARCH_FIELD);
    }

    public String getCatalogUpdateType() {
        return properties.getString(PROPERTY_UPDATE_TYPE);
    }

    public String getCatalogDirectory() {
        return properties.getString(PROPERTY_CATALOG_DIRECTORY);
    }

    public String getCatalogUrlLink() {
        return properties.getString(PROPERTY_CATALOG_URL_LINK);
    }

    public Integer getField001BaseLenght() {
        return properties.getInteger(PROPERTY_FIELD001_BASE_LENGHT, 0);
    }

    public String getField001BaseDefault() {
        return properties.getString(PROPERTY_FIELD001_BASE_DEFAULT);
    }

    public Integer getField001SysnoLenght() {
        return properties.getInteger(PROPERTY_FIELD001_SYSNO_LENGHT, 9);
    }

    public String getCatalogAuthorizationUrl() {
        return properties.getString(PROPERTY_AUTHORIZATION_CATALOG_URL);
    }

    public String getCatalogUpdateUrl() {
        return properties.getString(PROPERTY_UPDATE_CATALOG_URL);
    }

    public String getCatalogUsername() {
        return properties.getString(PROPERTY_LOGIN_USERNAME);
    }

    public String getCatalogPassword() {
        return properties.getString(PROPERTY_LOGIN_PASSWORD);
    }

    public String getUpdateField() {
        return properties.getString(PROPERTY_UPDATE_FIELD);
    }

    public String getUpdateSubfieldApp() {
        return properties.getString(PROPERTY_UPDATE_SUBFIELD_APP);
    }

    public String getUpdateSubfieldObject() {
        return properties.getString(PROPERTY_UPDATE_SUBFIELD_OBJECT);
    }

    public String getUpdateSubfieldDigitalized() {
        return properties.getString(PROPERTY_UPDATE_SUBFIELD_DIGITALIZED);
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

    public boolean allowCatalogUpdateRecord() {
        return AlephXmlUpdateCatalog.ID.equals(getCatalogUpdateType()) || VerbisUpdateCatalog.ID.equals(getCatalogUpdateType());
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
