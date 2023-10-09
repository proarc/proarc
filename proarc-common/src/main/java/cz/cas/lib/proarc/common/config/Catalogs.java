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
import cz.cas.lib.proarc.common.catalog.AlmaCatalog;
import cz.cas.lib.proarc.common.catalog.BibliographicCatalog;
import cz.cas.lib.proarc.common.catalog.DigitizationRegistryCatalog;
import cz.cas.lib.proarc.common.catalog.OaiCatalog;
import cz.cas.lib.proarc.common.catalog.Z3950Catalog;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;

/**
 * Manages bibliographic catalog configurations.
 *
 * @author Jan Pokorsky
 */
public final class Catalogs {

    private static final Logger LOG = Logger.getLogger(Catalogs.class.getName());
    static final String CATALOG_PREFIX = "catalog";
    static final String PROPERTY_CATALOGS = "catalogs";
    static final String PROPERTY_AUTHORITY_CATALOGS = "authorityCatalogs";
    static final String AUTHORITY_CATALOG_PREFIX = "authorityCatalog";

    public static final String PROPERTY_CUSTOM_TEMPLATE_PATH = "catalogs.customTemplate.path";

    private final Configuration config;

    public Catalogs(Configuration config) {
        this.config = config;
    }

    /**
     * Gets configuration of all registered catalogs.
     * @return list of configurations
     */
    public List<CatalogConfiguration> getConfigurations() {
        ArrayList<CatalogConfiguration> catalogs = new ArrayList<CatalogConfiguration>();
        for (String catalogId : config.getStringArray(PROPERTY_CATALOGS)) {
            CatalogConfiguration catalog = readConfiguration(catalogId);
            if (catalog != null) {
                catalogs.add(catalog);
            }
        }
        return catalogs;
    }

    public List<CatalogConfiguration> getAllowEditingRecordConfiguration() {
        ArrayList<CatalogConfiguration> catalogs = new ArrayList<CatalogConfiguration>();
        for (String catalogId : config.getStringArray(PROPERTY_CATALOGS)) {
            CatalogConfiguration catalog = readConfiguration(catalogId);
            if (catalog != null && catalog.allowCatalogUpdateRecord()) {
                catalogs.add(catalog);
            }
        }
        return catalogs;
    }

    /**
     * Gets configuration of all registered authority catalogs.
     * @return list of configurations
     */
    public List<CatalogConfiguration> getAuthorityConfigurations() {
        ArrayList<CatalogConfiguration> catalogs = new ArrayList<CatalogConfiguration>();
        for (String catalogId : config.getStringArray(PROPERTY_AUTHORITY_CATALOGS)) {
            CatalogConfiguration catalog = readAuthorityConfiguration(catalogId);
            if (catalog != null) {
                catalogs.add(catalog);
            }
        }
        return catalogs;
    }

    /**
     * Finds particular catalog.
     * @param id catalog id
     * @return catalog or {@code null}
     */
    public BibliographicCatalog findCatalog(String id) {
        CatalogConfiguration props = findConfiguration(id);
        String customTemplatePath = config.getString(PROPERTY_CUSTOM_TEMPLATE_PATH, null);

        if (props == null) {
            props = findAuthorityConfiguration(id);

            if (props == null) {
                return null;
            }
        }
        BibliographicCatalog catalog = DigitizationRegistryCatalog.get(props, customTemplatePath);
        if (catalog != null) {
            return catalog;
        }
        catalog = AlephXServer.get(props, customTemplatePath);
        if (catalog != null) {
            return catalog;
        }
        catalog = OaiCatalog.get(props, customTemplatePath);
        if (catalog != null) {
            return catalog;
        }
        catalog = Z3950Catalog.get(props, customTemplatePath);
        if (catalog != null) {
            return catalog;
        }
        catalog = AlmaCatalog.get(props, customTemplatePath);
        return catalog;
    }

    /**
     * Finds particular catalog configuration.
     * @param id catalog id
     * @return catalog configuration or {@code null}
     */
    public CatalogConfiguration findConfiguration(String id) {
        for (CatalogConfiguration catalog : getConfigurations()) {
            if (catalog.getId().equals(id)) {
                return catalog;
            }
        }
        return null;
    }

    public CatalogConfiguration findAuthorityConfiguration(String id) {
        for (CatalogConfiguration catalog : getAuthorityConfigurations()) {
            if (catalog.getId().equals(id)) {
                return catalog;
            }
        }
        return null;
    }

    private CatalogConfiguration readConfiguration(String catalogId) {
        String catalogPrefix = CATALOG_PREFIX + '.' + catalogId;
        Configuration catalogConfig = config.subset(catalogPrefix);
        CatalogConfiguration catalog = new CatalogConfiguration(catalogId, catalogPrefix, catalogConfig);
        if (!isValidProperty(catalogPrefix, CatalogConfiguration.PROPERTY_URL, catalog.getUrl())) {
            return null;
        }
        isValidProperty(catalogPrefix, CatalogConfiguration.PROPERTY_NAME, catalog.getName());
        return catalog;
    }

    private CatalogConfiguration readAuthorityConfiguration(String catalogId) {
        String catalogPrefix = AUTHORITY_CATALOG_PREFIX + '.' + catalogId;
        Configuration catalogConfig = config.subset(catalogPrefix);
        CatalogConfiguration catalog = new CatalogConfiguration(catalogId, catalogPrefix, catalogConfig);
        if (!isValidProperty(catalogPrefix, CatalogConfiguration.PROPERTY_URL, catalog.getUrl())) {
            return null;
        }
        isValidProperty(catalogPrefix, CatalogConfiguration.PROPERTY_NAME, catalog.getName());
        return catalog;
    }

    private static boolean isValidProperty(String prefix, String name, String value) {
        if (value == null || value.isEmpty()) {
            LOG.log(Level.WARNING, "Missing {0}.{1} property!", new Object[]{prefix, name});
            return false;
        }
        return true;
    }

}
