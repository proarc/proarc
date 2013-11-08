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
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.catalog.BibliographicCatalog;
import cz.cas.lib.proarc.common.catalog.MetadataItem;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.webapp.shared.rest.BibliographicCatalogResourceApi;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.logging.Logger;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.transform.TransformerException;

/**
 * The resource to list available bibliographic catalogs like Aleph
 * and registrdigitalizace.cz and to query them for metadata.
 *
 * @author Jan Pokorsky
 */
@Path(BibliographicCatalogResourceApi.PATH)
public class BibliographicCatalogResource {

    private static final Logger LOG = Logger.getLogger(BibliographicCatalogResource.class.getName());
    private final HttpHeaders httpHeaders;
    private final AppConfiguration appConfig;

    public BibliographicCatalogResource(
            @Context HttpHeaders httpHeaders
            ) throws AppConfigurationException {

        this.httpHeaders = httpHeaders;
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<CatalogDescriptor> findCatalog(
            @QueryParam(BibliographicCatalogResourceApi.CATALOG_ID) String id) {

        List<CatalogConfiguration> catalogs;
        if (id == null) {
            catalogs = appConfig.getCatalogs().getConfigurations();
        } else {
            CatalogConfiguration catalog = appConfig.getCatalogs().findConfiguration(id);
            catalogs = catalog != null ? Arrays.asList(catalog) : Collections.<CatalogConfiguration>emptyList();
        }
        ArrayList<CatalogDescriptor> result = new ArrayList<CatalogDescriptor>(catalogs.size());
        for (CatalogConfiguration cp : catalogs) {
            result.add(CatalogDescriptor.create(cp));
        }
        return new SmartGwtResponse<CatalogDescriptor>(result);
    }
    
    /**
     * Finds metadata in bibliographic catalog.
     *
     * @param catalog catalog descriptor
     * @param fieldName issn|isbn|ccnb
     * @param value value to query
     * @return list of metadata records
     */
    @Path(BibliographicCatalogResourceApi.FIND_PATH)
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public MetadataList find(
            @QueryParam(BibliographicCatalogResourceApi.FIND_CATALOG_PARAM) String catalog,
            @QueryParam(BibliographicCatalogResourceApi.FIND_FIELDNAME_PARAM) String fieldName,
            @QueryParam(BibliographicCatalogResourceApi.FIND_VALUE_PARAM) String value) throws TransformerException, IOException {

        List<Locale> acceptableLanguages = httpHeaders.getAcceptableLanguages();
        Locale locale = acceptableLanguages.isEmpty() ? null : acceptableLanguages.get(0);
        List<MetadataItem> result;
        BibliographicCatalog bCatalog = appConfig.getCatalogs().findCatalog(catalog);
        if (bCatalog != null) {
            result = bCatalog.find(fieldName, value, locale);
        } else {
            throw RestException.plainNotFound(BibliographicCatalogResourceApi.FIND_CATALOG_PARAM, catalog);
        }
        return new MetadataList(result);
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class CatalogDescriptor {

        public static CatalogDescriptor create(CatalogConfiguration cp) {
            return new CatalogDescriptor(cp.getId(), cp.getName());
        }

        @XmlElement(name = BibliographicCatalogResourceApi.CATALOG_ID)
        private String id;
        @XmlElement(name = BibliographicCatalogResourceApi.CATALOG_NAME)
        private String name;

        public CatalogDescriptor(String id, String name) {
            this.id = id;
            this.name = name;
        }

        public CatalogDescriptor() {
        }

    }

    /**
     * JAXB helper to list items.
     */
    @XmlRootElement(name = "metadataCatalogEntries")
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class MetadataList {

        @XmlElement(name = "entry")
        List<MetadataItem> list;

        public MetadataList() {
        }

        public MetadataList(List<MetadataItem> list) {
            this.list = list;
        }

    }

//    @XmlRootElement
//    @XmlAccessorType(XmlAccessType.FIELD)
//    public static class Criterion {
//
//        private String operator;
//        private String fieldName;
//        private String value;
//
//        public Criterion() {
//        }
//
//        public String getFieldName() {
//            return fieldName;
//        }
//
//        public String getValue() {
//            return value;
//        }
//
//        @Override
//        public String toString() {
//            return String.format("Criterion[operator: %s, fieldName: %s, value: %s]",
//                    operator, fieldName, value);
//        }
//
//    }

}
