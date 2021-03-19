/*
 * Copyright (C) 2021 Lukas Sykora
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

import cz.cas.lib.proarc.common.catalog.AuthorityItem;
import cz.cas.lib.proarc.common.catalog.BibliographicCatalog;
import cz.cas.lib.proarc.common.catalog.MetadataItem;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.config.CatalogQueryField;
import cz.cas.lib.proarc.webapp.shared.rest.AuthorityCatalogResourceApi;
import java.io.IOException;
import java.net.ConnectException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response.Status;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.transform.TransformerException;

/**
 * The resource to list available authorities catalogs like Aleph
 * and to query them for metadata.
 *
 * @author Lukáš Sýkora
 */
@Path(AuthorityCatalogResourceApi.PATH)
public class AuthorityCatalogResource {

    private static final Logger LOG = Logger.getLogger(AuthorityCatalogResource.class.getName());
    private final HttpHeaders httpHeaders;
    private final AppConfiguration appConfig;

    public enum Type {
        ALL, NAME, SUBJECT
    }

    public AuthorityCatalogResource(
            @Context HttpHeaders httpHeaders
    ) throws AppConfigurationException {

        this.httpHeaders = httpHeaders;
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<CatalogDescriptor> findAuthorityCatalog(
            @QueryParam(AuthorityCatalogResourceApi.CATALOG_ID) String id) {

        List<CatalogConfiguration> catalogs;
        if (id == null) {
            catalogs = appConfig.getCatalogs().getAuthorityConfigurations();
        } else {
            CatalogConfiguration catalog = appConfig.getCatalogs().findAuthorityConfiguration(id);
            catalogs = catalog != null ? Arrays.asList(catalog) : Collections.<CatalogConfiguration>emptyList();
        }
        ArrayList<CatalogDescriptor> result = new ArrayList<CatalogDescriptor>(catalogs.size());
        for (CatalogConfiguration cp : catalogs) {
            result.add(CatalogDescriptor.create(cp));
        }
        return new SmartGwtResponse<CatalogDescriptor>(result);
    }

    /**
     * Finds metadata in authority catalog.
     *
     * @param catalog catalog descriptor
     * @param fieldName id, surname, lastname
     * @param value value to query
     * @return list of metadata records
     */
    @Path(AuthorityCatalogResourceApi.FIND_PATH)
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public MetadataList findAuthority(
            @QueryParam(AuthorityCatalogResourceApi.FIND_CATALOG_PARAM) String catalog,
            @QueryParam(AuthorityCatalogResourceApi.FIND_FIELDNAME_PARAM) String fieldName,
            @QueryParam(AuthorityCatalogResourceApi.FIND_VALUE_PARAM) String value,
            @QueryParam(AuthorityCatalogResourceApi.FIELD_TYPE) Type type)
            throws TransformerException, IOException {

        MetadataList<MetadataItem> list = find(catalog, fieldName, value);
        List<AuthorityItem> authorities = new ArrayList<>();
        for (MetadataItem item : list.list) {
            switch (type) {
                case NAME:
                    if (item.getTitle().startsWith("N:")) {
                        AuthorityItem authority = new AuthorityItem(item, item.getTitle().substring(2));
                        authorities.add(authority);
                    }
                    break;
                case SUBJECT:
                    if (item.getTitle().startsWith("S:")) {
                        AuthorityItem authority = new AuthorityItem(item, item.getTitle().substring(2));
                        authorities.add(authority);
                    }
                    break;
                default:
                    AuthorityItem authority = new AuthorityItem(item, item.getTitle().substring(2));
                    authorities.add(authority);
            }
        }

        return new MetadataList<>(authorities);
    }

    public MetadataList find(String catalog, String fieldName, String value) throws TransformerException, IOException {

        List<Locale> acceptableLanguages = httpHeaders.getAcceptableLanguages();
        Locale locale = acceptableLanguages.isEmpty() ? null : acceptableLanguages.get(0);
        List<MetadataItem> result;
        BibliographicCatalog bCatalog = appConfig.getCatalogs().findCatalog(catalog);
        if (bCatalog != null) {
            try {
                result = bCatalog.find(fieldName, value, locale);
            } catch (ConnectException ex) {
                LOG.log(Level.FINE, catalog, ex);
                throw RestException.plainText(Status.SERVICE_UNAVAILABLE, ex.getLocalizedMessage());
            }
        } else {
            throw RestException.plainNotFound(AuthorityCatalogResourceApi.FIND_CATALOG_PARAM, catalog);
        }
        return new MetadataList<>(result);
    }


    @XmlAccessorType(XmlAccessType.FIELD)
    public static class CatalogDescriptor {

        public static CatalogDescriptor create(CatalogConfiguration cp) {
            List<CatalogQueryField> fields = cp.getQueryFields();
            ArrayList<FieldDescriptor> fieldDescriptors = new ArrayList<FieldDescriptor>(fields.size());
            for (CatalogQueryField field : fields) {
                fieldDescriptors.add(new FieldDescriptor(field.getName(), field.getTitle()));
            }
            return new CatalogDescriptor(cp.getId(), cp.getName(), fieldDescriptors);
        }

        @XmlElement(name = AuthorityCatalogResourceApi.CATALOG_ID)
        private String id;
        @XmlElement(name = AuthorityCatalogResourceApi.CATALOG_NAME)
        private String name;
        @XmlElement(name = AuthorityCatalogResourceApi.CATALOG_FIELDS)
        private List<FieldDescriptor> fields;

        public CatalogDescriptor(String id, String name, List<FieldDescriptor> fields) {
            this.id = id;
            this.name = name;
            this.fields = fields;
        }

        public CatalogDescriptor() {
        }

    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class FieldDescriptor {

        @XmlElement(name = AuthorityCatalogResourceApi.CATALOG_FIELD_ID)
        private String id;
        @XmlElement(name = AuthorityCatalogResourceApi.CATALOG_FIELD_TITLE)
        private String title;

        public FieldDescriptor() {
        }

        public FieldDescriptor(String id, String title) {
            this.id = id;
            this.title = title;
        }

    }

    /**
     * JAXB helper to list items.
     */
    @XmlRootElement(name = "metadataCatalogEntries")
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class MetadataList<E extends MetadataItem> {

        @XmlElement(name = "entry")
        List<E> list;

        public MetadataList() {
        }

        public MetadataList(List<E> list) {
            this.list = list;
        }

    }
}
