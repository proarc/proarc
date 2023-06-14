/*
 * Copyright (C) 2023 Lukas Sykora
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
package cz.cas.lib.proarc.webapp.server.rest.v2;

import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.AuthorityCatalogResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.AuthorityCatalogResourceApi;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;

/**
 * The resource to list available authorities catalogs like Aleph
 * and to query them for metadata.
 *
 * @author Lukáš Sýkora
 */
@Path(RestConfig.URL_API_VERSION_2 + "/" + AuthorityCatalogResourceApi.PATH)
public class AuthorityCatalogResource extends AuthorityCatalogResourceV1 {

    private static final Logger LOG = Logger.getLogger(AuthorityCatalogResource.class.getName());

    public AuthorityCatalogResource(
            @Context HttpHeaders httpHeaders
    ) throws AppConfigurationException {
        super(httpHeaders);
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<CatalogDescriptor> findAuthorityCatalog(
            @QueryParam(AuthorityCatalogResourceApi.CATALOG_ID) String id) {
        try {
            return super.findAuthorityCatalog(id);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @Path(AuthorityCatalogResourceApi.FIND_PATH)
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public MetadataList findAuthority(
            @QueryParam(AuthorityCatalogResourceApi.FIND_CATALOG_PARAM) String catalog,
            @QueryParam(AuthorityCatalogResourceApi.FIND_FIELDNAME_PARAM) String fieldName,
            @QueryParam(AuthorityCatalogResourceApi.FIND_VALUE_PARAM) String value,
            @QueryParam(AuthorityCatalogResourceApi.FIELD_TYPE) Type type
    ) {
        try {
            return super.findAuthority(catalog, fieldName, value, type);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return new MetadataList();
        }
    }
}
