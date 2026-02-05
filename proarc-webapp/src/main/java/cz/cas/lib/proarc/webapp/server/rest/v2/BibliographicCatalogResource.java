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
import cz.cas.lib.proarc.webapp.server.rest.ProArcResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.BibliographicCatalogResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.BibliographicCatalogResourceApi;
import jakarta.ws.rs.DefaultValue;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import java.util.logging.Level;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.URL_API_VERSION_2;

/**
 * The resource to list available bibliographic catalogs like Aleph
 * and registrdigitalizace.cz and to query them for metadata.
 *
 * @author Lukas Sykora
 */
@Path(URL_API_VERSION_2 + "/" + BibliographicCatalogResourceApi.PATH)
public class BibliographicCatalogResource extends BibliographicCatalogResourceV1 {

    private static final Logger LOG = Logger.getLogger(BibliographicCatalogResource.class.getName());

    public BibliographicCatalogResource(
            @Context HttpHeaders httpHeaders
    ) throws AppConfigurationException {
        super(httpHeaders);
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public ProArcResponse<CatalogDescriptor> findCatalog(
            @QueryParam(BibliographicCatalogResourceApi.CATALOG_ID) String id,
            @DefaultValue("false") @QueryParam(BibliographicCatalogResourceApi.CATALOG_ALLOW_UPDATE) Boolean allowCatalogUpdate
    ) {
        try {
            return super.findCatalog(id, allowCatalogUpdate);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }

    @Path(BibliographicCatalogResourceApi.FIND_PATH)
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public MetadataList find(
            @QueryParam(BibliographicCatalogResourceApi.FIND_CATALOG_PARAM) String catalog,
            @QueryParam(BibliographicCatalogResourceApi.FIND_FIELDNAME_PARAM) String fieldName,
            @QueryParam(BibliographicCatalogResourceApi.FIND_VALUE_PARAM) String value
    ) {
        try {
            return super.find(catalog, fieldName, value);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return new MetadataList();
        }
    }
}
