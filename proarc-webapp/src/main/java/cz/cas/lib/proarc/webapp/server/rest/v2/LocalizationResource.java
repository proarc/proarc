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

import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.webapp.server.rest.ProArcResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.LocalizationResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.LocalizationResourceApi;
import jakarta.ws.rs.DefaultValue;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.URL_API_VERSION_2;

/**
 *
 * @author Lukas Sykora
 */
@Path(URL_API_VERSION_2 + "/" + LocalizationResourceApi.PATH)
public class LocalizationResource extends LocalizationResourceV1 {

    private static final Logger LOG = Logger.getLogger(LocalizationResource.class.getName());

    public LocalizationResource(
            @Context HttpHeaders httpHeaders
            ) {
        super(httpHeaders);
    }

    @GET
    @Produces({MediaType.APPLICATION_JSON})
    public ProArcResponse<Item> getBundle(
            @QueryParam(LocalizationResourceApi.ITEM_BUNDLENAME) Set<BundleName> bundleNames,
            @DefaultValue("")
            @QueryParam(LocalizationResourceApi.GETBUNDLE_LOCALE_PARAM) String locale,
            @DefaultValue("true")
            @QueryParam(LocalizationResourceApi.GETBUNDLE_SORTED_PARAM) boolean sorted
    ) {
        try {
            return super.getBundle(bundleNames, locale, sorted);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return ProArcResponse.asError(t);
        }
    }
}
