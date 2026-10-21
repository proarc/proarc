/*
 * Copyright (C) 2026 Lukas Sykora
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
package cz.cas.lib.proarc.webapp.server.rest.v2;

import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.webapp.server.rest.v1.StatisticsResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.StatisticsResourceApi;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.URL_API_VERSION_2;

@Path(URL_API_VERSION_2 + "/" + StatisticsResourceApi.PATH)
public class StatisticsResource extends StatisticsResourceV1 {
    public StatisticsResource(@Context HttpHeaders httpHeaders,
            @Context HttpServletRequest httpServletRequest) throws AppConfigurationException {
        super(httpHeaders, httpServletRequest);
    }
}
