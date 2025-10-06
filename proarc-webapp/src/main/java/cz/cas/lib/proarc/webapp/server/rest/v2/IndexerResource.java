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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.server.rest.v2;

import com.google.gwt.http.client.Request;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.widget.UserRole;
import cz.cas.lib.proarc.webapp.server.rest.SmartGwtResponse;
import cz.cas.lib.proarc.webapp.server.rest.v1.IndexerResourceV1;
import cz.cas.lib.proarc.webapp.shared.rest.IndexerResourceApi;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.FormParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.ERR_NO_PERMISSION;
import static cz.cas.lib.proarc.webapp.server.rest.UserPermission.hasPermission;

@Path(RestConfig.URL_API_VERSION_2 + "/" + IndexerResourceApi.PATH)
public class IndexerResource extends IndexerResourceV1 {

    private static final Logger LOG = Logger.getLogger(IndexerResource.class.getName());

    public IndexerResource(
            @Context Request httpRequest,
            @Context SecurityContext securityContext,
            @Context HttpHeaders httpHeaders,
            @Context UriInfo uriInfo,
            @Context HttpServletRequest httpServletRequest) throws AppConfigurationException {
        super(httpRequest, securityContext, httpHeaders, uriInfo, httpServletRequest);
    }

    @POST
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> indexObjects () {

        if (!hasPermission(user, UserRole.PERMISSION_SOLR_FUNCTION, UserRole.PERMISSION_SYS_ADMIN_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }

        try {
            return super.indexObjects();
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(IndexerResourceApi.INDEX_PARENT_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> setParentsPid () {

        if (!hasPermission(user, UserRole.PERMISSION_SYS_ADMIN_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }

        try {
            return super.setParentsPid();
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }

    @POST
    @Path(IndexerResourceApi.OBJECT_PATH)
    @Produces({MediaType.APPLICATION_JSON})
    public SmartGwtResponse<SearchViewItem> indexDocument (
            @FormParam(IndexerResourceApi.DIGITALOBJECT_PID) String pid
    ) {
        if (!hasPermission(user, UserRole.PERMISSION_SOLR_FUNCTION)) {
            return SmartGwtResponse.asError(returnLocalizedMessage(ERR_NO_PERMISSION));
        }

        try {
            return super.indexDocument(pid);
        } catch (Throwable t) {
            LOG.log(Level.SEVERE, t.getMessage(), t);
            return SmartGwtResponse.asError(t);
        }
    }
}
