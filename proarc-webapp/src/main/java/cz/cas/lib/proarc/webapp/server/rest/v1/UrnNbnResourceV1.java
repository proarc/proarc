/*
 * Copyright (C) 2019 Lukas Sykora
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
package cz.cas.lib.proarc.webapp.server.rest.v1;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.urnnbn.UrnNbnConfiguration;
import cz.cas.lib.proarc.webapp.server.rest.ProArcResponse;
import cz.cas.lib.proarc.webapp.shared.rest.UrnNbnResourceApi;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.HttpHeaders;
import jakarta.ws.rs.core.MediaType;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.webapp.server.rest.RestConsts.URL_API_VERSION_1;

/**
 *
 * @author Lukas Sykora
 */
@Deprecated
@Path(URL_API_VERSION_1 + "/" + UrnNbnResourceApi.PATH)
public class UrnNbnResourceV1 {

    private static final Logger LOG = Logger.getLogger(UrnNbnResourceV1.class.getName());
    private final HttpHeaders httpHeaders;
    private final AppConfiguration appConfig;

    public UrnNbnResourceV1(
            @Context HttpHeaders httpHeaders
    ) throws AppConfigurationException {
        this.httpHeaders = httpHeaders;
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public ProArcResponse<ResolverDescriptor> findCatalog(
            @QueryParam(UrnNbnResourceApi.RESOLVER_ID) String id) {

        List<UrnNbnConfiguration.ResolverConfiguration> resolvers;
        if (id == null) {
            resolvers = appConfig.getUrnNbnConfiguration().getResolverConfigurations();
        } else {
            List<UrnNbnConfiguration.ResolverConfiguration> listOfResolvers = appConfig.getUrnNbnConfiguration().getResolverConfigurations();
            UrnNbnConfiguration.ResolverConfiguration resolver = findResolver(listOfResolvers);
            resolvers = resolver != null ? Arrays.asList(resolver) : Collections.<UrnNbnConfiguration.ResolverConfiguration>emptyList();
        }
        ArrayList<UrnNbnResourceV1.ResolverDescriptor> result = new ArrayList<>(resolvers.size());
        for (UrnNbnConfiguration.ResolverConfiguration rc : resolvers) {
            result.add(UrnNbnResourceV1.ResolverDescriptor.create(rc));
        }
        return new ProArcResponse<ResolverDescriptor>(result);
    }

    private UrnNbnConfiguration.ResolverConfiguration findResolver(List<UrnNbnConfiguration.ResolverConfiguration> listOfResolvers) {
        return null;
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class ResolverDescriptor {

        public static ResolverDescriptor create(UrnNbnConfiguration.ResolverConfiguration rc) {
            return new ResolverDescriptor(rc.getId(), rc.getTitle());
        }

        @XmlElement(name = UrnNbnResourceApi.RESOLVER_ID)
        private String id;
        @XmlElement(name = UrnNbnResourceApi.RESOLVER_NAME)
        private String name;

        public ResolverDescriptor(String id, String name) {
            this.id = id;
            this.name = name;
        }
    }
}
