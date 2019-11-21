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
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.urnnbn.UrnNbnConfiguration;
import cz.cas.lib.proarc.webapp.shared.rest.UrnNbnResourceApi;

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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

/**
 *
 * @author Lukas Sykora
 */
@Path(UrnNbnResourceApi.PATH)
public class UrnNbnResource {

    private static final Logger LOG = Logger.getLogger(UrnNbnResource.class.getName());
    private final HttpHeaders httpHeaders;
    private final AppConfiguration appConfig;

    public UrnNbnResource(
            @Context HttpHeaders httpHeaders
    ) throws AppConfigurationException {
        this.httpHeaders = httpHeaders;
        this.appConfig = AppConfigurationFactory.getInstance().defaultInstance();
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public SmartGwtResponse<UrnNbnResource.ResolverDescriptor> findCatalog(
            @QueryParam(UrnNbnResourceApi.RESOLVER_ID) String id) {

        List<UrnNbnConfiguration.ResolverConfiguration> resolvers;
        if (id == null) {
            resolvers = appConfig.getUrnNbnConfiguration().getResolverConfigurations();
        } else {
            List<UrnNbnConfiguration.ResolverConfiguration> listOfResolvers = appConfig.getUrnNbnConfiguration().getResolverConfigurations();
            UrnNbnConfiguration.ResolverConfiguration resolver = findResolver(listOfResolvers);
            resolvers = resolver != null ? Arrays.asList(resolver) : Collections.<UrnNbnConfiguration.ResolverConfiguration>emptyList();
        }
        ArrayList<UrnNbnResource.ResolverDescriptor> result = new ArrayList<>(resolvers.size());
        for (UrnNbnConfiguration.ResolverConfiguration rc : resolvers) {
            result.add(UrnNbnResource.ResolverDescriptor.create(rc));
        }
        return new SmartGwtResponse<UrnNbnResource.ResolverDescriptor>(result);
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
