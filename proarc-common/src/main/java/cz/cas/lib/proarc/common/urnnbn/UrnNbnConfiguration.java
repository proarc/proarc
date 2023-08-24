/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.urnnbn;

import cz.cas.lib.proarc.urnnbn.ResolverClient;
import org.apache.commons.configuration.Configuration;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

/**
 * The configuration of URN:NBN resolvers.
 *
 * @author Jan Pokorsky
 */
public class UrnNbnConfiguration {

    static final String PROPERTY_RESOLVERS = "urnnbn.resolvers";
    static final String PREFIX_RESOLVER = "urnnbn.resolver";

    private static final Logger LOG = Logger.getLogger(UrnNbnConfiguration.class.getName());

    private final Configuration config;

    public UrnNbnConfiguration(Configuration config) {
        this.config = config;
    }

    public ResolverClient getClient(ResolverConfiguration conf) {
        if (conf != null) {
            return new ResolverClient(conf.getUrl(), conf.getRegistrar(),
                    conf.getArchiver(), conf.getUser(), conf.getPasswd());
        }
        return null;
    }

    public ResolverClient findClient(String resolverId) {
        ResolverConfiguration conf = findResolverConfiguration(resolverId);
        return getClient(conf);
    }

    public ResolverConfiguration findResolverConfiguration(String resolverId) {
        String[] resolverIds = config.getStringArray(PROPERTY_RESOLVERS);
        List<ResolverConfiguration> confs = new ArrayList<ResolverConfiguration>();
        for (String rid : resolverIds) {
            if (rid.equals(resolverId)) {
                ResolverConfiguration resConf = new ResolverConfiguration(
                        rid, config.subset(PREFIX_RESOLVER + '.' + rid));
                return valid(resConf) ? resConf : null;
            }
        }
        return null;
    }

    public List<ResolverConfiguration> getResolverConfigurations() {
        String[] resolverIds = config.getStringArray(PROPERTY_RESOLVERS);
        List<ResolverConfiguration> confs = new ArrayList<ResolverConfiguration>();
        for (String resolverId : resolverIds) {
            ResolverConfiguration resConf = new ResolverConfiguration(
                    resolverId, config.subset(PREFIX_RESOLVER + '.' + resolverId));
            if (valid(resConf)) {
                confs.add(resConf);
            }
        }
        return confs;
    }

    private boolean valid(ResolverConfiguration conf) {
        boolean ok = true;
        if (conf.getRegistrar() == null) {
            warning(conf, ResolverConfiguration.PROPERTY_REGISTRAR);
            ok = false;
        }
        if (conf.getUrl() == null) {
            warning(conf, ResolverConfiguration.PROPERTY_URL);
            ok = false;
        } else if (conf.getUrl().contains("v3")) {
            LOG.warning(String.format("Unsupported %s.%s.%s in proarc.cfg - required URL version 4", PREFIX_RESOLVER, conf.getId(), ResolverConfiguration.PROPERTY_URL));
            ok = false;
        }
        if (conf.getUser()== null) {
            warning(conf, ResolverConfiguration.PROPERTY_USER);
            ok = false;
        }
        if (conf.getPasswd()== null) {
            warning(conf, ResolverConfiguration.PROPERTY_PASSWD);
            ok = false;
        }
        return ok;
    }

    private void warning(ResolverConfiguration conf, String propertyName) {
        LOG.warning(String.format("Missing %s.%s.%s in proarc.cfg",
                PREFIX_RESOLVER, conf.getId(), propertyName));
    }

    public static class ResolverConfiguration {

        static final String PROPERTY_ARCHIVER = "archiver";
        static final String PROPERTY_PASSWD = "passwd";
        static final String PROPERTY_REGISTRAR = "registrar";
        static final String PROPERTY_TITLE = "title";
        static final String PROPERTY_USER = "user";
        static final String PROPERTY_URL = "url";

        private final String id;
        private final Configuration config;

        public ResolverConfiguration(String id, Configuration config) {
            this.id = id;
            this.config = config;
        }

        public String getId() {
            return id;
        }

        public String getPasswd() {
            return config.getString(PROPERTY_PASSWD);
        }

        public String getRegistrar() {
            return config.getString(PROPERTY_REGISTRAR);
        }

        public Long getArchiver() {
            return config.getLong(PROPERTY_ARCHIVER, null);
        }

        public String getTitle() {
            return config.getString(PROPERTY_TITLE, getId());
        }

        public String getUrl() {
            return config.getString(PROPERTY_URL);
        }

        public String getUser() {
            return config.getString(PROPERTY_USER);
        }

    }

}
