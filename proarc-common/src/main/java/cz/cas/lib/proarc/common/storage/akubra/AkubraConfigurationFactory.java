/*
 * Copyright (C) 2022 Lukas Sykora
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
package cz.cas.lib.proarc.common.storage.akubra;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Lukas Sykora
 */
public final class AkubraConfigurationFactory {

    private static final Logger LOG = Logger.getLogger(AkubraConfigurationFactory.class.getName());
    private static final AkubraConfigurationFactory INSTANCE = new AkubraConfigurationFactory();

    private AkubraConfiguration defaultInstance;

    public static AkubraConfigurationFactory getInstance() {
        return INSTANCE;
    }

    public AkubraConfiguration create(File configHome) throws AppConfigurationException {
        return create(new HashMap<String, String>(), configHome);
    }

    /**
     * Creates configuration of the application. The lookup of default properties
     * searches the passed map, system properties and system environment.
     *
     * @param defaults   properties to override defaults.
     * @param configHome
     * @return the configuration
     * @throws AppConfigurationException
     */
    public AkubraConfiguration create(Map<String, String> defaults, File configHome) throws AppConfigurationException {
        readParameter(AppConfiguration.PROPERTY_USER_HOME, null, defaults);
        readParameter(AppConfiguration.PROPERTY_APP_HOME, AppConfiguration.ENV_APP_HOME, defaults);
        AkubraConfiguration pc;
        try {
            pc = new AkubraConfiguration(defaults, configHome);
        } catch (IOException ex) {
            throw new AppConfigurationException(ex);
        }
        return pc;
    }

    /** XXX replace with guice */
    public AkubraConfiguration defaultInstance(File configHome) throws AppConfigurationException {
        if (defaultInstance == null) {
            defaultInstance = create(configHome);
        }
        return defaultInstance;
    }

    public void setDefaultInstance(AkubraConfiguration config) {
        this.defaultInstance = config;
    }

    private static void readParameter(String name, String envName, Map<String, String> env) {
        if (env.get(name) != null) {
            return ;
        }
        String val = null;
        if (name != null) {
            val = System.getProperty(name);
        }
        if (val == null && envName != null) {
            val = System.getenv(envName);
        }
        if (val != null) {
            env.put(name, val);
            LOG.log(Level.INFO, "Parameter {0}: {1}", new Object[]{name, val});
        }
    }

}
