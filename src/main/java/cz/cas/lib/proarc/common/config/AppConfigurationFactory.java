/*
 * Copyright (C) 2012 Jan Pokorsky
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
package cz.cas.lib.proarc.common.config;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.ServletContext;

/**
 *
 * @author Jan Pokorsky
 */
public final class AppConfigurationFactory {

    private static final Logger LOG = Logger.getLogger(AppConfigurationFactory.class.getName());
    private static final AppConfigurationFactory INSTANCE = new AppConfigurationFactory();

    private AppConfiguration defaultInstance;

    public static AppConfigurationFactory getInstance() {
        return INSTANCE;
    }

    public AppConfiguration create() throws AppConfigurationException {
        return create(new HashMap<String, String>());
    }

    /**
     * Creates configuration of the application. The lookup of default properties
     * searches servlet init parameters, system properties and system environment.
     *
     * @param ctx servlet context
     * @return the configuration
     * @throws AppConfigurationException
     */
    public AppConfiguration create(ServletContext ctx) throws AppConfigurationException {
        Map<String, String> env = new HashMap<String, String>();
        readServletParameter(AppConfiguration.PROPERTY_APP_HOME, ctx, env);
        return create(env);
    }

    /**
     * Creates configuration of the application. The lookup of default properties
     * searches the passed map, system properties and system environment.
     * 
     * @param defaults properties to override defaults.
     * @return the configuration
     * @throws AppConfigurationException
     */
    public AppConfiguration create(Map<String, String> defaults) throws AppConfigurationException {
        readParameter(AppConfiguration.PROPERTY_USER_HOME, null, defaults);
        readParameter(AppConfiguration.PROPERTY_APP_HOME, AppConfiguration.ENV_APP_HOME, defaults);
        AppConfiguration pc;
        try {
            pc = new AppConfiguration(defaults);
        } catch (IOException ex) {
            throw new AppConfigurationException(ex);
        }
        return pc;
    }

    /** XXX replace with guice */
    public AppConfiguration defaultInstance() throws AppConfigurationException {
        if (defaultInstance == null) {
            defaultInstance = create();
        }
        return defaultInstance;
    }

    public void setDefaultInstance(AppConfiguration config) {
        this.defaultInstance = config;
    }

    private static String readServletParameter(String name, ServletContext ctx, Map<String, String> env) {
        String val = ctx.getInitParameter(name);
        if (val != null) {
            LOG.log(Level.INFO, "Init parameter {0}: {1}", new Object[]{name, val});
            env.put(name, val);
        }
        return val;
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
