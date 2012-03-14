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
package cz.incad.pas.editor.server.config;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Enumeration;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.configuration.CompositeConfiguration;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;

/**
 * Server side configurations.
 *
 * @author Jan Pokorsky
 */
public final class PasConfiguration {
    
    public static final String USER_HOME = "user.home";
    public static final String CONFIG_FOLDER = "cz.incad.pas.editor.server.config.home";
    public static final String CONFIG_FOLDER_NAME = ".pas";
    public static final String CONFIG_FILE_NAME = "paseditor.cfg";

    /** Path to configuration folder.
     * Internal configuration property interpolated on init.
     * Accessible as {@code ${paseditor.home}} in properties files.
     */
    static final String PROPERTY_CONFIG_HOME = "paseditor.home";
    
    private static final Logger LOG = Logger.getLogger(PasConfiguration.class.getName());

    private String homePath;
    private File configHome;
    private final Map<String, String> environment;
    /** read only configuration */
    private final Configuration config;

    PasConfiguration(Map<String, String> environment) {
        this.environment = environment;
        this.config = new CompositeConfiguration();
        init((CompositeConfiguration) config);
    }

    public File getConfigHome() {
        return configHome;
    }

    Configuration getConfiguration() {
        return config;
    }

    private void init(CompositeConfiguration cc) {
        File home = initHome(environment.get(USER_HOME));
        this.homePath = home.getPath();
        this.configHome = initConfigFolder(home, environment.get(CONFIG_FOLDER));
        try {
            // envConfig contains inerpolated properties
            PropertiesConfiguration envConfig = new PropertiesConfiguration();
            envConfig.addProperty(PROPERTY_CONFIG_HOME, configHome.getPath());
            cc.addConfiguration(envConfig);
            // external configuration editable by users
            cc.addConfiguration(new PropertiesConfiguration(new File(configHome, CONFIG_FILE_NAME)));
            try {
                // bundled default configurations
                Enumeration<URL> resources = PasConfiguration.class.getClassLoader()
                        .getResources("cz/incad/pas/editor/server/config/paseditor.properties");
                for (URL resource; resources.hasMoreElements(); ) {
                    resource = resources.nextElement();
                    LOG.log(Level.FINE, "classpath config: {0}", resource);
                    cc.addConfiguration(new PropertiesConfiguration(resource));
                }
            } catch (IOException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        } catch (ConfigurationException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    private static File initHome(String home) {
        home = (home == null) ? "" : home;
        File homeFile = new File(home);
        if (!homeFile.exists()) {
            throw new IllegalStateException(String.format("Cannot find home folder: '%s'!", home));
        }
        if (!homeFile.isDirectory()) {
            throw new IllegalStateException(String.format("Not a folder: '%s'!", home));
        }
        if (!(homeFile.canRead() && homeFile.canWrite())) {
            throw new IllegalStateException(String.format("Invalid access permissions for: '%s'!", home));
        }

        return homeFile;
    }

    private static File initConfigFolder(File home, String configPath) {
        File config;
        if (configPath != null) {
            config = new File(configPath);
        } else {
            config = new File(home, CONFIG_FOLDER_NAME);
        }
        if (config.exists()) {
            if (!config.isDirectory()) {
                throw new IllegalStateException(String.format("Not a folder: '%s'!", home));
            }
            if (!(config.canRead() && config.canWrite())) {
                throw new IllegalStateException(String.format("Invalid access permissions for: '%s'!", home));
            }
        } else {
            if (!config.mkdir()) {
                throw new IllegalStateException(String.format("Cannot create folder: '%s'!", home));
            }
        }
        LOG.log(Level.INFO, "config folder: {0}", config);
        return config;
    }

}
