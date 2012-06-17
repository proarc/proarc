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
import java.io.FileNotFoundException;
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
    
    public static final String ENV_USER_HOME = "user.home";
    /** environment property to declare nonstandard application home */
    public static final String ENV_APP_HOME = "proarc.home";
    public static final String DEFAULT_APP_HOME_NAME = ".proarc";
    public static final String CONFIG_FILE_NAME = "proarc.cfg";

    /** Path to configuration folder.
     * Internal configuration property interpolated on init.
     * Accessible as {@code ${proarc.home}} in properties files.
     */
    static final String PROPERTY_APP_HOME = "proarc.home";
    
    private static final Logger LOG = Logger.getLogger(PasConfiguration.class.getName());

    private String homePath;
    private File configHome;
    private final Map<String, String> environment;
    /** read only configuration */
    private final Configuration config;

    PasConfiguration(Map<String, String> environment) throws IOException {
        this.environment = environment;
        this.config = new CompositeConfiguration();
        init((CompositeConfiguration) config);
    }

    /**
     * Gets default target folder for newly created user home folders.
     */
    public File getDefaultUsersHome() throws IOException {
        String path = config.getString("proarc.users.home");
        File users = new File(path);
        if (!checkFile(users, false, true, true, true)) {
            users.mkdirs();
        }
        return users;
    }

    public String getFedoraUsername() {
        return config.getString("fedora.client.username");
    }

    public String getFedoraPassword() {
        return config.getString("fedora.client.password");
    }

    public String getFedoraUrl() {
        return config.getString("fedora.client.url");
    }

    public File getConfigHome() {
        return configHome;
    }

    Configuration getConfiguration() {
        return config;
    }

    private void init(CompositeConfiguration cc) throws IOException {
        File home = initHome(environment.get(ENV_USER_HOME));
        this.homePath = home.getPath();
        this.configHome = initConfigFolder(home, environment.get(ENV_APP_HOME));
        try {
            // envConfig contains inerpolated properties
            PropertiesConfiguration envConfig = new PropertiesConfiguration();
            envConfig.addProperty(PROPERTY_APP_HOME, configHome.getPath());
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

    private static File initHome(String home) throws IOException {
        home = (home == null) ? "" : home;
        File homeFile = new File(home);
        checkFile(homeFile, true, true, true, true);
        return homeFile;
    }

    private static File initConfigFolder(File home, String configPath) throws IOException {
        File config;
        if (configPath != null) {
            config = new File(configPath);
        } else {
            config = new File(home, DEFAULT_APP_HOME_NAME);
        }
        if (!checkFile(config, false, true, true, true)) {
            config.mkdir();
        }
        LOG.log(Level.FINE, "config folder: {0}", config);
        return config;
    }

    /**
     * checks file/folder parameters
     * @return {@code true} iff {@code f} exists
     */
    private static boolean checkFile(File f, boolean mustExist,
            Boolean expectDirectory, Boolean expectCanRead, Boolean expextCanWrite
            ) throws IOException {

        if (f.exists()) {
            if (expectDirectory != null) {
                if (expectDirectory && !f.isDirectory()) {
                    throw new IOException(String.format("Not a folder: '%s'!", f));
                } else if (!expectDirectory && f.isDirectory()) {
                    throw new IOException(String.format("Not a file: '%s'!", f));
                }
            }
            if (expectCanRead != null) {
                if (expectCanRead != f.canRead()) {
                    throw new IOException(String.format("Invalid read permission (=%s) for: '%s'!", !expectCanRead, f));
                }
            }
            if (expextCanWrite != null) {
                if (expextCanWrite != f.canWrite()) {
                    throw new IOException(String.format("Invalid write permission (=%s) for: '%s'!", !expextCanWrite, f));
                }
            }
            return true;
        } else if (mustExist) {
            if (expectDirectory != null && expectDirectory) {
                throw new FileNotFoundException(String.format("Folder '%s' not founf!", f));
            } else {
                throw new FileNotFoundException(String.format("File '%s' not founf!", f));
            }
        }
        return false;
    }

}
