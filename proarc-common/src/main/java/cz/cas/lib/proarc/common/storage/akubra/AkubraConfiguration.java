package cz.cas.lib.proarc.common.storage.akubra;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.URL;
import java.util.Enumeration;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.configuration.CompositeConfiguration;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.commons.configuration.reloading.FileChangedReloadingStrategy;

import static cz.cas.lib.proarc.common.config.AppConfiguration.initConfigFolder;

public class AkubraConfiguration {

    private static final Logger LOG = Logger.getLogger(AkubraConfiguration.class.getName());
    private static final String DEFAULT_PROPERTIES_RESOURCE = "cz/cas/lib/proarc/common/config/akubra.properties";

    public static final String PROPERTY_USER_HOME = "user.home";
    public static final String PROPERTY_APP_HOME = "proarc.home";
    public static final String CONFIG_FILE_NAME = "akubra.cfg";

    private Configuration config;
    private File configHome;
    private Map<String, String> environment;

    public AkubraConfiguration(Map<String, String> environment, File configHome) throws IOException {
        this.environment = environment;
        this.config = init();
        this.configHome = configHome;
    }

    private Configuration init() throws IOException {
        this.configHome = initConfigFolder(environment.get(PROPERTY_USER_HOME), environment.get(PROPERTY_APP_HOME));
        return buildConfiguration(new File(configHome, CONFIG_FILE_NAME));
    }

    private Configuration buildConfiguration(File cfgFile) {
        CompositeConfiguration cc = new CompositeConfiguration();
        buildConfiguration(cc, cfgFile);
        return cc;
    }

    public Configuration getConfiguration() {
        return this.config;
    }

    private void buildConfiguration(CompositeConfiguration cc, File cfgFile) {
        try {
            // envConfig contains interpolated properties
            PropertiesConfiguration envConfig = new PropertiesConfiguration();
            envConfig.addProperty(PROPERTY_APP_HOME, configHome.getPath());
            cc.addConfiguration(envConfig);
            // external configuration editable by users; UTF-8 expected
            PropertiesConfiguration external = new PropertiesConfiguration();
            external.setEncoding("UTF-8");
            FileChangedReloadingStrategy reloading = new FileChangedReloadingStrategy();
            external.setReloadingStrategy(reloading);
            external.setFile(cfgFile);
            cc.addConfiguration(external);
            try {
                // bundled default configurations
                Enumeration<URL> resources = AppConfiguration.class.getClassLoader()
                        .getResources(DEFAULT_PROPERTIES_RESOURCE);
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

    /**
     * Copies default properties as akubra.cfg.template.
     *
     * @throws IOException
     * @throws ConfigurationException
     */
    public void copyConfigTemplate() throws AppConfigurationException {
        try {
            copyConfigTemplateImpl();
        } catch (IOException ex) {
            throw new AppConfigurationException(String.valueOf(configHome), ex);
        }
    }

    public void copyConfigTemplateImpl() throws IOException {
        File cfgFile = new File(configHome, CONFIG_FILE_NAME + ".template");
        if (!cfgFile.exists() || cfgFile.exists() && cfgFile.isFile() && cfgFile.canWrite()) {
            Enumeration<URL> resources = AppConfiguration.class.getClassLoader()
                    .getResources(DEFAULT_PROPERTIES_RESOURCE);
            URL lastResource = null;
            while (resources.hasMoreElements()) {
                URL url = resources.nextElement();
                lastResource = url;
                System.out.println(url.toExternalForm());
            }

            if (lastResource == null) {
                throw new IllegalStateException(DEFAULT_PROPERTIES_RESOURCE);
            }
            InputStream resource = lastResource.openStream();
            BufferedReader reader = new BufferedReader(new InputStreamReader(resource, "ISO-8859-1"));
            try {
                // we need platform dependent line separator => PrintWriter
                PrintWriter writer = new PrintWriter(cfgFile, "UTF-8");
                try {
                    for (String line; (line = reader.readLine()) != null;) {
                        writer.println(line);
                    }
                    writer.println();
                } finally {
                    writer.close();
                }
            } finally {
                reader.close();
            }

        }
    }

    public String getSolrProcessingHost() {
        return config.getString("solrProcessingHost");
    }

    public String getSolrSearchHost() {
        return config.getString("solrSearchHost");
    }

    public String getSolrSearchLogin() {
        return config.getString("solrSearchLogin");
    }

    public String getSolrSearchPassword() {
        return config.getString("solrSearchPassword");
    }

    public String getObjectStorePath() {
        return config.getString("objectStore.path");
    }

    public String getObjectStorePattern() {
        return config.getString("objectStore.pattern");
    }

    public String getDatastreamStorePath() {
        return config.getString("datastreamStore.path");
    }

    public String getDatastreamStorePattern() {
        return config.getString("datastreamStore.pattern");
    }
}
