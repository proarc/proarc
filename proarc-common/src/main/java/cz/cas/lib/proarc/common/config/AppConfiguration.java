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

import cz.cas.lib.proarc.common.client.ClientOptions;
import cz.cas.lib.proarc.common.jobs.JobHandler;
import cz.cas.lib.proarc.common.kramerius.KrameriusOptions;
import cz.cas.lib.proarc.common.object.ndk.ModsRules;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.ndk.RdaRules;
import cz.cas.lib.proarc.common.process.export.ExportParams;
import cz.cas.lib.proarc.common.process.export.Kramerius4ExportOptions;
import cz.cas.lib.proarc.common.process.export.KwisExportOptions;
import cz.cas.lib.proarc.common.process.export.archive.ArchiveExportOptions;
import cz.cas.lib.proarc.common.process.export.desa.DesaServices;
import cz.cas.lib.proarc.common.process.export.mets.NdkExportOptions;
import cz.cas.lib.proarc.common.process.imports.ImportProfile;
import cz.cas.lib.proarc.common.storage.SearchOptions;
import cz.cas.lib.proarc.common.storage.Storage;
import cz.cas.lib.proarc.common.urnnbn.UrnNbnConfiguration;
import cz.cas.lib.proarc.common.workflow.WorkflowOptions;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.URL;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.configuration.CompositeConfiguration;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.commons.configuration.reloading.FileChangedReloadingStrategy;

import static cz.cas.lib.proarc.common.process.imports.ImportProfile.GENERATE_PDFA_PROCESSOR;

/**
 * Server side configurations.
 *
 * @author Jan Pokorsky
 */
public final class AppConfiguration {

    static {
        String iv = AppConfiguration.class.getPackage().getImplementationVersion();
        VERSION = iv == null ? "Unknown" : iv;
    }

    public static final String PROPERTY_USER_HOME = "user.home";
    /**
     * environment variable name to override default application home*
     */
    public static final String ENV_APP_HOME = "PROARC_HOME";
    public static final String DEFAULT_APP_HOME_NAME = ".proarc";
    public static final String CONFIG_FILE_NAME = "proarc.cfg";
    /**
     * The implementation version. E.g. {@code 1.0}
     */
    public static final String VERSION;

    public static final int MAX_SESSION_INTERVAL = 60 * 15; // 60 sekund * 15 minut

    /**
     * Path to configuration folder.
     * Internal configuration property interpolated on init.
     * Accessible as {@code ${proarc.home}} in properties files.
     */
    public static final String PROPERTY_APP_HOME = "proarc.home";
    private static final String PROPERTY_DIGOBJECT_PLUGINS = "digobject.plugins";
    private static final String PROPERTY_FEDORA_CLIENT_PASSWORD = "fedora.client.password";
    private static final String PROPERTY_FEDORA_CLIENT_URL = "fedora.client.url";
    private static final String PROPERTY_FEDORA_CLIENT_USERNAME = "fedora.client.username";
    private static final String PROPERTY_USERS_HOME = "proarc.users.home";
    private static final String PROPERTY_STORAGE = "proarc.storage";
    private static final String PROPERTY_SESSION_TIMEOUT = "proarc.sessionTimeout";
    public static final String EXPORT_KWIS_POST_PROCESSOR = "export.export_post_processor.processor";
    public static final String EXPORT_BAGIT_POST_PROCESSOR = "export.bagit_post_processor.processor";
    public static final String EXPORT_BAGIT_PATH = "export.bagit.path";
    public static final String EXPORT_BAGIT_FOXML_PATH = "export.bagitFoxml.path";
    public static final String EXPORT_LTP_CESNET_POST_PROCESSOR = "export.ltp_cesnet_post_processor.processor";
    public static final String EXPORT_LTP_CESNET_GROUP_TOKEN = "export.ltpCesnet.groupToken";
    public static final String EXPORT_LTP_CESNET_SCRIPT_PATH = "export.ltpCesnet.scriptPath";
    public static final String EXPORT_BAGIT_SCRIPT_PATH = "export.bagit.scriptPath";
//    public static final String PROPERTY_NEW_CLIENT_URL = "proarc.client.url";

    private static final Logger LOG = Logger.getLogger(AppConfiguration.class.getName());
    private static final String DEFAULT_PROPERTIES_RESOURCE = "cz/cas/lib/proarc/common/config/proarc.properties";

    private File configHome;
    private final Map<String, String> environment;
    /**
     * read only configuration
     */
    private final Configuration config;
    private final Map<ConfigurationProfile, Configuration> profileConfigCache =
            new HashMap<ConfigurationProfile, Configuration>();
    private final Profiles profiles;

    AppConfiguration(Map<String, String> environment) throws IOException {
        this.environment = environment;
        this.config = init();
        this.profiles = new Profiles(config, configHome);
    }

    /**
     * Gets default target folder for newly created user home folders.
     */
    public File getDefaultUsersHome() throws IOException {
        String path = config.getString(PROPERTY_USERS_HOME);
        File users = new File(path);
        if (!checkFile(users, false, true, true, true)) {
            users.mkdirs();
        }
        return users;
    }

    public int getMaxSessionTime() {
        int value = config.getInt(PROPERTY_SESSION_TIMEOUT, MAX_SESSION_INTERVAL);
        return value;
    }

    public Storage getTypeOfStorage() {
        Storage storage = Storage.fromString(config.getString(PROPERTY_STORAGE));
        if (storage == null) {
            throw new IllegalStateException("Unsupported value " + config.getString(PROPERTY_STORAGE) + " in element " + PROPERTY_STORAGE);
        }
        return storage;
    }

    public Configuration getExportPostProcessor() {
        String processor = config.getString(EXPORT_KWIS_POST_PROCESSOR, "-");
        return config.subset(ImportProfile.PROCESSOR + "." + processor);
    }

    public Configuration getPdfAGeneratorProcessor() {
        String processor = config.getString(GENERATE_PDFA_PROCESSOR, "-");
        return config.subset(ImportProfile.PROCESSOR + "." + processor);
    }

    public Configuration getBagitExportPostProcessor() {
        String processor = config.getString(EXPORT_BAGIT_POST_PROCESSOR, "-");
        return config.subset(ImportProfile.PROCESSOR + "." + processor);
    }

    public String getBagitExportPath() {
        return config.getString(EXPORT_BAGIT_PATH);
    }

    public String getBagitFoxmlExportPath() {
        return config.getString(EXPORT_BAGIT_FOXML_PATH);
    }

    public Configuration getLtpCesnetExportPostProcessor() {
        String processor = config.getString(EXPORT_LTP_CESNET_POST_PROCESSOR, "-");
        return config.subset(ImportProfile.PROCESSOR + "." + processor);
    }

    public String getLtpCesnetGroupToken() {
        return config.getString(EXPORT_LTP_CESNET_GROUP_TOKEN);
    }

    public String getLtpCesnetScriptPath() {
        return config.getString(EXPORT_LTP_CESNET_SCRIPT_PATH);
    }

    public String getBagitScriptPath() {
        return config.getString(EXPORT_BAGIT_SCRIPT_PATH);
    }

    public String getFedoraUsername() {
        return config.getString(PROPERTY_FEDORA_CLIENT_USERNAME);
    }

    public String getFedoraPassword() {
        return config.getString(PROPERTY_FEDORA_CLIENT_PASSWORD);
    }

    public String getFedoraUrl() {
        return config.getString(PROPERTY_FEDORA_CLIENT_URL);
    }

    public Catalogs getCatalogs() {
        return new Catalogs(config);
    }

    public Devices getDevices() {
        return new Devices(config);
    }

    public File getConfigHome() {
        return configHome;
    }

    public Profiles getProfiles() {
        return profiles;
    }

    public Configuration getJobCofig() {
        return config.subset(JobHandler.PROPERTY_JOBS);
    }

    private Configuration getProfileConfiguration(ConfigurationProfile cp) {
        if (ConfigurationProfile.DEFAULT.equals(cp.getId())
                || ConfigurationProfile.DEFAULT_ARCHIVE_IMPORT.equals(cp.getId())
                || ConfigurationProfile.DEFAULT_NDK_IMPORT.equals(cp.getId())) {
            return config;
        }
        Configuration profileConfig = profileConfigCache.get(cp);
        if (profileConfig != null) {
            return profileConfig;
        }
        File file = cp.getFile();
        if (file != null) {
            profileConfig = buildConfiguration(file);
            profileConfigCache.put(cp, profileConfig);
            return profileConfig;
        }
        throw new IllegalStateException("Unknown profile file: " + cp.toString());
    }

    public ImportProfile getImportConfiguration(ConfigurationProfile cp) {
        Configuration profileConfig = getProfileConfiguration(cp);
        return new ImportProfile(profileConfig, cp.getId());
    }

    public ImportProfile getImportConfiguration() {
        return new ImportProfile(config, ConfigurationProfile.DEFAULT);
    }

    public Kramerius4ExportOptions getKramerius4Export() {
        return Kramerius4ExportOptions.from(config);
    }

    public KwisExportOptions getKwisExportOptions() {
        return KwisExportOptions.from(config);
    }

    public NdkExportOptions getNdkExportOptions() {
        return NdkExportOptions.getOptions(config);
    }

    public ArchiveExportOptions getArchiveExportOptions() {
        return ArchiveExportOptions.getOptions(config);
    }

    public SearchOptions getSearchOptions() {
        return SearchOptions.getOptions(config);
    }

    public String getRules() {
        return RdaRules.getOptions(config).getRules();
    }

    public ModsRules getModsOptions() {
        return ModsRules.getOptions(config);
    }

    public ExportParams getExportParams() {
        return ExportParams.getParams(config);
    }

    public KrameriusOptions getKrameriusOptions() {
        return KrameriusOptions.getOptions(config);
    }

    public WorkflowOptions getWorkflowOptions() {
        return WorkflowOptions.getOptions(config);
    }

    public Configuration getAuthenticators() {
        return config;
    }

    public DesaServices getDesaServices() {
        return new DesaServices(config);
    }

    public UrnNbnConfiguration getUrnNbnConfiguration() {
        return new UrnNbnConfiguration(config);
    }

    public ClientOptions getClientOptions() {
        return ClientOptions.getOptions(config);
    }

//    public String getNewClientUrl() {
//        return config.getString(PROPERTY_NEW_CLIENT_URL);
//    }

    public String[] getPlugins() {
        String[] plugins = config.getStringArray(PROPERTY_DIGOBJECT_PLUGINS);
        if (plugins.length == 0) {
            plugins = new String[] {
                NdkPlugin.ID,
            };
        }
        return plugins;
    }

    public File getWorkflowConfiguration() {
        File file = new File(getConfigHome(), "workflow.xml");
        return file;
    }

    Configuration getConfiguration() {
        return config;
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
     * Copies default properties as proarc.cfg.template.
     *
     * @param configHome where to copy
     * @throws IOException
     * @throws ConfigurationException
     */
    public void copyConfigTemplate(File configHome) throws AppConfigurationException {
        try {
            copyConfigTemplateImpl(configHome);
        } catch (IOException ex) {
            throw new AppConfigurationException(String.valueOf(configHome), ex);
        }
    }

    private void copyConfigTemplateImpl(File configHome) throws IOException {
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

    public static File initHome(String home) throws IOException {
        home = (home == null) ? "" : home;
        File homeFile = new File(home);
        checkFile(homeFile, true, true, true, true);
        return homeFile;
    }

    public static File initConfigFolder(String userHome, String configPath) throws IOException {
        File config;
        if (configPath != null) {
            config = new File(configPath);
        } else {
            File home = initHome(userHome);
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
     *
     * @return {@code true} iff {@code f} exists
     */
    public static boolean checkFile(File f, boolean mustExist,
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
