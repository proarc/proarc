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
package cz.cas.lib.proarc.common.kramerius;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import java.io.File;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;

public class KrameriusOptions {

    private static final Logger LOG = Logger.getLogger(KrameriusOptions.class.getName());
    public static final String KRAMERIUS_INSTANCE_LOCAL = "local";
    private static final String PROP_KRAMERIUS_INSTANCES = "krameriusInstances";
    private static final String PREFIX_KRAMERIUS_INSTANCE = "krameriusInstance";

    private List<KrameriusInstance> krameriusInstances;

    public static KrameriusOptions getOptions(Configuration config) {
        KrameriusOptions options = new KrameriusOptions();

        List<KrameriusInstance> krameriusInstances = new ArrayList<>();
        String[] krameriusIds = config.getStringArray(PROP_KRAMERIUS_INSTANCES);
        for (String krameriusId : krameriusIds) {
            KrameriusInstance krameriusConf = new KrameriusInstance(krameriusId, config.subset(PREFIX_KRAMERIUS_INSTANCE + '.' + krameriusId));
            if (valid(krameriusConf)) {
                krameriusInstances.add(krameriusConf);
            }
        }

        options.setKrameriusInstances(krameriusInstances);
        return options;

    }

    public List<KrameriusInstance> getKrameriusInstances() {
        return krameriusInstances;
    }

    public void setKrameriusInstances(List<KrameriusInstance> krameriusInstances) {
        this.krameriusInstances = krameriusInstances;
    }

    public static KrameriusInstance findKrameriusInstance(List<KrameriusInstance> listOfInstances, String id) {
        if (id != null) {
            for (KrameriusInstance krameriusInstance : listOfInstances) {
                if (id.equals(krameriusInstance.getId())) {
                    return krameriusInstance;
                }
            }
        }
        return null;
    }

    public static File getExportFolder(String krameriusInstanceId, URI exportUri, AppConfiguration appConfig, String type) {
        if (krameriusInstanceId == null || krameriusInstanceId.isEmpty() || KRAMERIUS_INSTANCE_LOCAL.equals(krameriusInstanceId)) {
            return new File(exportUri);
        } else {
            KrameriusOptions.KrameriusInstance instance = findKrameriusInstance(appConfig.getKrameriusOptions().getKrameriusInstances(), krameriusInstanceId);
            if (KUtils.EXPORT_KRAMERIUS.equals(type)) {
                File exportFile = new File(instance.getExportFoxmlFolder());
                if (!exportFile.exists() || !exportFile.isDirectory() || !exportFile.canRead() || !exportFile.canWrite()) {
                    throw new IllegalArgumentException("Error s nakonfigurovanou cestou: " + instance.getExportFoxmlFolder() + " (zkontrolujte, ze cesta existuje a mate do ni prava na cteni a zapis.");
                } else {
                    return exportFile;
                }
            } else if (KUtils.EXPORT_NDK.equals(type)) {
                File exportFile = new File(instance.getExportNdkFolder());
                if (!exportFile.exists() || !exportFile.isDirectory() || !exportFile.canRead() || !exportFile.canWrite()) {
                    throw new IllegalArgumentException("Error s nakonfigurovanou cestou: " + instance.getExportNdkFolder() + " (zkontrolujte, ze cesta existuje a mate do ni prava na cteni a zapis.");
                } else {
                    return exportFile;
                }
            } else {
                throw new IllegalArgumentException("Nepodporovany typ exportu: " + type);
            }
        }
    }

    private static boolean valid(KrameriusInstance conf) {
        boolean ok = true;
        int version = 0;
        if (KRAMERIUS_INSTANCE_LOCAL.equals(conf.getId())) {
            return true; // pro local neni potreba zadna konfigurace
        }
        if (conf.getTitle() == null || conf.getTitle().isEmpty()) {
            warning(conf, KrameriusInstance.PROPERTY_TITLE);
        }
        if (conf.getVersion() == null || conf.getVersion().isEmpty()) {
            warning(conf, KrameriusInstance.PROPERTY_VERSION);
            return false;
        } else {
            String tmpVersion = conf.getVersion();
            tmpVersion = tmpVersion.replaceAll("[^0-9]", "");
            if (tmpVersion.startsWith("5")) {
                version = 5;
            } else if (tmpVersion.startsWith("7")) {
                version = 7;
            } else {
                LOG.warning(String.format("Unsupported value %s.%s.%s = %s in proarc.cfg",
                        PREFIX_KRAMERIUS_INSTANCE, conf.getId(), KrameriusInstance.PROPERTY_VERSION, conf.getVersion()));
                return false;
            }
        }
        if (conf.getType() == null || conf.getType().isEmpty()) {
            warning(conf, KrameriusInstance.PROPERTY_TYPE);
            ok = false;
        }
        if (conf.getUrl() == null) {
            warning(conf, KrameriusInstance.PROPERTY_URL);
            ok = false;
        }
        if (conf.getUrlParametrizedImportQuery() == null && conf.getUrlConvertImportQuery() == null) {
            warning(conf, KrameriusInstance.PROPERTY_URL_PARAMETRIZED_IMPORT_QUERY);
            warning(conf, KrameriusInstance.PROPERTY_URL_CONVERT_IMPORT_QUERY);
            ok = false;
        }
        if (conf.getUrlStateQuery() == null) {
            warning(conf, KrameriusInstance.PROPERTY_URL_STATE_QUERY);
            ok = false;
        }
        if (conf.getPassword() == null) {
            warning(conf, KrameriusInstance.PROPERTY_PASSWORD);
            ok = false;
        }
        if (conf.getUsername()== null) {
            warning(conf, KrameriusInstance.PROPERTY_USERNAME);
            ok = false;
        }
        if (conf.getExportFoxmlFolder() == null && conf.getExportNdkFolder() == null) {
            warning(conf, KrameriusInstance.PROPERTY_EXPORT_FOXML_FOLDER);
            warning(conf, KrameriusInstance.PROPERTY_EXPORT_NDK_FOLDER);
            ok = false;
        }
        if (conf.getKrameriusImportFoxmlFolder() == null && conf.getKrameriusConvertNdkFolder() == null) {
            warning(conf, KrameriusInstance.PROPERTY_KRAMERIUS_IMPORT_FOXML_FOLDER);
            warning(conf, KrameriusInstance.PROPERTY_KRAMERIUS_CONVERT_NDK_FOLDER);
            ok = false;
        }
        if (7 == version) {
            if (conf.getUrlLogin() == null) {
                warning(conf, KrameriusInstance.PROPERTY_URL_LOGIN);
                ok = false;
            }
            if (conf.getUrlDownloadFoxml() == null) {
                warning(conf, KrameriusInstance.PROPERTY_URL_DOWNLOAD_FOXML);
                ok = false;
            }
            if (conf.getUrlImage() == null) {
                warning(conf, KrameriusInstance.PROPERTY_URL_IMAGE);
                ok = false;
            }
            if (conf.getClientId() == null) {
                warning(conf, KrameriusInstance.PROPERTY_CLIENT_ID);
                ok = false;
            }
            if (conf.getClientSecret() == null) {
                warning(conf, KrameriusInstance.PROPERTY_CLIENT_SECRET);
                ok = false;
            }
            if (conf.getGrantType() == null) {
                warning(conf, KrameriusInstance.PROPERTY_GRANT_TYPE);
                ok = false;
            }
        } else if (5 == version) {
            if (conf.getKrameriusConvertNdkFolder() != null && conf.getKrameriusTargetConvertedFolder() == null) {
                warning(conf, KrameriusInstance.PROPERTY_KRAMERIUS_TARGET_CONVERTED_FOLDER);
                ok = false;
            }
        }

        return ok;
    }

    private static void warning(KrameriusInstance conf, String propertyName) {
        LOG.warning(String.format("Missing %s.%s.%s in proarc.cfg",
                PREFIX_KRAMERIUS_INSTANCE, conf.getId(), propertyName));
    }

    public static class KrameriusInstance {

        static final String PROPERTY_TITLE = "title";
        static final String PROPERTY_VERSION = "version";
        static final String PROPERTY_URL = "url";
        static final String PROPERTY_URL_LOGIN = "urlLogin";
        static final String PROPERTY_URL_PARAMETRIZED_IMPORT_QUERY = "urlParametrizedImportQuery";
        static final String PROPERTY_URL_CONVERT_IMPORT_QUERY = "urlConvertImportQuery";
        static final String PROPERTY_URL_STATE_QUERY = "urlStateQuery";
        static final String PROPERTY_URL_DOWNLOAD_FOXML = "urlDownloadFoxml";
        static final String PROPERTY_URL_IMAGE = "urlImage";
        static final String PROPERTY_USERNAME = "username";
        static final String PROPERTY_PASSWORD = "passwd";
        static final String PROPERTY_CLIENT_ID = "clientId";
        static final String PROPERTY_CLIENT_SECRET="clientSecret";
        static final String PROPERTY_GRANT_TYPE = "grantType";
        static final String PROPERTY_TYPE = "type";
        static final String PROPERTY_EXPORT_FOXML_FOLDER = "exportFoxmlFolder";
        static final String PROPERTY_KRAMERIUS_IMPORT_FOXML_FOLDER = "krameriusImportFoxmlFolder";
        static final String PROPERTY_EXPORT_NDK_FOLDER = "exportNdkFolder";
        static final String PROPERTY_KRAMERIUS_CONVERT_NDK_FOLDER = "krameriusConvertNdkFolder";
        static final String PROPERTY_KRAMERIUS_TARGET_CONVERTED_FOLDER = "krameriusTargetConvertedFolder";
        static final String PROPERTY_DELETE_AFTER_IMPORT = "deleteAfterImport";

        private final String id;
        private final Configuration config;

        public KrameriusInstance(String id, Configuration config) {
            this.id = id;
            this.config = config;
        }

        public String getId() {
            return id;
        }

        public String getVersion() {
            return config.getString(PROPERTY_VERSION);
        }

        public String getUrlLogin() {
            return config.getString(PROPERTY_URL_LOGIN);
        }

        public String getPassword() {
            return config.getString(PROPERTY_PASSWORD);
        }

        public String getClientId() {
            return config.getString(PROPERTY_CLIENT_ID);
        }

        public String getClientSecret() {
            return config.getString(PROPERTY_CLIENT_SECRET);
        }

        public String getGrantType() {
            return config.getString(PROPERTY_GRANT_TYPE);
        }

        public String getTitle() {
            return config.getString(PROPERTY_TITLE);
        }

        public String getUrl() {
            return config.getString(PROPERTY_URL);
        }

        public String getUsername() {
            return config.getString(PROPERTY_USERNAME);
        }

        public String getExportFoxmlFolder() {
            return config.getString(PROPERTY_EXPORT_FOXML_FOLDER);
        }

        public String getExportNdkFolder() {
            return config.getString(PROPERTY_EXPORT_NDK_FOLDER);
        }

        public String getType() {
            return config.getString(PROPERTY_TYPE);
        }

        public String getUrlParametrizedImportQuery() {
            return config.getString(PROPERTY_URL_PARAMETRIZED_IMPORT_QUERY);
        }

        public String getUrlConvertImportQuery() {
            return config.getString(PROPERTY_URL_CONVERT_IMPORT_QUERY);
        }

        public String getUrlImage() {
            return config.getString(PROPERTY_URL_IMAGE);
        }

        public String getUrlStateQuery() {
            return config.getString(PROPERTY_URL_STATE_QUERY);
        }

        public String getUrlDownloadFoxml() {
            return config.getString(PROPERTY_URL_DOWNLOAD_FOXML);
        }

        public String getKrameriusImportFoxmlFolder() {
            return config.getString(PROPERTY_KRAMERIUS_IMPORT_FOXML_FOLDER);
        }

        public String getKrameriusConvertNdkFolder() {
            return config.getString(PROPERTY_KRAMERIUS_CONVERT_NDK_FOLDER);
        }

        public String getKrameriusTargetConvertedFolder () {
            return config.getString(PROPERTY_KRAMERIUS_TARGET_CONVERTED_FOLDER);
        }

        public boolean deleteAfterImport() {
            return config.getBoolean(PROPERTY_DELETE_AFTER_IMPORT);
        }

        public boolean isTestType() {
            String type = getType();
            return "test".equalsIgnoreCase(type);
        }
    }


}
