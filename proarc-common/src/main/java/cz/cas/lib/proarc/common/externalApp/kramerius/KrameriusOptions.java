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
package cz.cas.lib.proarc.common.externalApp.kramerius;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import java.io.File;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.commons.configuration2.Configuration;

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
        if (id == null || listOfInstances == null) {
            return null;
        }
        return listOfInstances.stream()
                .filter(instance -> id.equals(instance.getId()))
                .findFirst()
                .orElse(null);
    }

    public static File getExportFolder(String krameriusInstanceId, URI exportUri, AppConfiguration appConfig, String type) {
        if (isBlank(krameriusInstanceId) || KRAMERIUS_INSTANCE_LOCAL.equals(krameriusInstanceId)) {
            return new File(exportUri);
        }
        KrameriusInstance instance = findKrameriusInstance(
                appConfig.getKrameriusOptions().getKrameriusInstances(), krameriusInstanceId);
        if (instance == null) {
            throw new IllegalArgumentException("Unknown Kramerius instance: " + krameriusInstanceId);
        }
        String configuredPath = switch (type) {
            case KUtils.EXPORT_KRAMERIUS -> instance.getExportFoxmlFolder();
            case KUtils.EXPORT_NDK -> instance.getExportNdkFolder();
            default -> throw new IllegalArgumentException("Unsupported export type: " + type);
        };
        File exportFolder = new File(configuredPath);
        if (!exportFolder.isDirectory() || !exportFolder.canRead() || !exportFolder.canWrite()) {
            throw new IllegalArgumentException(
                    "Kramerius export folder is not a readable and writable directory: " + configuredPath);
        }
        return exportFolder;
    }

    private static boolean valid(KrameriusInstance conf) {
        boolean ok = true;
        if (KRAMERIUS_INSTANCE_LOCAL.equals(conf.getId())) {
            return true; // pro local neni potreba zadna konfigurace
        }
        if (isBlank(conf.getTitle())) {
            warning(conf, KrameriusInstance.PROPERTY_TITLE);
        }
        if (isBlank(conf.getVersion())) {
            warning(conf, KrameriusInstance.PROPERTY_VERSION);
            return false;
        }
        if (conf.getMajorVersion() == KrameriusVersion.UNSUPPORTED) {
            LOG.warning(String.format("Unsupported value %s.%s.%s = %s in proarc.cfg",
                    PREFIX_KRAMERIUS_INSTANCE, conf.getId(), KrameriusInstance.PROPERTY_VERSION, conf.getVersion()));
            return false;
        }
        ok &= present(conf, conf.getType(), KrameriusInstance.PROPERTY_TYPE);
        ok &= present(conf, conf.getUrl(), KrameriusInstance.PROPERTY_URL);
        if (isBlank(conf.getUrlParametrizedImportQuery()) && isBlank(conf.getUrlConvertImportQuery())) {
            warning(conf, KrameriusInstance.PROPERTY_URL_PARAMETRIZED_IMPORT_QUERY);
            warning(conf, KrameriusInstance.PROPERTY_URL_CONVERT_IMPORT_QUERY);
            ok = false;
        }
        ok &= present(conf, conf.getUrlStateQuery(), KrameriusInstance.PROPERTY_URL_STATE_QUERY);
        ok &= present(conf, conf.getPassword(), KrameriusInstance.PROPERTY_PASSWORD);
        ok &= present(conf, conf.getUsername(), KrameriusInstance.PROPERTY_USERNAME);
        if (isBlank(conf.getExportFoxmlFolder()) && isBlank(conf.getExportNdkFolder())) {
            warning(conf, KrameriusInstance.PROPERTY_EXPORT_FOXML_FOLDER);
            warning(conf, KrameriusInstance.PROPERTY_EXPORT_NDK_FOLDER);
            ok = false;
        }
        if (isBlank(conf.getKrameriusImportFoxmlFolder()) && isBlank(conf.getKrameriusConvertNdkFolder())) {
            warning(conf, KrameriusInstance.PROPERTY_KRAMERIUS_IMPORT_FOXML_FOLDER);
            warning(conf, KrameriusInstance.PROPERTY_KRAMERIUS_CONVERT_NDK_FOLDER);
            ok = false;
        }
        if (conf.getMajorVersion() == KrameriusVersion.V7) {
            ok &= present(conf, conf.getUrlLogin(), KrameriusInstance.PROPERTY_URL_LOGIN);
            ok &= present(conf, conf.getUrlDownloadFoxml(), KrameriusInstance.PROPERTY_URL_DOWNLOAD_FOXML);
            ok &= present(conf, conf.getUrlImage(), KrameriusInstance.PROPERTY_URL_IMAGE);
            ok &= present(conf, conf.getClientId(), KrameriusInstance.PROPERTY_CLIENT_ID);
            ok &= present(conf, conf.getClientSecret(), KrameriusInstance.PROPERTY_CLIENT_SECRET);
            ok &= present(conf, conf.getGrantType(), KrameriusInstance.PROPERTY_GRANT_TYPE);
            ok &= present(conf, conf.getUrlLicense(), KrameriusInstance.PROPERTY_URL_LICENSE);
        } else if (conf.getMajorVersion() == KrameriusVersion.V5) {
            if (!isBlank(conf.getKrameriusConvertNdkFolder())) {
                ok &= present(
                        conf,
                        conf.getKrameriusTargetConvertedFolder(),
                        KrameriusInstance.PROPERTY_KRAMERIUS_TARGET_CONVERTED_FOLDER);
            }
        }

        return ok;
    }

    private static void warning(KrameriusInstance conf, String propertyName) {
        LOG.warning(String.format("Missing %s.%s.%s in proarc.cfg",
                PREFIX_KRAMERIUS_INSTANCE, conf.getId(), propertyName));
    }

    private static boolean present(KrameriusInstance conf, String value, String propertyName) {
        if (!isBlank(value)) {
            return true;
        }
        warning(conf, propertyName);
        return false;
    }

    private static boolean isBlank(String value) {
        return value == null || value.isBlank();
    }

    public static class KrameriusInstance {

        static final String PROPERTY_TITLE = "title";
        static final String PROPERTY_VERSION = "version";
        static final String PROPERTY_URL = "url";
        static final String PROPERTY_URL_KEYCLOAK = "urlKeycloak";
        static final String PROPERTY_URL_LOGIN = "urlLogin";
        static final String PROPERTY_URL_PARAMETRIZED_IMPORT_QUERY = "urlParametrizedImportQuery";
        static final String PROPERTY_URL_CONVERT_IMPORT_QUERY = "urlConvertImportQuery";
        static final String PROPERTY_URL_STATE_QUERY = "urlStateQuery";
        static final String PROPERTY_URL_DOWNLOAD_FOXML = "urlDownloadFoxml";
        static final String PROPERTY_URL_IMAGE = "urlImage";
        static final String PROPERTY_URL_LICENSE = "urlLicense";
        static final String PROPERTY_URL_COLLECTIONS = "urlCollections";
        static final String PROPERTY_URL_UPLOAD_STREAM = "urlUploadStream";
        static final String PROPERTY_URL_INDEX = "urlProcesses";
        static final String PROPERTY_USERNAME = "username";
        static final String PROPERTY_PASSWORD = "passwd";
        static final String PROPERTY_CLIENT_ID = "clientId";
        static final String PROPERTY_CLIENT_SECRET = "clientSecret";
        static final String PROPERTY_GRANT_TYPE = "grantType";
        static final String PROPERTY_TYPE = "type";
        static final String PROPERTY_EXPORT_FOXML_FOLDER = "exportFoxmlFolder";
        static final String PROPERTY_KRAMERIUS_IMPORT_FOXML_FOLDER = "krameriusImportFoxmlFolder";
        static final String PROPERTY_EXPORT_NDK_FOLDER = "exportNdkFolder";
        static final String PROPERTY_KRAMERIUS_CONVERT_NDK_FOLDER = "krameriusConvertNdkFolder";
        static final String PROPERTY_KRAMERIUS_TARGET_CONVERTED_FOLDER = "krameriusTargetConvertedFolder";
        static final String PROPERTY_DELETE_AFTER_IMPORT = "deleteAfterImport";
        static final String PROPERTY_UPLOAD_TO_CATALOG = "uploadToCatalog";
        static final String PROPERTY_PATH_TYPE = "pathType";

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

        KrameriusVersion getMajorVersion() {
            return KrameriusVersion.from(getVersion());
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

        public String getUrlKeycloak() {
            return config.getString(PROPERTY_URL_KEYCLOAK);
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

        public String getUrlLicense() {
            return config.getString(PROPERTY_URL_LICENSE);
        }

        public String getUrlCollections() {
            String urlCollections = config.getString(PROPERTY_URL_COLLECTIONS);
            if (urlCollections == null || urlCollections.isEmpty()) {
                return "/search/api/admin/v7.0/collections";
            }
            return urlCollections;
        }

        public String getUrlUploadStream() {
            String urlUploadStram = config.getString(PROPERTY_URL_UPLOAD_STREAM);
            if (urlUploadStram == null || urlUploadStram.isEmpty()) {
                return "/search/api/admin/v7.0/items/";
            }
            return urlUploadStram;
        }

        public String getUrlIndex() {
            String urlIndex = config.getString(PROPERTY_URL_INDEX);
            if (urlIndex == null || urlIndex.isEmpty()) {
                return "/search/api/admin/v7.0/processes/";
            }
            return urlIndex;
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

        public String getKrameriusTargetConvertedFolder() {
            return config.getString(PROPERTY_KRAMERIUS_TARGET_CONVERTED_FOLDER);
        }

        public boolean deleteAfterImport() {
            return config.getBoolean(PROPERTY_DELETE_AFTER_IMPORT);
        }

        public String uploadToCatalog() {
            return config.getString(PROPERTY_UPLOAD_TO_CATALOG);
        }

        public boolean isTestType() {
            String type = getType();
            return "test".equalsIgnoreCase(type);
        }

        public String getPathType() {
            String pathtype = config.getString(PROPERTY_PATH_TYPE, "relative");
            if (!("relative".equals(pathtype) || "absolute".equals(pathtype))) {
                LOG.warning("Unsupported path type: " + pathtype + " using default value \"relative\".");
                return "relative";
            }
            return pathtype;
        }

        public List<KrameriusLicense> getLicenses() {
            if (KRAMERIUS_INSTANCE_LOCAL.equals(this.getId())) {
                return null;
            } else if (!isVersion7()) {
                return null;
            } else {
                List<KrameriusLicense> licenses = null;
                try (KrameriusClient client = new KrameriusClient(this.getUrl())) {
                    licenses = client.getLicenses(this);
                } catch (Exception ex) {
                    LOG.log(Level.WARNING, "Cannot load licenses from Kramerius " + getId() + ".", ex);
                }
                return licenses;
            }
        }

        public List<KrameriusCollection> getCollections() {
            if (KRAMERIUS_INSTANCE_LOCAL.equals(this.getId()) || !isVersion7()) {
                return null;
            }
            try (KrameriusClient client = new KrameriusClient(this.getUrl())) {
                return client.getCollections(this);
            } catch (Exception ex) {
                LOG.log(Level.WARNING, "Cannot load collections from Kramerius " + getId() + ".", ex);
                return null;
            }
        }

        private boolean isVersion7() {
            return getMajorVersion() == KrameriusVersion.V7;
        }

        public boolean supportsCollections() {
            return isVersion7();
        }

        public static class KrameriusLicense {
            private final String id;
            private final String name;
            private final String description;

            public KrameriusLicense(String id, String name, String description) {
                this.id = id;
                this.name = name;
                this.description = description;
            }

            public String getId() {
                return id;
            }

            public String getName() {
                return name;
            }

            public String getDescription() {
                return description;
            }
        }
    }


}
