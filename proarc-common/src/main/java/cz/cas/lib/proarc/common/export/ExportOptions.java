/*
 * Copyright (C) 2020 Lukas Sykora
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
package cz.cas.lib.proarc.common.export;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import org.apache.commons.configuration.Configuration;

/**
 * Settings for export
 *
 * @author Lukas Sykora
 */
public class ExportOptions {

    private static final Logger LOG = Logger.getLogger(ExportOptions.class.getName());

    public static final String KRAMERIUS_INSTANCE_LOCAL = "local";

    static final String PROP_DELETE_PACKAGE = "export.deletePackageIfUrnNbnIsMissing";
    static final String PROP_OVERWRITE_PACKAGE = "export.overwritePackage";
    static final String PROP_JOURNALS_INFO_PATH = "export.cejsh_crossref.journals.path";

    static final String PROP_KRAMERIUS_INSTANCES = "export.krameriusInstances";
    static final String PREFIX_KRAMERIUS_INSTANCE = "export.krameriusInstance";

    private boolean deletePackage;
    private boolean overwritePackage;
    private String journalsInfoPath;
    private List<KrameriusInstance> krameriusInstances;

    public static ExportOptions getOptions(Configuration config) {
        ExportOptions options = new ExportOptions();

        String deletePackage = config.getString(PROP_DELETE_PACKAGE);
        options.setDeletePackage(Boolean.TRUE.equals(Boolean.parseBoolean(deletePackage)));

        String overwritePackage = config.getString(PROP_OVERWRITE_PACKAGE);
        options.setOverwritePackage(Boolean.TRUE.equals(Boolean.parseBoolean(overwritePackage)));

        String journalsInfoPath = config.getString(PROP_JOURNALS_INFO_PATH);
        options.setJournalsInfoPath(journalsInfoPath);

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

    public boolean isDeletePackage() {
        return deletePackage;
    }

    public void setDeletePackage(boolean deletePackage) {
        this.deletePackage = deletePackage;
    }

    public boolean isOverwritePackage() {
        return overwritePackage;
    }

    public void setOverwritePackage(boolean overwritePackage) {
        this.overwritePackage = overwritePackage;
    }

    public String getJournalsInfoPath() {
        return journalsInfoPath;
    }

    public void setJournalsInfoPath(String journalsInfoPath) {
        this.journalsInfoPath = journalsInfoPath;
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

    private static boolean valid(KrameriusInstance conf) {
        boolean ok = true;
        if (KRAMERIUS_INSTANCE_LOCAL.equals(conf.getId())) {
            return true; // pro local neni potreba zadna konfigurace
        }
        if (conf.getType() == null) {
            warning(conf, KrameriusInstance.PROPERTY_TYPE);
            ok = false;
        }
        if (conf.getUrl() == null) {
            warning(conf, KrameriusInstance.PROPERTY_URL);
            ok = false;
        }
        if (conf.getParametrizedImportQuery() == null) {
            warning(conf, KrameriusInstance.PROPERTY_PARAMETRIZED_IMPORT_QUERY);
            ok = false;
        }
        if (conf.getStateQuery() == null) {
            warning(conf, KrameriusInstance.PROPERTY_STATE_QUERY);
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
        if (conf.getExportFolder() == null) {
            warning(conf, KrameriusInstance.PROPERTY_EXPORT_FOLDER);
            ok = false;
        }
        if (conf.getKrameriusImportFoxmlFolder() == null) {
            warning(conf, KrameriusInstance.PROPERTY_KRAMERIUS_IMPORT_FOXML_FOLDER);
            ok = false;
        }
        return ok;
    }

    private static void warning(KrameriusInstance conf, String propertyName) {
        LOG.warning(String.format("Missing %s.%s.%s in proarc.cfg",
                PREFIX_KRAMERIUS_INSTANCE, conf.getId(), propertyName));
    }


    public static class KrameriusInstance {

        static final String PROPERTY_TITLE = "title";
        static final String PROPERTY_URL = "url";
        static final String PROPERTY_PARAMETRIZED_IMPORT_QUERY = "parametrizedImportQuery";
        static final String PROPERTY_STATE_QUERY = "stateQuery";
        static final String PROPERTY_USERNAME = "username";
        static final String PROPERTY_PASSWORD = "passwd";
        static final String PROPERTY_TYPE = "type";
        static final String PROPERTY_EXPORT_FOLDER = "exportFolder";
        static final String PROPERTY_KRAMERIUS_IMPORT_FOXML_FOLDER = "krameriusImportFoxmlFolder";
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

        public String getPassword() {
            return config.getString(PROPERTY_PASSWORD);
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

        public String getExportFolder() {
            return config.getString(PROPERTY_EXPORT_FOLDER);
        }

        public String getType() {
            return config.getString(PROPERTY_TYPE);
        }

        public String getParametrizedImportQuery() {
            return config.getString(PROPERTY_PARAMETRIZED_IMPORT_QUERY);
        }

        public String getStateQuery() {
            return config.getString(PROPERTY_STATE_QUERY);
        }

        public String getKrameriusImportFoxmlFolder() {
            return config.getString(PROPERTY_KRAMERIUS_IMPORT_FOXML_FOLDER);
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
