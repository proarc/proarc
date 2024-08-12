/*
 * Copyright (C) 2021 Lukas Sykora
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
package cz.cas.lib.proarc.common.info;

import com.sun.xml.ws.util.StringUtils;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.workflow.profile.WorkflowProfiles;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

public class ApplicationInfo {

    String version;
    String timestamp;
    String revision;
    String rdflowVersion;
    String storage;
    String database;
    String stableConfigFile;
    Boolean stableConfig;
    String stableLanguageCsFile;
    Boolean stableLanguageCs;
    String stableLanguageCsEnFile;
    Boolean stableLanguageCsEn;
    String stableLanguageEnFile;
    Boolean stableLanguageEn;
    String error;

    public void initValues(AppConfiguration config, Boolean fullLoad) throws IOException {
        this.database = "PostgreSQL";
        this.version = config.getNdkExportOptions().getVersion();
        this.timestamp = config.getNdkExportOptions().getTimestamp();
        this.revision = config.getNdkExportOptions().getRevision();
        this.rdflowVersion = WorkflowProfiles.getInstance().getProfiles().getVersion();
        this.storage = StringUtils.capitalize(config.getTypeOfStorage().name().toLowerCase());
        this.stableConfig = isExistingFile(config.getClientOptions().getPathConfig());
        this.stableLanguageCs = isExistingFile(config.getClientOptions().getPathLanguageCs());
        this.stableLanguageCsEn = isExistingFile(config.getClientOptions().getPathLanguageCsEn());
        this.stableLanguageEn = isExistingFile(config.getClientOptions().getPathLanguageEn());
        if (fullLoad != null && fullLoad) {
            if (this.stableConfig) {
                this.stableConfigFile = loadFileContent(new File(config.getClientOptions().getPathConfig()));
            } else {
                this.stableConfigFile = "";
            }
            if (this.stableLanguageCs) {
                this.stableLanguageCsFile = loadFileContent(new File(config.getClientOptions().getPathLanguageCs()));
            } else {
                this.stableLanguageCsFile = "";
            }
            if (this.stableConfig) {
                this.stableLanguageCsEnFile = loadFileContent(new File(config.getClientOptions().getPathLanguageCsEn()));
            } else {
                this.stableLanguageCsEnFile = null;
            }
            if (this.stableConfig) {
                this.stableLanguageEnFile = loadFileContent(new File(config.getClientOptions().getPathLanguageEn()));
            } else {
                this.stableLanguageEnFile = null;
            }
        }
    }

    public void loadFile(AppConfiguration config, String type) throws IOException {
        if (type == null || type.isEmpty()) {
            this.error = "Parameter \"type\" is null or empty.";
        } else {
            switch (type) {
                case "config":
                    if (isExistingFile(config.getClientOptions().getPathConfig())) {
                        this.stableConfigFile = loadFileContent(new File(config.getClientOptions().getPathConfig()));
                    } else {
                        this.error = "File " + config.getClientOptions().getPathConfig() + " is not available to read!";
                    }
                    break;
                case "languageCs":
                case "cs":
                    if (isExistingFile(config.getClientOptions().getPathLanguageCs())) {
                        this.stableLanguageCsFile = loadFileContent(new File(config.getClientOptions().getPathLanguageCs()));
                    } else {
                        this.error = "File " + config.getClientOptions().getPathLanguageCs() + " is not available to read!";
                    }
                    break;
                case "languageEn":
                case "en":
                    if (isExistingFile(config.getClientOptions().getPathLanguageEn())) {
                        this.stableLanguageEnFile = loadFileContent(new File(config.getClientOptions().getPathLanguageEn()));
                    } else {
                        this.error = "File " + config.getClientOptions().getPathLanguageEn() + " is not available to read!";
                    }
                    break;
                case "languageCsEn":
                case "csen":
                    if (isExistingFile(config.getClientOptions().getPathLanguageCsEn())) {
                        this.stableLanguageCsEnFile = loadFileContent(new File(config.getClientOptions().getPathLanguageCsEn()));
                    } else {
                        this.error = "File " + config.getClientOptions().getPathLanguageCsEn() + " is not available to read!";
                    }
                    break;
                default:
                    this.error = "Unknown value (" + type + ") for parameter \"type\".";
            }
        }
    }

    private String loadFileContent(File file) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8));
        StringBuffer buffer = new StringBuffer();
        String line = reader.readLine();
        while (line != null) {
            buffer.append(line).append(System.lineSeparator());
            line = reader.readLine();
        }
        reader.close();
        return buffer.toString();

    }

    private Boolean isExistingFile(String pathConfig) {
        if (pathConfig == null || pathConfig.isEmpty()) {
            return false;
        } else {
            File file = new File(pathConfig);
            return file.exists() && file.isFile() && file.canRead();
        }
    }

    public String getVersion() {
        return version;
    }

    public String getTimestamp() {
        return timestamp;
    }

    public String getRevision() {
        return revision;
    }

    public String getRdflowVersion() {
        return rdflowVersion;
    }

    public String getStorage() {
        return storage;
    }

    public String getDatabase() {
        return database;
    }

    public String getStableConfigFile() {
        return stableConfigFile;
    }

    public Boolean getStableConfig() {
        return stableConfig;
    }

    public String getStableLanguageCsFile() {
        return stableLanguageCsFile;
    }

    public Boolean getStableLanguageCs() {
        return stableLanguageCs;
    }

    public String getStableLanguageCsEnFile() {
        return stableLanguageCsEnFile;
    }

    public Boolean getStableLanguageCsEn() {
        return stableLanguageCsEn;
    }

    public String getStableLanguageEnFile() {
        return stableLanguageEnFile;
    }

    public Boolean getStableLanguageEn() {
        return stableLanguageEn;
    }

    public String getError() {
        return error;
    }

    @Override
    public String toString() {
        return "Info{" + "version=" + version + ", revision=" + revision + ", timestamp=" + timestamp + ", rdflowVersion=" + rdflowVersion + "}";
    }
}
