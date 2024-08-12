package cz.cas.lib.proarc.common.client;/*
 * Copyright (C) 2024 Lukas Sykora
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
import org.apache.commons.configuration.Configuration;

/**
 * Settings for Client
 *
 * @author lsykora
 */
public class ClientOptions {
    private static final String PROPERTY_NEW_CLIENT_URL = "proarc.client.url";
    private static final String PROPERTY_NEW_CLIENT_PATH_CONFIG = "proarc.client.path.config";
    private static final String PROPERTY_NEW_CLIENT_PATH_LANGUAGE_CS = "proarc.client.path.languageCs";
    private static final String PROPERTY_NEW_CLIENT_PATH_LANGUAGE_CSEN = "proarc.client.path.languageCsEn";
    private static final String PROPERTY_NEW_CLIENT_PATH_LANGUAGE_EN = "proarc.client.path.languageEn";

    private String clientUrl;
    private String pathConfig;
    private String pathLanguageCs;
    private String pathLanguageCsEn;
    private String pathLanguageEn;

    public static ClientOptions getOptions(Configuration config) {
        ClientOptions options = new ClientOptions();

        String clientUrl = config.getString(PROPERTY_NEW_CLIENT_URL);
        if (clientUrl != null && !clientUrl.isEmpty()) {
            options.setClientUrl(clientUrl);
        }

        String pathConfig = config.getString(PROPERTY_NEW_CLIENT_PATH_CONFIG);
        if (pathConfig != null && !pathConfig.isEmpty()) {
            options.setPathConfig(pathConfig);
        }

        String pathLanguageCs = config.getString(PROPERTY_NEW_CLIENT_PATH_LANGUAGE_CS);
        if (pathLanguageCs != null && !pathLanguageCs.isEmpty()) {
            options.setPathLanguageCs(pathLanguageCs);
        }

        String pathLanguageCsEn = config.getString(PROPERTY_NEW_CLIENT_PATH_LANGUAGE_CSEN);
        if (pathLanguageCsEn != null && !pathLanguageCsEn.isEmpty()) {
            options.setPathLanguageCsEn(pathLanguageCsEn);
        }

        String pathLanguageEn = config.getString(PROPERTY_NEW_CLIENT_PATH_LANGUAGE_EN);
        if (pathLanguageEn != null && !pathLanguageEn.isEmpty()) {
            options.setPathLanguageEn(pathLanguageEn);
        }
        return options;
    }

    public String getClientUrl() {
        return clientUrl;
    }

    public void setClientUrl(String clientUrl) {
        this.clientUrl = clientUrl;
    }

    public String getPathConfig() {
        return pathConfig;
    }

    public void setPathConfig(String pathConfig) {
        this.pathConfig = pathConfig;
    }

    public String getPathLanguageCs() {
        return pathLanguageCs;
    }

    public void setPathLanguageCs(String pathLanguageCs) {
        this.pathLanguageCs = pathLanguageCs;
    }

    public String getPathLanguageCsEn() {
        return pathLanguageCsEn;
    }

    public void setPathLanguageCsEn(String pathLanguageCsEn) {
        this.pathLanguageCsEn = pathLanguageCsEn;
    }

    public String getPathLanguageEn() {
        return pathLanguageEn;
    }

    public void setPathLanguageEn(String pathLanguageEn) {
        this.pathLanguageEn = pathLanguageEn;
    }
}
