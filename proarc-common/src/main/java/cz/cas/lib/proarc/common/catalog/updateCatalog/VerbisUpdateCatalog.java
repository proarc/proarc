/*
 * Copyright (C) 2024 Lukas Sykora
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

package cz.cas.lib.proarc.common.catalog.updateCatalog;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.logging.Logger;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicHeader;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_AUTHORIZATION_CATALOG_URL;
import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_LOGIN_PASSWORD;
import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_LOGIN_USERNAME;
import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_UPDATE_CATALOG_URL;
import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_UPDATE_FIELD;
import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_UPDATE_SUBFIELD_APP;
import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_UPDATE_SUBFIELD_DIGITALIZED;
import static cz.cas.lib.proarc.common.config.CatalogConfiguration.PROPERTY_UPDATE_SUBFIELD_OBJECT;
import static java.net.HttpURLConnection.HTTP_OK;


/**
 * Class that updates Verbis Catalogs record
 *
 * @author Lukas Sykora
 */
public class VerbisUpdateCatalog extends UpdateCatalog {

    private static final Logger LOG = Logger.getLogger(VerbisUpdateCatalog.class.getName());
    public static final String ID = "VERBIS";

    public VerbisUpdateCatalog(AppConfiguration appConfiguration, AkubraConfiguration akubraConfiguration, UserProfile user) {
        super(appConfiguration, akubraConfiguration, user);
    }

    @Override
    protected boolean allowUpdateRecord(CatalogConfiguration catalog) {
        boolean ok = true;
        if (catalog.getCatalogAuthorizationUrl() == null || catalog.getCatalogAuthorizationUrl().isEmpty()) {
            LOG.severe(String.format("Missing %s.%s in proarc.cfg",  catalog.getPrefix(), PROPERTY_AUTHORIZATION_CATALOG_URL));
            ok = false;
        }
        if (catalog.getCatalogUpdateUrl() == null || catalog.getCatalogUpdateUrl().isEmpty()) {
            LOG.severe(String.format("Missing %s.%s in proarc.cfg",  catalog.getPrefix(), PROPERTY_UPDATE_CATALOG_URL));
            ok = false;
        }
        if (catalog.getCatalogUsername() == null || catalog.getCatalogUsername().isEmpty()) {
            LOG.severe(String.format("Missing %s.%s in proarc.cfg",  catalog.getPrefix(), PROPERTY_LOGIN_USERNAME));
            ok = false;
        }
        if (catalog.getCatalogPassword() == null || catalog.getCatalogPassword().isEmpty()) {
            LOG.severe(String.format("Missing %s.%s in proarc.cfg",  catalog.getPrefix(), PROPERTY_LOGIN_PASSWORD));
            ok = false;
        }
        if (catalog.getUpdateField() == null || catalog.getUpdateField().isEmpty()) {
            LOG.severe(String.format("Missing %s.%s in proarc.cfg",  catalog.getPrefix(), PROPERTY_UPDATE_FIELD));
            ok = false;
        }
        if (catalog.getUpdateSubfieldApp() == null || catalog.getUpdateSubfieldApp().isEmpty()) {
            LOG.severe(String.format("Missing %s.%s in proarc.cfg",  catalog.getPrefix(), PROPERTY_UPDATE_SUBFIELD_APP));
            ok = false;
        }
        if (catalog.getUpdateSubfieldObject() == null || catalog.getUpdateSubfieldObject().isEmpty()) {
            LOG.severe(String.format("Missing %s.%s in proarc.cfg",  catalog.getPrefix(), PROPERTY_UPDATE_SUBFIELD_OBJECT));
            ok = false;
        }
        if (catalog.getUpdateSubfieldDigitalized() == null || catalog.getUpdateSubfieldDigitalized().isEmpty()) {
            LOG.severe(String.format("Missing %s.%s in proarc.cfg",  catalog.getPrefix(), PROPERTY_UPDATE_SUBFIELD_DIGITALIZED));
            ok = false;
        }
        return ok;
    }


    @Override
    public boolean process(CatalogConfiguration catalogConfiguration, String field001, String pid) throws DigitalObjectException, JSONException, IOException {
        if (allowUpdateRecord(catalogConfiguration)) {
            String verbisToken = getCatalogToken(catalogConfiguration);
            return updateRecord(catalogConfiguration, verbisToken, field001, pid);
        } else {
            LOG.severe("Catalog with id " + catalogConfiguration.getId() + " does not support Record modification");
            throw new IOException("Catalog with id " + catalogConfiguration.getId() + " does not support Record modification");
        }
    }

    protected void validateValues(String verbisToken, String field001, String uuid) throws IOException {
        if (verbisToken == null || verbisToken.isEmpty()) {
            throw new IOException("Verbis token is null or empty");
        }
        if (field001 == null || field001.isEmpty()) {
            throw new IOException("Field 001 id is null or empty");
        }
        if (uuid == null || uuid.isEmpty()) {
            throw new IOException("UUID is null or empty");
        }
    }

    private String getCatalogToken(CatalogConfiguration catalogConfig) throws IOException, JSONException {
        String client_id = catalogConfig.getCatalogUsername();
        String client_secret = catalogConfig.getCatalogPassword();

        String encode = Base64.getEncoder().encodeToString((client_id + ":" + client_secret).getBytes(StandardCharsets.UTF_8));

        HttpClient httpClient = HttpClients.createDefault();
        HttpPost httpPost = new HttpPost(catalogConfig.getCatalogAuthorizationUrl());
        httpPost.setHeader(new BasicHeader("Content-Type", "application/x-www-form-urlencoded"));
        httpPost.setHeader(new BasicHeader("Authorization", String.format("Basic %s", encode)));

        List<NameValuePair> params = new ArrayList<>();
        params.add(new BasicNameValuePair("grant_type", "client_credentials"));

        httpPost.setEntity(new UrlEncodedFormEntity(params, "UTF-8"));

        HttpResponse response = httpClient.execute(httpPost);
        if (HTTP_OK == response.getStatusLine().getStatusCode()) {
            HttpEntity entity = response.getEntity();
            if (entity != null) {
                String result = EntityUtils.toString(response.getEntity());
                if (result != null && !result.isEmpty()) {
                    JSONObject object = new JSONObject(result);
                    String accessToken = object.getString("access_token");
                    if (accessToken == null || accessToken.isEmpty()) {
                        throw new IOException("Connected to Catalog but access token is null or empty: " + accessToken);
                    } else {
                        return accessToken;
                    }
                } else {
                    throw new IOException("Connected to Catalog but unexpected response." + result);
                }
            } else {
                throw new IOException("Connected to Catalog but entity is null");
            }
        } else {
            throw new IOException("Connecing to Catalog ended with code " + response.getStatusLine().getStatusCode());
        }
    }

    private boolean updateRecord(CatalogConfiguration bCatalog, String catalogToken, String field001, String uuid) throws IOException {
        validateValues(catalogToken, field001, uuid);

        HttpClient httpClient = HttpClients.createDefault();
        HttpPost httpPost = new HttpPost(bCatalog.getCatalogUpdateUrl());
        httpPost.setHeader(new BasicHeader("Authorization", String.format("Bearer %s", catalogToken)));
        httpPost.setHeader(new BasicHeader("Content-Type", "application/json; charset=UTF-8"));

        String json = "{" +
                "    \"reusing\": [{\"id\": \"f1:" + field001 + "\"}]," +
                "    \"value\": [" +
                "        {\n" +
                "                \"reusing\": [{" +
                "                       \"type\": \"D." + bCatalog.getUpdateField() + "\"," +
                "                       \"havingField\": {\"type\": \"D." + bCatalog.getUpdateField() + "." + bCatalog.getUpdateSubfieldApp() + "\", \"value\": \"proarcId\"}" +
                "                }]," +
                "            \"set\": {" +
                "                \"type\": \"D." + bCatalog.getUpdateField() + "\"," +
                "                \"value\": [" +
                "                    {" +
                "                        \"reusing\": [{\"type\": \"D." + bCatalog.getUpdateField() + "." + bCatalog.getUpdateSubfieldApp() + "\"}]," +
                "                        \"set\": {\"type\": \"D." + bCatalog.getUpdateField() + "." + bCatalog.getUpdateSubfieldApp() + "\", \"value\": \"proarcId\"}" +
                "                    }," +
                "                    {" +
                "                        \"reusing\": [{\"type\": \"D." + bCatalog.getUpdateField() + "." + bCatalog.getUpdateSubfieldObject() + "\"}]," +
                "                        \"set\": {\"type\": \"D." + bCatalog.getUpdateField() + "." + bCatalog.getUpdateSubfieldObject() + "\", \"value\": \"" + uuid + "\"}" +
                "                    }," +
                "                    {" +
                "                        \"reusing\": [{\"type\": \"D." + bCatalog.getUpdateField() + "." + bCatalog.getUpdateSubfieldDigitalized() + "\"}]," +
                "                        \"set\": {\"type\": \"D." + bCatalog.getUpdateField() + "." + bCatalog.getUpdateSubfieldDigitalized() + "\", \"value\": \"zdigitalizováno\"}" +
                "                    }" +
                "                ]" +
                "            }" +
                "        }" +
                "    ]" +
                "}";

        json = json.replaceAll("\\s+"," ");
        httpPost.setEntity(new StringEntity(json, ContentType.APPLICATION_JSON));

        HttpResponse response = httpClient.execute(httpPost);
        if (HTTP_OK == response.getStatusLine().getStatusCode()) {
            HttpEntity entity = response.getEntity();
            return true;
        } else {
            LOG.severe(EntityUtils.toString(response.getEntity()));
            throw new IOException("Connecing to Catalog ended with code " + response.getStatusLine().getStatusCode());
        }
    }
}
