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

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicHeader;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import static java.net.HttpURLConnection.HTTP_OK;

public class K7Authenticator {

    private static final Logger LOG = Logger.getLogger(K7Authenticator.class.getName());

    private KrameriusOptions.KrameriusInstance instance;

    public K7Authenticator(KrameriusOptions.KrameriusInstance instance) {
        this.instance = instance;
    }

    public String authenticate() throws IOException, JSONException {
        String loginUrl = instance.getUrl() + instance.getUrlLogin();

        LOG.info("Trying to authenticate " + loginUrl);

        HttpClient httpClient = HttpClients.createDefault();
        HttpPost httpPost = new HttpPost(loginUrl);

        List<NameValuePair> params = new ArrayList<>();
        params.add(new BasicNameValuePair("username", instance.getUsername()));
        params.add(new BasicNameValuePair("password", instance.getPassword()));
        params.add(new BasicNameValuePair("client_id", instance.getClientId()));
        params.add(new BasicNameValuePair("client_secret", instance.getClientSecret()));
        params.add(new BasicNameValuePair("grant_type", instance.getGrantType()));

        httpPost.setHeader(new BasicHeader("Keep-Alive", "timeout=600, max=1000"));
        httpPost.setHeader(new BasicHeader("Content-Type", "application/x-www-form-urlencoded"));
        httpPost.setHeader(new BasicHeader("Connection", "Keep-Alive, Upgrade"));

        httpPost.setEntity(new UrlEncodedFormEntity(params, "UTF-8"));

        HttpResponse response = httpClient.execute(httpPost);
        if (HTTP_OK == response.getStatusLine().getStatusCode()) {
            HttpEntity entity = response.getEntity();
            if (entity != null) {
                String result = EntityUtils.toString(response.getEntity());
                if (result.startsWith("{")) {
                    JSONObject jsonObject = new JSONObject(result);
                    String token = jsonObject.optString("access_token");
                    if (token != null || !token.isEmpty()) {
                        LOG.fine("Connected to Kramerius and get token " + token);
                        return token;
                    } else {
                        LOG.severe("Connected to Kramerius but access_token is null");
                    }
                } else if (result.startsWith("[")){
                    JSONArray jsonArray = new JSONArray(result);
                    for (int i = 0; i < jsonArray.length(); i++) {
                        JSONObject jsonObject = jsonArray.getJSONObject(0);
                        String token = jsonObject.optString("access_token");
                        if (token != null || !token.isEmpty()) {
                            LOG.fine("Connected to Kramerius and get token " + token);
                            return token;
                        } else {
                            LOG.severe("Connected to Kramerius but access_token is null");
                        }
                    }
                } else {
                    LOG.severe("Connected to Kramerius but can not found access_token");
                    throw new IOException("Connected to Kramerius but can not found access_token");
                }
            } else {
                LOG.severe("Connected to Kramerius but entity is null");
                throw new IOException("Connected to Kramerius but entity is null");
            }
            LOG.severe("Connected to Kramerius but access_token is null");
            throw new IOException("Connected to Kramerius but access_token is null");
        } else {
            LOG.severe("Connecing to Kramerius ended with code " + response.getStatusLine().getStatusCode());
            throw new IOException("Connecing to Kramerius ended with code " + response.getStatusLine().getStatusCode());
        }
    }
}
