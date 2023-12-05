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
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.logging.Logger;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicHeader;
import org.apache.http.util.EntityUtils;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import static java.net.HttpURLConnection.HTTP_INTERNAL_ERROR;
import static java.net.HttpURLConnection.HTTP_OK;

public class K7Downloader {

    private static final Logger LOG = Logger.getLogger(K7Downloader.class.getName());

    private final AppConfiguration appConfig;
    private final KrameriusOptions.KrameriusInstance instance;

    public K7Downloader(AppConfiguration appConfig, KrameriusOptions.KrameriusInstance instance) {
        this.appConfig = appConfig;
        this.instance = instance;
    }

    public String downloadFromK7(String krameriusPid, String token) throws IOException, JSONException {
        String downloadFoxmlUrl = instance.getUrl() + instance.getUrlDownloadFoxml() + krameriusPid + "/foxml";
        LOG.info("Trying to download FOXML from " + downloadFoxmlUrl);

        HttpClient httpClient = HttpClients.createDefault();
        HttpGet httpGet = new HttpGet(downloadFoxmlUrl);

        httpGet.setHeader(new BasicHeader("Keep-Alive", "timeout=600, max=1000"));
        if (token != null && !token.isEmpty()) {
            httpGet.setHeader(new BasicHeader("Authorization", "Bearer " + token));
        }
        httpGet.setHeader(new BasicHeader("Connection", "Keep-Alive, Upgrade"));
        httpGet.setHeader(new BasicHeader("Accept-Language", "cs,en;q=0.9,de;q=0.8,cs-CZ;q=0.7,sk;q=0.6"));

        HttpResponse response = httpClient.execute(httpGet);
        if (HTTP_OK == response.getStatusLine().getStatusCode()) {
            HttpEntity entity = response.getEntity();
            if (entity != null) {
                String result = EntityUtils.toString(response.getEntity(), StandardCharsets.UTF_8);
                if (result != null && !result.isEmpty()) {
                    return result;
                } else {
                    LOG.warning("Downloaded FOXML but result is null or empty");
                    throw new IOException("Downloaded FOXML but result is null or empty");
                }
            } else {
                LOG.warning("Downloaded FOXML but entity is null");
                throw new IOException("Downloaded FOXML but entity is null");
            }
        } else if (HTTP_INTERNAL_ERROR == response.getStatusLine().getStatusCode()) {
            LOG.warning("Downloading FOXML ended with code " + response.getStatusLine().getStatusCode());
            HttpEntity entity = response.getEntity();
            if (entity != null) {
                String result = EntityUtils.toString(response.getEntity());
                if (result != null && !result.isEmpty()) {
                    JSONObject object = new JSONObject(result);
                    LOG.warning("Downloaded FOXML ended with code " + response.getStatusLine().getStatusCode() + " and reason is " + object.get("message"));
                    throw new IOException("Downloaded FOXML ended with code " + response.getStatusLine().getStatusCode() + " and reason is " + object.get("message"));
                } else {
                    LOG.warning("Downloaded FOXML ended with code " + response.getStatusLine().getStatusCode() + " and the result is null");
                    throw new IOException("Downloaded FOXML ended with code " + response.getStatusLine().getStatusCode() + " and the result is null");
                }
            } else {
                LOG.warning("Downloaded FOXML ended with code " + response.getStatusLine().getStatusCode() + " and the entity is null");
                throw new IOException("Downloaded FOXML ended with code " + response.getStatusLine().getStatusCode() + " and the entity is null");
            }
        } else {
            LOG.warning("Downloading FOXML ended with code " + response.getStatusLine().getStatusCode());
            throw new IOException("Downloading FOXML ended with code " + response.getStatusLine().getStatusCode());
        }
    }

    public void saveFoxml(String foxmlContent, String krameriusPid) throws IOException {
        File pidFoxml = getFile(appConfig, instance, krameriusPid);
        if (pidFoxml.exists()) {
            MetsUtils.deleteFolder(pidFoxml);
        }

        if (!pidFoxml.createNewFile()) {
            LOG.warning("Can not create file " + pidFoxml.getAbsolutePath());
            throw new IOException("Can not create file " + pidFoxml.getAbsolutePath());
        }

        FileOutputStream outputStream = new FileOutputStream(pidFoxml);
        OutputStreamWriter outputStreamWriter = new OutputStreamWriter(outputStream, StandardCharsets.UTF_8);
        byte[] foxmlToBytes = foxmlContent.getBytes();
        outputStreamWriter.write(foxmlContent);

        outputStreamWriter.close();
        outputStream.close();
    }

    private File getFile(AppConfiguration appConfig, KrameriusOptions.KrameriusInstance instance, String krameriusPid) throws IOException {
        return KUtils.getFile(appConfig, instance.getId(), krameriusPid);
    }
}
