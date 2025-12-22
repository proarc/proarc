package cz.cas.lib.proarc.common.kramerius;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.MixEditor;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.logging.Logger;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicHeader;
import org.apache.http.util.EntityUtils;
import org.codehaus.jettison.json.JSONException;

import static java.net.HttpURLConnection.HTTP_ACCEPTED;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_OK;

public class K7Uploader {

    private static final Logger LOGGER = Logger.getLogger(K7Importer.class.getName());

    private final AppConfiguration appConfig;
    private final KrameriusOptions.KrameriusInstance instance;


    public K7Uploader(AppConfiguration appConfig, KrameriusOptions.KrameriusInstance instance) {
        this.appConfig = appConfig;
        this.instance = instance;
    }

    public void uploadXml(String pid, String xml, String streamId) throws IOException, DigitalObjectException, JSONException {
        if (xml == null || xml.isEmpty()) {
            throw new DigitalObjectException(pid, streamId + " to upload is null or empty");
        }
        if (pid == null || pid.isEmpty()) {
            throw new DigitalObjectException(pid, "PID to update is null or empty");
        }

        K7Authenticator authenticator = new K7Authenticator(instance);
        String token = authenticator.authenticate();

        uploadStream(instance, token, pid, streamId, xml);
    }

    private void uploadStream(KrameriusOptions.KrameriusInstance instance, String token, String pid, String streamId, String content) throws IOException {
        String urlUploadStream = getUrlUploadStream(instance, pid, streamId);
        LOGGER.info(String.format("Trying to upload %s to %s.", streamId, urlUploadStream));

        HttpClient httpClient = HttpClients.createDefault();
        HttpPut httpPut = new HttpPut(urlUploadStream);

        httpPut.setHeader(new BasicHeader("Keep-Alive", "timeout=600, max=1000"));
        if (token != null && !token.isEmpty()) {
            httpPut.setHeader(new BasicHeader("Authorization", "Bearer " + token));
        }
        httpPut.setHeader(new BasicHeader("Connection", "Keep-Alive, Upgrade"));
        httpPut.setHeader(new BasicHeader("Accept-Language", "cs,en;q=0.9,de;q=0.8,cs-CZ;q=0.7,sk;q=0.6"));
        httpPut.setHeader(new BasicHeader("Content-Type", getMimeType(streamId)));

        ByteArrayEntity entity = new ByteArrayEntity(content.getBytes(StandardCharsets.UTF_8));
        httpPut.setEntity(entity);

        HttpResponse response = httpClient.execute(httpPut);
        operateResponse(pid, streamId, response);
    }

    private String getMimeType(String streamId) throws IOException {
        if (ModsStreamEditor.DATASTREAM_ID.equals(streamId) || DcStreamEditor.DATASTREAM_ID.equals(streamId) ||
                AltoDatastream.ALTO_ID.equals(streamId) || RelationEditor.DATASTREAM_ID.equals(streamId) ||
                MixEditor.RAW_ID.equals(streamId) || MixEditor.NDK_ARCHIVAL_ID.equals(streamId) || FoxmlUtils.DS_AUDIT_ID.equals(streamId)) {
            return "application/xml; charset=UTF-8";
        } else {
            throw new IOException("Unsupporter streamId " + streamId);
        }
    }

    private String getUrlUploadStream(KrameriusOptions.KrameriusInstance instance, String pid, String streamId) {
        return instance.getUrl() + instance.getUrlUploadStream() + pid + "/akubra/updateManaged/" + streamId;
    }

    private void operateResponse(String pid, String streamId, HttpResponse response) throws IOException {
        if (HTTP_OK == response.getStatusLine().getStatusCode() || HTTP_CREATED == response.getStatusLine().getStatusCode() || HTTP_ACCEPTED == response.getStatusLine().getStatusCode()) {
            HttpEntity entity = response.getEntity();
            if (entity != null) {
                String result = EntityUtils.toString(response.getEntity());
                if (result != null && result.isEmpty()) {
                    LOGGER.info(pid + "/" + streamId + " - Uploaded to Kramerius ");
                } else {
                    LOGGER.warning("Created Importing process, but unexpected response." + result);
                    throw new IOException("Created Importing process, but unexpected response." + result);
                }
            } else {
                LOGGER.warning("Downloaded FOXML but entity is null");
                throw new IOException("Downloaded FOXML but entity is null");
            }
        } else {
            LOGGER.warning("Importing FOXML ended with code " + response.getStatusLine().getStatusCode());
            throw new IOException("Importing FOXML ended with code " + response.getStatusLine().getStatusCode());
        }
    }
}
