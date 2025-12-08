package cz.cas.lib.proarc.common.kramerius;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import java.io.IOException;
import java.util.logging.Logger;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicHeader;
import org.apache.http.util.EntityUtils;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import static cz.cas.lib.proarc.common.kramerius.K7Importer.getState;
import static java.net.HttpURLConnection.HTTP_ACCEPTED;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_OK;

public class K7Indexer {

    private static final Logger LOGGER = Logger.getLogger(K7Importer.class.getName());

    private final AppConfiguration appConfig;
    private final KrameriusOptions.KrameriusInstance instance;


    public K7Indexer(AppConfiguration appConfig, KrameriusOptions.KrameriusInstance instance) {
        this.appConfig = appConfig;
        this.instance = instance;
    }

    public KUtils.ImportState indexDocument(String pid) throws IOException, DigitalObjectException, JSONException, InterruptedException {
        if (pid == null || pid.isEmpty()) {
            throw new DigitalObjectException(pid, "PID to index is null or empty");
        }

        K7Authenticator authenticator = new K7Authenticator(instance);
        String token = authenticator.authenticate();

        return indexDocument(instance, token, pid);
    }

    private KUtils.ImportState indexDocument(KrameriusOptions.KrameriusInstance instance, String token, String pid) throws IOException, JSONException, InterruptedException {
        String urlUploadStream = getUrlIndex(instance);
        LOGGER.info(String.format("Trying to index %s of %s.", pid, instance.getUrl()));

        String json = "{\"defid\": \"new_indexer_index_object\", \"params\": { \"type\": \"OBJECT\", \"pid\": \"" + pid + "\", \"ignoreInconsistentObjects\": true } }";

        LOGGER.info("Trying to create new Kramerius process " + json + ".");

        HttpClient httpClient = HttpClients.createDefault();
        HttpPost httpPost = new HttpPost(urlUploadStream);

        httpPost.setHeader(new BasicHeader("Connection", "keep-alive"));
        if (token != null && !token.isEmpty()) {
            httpPost.setHeader(new BasicHeader("Authorization", "Bearer " + token));
        }
        httpPost.setHeader(new BasicHeader("Content-Type", "application/json"));

        httpPost.setEntity(new StringEntity(json, ContentType.APPLICATION_JSON));

        HttpResponse response = httpClient.execute(httpPost);

        if (HTTP_OK == response.getStatusLine().getStatusCode() || HTTP_CREATED == response.getStatusLine().getStatusCode() || HTTP_ACCEPTED == response.getStatusLine().getStatusCode()) {
            HttpEntity entity = response.getEntity();
            if (entity != null) {
                String result = EntityUtils.toString(response.getEntity());
                if (result != null && !result.isEmpty()) {
                    JSONObject object = new JSONObject(result);
                    String processUuid = object.getString("uuid");
                    if (processUuid == null || processUuid.isEmpty()) {
                        LOGGER.warning("Created Kramerius import success, but ProArc does not get id of this process, so state is unknown.");
                        throw new IOException("Created Kramerius import success, but ProArc does not get id of this process, so state is unknown.");
                    }
                    KUtils.ImportState state = getState(processUuid, token, instance);
                    LOGGER.info("Requesting Kramerius import success, server response is (process: " + state.getProcessState() + ", batch: " + state.getBatchState() + ").");
                    return state;
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

    private String getUrlIndex(KrameriusOptions.KrameriusInstance instance) {
        return instance.getUrl() + instance.getUrlIndex();
    }
}
