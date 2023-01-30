package cz.cas.lib.proarc.common.kramerius;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicHeader;
import org.apache.http.util.EntityUtils;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_PLANNED;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_RUNNING;
import static java.net.HttpURLConnection.HTTP_ACCEPTED;
import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_OK;

public class K7Importer {

    private static final Logger LOG = Logger.getLogger(K7Importer.class.getName());

    private final AppConfiguration appConfiguration;
    private final KrameriusOptions.KrameriusInstance instance;

    public K7Importer(AppConfiguration appconfig, KrameriusOptions.KrameriusInstance instance) {
        this.appConfiguration = appconfig;
        this.instance = instance;
    }

    public String importToKramerius(File exportFolder, boolean updateExisting) throws JSONException, IOException, InterruptedException {

        K7Authenticator authenticator = new K7Authenticator(instance);
        String token = authenticator.authenticate();

        String parametrizedQueryUrl = instance.getUrl() + instance.getUrlParametrizedImportQuery();
        LOG.info("Trying to create new Kramerius process " + parametrizedQueryUrl);

        HttpClient httpClient = HttpClients.createDefault();
        HttpPost httpPost = new HttpPost(parametrizedQueryUrl);

        httpPost.setHeader(new BasicHeader("Connection", "keep-alive"));
        if (token != null && !token.isEmpty()) {
            httpPost.setHeader(new BasicHeader("Authorization", "Bearer " + token));
        }
        httpPost.setHeader(new BasicHeader("Content-Type", "application/json"));

        String json = "{\"defid\":\"import\",\"params\": {\"inputDataDir\":\"/" + exportFolder.getName() + "\",\"startIndexer\": true, \"updateExisting\": " + updateExisting + "}}";

        httpPost.setEntity(new StringEntity(json.toString(), ContentType.APPLICATION_JSON));

        HttpResponse response = httpClient.execute(httpPost);

        if (HTTP_OK == response.getStatusLine().getStatusCode() || HTTP_CREATED == response.getStatusLine().getStatusCode() || HTTP_ACCEPTED == response.getStatusLine().getStatusCode()) {
            HttpEntity entity = response.getEntity();
            if (entity != null) {
                String result = EntityUtils.toString(response.getEntity());
                if (result != null && !result.isEmpty()) {
                    JSONObject object = new JSONObject(result);
                    String processId = object.getString("id");
                    if (processId == null || processId.isEmpty()) {
                        LOG.warning("Created Kramerius import success, but ProArc does not get id of this process, so state is unknown.");
                        throw new IOException("Created Kramerius import success, but ProArc does not get id of this process, so state is unknown.");
                    }
                    String state = getState(processId, token);
                    LOG.info("Created Kramerius import success and state of this process is " + state);
                    return state;
                } else {
                    LOG.warning("Created Importing process, but unexpected response." + result);
                    throw new IOException("Created Importing process, but unexpected response." + result);
                }
            } else {
                LOG.warning("Downloaded FOXML but entity is null");
                throw new IOException("Downloaded FOXML but entity is null");
            }
        } else {
            LOG.warning("Importing FOXML ended with code " + response.getStatusLine().getStatusCode());
            throw new IOException("Importing FOXML ended with code " + response.getStatusLine().getStatusCode());
        }
    }

    private String getState(String processId, String token) throws IOException, InterruptedException, JSONException {

        String processQueryUrl = instance.getUrl() + instance.getUrlStateQuery() + processId;
        LOG.info("Trying to get Kramerius process status" + processQueryUrl);

        String state = KRAMERIUS_PROCESS_PLANNED;

        while (state.equals(KRAMERIUS_PROCESS_PLANNED) || state.equals(KRAMERIUS_PROCESS_RUNNING)) {

            HttpClient httpClient = HttpClients.createDefault();
            HttpGet httpGet = new HttpGet(processQueryUrl);

            httpGet.setHeader(new BasicHeader("Connection", "keep-alive"));
            if (token != null && !token.isEmpty()) {
                httpGet.setHeader(new BasicHeader("Authorization", "Bearer " + token));
            }
            httpGet.setHeader(new BasicHeader("Content-Type", "application/json"));

            HttpResponse response = httpClient.execute(httpGet);
            if (response.getStatusLine().getStatusCode() < HTTP_BAD_REQUEST) {
                String result = EntityUtils.toString(response.getEntity(), StandardCharsets.UTF_8);
                JSONObject objectProcess = new JSONObject(result).getJSONObject("process");
                if (objectProcess != null) {
                    state = objectProcess.getString("state");
                } else {
                    throw new IOException("ProArc can not get state of process " + processId);
                }
            }
            if (state.equals(KRAMERIUS_PROCESS_PLANNED) || state.equals(KRAMERIUS_PROCESS_RUNNING)) {
                TimeUnit.SECONDS.sleep(20);
            }
        }
        return state;
    }
}
