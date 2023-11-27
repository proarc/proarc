package cz.cas.lib.proarc.common.kramerius;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicHeader;
import org.apache.http.util.EntityUtils;
import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import static java.net.HttpURLConnection.HTTP_INTERNAL_ERROR;
import static java.net.HttpURLConnection.HTTP_OK;

public class K7License {

    private static final Logger LOG = Logger.getLogger(K7License.class.getName());

    public List<KrameriusOptions.KrameriusInstance.KrameriusLicense> getLicenses(String url) throws IOException, JSONException {

        LOG.info("Trying to download FOXML from " + url);

        HttpClient httpClient = HttpClients.createDefault();
        HttpGet httpGet = new HttpGet(url);

        httpGet.setHeader(new BasicHeader("Keep-Alive", "timeout=600, max=1000"));
        httpGet.setHeader(new BasicHeader("Connection", "Keep-Alive, Upgrade"));
        httpGet.setHeader(new BasicHeader("Accept-Language", "cs,en;q=0.9,de;q=0.8,cs-CZ;q=0.7,sk;q=0.6"));

        HttpResponse response = httpClient.execute(httpGet);
        if (HTTP_OK == response.getStatusLine().getStatusCode()) {
            HttpEntity entity = response.getEntity();
            if (entity != null) {
                String result = EntityUtils.toString(response.getEntity(), StandardCharsets.UTF_8);
                if (result.startsWith("[")) {
                    JSONArray jsonArray = new JSONArray(result);
                    List<KrameriusOptions.KrameriusInstance.KrameriusLicense> licenses = new ArrayList<>();
                    for (int i = 0; i < jsonArray.length(); i++) {
                        JSONObject jsonObject = jsonArray.getJSONObject(i);
                        String id = jsonObject.optString("id");
                        String name = jsonObject.optString("name");
                        String description = jsonObject.optString("description");
                        licenses.add(new KrameriusOptions.KrameriusInstance.KrameriusLicense(id, name, description));
                    }
                    return licenses;
                }
                return null;
            } else {
                LOG.warning("Getting license but entity is null");
                throw new IOException("Getting license but entity is null");
            }
        } else if (HTTP_INTERNAL_ERROR == response.getStatusLine().getStatusCode()) {
            LOG.warning("Getting license ended with code " + response.getStatusLine().getStatusCode());
            HttpEntity entity = response.getEntity();
            if (entity != null) {
                String result = EntityUtils.toString(response.getEntity());
                if (result != null && !result.isEmpty()) {
                    JSONObject object = new JSONObject(result);
                    LOG.warning("Getting license  ended with code " + response.getStatusLine().getStatusCode() + " and reason is " + object.get("message"));
                    throw new IOException("Getting license  ended with code " + response.getStatusLine().getStatusCode() + " and reason is " + object.get("message"));
                } else {
                    LOG.warning("Getting license  ended with code " + response.getStatusLine().getStatusCode() + " and the result is null");
                    throw new IOException("Getting license  ended with code " + response.getStatusLine().getStatusCode() + " and the result is null");
                }
            } else {
                LOG.warning("Getting license  ended with code " + response.getStatusLine().getStatusCode() + " and the entity is null");
                throw new IOException("Getting license  ended with code " + response.getStatusLine().getStatusCode() + " and the entity is null");
            }
        } else {
            LOG.warning("Getting license ended with code " + response.getStatusLine().getStatusCode());
            throw new IOException("Getting license ended with code " + response.getStatusLine().getStatusCode());
        }
    }
}
