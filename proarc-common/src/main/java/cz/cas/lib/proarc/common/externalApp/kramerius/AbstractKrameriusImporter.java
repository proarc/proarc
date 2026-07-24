package cz.cas.lib.proarc.common.externalApp.kramerius;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.util.EntityUtils;
import org.json.JSONObject;

abstract class AbstractKrameriusImporter implements KrameriusImporter {

    protected final KrameriusOptions.KrameriusInstance instance;

    protected AbstractKrameriusImporter(KrameriusOptions.KrameriusInstance instance) {
        this.instance = instance;
    }

    protected final String post(
            CloseableHttpClient client,
            String url,
            JSONObject payload,
            int... expectedStatuses
    ) throws IOException {
        HttpPost request = new HttpPost(url);
        addHeaders(request);
        request.setEntity(new StringEntity(payload.toString(), ContentType.APPLICATION_JSON));
        try (CloseableHttpResponse response = client.execute(request)) {
            String body = readBody(response.getEntity());
            int status = response.getStatusLine().getStatusCode();
            if (Arrays.stream(expectedStatuses).noneMatch(expected -> expected == status)) {
                throw new IOException("Kramerius import failed with status " + status + ". " + body);
            }
            return body;
        }
    }

    protected final void addHeaders(HttpRequestBase request) {
        request.setHeader("Content-Type", ContentType.APPLICATION_JSON.getMimeType());
        addAuthorizationHeaders(request);
    }

    protected abstract void addAuthorizationHeaders(HttpRequestBase request);

    protected final String readBody(HttpEntity entity) throws IOException {
        return entity == null ? "" : EntityUtils.toString(entity, StandardCharsets.UTF_8);
    }

    protected record ImportRequest(String url, JSONObject payload) {
    }
}
