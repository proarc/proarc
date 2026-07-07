package cz.cas.lib.proarc.common.metacheck;

import java.io.IOException;
import java.net.URI;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public abstract class HttpAbstractClient implements AutoCloseable {

    private final String apiUrl;
    private String apiKey;
    private final CloseableHttpClient httpClient;

    protected HttpAbstractClient(String apiUrl, String apiKey) {
        if (apiUrl == null || apiUrl.trim().isEmpty()) {
            throw new IllegalArgumentException("MetaCheck API URL is not configured.");
        }
        this.apiUrl = normalizeApiUrl(apiUrl);
        this.apiKey = apiKey;
        this.httpClient = HttpClients.createDefault();
    }

    protected HttpAbstractClient(String apiUrl) {
        if (apiUrl == null || apiUrl.trim().isEmpty()) {
            throw new IllegalArgumentException("MetaCheck API URL is not configured.");
        }
        this.apiUrl = normalizeApiUrl(apiUrl);
        this.httpClient = HttpClients.createDefault();
    }

    protected JSONArray getArray(String path) throws IOException, JSONException {
        HttpGet httpGet = new HttpGet(resolve(path));
        addHeaders(httpGet);
        String response = execute(httpGet, 200);
        return new JSONArray(response);
    }

    protected JSONObject postForm(String path, HttpEntity entity, int... expectedStatusCodes) throws IOException, JSONException {
        HttpPost httpPost = new HttpPost(resolve(path));
        addHeaders(httpPost);
        httpPost.setEntity(entity);
        String response = execute(httpPost, expectedStatusCodes);
        return new JSONObject(response);
    }

    private URI resolve(String path) {
        String normalizedPath = path.startsWith("/") ? path.substring(1) : path;
        return URI.create(apiUrl + "/" + normalizedPath);
    }

    private void addHeaders(HttpRequestBase request) {
        request.setHeader("Accept", "application/json");
        if (apiKey != null && !apiKey.trim().isEmpty()) {
            request.setHeader("api-key", apiKey);
        }
    }

    private String execute(HttpRequestBase request, int... expectedStatusCodes) throws IOException {
        HttpResponse response = httpClient.execute(request);
        int statusCode = response.getStatusLine().getStatusCode();
        String responseBody = response.getEntity() == null ? "" : EntityUtils.toString(response.getEntity(), "UTF-8");
        for (int expectedStatusCode : expectedStatusCodes) {
            if (statusCode == expectedStatusCode) {
                return responseBody;
            }
        }
        throw new IOException("MetaCheck API call failed with status " + statusCode + ". " + responseBody);
    }

    private String normalizeApiUrl(String apiUrl) {
        String normalized = apiUrl.trim();
        while (normalized.endsWith("/")) {
            normalized = normalized.substring(0, normalized.length() - 1);
        }
        return normalized;
    }

    @Override
    public void close() throws IOException {
        httpClient.close();
    }
}
