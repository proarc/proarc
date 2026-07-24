package cz.cas.lib.proarc.common.externalApp.kramerius;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.json.JSONException;
import org.json.JSONObject;

import static cz.cas.lib.proarc.common.externalApp.kramerius.KUtils.KRAMERIUS_BATCH_FAILED_V7;
import static cz.cas.lib.proarc.common.externalApp.kramerius.KUtils.KRAMERIUS_BATCH_PLANNED_V7;
import static cz.cas.lib.proarc.common.externalApp.kramerius.KUtils.KRAMERIUS_BATCH_RUNNING_V7;
import static cz.cas.lib.proarc.common.externalApp.kramerius.KUtils.KRAMERIUS_PROCESS_FINISHED;
import static cz.cas.lib.proarc.common.externalApp.kramerius.KUtils.KRAMERIUS_PROCESS_NOT_RUNNING;
import static cz.cas.lib.proarc.common.externalApp.kramerius.KUtils.KRAMERIUS_PROCESS_PLANNED;
import static cz.cas.lib.proarc.common.externalApp.kramerius.KUtils.KRAMERIUS_PROCESS_RUNNING;
import static java.net.HttpURLConnection.HTTP_ACCEPTED;
import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_FORBIDDEN;
import static java.net.HttpURLConnection.HTTP_OK;

final class K7Importer extends AbstractKrameriusImporter {

    private static final Logger LOG = Logger.getLogger(K7Importer.class.getName());
    private static final int MAX_FORBIDDEN_RETRIES = 25;
    private static final int MAX_NOT_RUNNING_RETRIES = 5;

    private final String token;

    K7Importer(KrameriusOptions.KrameriusInstance instance, String token) {
        super(instance);
        this.token = token;
    }

    @Override
    public KUtils.ImportState importToKramerius(
            File exportFolder,
            boolean updateExisting,
            String exportType,
            String policy,
            String license
    ) throws JSONException, IOException, InterruptedException {
        ImportRequest importRequest = createImportRequest(
                exportFolder, updateExisting, exportType, policy, license);
        try (CloseableHttpClient client = HttpClients.createDefault()) {
            String response = post(
                    client, importRequest.url(), importRequest.payload(), HTTP_OK, HTTP_CREATED, HTTP_ACCEPTED);
            String processUuid = new JSONObject(response).optString("uuid");
            if (processUuid.isBlank()) {
                throw new IOException("Kramerius did not return the import process UUID.");
            }
            KUtils.ImportState state = pollState(client, processUuid);
            LOG.info("Kramerius import finished with process state " + state.getProcessState()
                    + " and batch state " + state.getBatchState() + ".");
            return state;
        }
    }

    static KUtils.ImportState getState(
            String processUuid,
            String token,
            KrameriusOptions.KrameriusInstance instance
    ) throws IOException, InterruptedException, JSONException {
        try (CloseableHttpClient client = HttpClients.createDefault()) {
            return new K7Importer(instance, token).pollState(client, processUuid);
        }
    }

    private ImportRequest createImportRequest(
            File exportFolder,
            boolean updateExisting,
            String exportType,
            String policy,
            String license
    ) {
        JSONObject params = new JSONObject();
        String definition;
        String url;
        if (KUtils.EXPORT_KRAMERIUS.equals(exportType)) {
            definition = "import";
            url = instance.getUrl() + instance.getUrlParametrizedImportQuery();
            params.put("inputDataDir", instance.getKrameriusImportFoxmlFolder() + exportFolder.getName());
            params.put("startIndexer", true);
            params.put("updateExisting", updateExisting);
        } else {
            definition = "convert_and_import";
            url = instance.getUrl() + instance.getUrlConvertImportQuery();
            params.put("inputDataDir", instance.getKrameriusConvertNdkFolder() + exportFolder.getName());
            params.put("policy", resolvePolicy(policy, license));
            params.put("license", license);
            params.put("startIndexer", true);
            params.put("useIIPServer", true);
        }
        params.put("pathtype", instance.getPathType());
        return new ImportRequest(url, new JSONObject()
                .put("defid", definition)
                .put("params", params));
    }

    private KUtils.ImportState pollState(
            CloseableHttpClient client,
            String processUuid
    ) throws IOException, InterruptedException, JSONException {
        String url = instance.getUrl() + instance.getUrlStateQuery() + processUuid;
        String processState = KRAMERIUS_PROCESS_PLANNED;
        String batchState = KRAMERIUS_BATCH_PLANNED_V7;
        int forbiddenRetries = 0;
        int notRunningRetries = 0;

        while (true) {
            HttpGet request = new HttpGet(url);
            addHeaders(request);
            try (CloseableHttpResponse response = client.execute(request)) {
                int status = response.getStatusLine().getStatusCode();
                if (status < HTTP_BAD_REQUEST) {
                    forbiddenRetries = 0;
                    JSONObject result = new JSONObject(readBody(response.getEntity()));
                    processState = getState(result, "process", processUuid);
                    batchState = getState(result, "batch", processUuid);
                } else if (status == HTTP_FORBIDDEN && forbiddenRetries++ < MAX_FORBIDDEN_RETRIES) {
                    TimeUnit.SECONDS.sleep(30);
                    continue;
                } else {
                    processState = KRAMERIUS_BATCH_FAILED_V7;
                    break;
                }
            }

            if (isRunning(processState, batchState)) {
                TimeUnit.SECONDS.sleep(20);
            } else if (KRAMERIUS_PROCESS_NOT_RUNNING.equals(processState)
                    || KRAMERIUS_PROCESS_NOT_RUNNING.equals(batchState)) {
                if (notRunningRetries++ < MAX_NOT_RUNNING_RETRIES) {
                    TimeUnit.SECONDS.sleep(10);
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        return new KUtils.ImportState(processState, batchState);
    }

    private String getState(JSONObject result, String key, String processUuid) throws IOException {
        JSONObject state = result.optJSONObject(key);
        if (state == null || state.optString("state").isBlank()) {
            throw new IOException("Kramerius did not return " + key + " state for process " + processUuid + ".");
        }
        return state.getString("state");
    }

    private boolean isRunning(String processState, String batchState) {
        return KRAMERIUS_PROCESS_PLANNED.equals(processState)
                || KRAMERIUS_PROCESS_RUNNING.equals(processState)
                || (KRAMERIUS_PROCESS_FINISHED.equals(processState)
                && (KRAMERIUS_BATCH_PLANNED_V7.equals(batchState)
                || KRAMERIUS_BATCH_RUNNING_V7.equals(batchState)));
    }

    private String resolvePolicy(String policy, String license) {
        if (license != null && !license.isBlank()) {
            return "PRIVATE";
        }
        return "PUBLIC".equalsIgnoreCase(policy) ? "PUBLIC" : "PRIVATE";
    }

    @Override
    protected void addAuthorizationHeaders(HttpRequestBase request) {
        request.setHeader("Connection", "keep-alive");
        if (token != null && !token.isBlank()) {
            request.setHeader("Authorization", "Bearer " + token);
        }
    }
}
