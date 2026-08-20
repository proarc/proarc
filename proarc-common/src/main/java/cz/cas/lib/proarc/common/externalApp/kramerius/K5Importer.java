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
package cz.cas.lib.proarc.common.externalApp.kramerius;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import static cz.cas.lib.proarc.common.externalApp.kramerius.KUtils.KRAMERIUS_BATCH_FAILED_V5;
import static cz.cas.lib.proarc.common.externalApp.kramerius.KUtils.KRAMERIUS_BATCH_NO_BATCH_V5;
import static cz.cas.lib.proarc.common.externalApp.kramerius.KUtils.KRAMERIUS_BATCH_STARTED_V5;
import static cz.cas.lib.proarc.common.externalApp.kramerius.KUtils.KRAMERIUS_PROCESS_FINISHED;
import static cz.cas.lib.proarc.common.externalApp.kramerius.KUtils.KRAMERIUS_PROCESS_PLANNED;
import static cz.cas.lib.proarc.common.externalApp.kramerius.KUtils.KRAMERIUS_PROCESS_RUNNING;
import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_FORBIDDEN;
import static java.net.HttpURLConnection.HTTP_ACCEPTED;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_OK;

final class K5Importer extends AbstractKrameriusImporter {

    private static final Logger LOG = Logger.getLogger(K5Importer.class.getName());
    private static final int CONNECT_TIMEOUT_MILLIS = 5_000;
    private static final int MAX_FORBIDDEN_RETRIES = 25;

    private final String authorization;

    K5Importer(KrameriusOptions.KrameriusInstance instance) {
        super(instance);
        String credentials = instance.getUsername() + ":" + instance.getPassword();
        authorization = "Basic " + Base64.getEncoder()
                .encodeToString(credentials.getBytes(StandardCharsets.UTF_8));
    }

    @Override
    public KUtils.ImportState importToKramerius(
            File exportFolder,
            boolean updateExisting,
            String exportType,
            String policy,
            String license,
            boolean updateMods,
            List<String> collections
    ) throws JSONException, IOException, InterruptedException {
        ImportRequest importRequest = createImportRequest(exportFolder, updateExisting, exportType, policy);
        try (CloseableHttpClient client = HttpClients.custom()
                .setDefaultRequestConfig(org.apache.http.client.config.RequestConfig.custom()
                        .setConnectTimeout(CONNECT_TIMEOUT_MILLIS)
                        .build())
                .build()) {
            String response = post(
                    client, importRequest.url(), importRequest.payload(), HTTP_OK, HTTP_CREATED, HTTP_ACCEPTED);
            String processUuid = new JSONObject(response).optString("uuid");
            if (processUuid.isBlank()) {
                throw new IOException("Kramerius did not return the import process UUID.");
            }
            KUtils.ImportState state = pollState(
                    client, instance.getUrl() + instance.getUrlStateQuery() + processUuid);
            LOG.info("Kramerius import finished with process state " + state.getProcessState()
                    + " and batch state " + state.getBatchState() + ".");
            return state;
        }
    }

    private ImportRequest createImportRequest(
            File exportFolder,
            boolean updateExisting,
            String exportType,
            String policy
    ) {
        JSONObject mapping = new JSONObject();
        String url;
        if (KUtils.EXPORT_KRAMERIUS.equals(exportType)) {
            url = instance.getUrl() + instance.getUrlParametrizedImportQuery();
            mapping.put("importDirectory", instance.getKrameriusImportFoxmlFolder() + exportFolder.getName());
            mapping.put("startIndexer", "true");
            mapping.put("updateExisting", Boolean.toString(updateExisting));
        } else {
            url = instance.getUrl() + instance.getUrlConvertImportQuery();
            mapping.put("convertDirectory", instance.getKrameriusConvertNdkFolder() + exportFolder.getName());
            mapping.put("convertTargetDirectory", instance.getKrameriusTargetConvertedFolder());
            mapping.put("ingestSkip", "false");
            mapping.put("startIndexer", "true");
            mapping.put("defaultRights", Boolean.toString("PUBLIC".equalsIgnoreCase(policy)));
        }
        return new ImportRequest(url, new JSONObject().put("mapping", mapping));
    }

    private KUtils.ImportState pollState(
            CloseableHttpClient client,
            String url
    ) throws IOException, InterruptedException, JSONException {
        String processState = KRAMERIUS_PROCESS_PLANNED;
        String batchState = KRAMERIUS_BATCH_NO_BATCH_V5;
        int forbiddenRetries = 0;

        while (isRunning(processState, batchState)) {
            HttpGet request = new HttpGet(url);
            addHeaders(request);
            try (CloseableHttpResponse response = client.execute(request)) {
                int status = response.getStatusLine().getStatusCode();
                if (status < HTTP_BAD_REQUEST) {
                    forbiddenRetries = 0;
                    JSONArray result = new JSONArray(readBody(response.getEntity()));
                    if (result.isEmpty()) {
                        throw new IOException("Kramerius returned an empty process state.");
                    }
                    JSONObject state = result.getJSONObject(0);
                    processState = state.getString("state");
                    batchState = state.getString("batchState");
                } else if (status == HTTP_FORBIDDEN && forbiddenRetries++ < MAX_FORBIDDEN_RETRIES) {
                    TimeUnit.SECONDS.sleep(30);
                    continue;
                } else {
                    processState = KRAMERIUS_BATCH_FAILED_V5;
                    break;
                }
            }
            if (isRunning(processState, batchState)) {
                TimeUnit.SECONDS.sleep(20);
            }
        }
        return new KUtils.ImportState(processState, batchState);
    }

    private boolean isRunning(String processState, String batchState) {
        return KRAMERIUS_PROCESS_PLANNED.equals(processState)
                || KRAMERIUS_PROCESS_RUNNING.equals(processState)
                || (KRAMERIUS_PROCESS_FINISHED.equals(processState)
                && KRAMERIUS_BATCH_STARTED_V5.equals(batchState));
    }

    @Override
    protected void addAuthorizationHeaders(HttpRequestBase request) {
        request.setHeader("Authorization", authorization);
    }
}
