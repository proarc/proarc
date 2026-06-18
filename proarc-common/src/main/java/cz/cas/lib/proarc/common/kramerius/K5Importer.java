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
import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import org.apache.commons.io.IOUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_FAILED_V5;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_NO_BATCH_V5;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_BATCH_STARTED_V5;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_FINISHED;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_PLANNED;
import static cz.cas.lib.proarc.common.kramerius.KUtils.KRAMERIUS_PROCESS_RUNNING;
import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;
import static java.net.HttpURLConnection.HTTP_FORBIDDEN;


public class K5Importer {

    private static final Logger LOG = Logger.getLogger(K5Importer.class.getName());

    private final AppConfiguration appConfig;
    private final KrameriusOptions.KrameriusInstance instance;

    public K5Importer(AppConfiguration appConfig, KrameriusOptions.KrameriusInstance instance) {
        this.appConfig = appConfig;
        this.instance = instance;
    }

    public KUtils.ImportState importToKramerius(File exportFolder, boolean updateExisting, String exportType, String policy) throws JSONException, IOException, InterruptedException {
        String credentials = instance.getUsername() + ":" + instance.getPassword();

        String proarcExport = "";
        String query = "";
        String json = "";

        if (KUtils.EXPORT_KRAMERIUS.equals(exportType)) {
            query =  instance.getUrl() + instance.getUrlParametrizedImportQuery();
            proarcExport = instance.getExportFoxmlFolder() + exportFolder.getName();
            String krameriusFoxmlImport = proarcExport.replace(instance.getExportFoxmlFolder(), instance.getKrameriusImportFoxmlFolder());
            json =
                    "{" +
                            "\"mapping\":" +
                            "{" +
                            "\"importDirectory\":\"" + krameriusFoxmlImport + "\"," +
                            "\"startIndexer\":\"true\"," +
                            "\"updateExisting\":\"" + updateExisting + "\"" +
                            "}" +
                            "}";
        } else if (KUtils.EXPORT_NDK.equals(exportType)) {
            query = instance.getUrl() + instance.getUrlConvertImportQuery();
            proarcExport = instance.getExportNdkFolder() + exportFolder.getName();
            String ndkImport = proarcExport.replace(instance.getExportNdkFolder(), instance.getKrameriusConvertNdkFolder());
            json = "{" +
                        "\"mapping\":" +
                        "{" +
                            "\"convertDirectory\":\"" + ndkImport + "\"," +
                            "\"convertTargetDirectory\":\"" + instance.getKrameriusTargetConvertedFolder() + "\"," +
                            "\"ingestSkip\":\"false\"," +
                            "\"startIndexer\":\"true\"," +
                            "\"defaultRights\":\"" + String.valueOf(getPolicy(policy)) + "\"" +
                        "}" +
                    "}";
        }

        URL url = new URL(query);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setConnectTimeout(5000);
        String encoded = Base64.getEncoder().encodeToString((credentials).getBytes(StandardCharsets.UTF_8));
        conn.setRequestProperty("Authorization", "Basic " + encoded);
        conn.setRequestProperty("Content-Type", "application/json; charset=UTF-8");
        conn.setDoOutput(true);
        conn.setDoInput(true);
        conn.setRequestMethod("POST");

        OutputStream os = conn.getOutputStream();
        os.write(json.getBytes("UTF-8"));
        os.close();

        int status = conn.getResponseCode();
        InputStream in = null;
        KUtils.ImportState state = null;

        try {
            if (status < HTTP_BAD_REQUEST) {
                in = new BufferedInputStream(conn.getInputStream());
                String result = IOUtils.toString(in, StandardCharsets.UTF_8);
                LOG.info("Kramerius response is " + result);
                in.close();
                conn.disconnect();

                // zjištění uuid procesu
                JSONObject object = new JSONObject(result);
                String processUuid = object.get("uuid").toString();
                query = instance.getUrl() + instance.getUrlStateQuery() + processUuid;
                state = getState(query, credentials);
                LOG.info("Requesting Kramerius import success, server response is (process: " + state.getProcessState() + ", batch: " + state.getBatchState() + ").");
                return state;
            } else {
                if (conn.getErrorStream() == null) {
                    in = new BufferedInputStream(conn.getInputStream());
                } else {
                    in = new BufferedInputStream(conn.getErrorStream());
                }
                String result = IOUtils.toString(in, "UTF-8");
                in.close();
                conn.disconnect();
                LOG.warning("Requesting Kramerius import failed, server response was : " + result);
                throw new IllegalArgumentException("Requesting Kramerius import failed, server response was : " + result);
            }
        } finally {
            if (in != null) {
                in.close();
            }
            conn.disconnect();
        }
    }

    private boolean getPolicy(String policy) {
        return "PUBLIC".equalsIgnoreCase(policy);
    }

    public static KUtils.ImportState getState(String query, String credentials) throws IOException, InterruptedException, JSONException {
        String state = KRAMERIUS_PROCESS_PLANNED;
        String batchState = KRAMERIUS_BATCH_NO_BATCH_V5;
        int error403counter = 0;

        while (state.equals(KRAMERIUS_PROCESS_PLANNED) || state.equals(KRAMERIUS_PROCESS_RUNNING) || (state.equals(KRAMERIUS_PROCESS_FINISHED) && batchState.equals(KRAMERIUS_BATCH_STARTED_V5))) {
            URL url = new URL(query);
            HttpURLConnection conn = (HttpURLConnection) url.openConnection();
            conn.setConnectTimeout(5000);
            url = new URL(query);
            conn = (HttpURLConnection) url.openConnection();
            conn.setConnectTimeout(5000);
            String encoded = Base64.getEncoder().encodeToString((credentials).getBytes(StandardCharsets.UTF_8));
            conn.setRequestProperty("Authorization", "Basic " + encoded);
            conn.setRequestProperty("Content-Type", "application/json; charset=UTF-8");
            conn.setDoOutput(true);
            conn.setDoInput(true);
            conn.setRequestMethod("GET");
            int status = conn.getResponseCode();
            InputStream in = new BufferedInputStream(conn.getInputStream());

            if (status < HTTP_BAD_REQUEST) {
                error403counter = 0;
                String result = IOUtils.toString(in, StandardCharsets.UTF_8);
                JSONObject object = (new JSONArray(result)).getJSONObject(0);
                state = object.get("state").toString();
                batchState = object.get("batchState").toString();
                LOG.info("Kramerius response to query " + query + " is " + state);
                in.close();
                conn.disconnect();
            } else {
                if (status == HTTP_FORBIDDEN) {
                    if (error403counter < 25) {
                        error403counter++;
                        TimeUnit.SECONDS.sleep(30);
                    } else {
                        in.close();
                        conn.disconnect();
                        state = KRAMERIUS_BATCH_FAILED_V5;
                        break;
                    }
                } else {
                    in.close();
                    conn.disconnect();
                    state = KRAMERIUS_BATCH_FAILED_V5;
                    break;
                }
            }
            if (state.equals(KRAMERIUS_PROCESS_PLANNED) || state.equals(KRAMERIUS_PROCESS_RUNNING) || (state.equals(KRAMERIUS_PROCESS_FINISHED) && batchState.equals(KRAMERIUS_BATCH_STARTED_V5))) {
                TimeUnit.SECONDS.sleep(20);
            }
        }
        return new KUtils.ImportState(state, batchState);
    }
}
