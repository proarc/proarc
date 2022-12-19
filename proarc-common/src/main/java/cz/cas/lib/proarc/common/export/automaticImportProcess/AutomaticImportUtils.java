/*
 * Copyright (C) 2022 Lukas Sykora
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

package cz.cas.lib.proarc.common.export.automaticImportProcess;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.concurrent.TimeUnit;
import org.apache.commons.io.IOUtils;
import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;

/**
 * @author Lukáš Sýkora
 * na zaklade aplikace Merlin z MZK
 */
public class AutomaticImportUtils {

    public static final String KRAMERIUS_PROCESS_FINISHED = "FINISHED";
    public static final String KRAMERIUS_PROCESS_WARNING = "WARNING";
    public static final String KRAMERIUS_PROCESS_FAILED = "FAILED";
    public static final String KRAMERIUS_PROCESS_ERROR = "ERROR";
    public static final String KRAMERIUS_PROCESS_PLANNED = "PLANNED";
    public static final String KRAMERIUS_PROCESS_RUNNING = "RUNNING";

    public static String getState(String query, String credentials) throws IOException, InterruptedException, JSONException {
        String state = KRAMERIUS_PROCESS_PLANNED;

        while (state.equals(KRAMERIUS_PROCESS_PLANNED) || state.equals(KRAMERIUS_PROCESS_RUNNING)) {
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
                String result = IOUtils.toString(in, StandardCharsets.UTF_8);
                JSONObject object = (new JSONArray(result)).getJSONObject(0);
                state = object.get("state").toString();
                in.close();
                conn.disconnect();
            } else {
                in.close();
                conn.disconnect();
                break;
            }
            if (state.equals(KRAMERIUS_PROCESS_PLANNED) || state.equals(KRAMERIUS_PROCESS_RUNNING)) {
                TimeUnit.SECONDS.sleep(20);
            }
        }
        return state;
    }
}
