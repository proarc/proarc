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

import cz.cas.lib.proarc.common.export.ExportOptions;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import org.apache.commons.io.IOUtils;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import static cz.cas.lib.proarc.common.export.automaticImportProcess.AutomaticImportUtils.getState;
import static java.net.HttpURLConnection.HTTP_BAD_REQUEST;


/**
 * @author Lukáš Sýkora
 * na zaklade aplikace Merlin z MZK
 */
public class AutomaticImportFoxml {

    private final ExportOptions.KrameriusInstance instance;
    private final File exportFolder;

    public AutomaticImportFoxml(ExportOptions.KrameriusInstance instance, File exportFolder) {
        this.instance = instance;
        this.exportFolder = exportFolder;
    }

    public String importToKramerius() throws JSONException, IOException, InterruptedException {
        String credentials = instance.getUsername() + ":" + instance.getPassword();

        String proarcExport = instance.getExportFolder() + exportFolder.getName();
        String krameriusFoxmlImport = proarcExport.replace(instance.getExportFolder(), instance.getKrameriusImportFoxmlFolder());

        String query = instance.getUrl() + instance.getParametrizedImportQuery();
        String json =
                "{" +
                        "\"mapping\":" +
                        "{" +
                        "\"importDirectory\":\"" + krameriusFoxmlImport + "\"," +
                        "\"startIndexer\":\"true\"," +
                        "\"updateExisting\":\"false\"" +
                        "}" +
                        "}";

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
        String state = null;

        try {
            if (status < HTTP_BAD_REQUEST) {
                in = new BufferedInputStream(conn.getInputStream());
                String result = IOUtils.toString(in, StandardCharsets.UTF_8);
                System.out.println("Kramerius response:");
                System.out.println(result);
                in.close();
                conn.disconnect();

                // zjištění uuid procesu
                JSONObject object = new JSONObject(result);
                String processUuid = object.get("uuid").toString();
                query = instance.getUrl() + instance.getStateQuery() + processUuid;
                state = getState(query, credentials);
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
                throw new IllegalArgumentException("Requesting Kramerius import failed, server response was : " + result);
            }
        } finally {
            if (in != null) {
               in.close();
            }
            conn.disconnect();
        }
    }
}
