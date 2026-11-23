/*
 * Copyright (C) 2026 Lukas Sykora
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
package cz.cas.lib.proarc.common.externalApp.ltpCesnet;

import cz.cas.lib.proarc.common.externalApp.ltp.PreservationProvider;
import cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo;
import java.io.File;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.io.RandomAccessFile;
import java.net.URI;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Locale;
import org.apache.commons.configuration2.Configuration;
import org.apache.http.HttpHeaders;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;

/** CESNET API v2: application token -> registration -> S3 parts -> completion. */
public final class LtpCesnetClient implements PreservationProvider {
    public static final String ID = "cesnet";
    private final String endpoint;
    private final String group;
    private final TokenProvider credentials;
    private final CloseableHttpClient client;

    public interface TokenProvider {
        String get() throws IOException;
        default void rejected(String token) { }
    }

    public LtpCesnetClient(Configuration configuration) {
        this(configuration, () -> configuration.getString("token"));
    }

    public LtpCesnetClient(Configuration configuration, TokenProvider credentials) {
        this.credentials = credentials;
        endpoint = configuration.getString("apiUrl", "https://ltp.cesnet.cz/api").replaceAll("/+$", "");
        group = configuration.getString("groupToken");
        RequestConfig timeouts = RequestConfig.custom()
                .setConnectTimeout(configuration.getInt("connectTimeout", 30000))
                .setConnectionRequestTimeout(configuration.getInt("connectTimeout", 30000))
                .setSocketTimeout(configuration.getInt("readTimeout", 300000)).build();
        // POST/PUT are explicitly retried by the workflow, never implicitly by the HTTP client.
        client = HttpClients.custom().setDefaultRequestConfig(timeouts)
                .disableAutomaticRetries().disableRedirectHandling().build();
    }

    private String token() throws IOException {
        String token = credentials.get();
        if (token == null || token.isBlank()) throw new CesnetTokenException(CesnetTokenException.Reason.MISSING);
        if (token.chars().anyMatch(Character::isWhitespace)) throw new IOException("Invalid CESNET token format");
        return token;
    }

    /** Read-only preflight; never creates an archive or a new application token. */
    public void validateToken() throws IOException {
        execute(authorize(new HttpGet(endpoint + "/auth/account/")), 200);
    }

    private HttpRequestBase authorize(HttpRequestBase request) throws IOException {
        request.setHeader(HttpHeaders.AUTHORIZATION, "Token " + token());
        request.setHeader(HttpHeaders.ACCEPT, "application/json");
        return request;
    }

    private String groupQuery() throws IOException {
        if (group == null || group.isBlank()) throw new IOException("Missing export.ltpCesnet.groupToken");
        return "group=" + URLEncoder.encode(group, StandardCharsets.UTF_8);
    }

    @Override
    public RemotePackage check(String packageId) throws IOException {
        return readPackage(object(execute(authorize(new HttpGet(endpoint + "/v2/archive/"
                + id(packageId) + "/?" + groupQuery())), 200)));
    }

    @Override
    public RemotePackage submit(File archive, JSONObject metadata, ArchivInfo submission,
                                RegistrationListener listener) throws IOException {
        if (!archive.isFile() || archive.length() == 0) throw new IOException("Missing or empty CESNET archive");
        JSONObject data = new JSONObject(metadata.toString());
        JSONObject userMetadata = data.optJSONObject("user_metadata");
        if (userMetadata == null) userMetadata = new JSONObject();
        userMetadata.put("proarc_operation", submission.getOperationId());
        data.put("user_metadata", userMetadata).put("group", group).put("file_size", archive.length());
        groupQuery();
        // Reconcile a lost response before registering anything again. Metadata equality is checked locally.
        RemotePackage existing = submission.getPackageId() == null
                ? findOperation(submission.getOperationId()) : check(submission.getPackageId());
        JSONObject session;
        if (existing != null) {
            listener.registered(existing);
            if (existing.state() == ArchivInfo.State.STORED
                    || existing.state() == ArchivInfo.State.UPLOADED) return existing;
            if (existing.state() == ArchivInfo.State.FAILED) {
                throw new IOException("CESNET package failed validation or upload; a new export is required");
            }
            HttpPost resume = new HttpPost(endpoint + "/v2/archive/" + id(existing.id()) + "/create_multipart/");
            resume.setEntity(json(data));
            session = object(execute(authorize(resume), 200, 201));
        } else {
            // A confirmed repeat is an independent package, never a new archive version.
            HttpPost create = new HttpPost(endpoint + "/v2/archive/");
            create.setEntity(json(data));
            session = object(execute(authorize(create), 200, 201));
            String packageId = session.optString("id", "");
            if (packageId.isBlank()) {
                existing = findOperation(submission.getOperationId());
                if (existing == null) throw new IOException("CESNET registration returned no recoverable package ID");
            } else {
                existing = new RemotePackage(id(packageId), session.has("version") ? session.getInt("version") : null,
                        ArchivInfo.State.UPLOADING, null, null);
            }
            // Persist the ID before sending the first byte to S3.
            listener.registered(existing);
        }
        upload(archive, session);
        RemotePackage completed = findOperation(submission.getOperationId());
        if (completed == null) completed = check(existing.id());
        if (completed.state() == ArchivInfo.State.FAILED) throw new IOException("CESNET rejected the uploaded archive");
        return completed;
    }

    private RemotePackage findOperation(String operationId) throws IOException {
        for (int offset = 0; ; offset += 100) {
            JSONObject page = object(execute(authorize(new HttpGet(endpoint + "/v2/archive/?"
                    + groupQuery() + "&limit=100&offset=" + offset)), 200));
            JSONArray results = page.getJSONArray("results");
            for (int i = 0; i < results.length(); i++) {
                JSONObject item = results.getJSONObject(i);
                JSONObject metadata = item.optJSONObject("user_metadata");
                if (metadata != null && operationId.equals(metadata.optString("proarc_operation"))) return readPackage(item);
            }
            if (results.isEmpty() || offset + results.length() >= page.getInt("count")) return null;
        }
    }

    @Override public RemotePackage recover(String operationId) throws IOException { return findOperation(operationId); }

    private void upload(File archive, JSONObject session) throws IOException {
        long chunkSize = session.getLong("chunk_size");
        // Bound memory independently of total archive size; honor the server's multipart boundaries.
        if (chunkSize <= 0 || chunkSize > 256L * 1024 * 1024) throw new IOException("Unsupported CESNET chunk size");
        JSONArray urls = session.getJSONArray("parts_url");
        long count = (archive.length() + chunkSize - 1) / chunkSize;
        if (count != urls.length()) throw new IOException("Incomplete CESNET multipart registration");
        JSONArray parts = new JSONArray();
        try (RandomAccessFile input = new RandomAccessFile(archive, "r")) {
            for (int part = 0; part < count; part++) {
                interrupted();
                byte[] bytes = new byte[(int) Math.min(chunkSize, archive.length() - input.getFilePointer())];
                input.readFully(bytes);
                IOException failure = null;
                for (int attempt = 0; attempt < 3; attempt++) {
                    interrupted();
                    // Presigned S3 requests must never receive the CESNET authorization header.
                    HttpPut put = new HttpPut(URI.create(urls.getString(part)));
                    put.setEntity(new ByteArrayEntity(bytes, ContentType.APPLICATION_OCTET_STREAM));
                    try (CloseableHttpResponse response = client.execute(put)) {
                        int status = response.getStatusLine().getStatusCode();
                        if (status < 200 || status >= 300 || response.getFirstHeader("ETag") == null) {
                            throw new IOException("CESNET S3 part " + (part + 1) + " failed (HTTP " + status + ")");
                        }
                        parts.put(new JSONObject().put("PartNumber", part + 1)
                                .put("ETag", response.getFirstHeader("ETag").getValue().replace("\"", "")));
                        EntityUtils.consume(response.getEntity());
                        failure = null;
                        break;
                    } catch (IOException ex) { failure = ex; }
                }
                if (failure != null) throw failure;
            }
        }
        HttpPost finish = new HttpPost(session.getString("finish_url"));
        finish.setEntity(json(new JSONObject().put("parts", parts).put("filename", archive.getName())
                .put("checksum_update", session.get("checksum_update"))
                .put("upload_id", session.get("upload_id")).put("origin", session.get("origin"))));
        // Like the reference client, finish uses the signed URL, without API credentials.
        execute(finish, 200, 201, 202, 204);
    }

    public static RemotePackage readPackage(JSONObject value) throws IOException {
        String integrity = value.optString("integrity_status", "");
        JSONObject data = value.optJSONObject("data_status");
        Object upload = data == null ? null : data.opt("upload_status");
        if (upload == null && data != null) upload = data.opt("upload");
        if (upload instanceof JSONObject object) upload = object.opt("status");
        String uploadStatus = upload == null ? "" : upload.toString();
        String description = value.optString("data_status_description", "").toLowerCase(Locale.ROOT);
        boolean stored = "2".equals(uploadStatus) || "Stored In Archive".equalsIgnoreCase(uploadStatus)
                || description.contains("stored in archive");
        boolean valid = "1".equals(integrity) || "Archive is valid".equalsIgnoreCase(integrity);
        boolean failed = "3".equals(uploadStatus) || uploadStatus.toLowerCase(Locale.ROOT).contains("failed")
                || description.contains("upload failed") || "2".equals(integrity) || "3".equals(integrity)
                || "4".equals(integrity) || integrity.toLowerCase(Locale.ROOT).matches(".*(invalid|deleted|degraded).*");
        ArchivInfo.State state = failed ? ArchivInfo.State.FAILED
                : stored && valid ? ArchivInfo.State.STORED
                : stored || "1".equals(uploadStatus) || "Uploading To Archive".equalsIgnoreCase(uploadStatus)
                || description.contains("uploading to archive") ? ArchivInfo.State.UPLOADED
                : ArchivInfo.State.UPLOADING;
        return new RemotePackage(id(value.get("id").toString()), value.has("version") ? value.getInt("version") : null,
                state, integrity, uploadStatus.isBlank() ? value.optString("data_status_description") : uploadStatus);
    }

    private static String id(String id) throws IOException {
        if (id == null || !id.matches("[0-9]+")) throw new IOException("Invalid CESNET package ID");
        return id;
    }

    private static JSONObject object(String response) throws IOException {
        try {
            Object value = new JSONTokener(response).nextValue();
            if (value instanceof String string) value = new JSONTokener(string).nextValue();
            if (value instanceof JSONObject object) return object;
        } catch (RuntimeException ex) { throw new IOException("Invalid CESNET JSON response"); }
        throw new IOException("Expected a CESNET JSON object");
    }

    private static StringEntity json(JSONObject value) { return new StringEntity(value.toString(), ContentType.APPLICATION_JSON); }

    private static void interrupted() throws InterruptedIOException {
        if (Thread.currentThread().isInterrupted()) throw new InterruptedIOException("CESNET transfer interrupted");
    }

    private String execute(HttpRequestBase request, int... expected) throws IOException {
        interrupted();
        try (CloseableHttpResponse response = client.execute(request)) {
            int status = response.getStatusLine().getStatusCode();
            org.apache.http.Header authorization = request.getFirstHeader(HttpHeaders.AUTHORIZATION);
            if (status == 401 && authorization != null && authorization.getValue().startsWith("Token ")) {
                credentials.rejected(authorization.getValue().substring("Token ".length()));
                throw new CesnetTokenException(CesnetTokenException.Reason.REJECTED);
            }
            for (int code : expected) if (code == status) {
                return response.getEntity() == null ? "" : EntityUtils.toString(response.getEntity(), StandardCharsets.UTF_8);
            }
            // Error bodies and signed URLs can contain credentials; never include them in persisted batch errors.
            throw new IOException("CESNET " + request.getMethod() + " failed (HTTP " + status + ")");
        } catch (CesnetTokenException ex) {
            throw ex;
        } catch (IOException ex) {
            throw new IOException("CESNET " + request.getMethod() + " failed; check connectivity and API configuration"
                    + (ex.getMessage() != null && ex.getMessage().startsWith("CESNET ") ? ": " + ex.getMessage() : ""));
        }
    }

    @Override public void close() throws IOException { client.close(); }
}
