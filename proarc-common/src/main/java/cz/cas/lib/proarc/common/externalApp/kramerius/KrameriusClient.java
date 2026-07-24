package cz.cas.lib.proarc.common.externalApp.kramerius;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dublincore.DcStreamEditor;
import cz.cas.lib.proarc.common.externalApp.HttpAbstractClient;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.ocr.AltoDatastream;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.FoxmlUtils;
import cz.cas.lib.proarc.common.storage.MixEditor;
import cz.cas.lib.proarc.common.storage.relation.RelationEditor;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;
import java.util.stream.Stream;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.message.BasicNameValuePair;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import static java.net.HttpURLConnection.HTTP_ACCEPTED;
import static java.net.HttpURLConnection.HTTP_CREATED;
import static java.net.HttpURLConnection.HTTP_OK;

/**
 * Client for all communication with supported Kramerius versions.
 *
 * <p>The instance-specific paths and credentials are passed to individual
 * operations while the underlying HTTP connection pool is shared for the
 * lifetime of this client.</p>
 */
public final class KrameriusClient extends HttpAbstractClient {

    private static final Logger LOG = Logger.getLogger(KrameriusClient.class.getName());
    private static final String XML_CONTENT_TYPE = "application/xml; charset=UTF-8";
    private static final Set<String> XML_STREAM_IDS = Set.of(
            ModsStreamEditor.DATASTREAM_ID,
            DcStreamEditor.DATASTREAM_ID,
            AltoDatastream.ALTO_ID,
            RelationEditor.DATASTREAM_ID,
            MixEditor.RAW_ID,
            MixEditor.NDK_ARCHIVAL_ID,
            FoxmlUtils.DS_AUDIT_ID);
    private static final Set<PosixFilePermission> SHARED_DIRECTORY_PERMISSIONS =
            PosixFilePermissions.fromString("rwxrwxrwx");

    private AuthenticationKey authenticationKey;
    private String accessToken;

    public KrameriusClient(String apiUrl) {
        super(apiUrl);
    }

    public String authenticate(KrameriusOptions.KrameriusInstance instance) throws IOException, JSONException {
        requireVersion(instance, KrameriusVersion.V7);
        String loginUrl = isBlank(instance.getUrlKeycloak())
                ? resolve(instance.getUrlLogin()).toString()
                : append(instance.getUrlKeycloak(), instance.getUrlLogin());

        List<NameValuePair> params = new ArrayList<>();
        params.add(new BasicNameValuePair("username", instance.getUsername()));
        params.add(new BasicNameValuePair("password", instance.getPassword()));
        params.add(new BasicNameValuePair("client_id", instance.getClientId()));
        params.add(new BasicNameValuePair("client_secret", instance.getClientSecret()));
        params.add(new BasicNameValuePair("grant_type", instance.getGrantType()));

        HttpPost request = new HttpPost(URI.create(loginUrl));
        request.setHeader("Keep-Alive", "timeout=600, max=1000");
        request.setHeader("Connection", "Keep-Alive, Upgrade");
        request.setEntity(new UrlEncodedFormEntity(params, StandardCharsets.UTF_8));

        String response = execute(request, HTTP_OK);
        String token = readAccessToken(response);
        if (isBlank(token)) {
            throw new IOException("Kramerius authentication response does not contain access_token.");
        }
        authenticationKey = AuthenticationKey.from(instance);
        accessToken = token;
        return token;
    }

    public List<KrameriusOptions.KrameriusInstance.KrameriusLicense> getLicenses(
            KrameriusOptions.KrameriusInstance instance
    ) throws IOException, JSONException {
        requireVersion(instance, KrameriusVersion.V7);
        HttpGet request = new HttpGet(resolve(instance.getUrlLicense()));
        addCommonHeaders(request);
        JSONArray response = new JSONArray(execute(request, HTTP_OK));
        List<KrameriusOptions.KrameriusInstance.KrameriusLicense> licenses = new ArrayList<>();
        for (int i = 0; i < response.length(); i++) {
            JSONObject license = response.getJSONObject(i);
            licenses.add(new KrameriusOptions.KrameriusInstance.KrameriusLicense(
                    license.optString("id"),
                    license.optString("name"),
                    license.optString("description")));
        }
        return licenses;
    }

    public String downloadFoxml(
            KrameriusOptions.KrameriusInstance instance,
            String krameriusPid
    ) throws IOException, JSONException {
        requireVersion(instance, KrameriusVersion.V7);
        String token = getAccessToken(instance);
        HttpGet request = new HttpGet(URI.create(
                resolve(instance.getUrlDownloadFoxml()).toString() + krameriusPid + "/foxml"));
        addCommonHeaders(request);
        addBearerToken(request, token);
        String foxml = execute(request, HTTP_OK);
        if (isBlank(foxml)) {
            throw new IOException("Downloaded FOXML is empty.");
        }
        return foxml;
    }

    public void downloadFoxml(
            AppConfiguration appConfig,
            KrameriusOptions.KrameriusInstance instance,
            String krameriusPid
    ) throws IOException, JSONException {
        saveFoxml(appConfig, instance, downloadFoxml(instance, krameriusPid), krameriusPid);
    }

    public void saveFoxml(
            AppConfiguration appConfig,
            KrameriusOptions.KrameriusInstance instance,
            String foxmlContent,
            String krameriusPid
    ) throws IOException {
        Path pidFoxml = KUtils.getFile(appConfig, instance.getId(), krameriusPid).toPath();
        Files.writeString(
                pidFoxml,
                foxmlContent,
                StandardCharsets.UTF_8,
                StandardOpenOption.CREATE,
                StandardOpenOption.TRUNCATE_EXISTING);
    }

    public void uploadXml(
            KrameriusOptions.KrameriusInstance instance,
            String pid,
            String xml,
            String streamId
    ) throws IOException, DigitalObjectException, JSONException {
        requireVersion(instance, KrameriusVersion.V7);
        if (isBlank(xml)) {
            throw new DigitalObjectException(pid, streamId + " to upload is null or empty");
        }
        if (isBlank(pid)) {
            throw new DigitalObjectException(pid, "PID to update is null or empty");
        }

        HttpPut request = new HttpPut(URI.create(
                resolve(instance.getUrlUploadStream()).toString() + pid + "/akubra/updateManaged/" + streamId));
        addCommonHeaders(request);
        addBearerToken(request, getAccessToken(instance));
        request.setHeader("Content-Type", getMimeType(streamId));
        request.setEntity(new ByteArrayEntity(xml.getBytes(StandardCharsets.UTF_8)));
        execute(request, HTTP_OK, HTTP_CREATED, HTTP_ACCEPTED);
    }

    public KUtils.ImportState indexDocument(
            KrameriusOptions.KrameriusInstance instance,
            String pid
    ) throws IOException, DigitalObjectException, JSONException, InterruptedException {
        requireVersion(instance, KrameriusVersion.V7);
        if (isBlank(pid)) {
            throw new DigitalObjectException(pid, "PID to index is null or empty");
        }

        String token = getAccessToken(instance);
        HttpPost request = new HttpPost(resolve(instance.getUrlIndex()));
        addBearerToken(request, token);
        request.setHeader("Connection", "keep-alive");
        JSONObject payload = new JSONObject()
                .put("defid", "new_indexer_index_object")
                .put("params", new JSONObject()
                        .put("type", "TREE_AND_FOSTER_TREES")
                        .put("pid", pid)
                        .put("ignoreInconsistentObjects", true));
        request.setEntity(new StringEntity(payload.toString(), ContentType.APPLICATION_JSON));
        JSONObject response = new JSONObject(execute(request, HTTP_OK, HTTP_CREATED, HTTP_ACCEPTED));
        String processUuid = response.optString("uuid");
        if (isBlank(processUuid)) {
            throw new IOException("Kramerius did not return the indexing process UUID.");
        }
        return K7Importer.getState(processUuid, token, instance);
    }

    public KUtils.ImportState importToKramerius(
            KrameriusOptions.KrameriusInstance instance,
            File exportFolder,
            boolean updateExisting,
            String exportType,
            String policy,
            String license
    ) throws JSONException, IOException, InterruptedException {
        allowSupportedVersion(instance);
        prepareExportFolder(exportFolder);
        validateExportType(exportType);
        KrameriusImporter importer = switch (instance.getMajorVersion()) {
            case V5 -> new K5Importer(instance);
            case V7 -> new K7Importer(instance, getAccessToken(instance));
            case UNSUPPORTED -> throw unsupportedVersion(instance);
        };
        return importer.importToKramerius(
                exportFolder, updateExisting, exportType, policy, license);
    }

    private void prepareExportFolder(File exportFolder) throws IOException {
        if (exportFolder == null || !exportFolder.isDirectory()) {
            throw new IOException("Kramerius export folder does not exist or is not a directory: " + exportFolder);
        }
        if (!FileSystems.getDefault().supportedFileAttributeViews().contains("posix")) {
            return;
        }
        try (Stream<Path> paths = Files.walk(exportFolder.toPath())) {
            paths.forEach(path -> setSharedPermissions(path, exportFolder));
        }
    }

    private String getMimeType(String streamId) throws IOException {
        if (XML_STREAM_IDS.contains(streamId)) {
            return XML_CONTENT_TYPE;
        }
        throw new IOException("Unsupported streamId " + streamId);
    }

    private String readAccessToken(String response) throws JSONException {
        if (response.startsWith("{")) {
            return new JSONObject(response).optString("access_token");
        }
        if (response.startsWith("[")) {
            JSONArray values = new JSONArray(response);
            for (int i = 0; i < values.length(); i++) {
                String token = values.getJSONObject(i).optString("access_token");
                if (!isBlank(token)) {
                    return token;
                }
            }
        }
        return null;
    }

    private void addCommonHeaders(org.apache.http.client.methods.HttpRequestBase request) {
        request.setHeader("Keep-Alive", "timeout=600, max=1000");
        request.setHeader("Connection", "Keep-Alive, Upgrade");
        request.setHeader("Accept-Language", "cs,en;q=0.9,de;q=0.8,cs-CZ;q=0.7,sk;q=0.6");
    }

    private void addBearerToken(org.apache.http.client.methods.HttpRequestBase request, String token) {
        if (!isBlank(token)) {
            request.setHeader("Authorization", "Bearer " + token);
        }
    }

    private String getAccessToken(KrameriusOptions.KrameriusInstance instance) throws IOException, JSONException {
        if (AuthenticationKey.from(instance).equals(authenticationKey) && !isBlank(accessToken)) {
            return accessToken;
        }
        return authenticate(instance);
    }

    private String append(String baseUrl, String path) {
        if (baseUrl.endsWith("/") && path.startsWith("/")) {
            return baseUrl + path.substring(1);
        }
        if (!baseUrl.endsWith("/") && !path.startsWith("/")) {
            return baseUrl + "/" + path;
        }
        return baseUrl + path;
    }

    private void allowSupportedVersion(KrameriusOptions.KrameriusInstance instance) throws IOException {
        if (instance == null || instance.getMajorVersion() == KrameriusVersion.UNSUPPORTED) {
            throw unsupportedVersion(instance);
        }
    }

    private void requireVersion(
            KrameriusOptions.KrameriusInstance instance,
            KrameriusVersion required
    ) throws IOException {
        if (instance == null || instance.getMajorVersion() != required) {
            throw new IOException("Operation is supported only for Kramerius "
                    + (required == KrameriusVersion.V7 ? "7.x." : "5.x."));
        }
    }

    private IOException unsupportedVersion(KrameriusOptions.KrameriusInstance instance) {
        String version = instance == null ? null : instance.getVersion();
        return new IOException("Unknown Kramerius version \"" + version
                + "\". Expected values are 5.x or 7.x.");
    }

    private void validateExportType(String exportType) {
        if (!KUtils.EXPORT_KRAMERIUS.equals(exportType) && !KUtils.EXPORT_NDK.equals(exportType)) {
            throw new IllegalArgumentException("Unsupported Kramerius export type: " + exportType);
        }
    }

    private boolean isBlank(String value) {
        return value == null || value.trim().isEmpty();
    }

    private void setSharedPermissions(Path path, File exportFolder) {
        try {
            Files.setPosixFilePermissions(path, SHARED_DIRECTORY_PERMISSIONS);
        } catch (IOException ex) {
            LOG.fine("Cannot change permissions in " + exportFolder + ": " + ex.getMessage());
        }
    }

    private record AuthenticationKey(
            String url,
            String keycloakUrl,
            String loginPath,
            String username,
            String password,
            String clientId,
            String clientSecret,
            String grantType
    ) {
        static AuthenticationKey from(KrameriusOptions.KrameriusInstance instance) {
            return new AuthenticationKey(
                    instance.getUrl(),
                    instance.getUrlKeycloak(),
                    instance.getUrlLogin(),
                    instance.getUsername(),
                    instance.getPassword(),
                    instance.getClientId(),
                    instance.getClientSecret(),
                    instance.getGrantType());
        }
    }
}
