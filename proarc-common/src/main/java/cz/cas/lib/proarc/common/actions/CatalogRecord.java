package cz.cas.lib.proarc.common.actions;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.CatalogConfiguration;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.fedora.FoxmlUtils;
import cz.cas.lib.proarc.common.fedora.XmlStreamEditor;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.mods.ModsStreamEditor;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.RecordIdentifierDefinition;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.logging.Logger;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicHeader;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import static java.net.HttpURLConnection.HTTP_OK;

public class CatalogRecord {

    private static final Logger LOG = Logger.getLogger(CatalogRecord.class.getName());

    private AppConfiguration appConfig;
    private AkubraConfiguration akubraConfiguration;
    private UserProfile user;

    public CatalogRecord(AppConfiguration appConfig, AkubraConfiguration akubraConfiguration, UserProfile user) {
        this.appConfig = appConfig;
        this.akubraConfiguration = akubraConfiguration;
        this.user = user;
    }


    public boolean update(String catalogId, String pid) throws DigitalObjectException, JSONException, IOException {

        String object001 = getObjectField001(pid);
        if (object001 == null || object001.isEmpty()) {
            throw new DigitalObjectException(pid, "Missing field 001");
        }

        CatalogConfiguration bCatalog = appConfig.getCatalogs().findConfiguration(catalogId);

        if (bCatalog != null) {
            if (bCatalog.allowCatalogUpdateRecord()) {
                String verbisToken = getCatalogToken(bCatalog);
                return updateRecord(bCatalog, verbisToken, object001, pid);
            } else {
                LOG.severe("Catalog with id " + catalogId + " does not support Record modification");
                throw new IOException("Catalog with id " + catalogId + " does not support Record modification");
            }
        } else {
            LOG.severe("No catalog configuration for id " + catalogId);
            throw new IOException("No catalog configuration for id " + catalogId);
        }
    }

    private boolean updateRecord(CatalogConfiguration bCatalog, String catalogToken, String field001, String uuid) throws IOException {
        validateValues(catalogToken, field001, uuid);

        HttpClient httpClient = HttpClients.createDefault();
        HttpPost httpPost = new HttpPost(bCatalog.getCatalogUpdateUrl());
        httpPost.setHeader(new BasicHeader("Authorization", String.format("Bearer %s", catalogToken)));
        httpPost.setHeader(new BasicHeader("Content-Type", "application/json; charset=UTF-8"));

        String json = "{" +
                "    \"reusing\": [{\"id\": \"f1:" + field001 + "\"}]," +
                "    \"value\": [" +
                "        {\n" +
                "                \"reusing\": [{" +
                "                       \"type\": \"D." + bCatalog.getUpdateField() + "\"," +
                "                       \"havingField\": {\"type\": \"D." + bCatalog.getUpdateField() + "." + bCatalog.getUpdateSubfieldApp() + "\", \"value\": \"proarcId\"}" +
                "                }]," +
                "            \"set\": {" +
                "                \"type\": \"D." + bCatalog.getUpdateField() + "\"," +
                "                \"value\": [" +
                "                    {" +
                "                        \"reusing\": [{\"type\": \"D." + bCatalog.getUpdateField() + "." + bCatalog.getUpdateSubfieldApp() + "\"}]," +
                "                        \"set\": {\"type\": \"D." + bCatalog.getUpdateField() + "." + bCatalog.getUpdateSubfieldApp() + "\", \"value\": \"proarcId\"}" +
                "                    }," +
                "                    {" +
                "                        \"reusing\": [{\"type\": \"D." + bCatalog.getUpdateField() + "." + bCatalog.getUpdateSubfieldObject() + "\"}]," +
                "                        \"set\": {\"type\": \"D." + bCatalog.getUpdateField() + "." + bCatalog.getUpdateSubfieldObject() + "\", \"value\": \"" + uuid + "\"}" +
                "                    }," +
                "                    {" +
                "                        \"reusing\": [{\"type\": \"D." + bCatalog.getUpdateField() + "." + bCatalog.getUpdateSubfieldDigitalized() + "\"}]," +
                "                        \"set\": {\"type\": \"D." + bCatalog.getUpdateField() + "." + bCatalog.getUpdateSubfieldDigitalized() + "\", \"value\": \"zdigitalizováno\"}" +
                "                    }" +
                "                ]" +
                "            }" +
                "        }" +
                "    ]" +
                "}";

        json = json.replaceAll("\\s+"," ");
        httpPost.setEntity(new StringEntity(json, ContentType.APPLICATION_JSON));

        HttpResponse response = httpClient.execute(httpPost);
        if (HTTP_OK == response.getStatusLine().getStatusCode()) {
            HttpEntity entity = response.getEntity();
            return true;
        } else {
            LOG.severe(EntityUtils.toString(response.getEntity()));
            throw new IOException("Connecing to Catalog ended with code " + response.getStatusLine().getStatusCode());
        }
    }

    private void validateValues(String verbisToken, String field001, String uuid) throws IOException {
        if (verbisToken == null || verbisToken.isEmpty()) {
            throw new IOException("Verbis token is null or empty");
        }
        if (field001 == null || field001.isEmpty()) {
            throw new IOException("Field 001 id is null or empty");
        }
        if (uuid == null || uuid.isEmpty()) {
            throw new IOException("UUID is null or empty");
        }
    }

    private String getCatalogToken(CatalogConfiguration catalogConfig) throws IOException, JSONException {
        String client_id = catalogConfig.getCatalogUsername();
        String client_secret = catalogConfig.getCatalogPassword();

        String encode = Base64.getEncoder().encodeToString((client_id + ":" + client_secret).getBytes(StandardCharsets.UTF_8));

        HttpClient httpClient = HttpClients.createDefault();
        HttpPost httpPost = new HttpPost(catalogConfig.getCatalogAuthorizationUrl());
        httpPost.setHeader(new BasicHeader("Content-Type", "application/x-www-form-urlencoded"));
        httpPost.setHeader(new BasicHeader("Authorization", String.format("Basic %s", encode)));

        List<NameValuePair> params = new ArrayList<>();
        params.add(new BasicNameValuePair("grant_type", "client_credentials"));

        httpPost.setEntity(new UrlEncodedFormEntity(params, "UTF-8"));

        HttpResponse response = httpClient.execute(httpPost);
        if (HTTP_OK == response.getStatusLine().getStatusCode()) {
            HttpEntity entity = response.getEntity();
            if (entity != null) {
                String result = EntityUtils.toString(response.getEntity());
                if (result != null && !result.isEmpty()) {
                    JSONObject object = new JSONObject(result);
                    String accessToken = object.getString("access_token");
                    if (accessToken == null || accessToken.isEmpty()) {
                        throw new IOException("Connected to Catalog but access token is null or empty: " + accessToken);
                    } else {
                        return accessToken;
                    }
                } else {
                    throw new IOException("Connected to Catalog but unexpected response." + result);
                }
            } else {
                throw new IOException("Connected to Catalog but entity is null");
            }
        } else {
            throw new IOException("Connecing to Catalog ended with code " + response.getStatusLine().getStatusCode());
        }
    }

    private String getObjectField001(String pid) throws DigitalObjectException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        FedoraObject fo = dom.find(pid, null);

        XmlStreamEditor xml = fo.getEditor(FoxmlUtils.inlineProfile(MetadataHandler.DESCRIPTION_DATASTREAM_ID, ModsConstants.NS, MetadataHandler.DESCRIPTION_DATASTREAM_LABEL));
        ModsStreamEditor modsStreamEditor = new ModsStreamEditor(xml, fo);
        ModsDefinition mods = modsStreamEditor.read();

        for (RecordInfoDefinition recordInfo : mods.getRecordInfo()) {
            for (RecordIdentifierDefinition recordInfoIdentifier : recordInfo.getRecordIdentifier()) {
                return recordInfoIdentifier.getValue();
            }
        }
        return null;
    }
}
