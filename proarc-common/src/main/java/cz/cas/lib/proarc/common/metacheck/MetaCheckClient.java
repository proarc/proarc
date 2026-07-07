package cz.cas.lib.proarc.common.metacheck;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.message.BasicNameValuePair;
import org.json.JSONArray;
import org.json.JSONException;

public class MetaCheckClient extends HttpAbstractClient {

    public MetaCheckClient(String apiUrl) {
        super(apiUrl);
    }

    public List<MetaCheckEngine> listEngines() throws IOException, JSONException {
        JSONArray enginesJson = getArray("/rest/v1/batch/engines");
        List<MetaCheckEngine> engines = new ArrayList<>();
        for (int i = 0; i < enginesJson.length(); i++) {
            engines.add(new MetaCheckEngine(enginesJson.getJSONObject(i)));
        }
        return engines;
    }

    public MetaCheckBatch addBatch(String folder, Integer engine, Integer proarcBatchId) throws IOException, JSONException {
        List<NameValuePair> params = new ArrayList<>();
        params.add(new BasicNameValuePair("folder", folder));
        if (engine != null) {
            params.add(new BasicNameValuePair("engine", String.valueOf(engine)));
        }
        if (proarcBatchId != null) {
            params.add(new BasicNameValuePair("proarcBatchId", proarcBatchId.toString()));
        }
        return new MetaCheckBatch(postForm("/rest/v1/batch", new UrlEncodedFormEntity(params, StandardCharsets.UTF_8), 202));
    }
}
