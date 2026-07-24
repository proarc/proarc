package cz.cas.lib.proarc.common.externalApp.metacheck;

import cz.cas.lib.proarc.common.externalApp.HttpAbstractClient;
import cz.cas.lib.proarc.common.object.ValueMap;
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

    public ValueMap getEnginesList() throws IOException, JSONException {
        List<MetaCheckEnginesDescriptor> engineDescriptors = new ArrayList<>();
        List<MetaCheckEngine> engines = listEngines();
        for (int i = 0; i < engines.size(); i++) {
            MetaCheckEngine engine = engines.get(i);
            engineDescriptors.add(new MetaCheckEnginesDescriptor(
                    i + 1,
                    engine.getName(),
                    engine.getDescription()));
        }
        return new ValueMap<>("metakat.engines", engineDescriptors);
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

    public static class MetaCheckEnginesDescriptor {

        private final int id;
        private final String label;
        private final String description;

        public MetaCheckEnginesDescriptor(int id, String label, String description) {
            this.id = id;
            this.label = label;
            this.description = description;
        }

        public int getId() {
            return id;
        }

        public String getLabel() {
            return label;
        }

        public String getDescription() {
            return description;
        }
    }
}
