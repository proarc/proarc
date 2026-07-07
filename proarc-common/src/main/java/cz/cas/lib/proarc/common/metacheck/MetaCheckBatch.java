package cz.cas.lib.proarc.common.metacheck;

import org.json.JSONObject;

public class MetaCheckBatch {

    private final Integer batchId;
    private final String state;
    private final Integer proarcBatchId;
    private final String path;
    private final String log;

    public MetaCheckBatch(JSONObject json) {
        this.batchId = json.has("batchId") && !json.isNull("batchId") ? json.getInt("batchId") : null;
        this.state = json.optString("state", null);
        this.proarcBatchId = json.has("proarcBatchId") && !json.isNull("proarcBatchId") ? json.getInt("proarcBatchId") : null;
        this.path = json.optString("path", null);
        this.log = json.optString("log", null);
    }

    public Integer getBatchId() {
        return batchId;
    }

    public String getState() {
        return state;
    }

    public Integer getProarcBatchId() {
        return proarcBatchId;
    }

    public String getPath() {
        return path;
    }

    public String getLog() {
        return log;
    }
}
