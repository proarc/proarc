package cz.cas.lib.proarc.common.metacheck;

import org.json.JSONObject;

public class MetaCheckEngine {

    private final String name;
    private final String description;
    private final String version;
    private final Boolean defaultEngine;
    private final Boolean active;

    public MetaCheckEngine(JSONObject json) {
        this.name = json.optString("name", null);
        this.description = json.optString("description", null);
        this.version = json.optString("version", null);
        this.defaultEngine = json.has("defaultEngine") && !json.isNull("defaultEngine") ? json.getBoolean("defaultEngine") : null;
        this.active = json.has("active") && !json.isNull("active") ? json.getBoolean("active") : null;
    }

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }

    public String getVersion() {
        return version;
    }

    public Boolean getDefaultEngine() {
        return defaultEngine;
    }

    public Boolean getActive() {
        return active;
    }
}
