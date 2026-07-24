package cz.cas.lib.proarc.common.externalApp.kramerius;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

public final class KrameriusCollection {

    private final String pid;
    private final Map<String, String> names;

    public KrameriusCollection(String pid, Map<String, String> names) {
        this.pid = pid;
        this.names = Collections.unmodifiableMap(new LinkedHashMap<>(names));
    }

    public String getPid() {
        return pid;
    }

    public Map<String, String> getNames() {
        return names;
    }
}
