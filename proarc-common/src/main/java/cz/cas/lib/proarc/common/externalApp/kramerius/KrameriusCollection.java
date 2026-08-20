package cz.cas.lib.proarc.common.externalApp.kramerius;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

public final class KrameriusCollection {

    private final String pid;
    private final Map<String, String> names;
    private final Map<String, String> descriptions;

    public KrameriusCollection(String pid, Map<String, String> names) {
        this(pid, names, Collections.emptyMap());
    }

    public KrameriusCollection(
            String pid,
            Map<String, String> names,
            Map<String, String> descriptions
    ) {
        this.pid = pid;
        this.names = Collections.unmodifiableMap(new LinkedHashMap<>(names));
        this.descriptions = Collections.unmodifiableMap(new LinkedHashMap<>(descriptions));
    }

    public String getPid() {
        return pid;
    }

    public Map<String, String> getNames() {
        return names;
    }

    public Map<String, String> getDescriptions() {
        return descriptions;
    }
}
