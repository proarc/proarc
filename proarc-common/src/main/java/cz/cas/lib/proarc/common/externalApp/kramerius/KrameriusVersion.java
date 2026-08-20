package cz.cas.lib.proarc.common.externalApp.kramerius;

enum KrameriusVersion {
    V5,
    V7,
    UNSUPPORTED;

    static KrameriusVersion from(String value) {
        if (value == null) {
            return UNSUPPORTED;
        }
        String digits = value.replaceAll("[^0-9]", "");
        if (digits.startsWith("5")) {
            return V5;
        }
        if (digits.startsWith("7")) {
            return V7;
        }
        return UNSUPPORTED;
    }
}
