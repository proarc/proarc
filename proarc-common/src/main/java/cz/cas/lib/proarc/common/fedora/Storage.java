package cz.cas.lib.proarc.common.fedora;

public enum Storage {

    LOCAL, FEDORA, AKUBRA;

    public static Storage fromString(String value) {
        if (value != null) {
            value = value.toUpperCase();
            for (Storage storage : values()) {
                if (storage.name().equals(value)) {
                    return storage;
                }
            }
        }
        return null;
    }
}
