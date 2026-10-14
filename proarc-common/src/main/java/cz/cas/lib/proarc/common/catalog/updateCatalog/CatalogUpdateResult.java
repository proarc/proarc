package cz.cas.lib.proarc.common.catalog.updateCatalog;

/** Successful catalog operation, optionally requiring review. Failures are exceptions. */
public final class CatalogUpdateResult {
    private final boolean warning;
    private final String message;

    public CatalogUpdateResult(boolean warning, String message) {
        this.warning = warning;
        this.message = message;
    }

    public boolean warning() { return warning; }
    public String message() { return message; }

    public static CatalogUpdateResult success(String message) {
        return new CatalogUpdateResult(false, message);
    }

    public static CatalogUpdateResult review(String message) {
        return new CatalogUpdateResult(true, message);
    }
}
