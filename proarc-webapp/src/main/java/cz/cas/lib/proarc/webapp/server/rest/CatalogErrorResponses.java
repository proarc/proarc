/*
 * Copyright (C) 2026 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.catalog.CatalogErrorType;
import cz.cas.lib.proarc.common.catalog.CatalogException;
import java.util.List;
import java.util.Locale;

public final class CatalogErrorResponses {

    private static final String SEARCH_FAILED_KEY = "catalog.search-failed";

    private CatalogErrorResponses() {
    }

    public static ProArcResponse<?> asError(
            CatalogException exception, String catalog, List<Locale> acceptableLanguages) {

        boolean czech = isCzech(acceptableLanguages);
        String message = switch (exception.getType()) {
            case CONNECTION_FAILED -> czech
                    ? "Ke katalogu " + catalog + " se nepodařilo připojit."
                    : "Could not connect to catalog " + catalog + ".";
            case REMOTE_ERROR -> czech
                    ? "Katalog " + catalog + " vrátil chybu."
                    : "Catalog " + catalog + " returned an error.";
            case TRANSFORMATION_FAILED -> czech
                    ? "Záznam z katalogu " + catalog + " se nepodařilo převést do MODS."
                    : "A record from catalog " + catalog + " could not be transformed to MODS.";
        };
        return ProArcResponse.asError(exception.getType().getErrorKey(), message);
    }

    public static ProArcResponse<?> unexpectedError(List<Locale> acceptableLanguages) {
        String message = isCzech(acceptableLanguages)
                ? "Při vyhledávání v katalogu došlo k neočekávané chybě."
                : "An unexpected error occurred while searching the catalog.";
        return ProArcResponse.asError(SEARCH_FAILED_KEY, message);
    }

    private static boolean isCzech(List<Locale> acceptableLanguages) {
        return acceptableLanguages != null
                && !acceptableLanguages.isEmpty()
                && "cs".equals(acceptableLanguages.get(0).getLanguage());
    }
}
