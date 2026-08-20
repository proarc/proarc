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
import java.io.IOException;
import java.util.List;
import java.util.Locale;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CatalogErrorResponsesTest {

    @Test
    void createsLocalizedStructuredError() {
        CatalogException exception = CatalogException.connectionFailed(
                "test", new IOException("technical detail"));

        ProArcResponse<?> result = CatalogErrorResponses.asError(
                exception, "Testovací katalog", List.of(Locale.forLanguageTag("cs")));

        assertEquals(ProArcResponse.STATUS_VALIDATION_ERROR, result.getStatus());
        assertTrue(result.getErrors().containsKey(CatalogErrorType.CONNECTION_FAILED.getErrorKey()));
        assertEquals(
                "Ke katalogu Testovací katalog se nepodařilo připojit.",
                result.getErrors()
                        .get(CatalogErrorType.CONNECTION_FAILED.getErrorKey())
                        .get(0)
                        .getErrorMessage());
    }
}
