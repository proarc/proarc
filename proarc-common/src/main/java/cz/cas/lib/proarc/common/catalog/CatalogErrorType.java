/*
 * Copyright (C) 2026 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.catalog;

public enum CatalogErrorType {

    CONNECTION_FAILED("catalog.connection-failed"),
    REMOTE_ERROR("catalog.remote-error"),
    TRANSFORMATION_FAILED("catalog.transformation-failed");

    private final String errorKey;

    CatalogErrorType(String errorKey) {
        this.errorKey = errorKey;
    }

    public String getErrorKey() {
        return errorKey;
    }
}
