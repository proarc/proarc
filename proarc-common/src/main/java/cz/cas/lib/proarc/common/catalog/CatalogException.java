/*
 * Copyright (C) 2026 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.catalog;

import java.io.IOException;
import java.net.ConnectException;
import java.net.NoRouteToHostException;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;

public final class CatalogException extends IOException {

    private static final long serialVersionUID = 1L;

    private final CatalogErrorType type;

    public CatalogException(CatalogErrorType type, String message, Throwable cause) {
        super(message, cause);
        this.type = type;
    }

    public CatalogErrorType getType() {
        return type;
    }

    public static CatalogException connectionFailed(String catalog, Throwable cause) {
        return new CatalogException(
                CatalogErrorType.CONNECTION_FAILED,
                "Connection to catalog " + catalog + " failed.",
                cause);
    }

    public static CatalogException remoteError(String catalog, Throwable cause) {
        return new CatalogException(
                CatalogErrorType.REMOTE_ERROR,
                "Catalog " + catalog + " returned an error.",
                cause);
    }

    public static CatalogException transformationFailed(String catalog, Throwable cause) {
        return new CatalogException(
                CatalogErrorType.TRANSFORMATION_FAILED,
                "Catalog record from " + catalog + " could not be transformed to MODS.",
                cause);
    }

    public static CatalogException fromIOException(String catalog, IOException cause) {
        return isConnectionFailure(cause)
                ? connectionFailed(catalog, cause)
                : remoteError(catalog, cause);
    }

    public static boolean isConnectionFailure(Throwable throwable) {
        for (Throwable cause = throwable; cause != null; cause = cause.getCause()) {
            if (cause instanceof ConnectException
                    || cause instanceof NoRouteToHostException
                    || cause instanceof SocketException
                    || cause instanceof SocketTimeoutException
                    || cause instanceof UnknownHostException) {
                return true;
            }
        }
        return false;
    }
}
