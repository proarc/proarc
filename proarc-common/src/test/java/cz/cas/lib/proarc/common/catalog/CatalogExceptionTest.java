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
import java.net.SocketTimeoutException;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

class CatalogExceptionTest {

    @Test
    void classifiesNestedNetworkFailure() {
        SocketTimeoutException timeout = new SocketTimeoutException("timeout");
        IOException io = new IOException("request failed", timeout);

        CatalogException result = CatalogException.fromIOException("test", io);

        assertEquals(CatalogErrorType.CONNECTION_FAILED, result.getType());
        assertSame(io, result.getCause());
    }

    @Test
    void classifiesOtherIoFailureAsRemoteError() {
        IOException io = new IOException("invalid response");

        CatalogException result = CatalogException.fromIOException("test", io);

        assertEquals(CatalogErrorType.REMOTE_ERROR, result.getType());
        assertSame(io, result.getCause());
    }
}
