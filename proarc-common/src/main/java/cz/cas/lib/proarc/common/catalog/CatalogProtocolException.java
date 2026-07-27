/*
 * Copyright (C) 2026 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package cz.cas.lib.proarc.common.catalog;

import javax.xml.transform.TransformerException;

final class CatalogProtocolException extends TransformerException {

    private static final long serialVersionUID = 1L;

    CatalogProtocolException(String message, Throwable cause) {
        super(message, cause);
    }
}
