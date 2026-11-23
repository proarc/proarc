/*
 * Copyright (C) 2026 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.externalApp.ltpCesnet;

import java.io.IOException;

/** Safe to display and persist; contains neither credentials nor remote response bodies. */
public final class CesnetTokenException extends IOException {
    public enum Reason { MISSING, EXPIRED, REJECTED }
    private final Reason reason;

    public CesnetTokenException(Reason reason) {
        super(switch (reason) {
            case MISSING -> "Chybí přístupový token CESNET. Zadejte nový token.";
            case EXPIRED -> "Uplynula osmihodinová doba uložení tokenu CESNET. Zadejte nový token.";
            case REJECTED -> "CESNET odmítl přístupový token (HTTP 401). Zadejte nový token a opakujte nahrání.";
        });
        this.reason = reason;
    }

    public Reason getReason() { return reason; }
}
