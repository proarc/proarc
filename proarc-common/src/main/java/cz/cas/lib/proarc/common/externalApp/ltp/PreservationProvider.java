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
package cz.cas.lib.proarc.common.externalApp.ltp;

import java.io.File;
import java.io.IOException;
import org.json.JSONObject;

/** Provider-specific authentication and transfer, independent of ProArc storage and catalogs. */
public interface PreservationProvider extends AutoCloseable {
    record RemotePackage(String id, Integer version, ArchivInfo.State state,
                         String integrityStatus, String uploadStatus) { }

    @FunctionalInterface
    interface RegistrationListener {
        void registered(RemotePackage remote) throws IOException;
    }

    RemotePackage submit(File archive, JSONObject metadata, ArchivInfo submission,
                         RegistrationListener listener) throws IOException;
    RemotePackage check(String packageId) throws IOException;
    default RemotePackage recover(String operationId) throws IOException { return null; }
    @Override void close() throws IOException;
}
