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
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.externalApp.ltp.ArchivInfo;
import cz.cas.lib.proarc.common.process.external.cesnet.CesnetProcess;
import cz.cas.lib.proarc.webapp.server.rest.v1.ExportResourceV1.ExportResult;
import java.util.List;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class CesnetUploadResponseTest {
    @Test void atmSerializesLatestArchiveInfoAsObject() throws Exception {
        var object = new cz.cas.lib.proarc.common.storage.LocalStorage().create();
        var relations = new cz.cas.lib.proarc.common.storage.relation.RelationEditor(object);
        ArchivInfo record = new ArchivInfo();
        record.setProvider("cesnet");
        record.setPackageId("23");
        record.setState(ArchivInfo.State.STORED);
        record.setStoredAt("2026-11-19T15:00:00Z");
        relations.setArchivInfo(record);
        relations.write(relations.getLastModified(), "test");
        var atm = new cz.cas.lib.proarc.common.storage.AtmEditor(object, null).read();
        var mapper = new JacksonProvider().getContext(ProArcResponse.class);
        var json = mapper.readTree(mapper.writeValueAsString(new ProArcResponse<>(atm)));
        var response = json.has("response") ? json.get("response") : json;
        var data = response.get("data").get(0);
        var info = data.get("archivInfo");
        assertNotNull(info);
        assertTrue(info.isObject());
        assertEquals("23", info.get("packageId").asText());
        assertEquals("STORED", info.get("state").asText());
        assertEquals(record.getStoredAt(), info.get("storedAt").asText());
        assertFalse(data.has("preservation"));
        assertFalse(data.has("ltpRecords"));
        assertFalse(info.has("parseType"));
    }

    @Test void tokenStatusSerializesWithoutCredentials() throws Exception {
        var status = new cz.cas.lib.proarc.common.externalApp.ltpCesnet.CesnetTokenStore.Status(true, "2026-11-19T18:00:00Z");
        var mapper = new JacksonProvider().getContext(ProArcResponse.class);
        var json = mapper.readTree(mapper.writeValueAsString(new ProArcResponse<>(status)));
        var response = json.has("response") ? json.get("response") : json;
        var data = response.get("data").get(0);
        assertTrue(data.get("present").asBoolean());
        assertEquals("2026-11-19T18:00:00Z", data.get("expiresAt").asText());
        assertEquals(2, data.size());
    }
    @Test void confirmationResponseContainsPackageStateWithoutSchedulingAnExport() throws Exception {
        ArchivInfo record = new ArchivInfo();
        record.setPackageId("12");
        record.setState(ArchivInfo.State.UPLOADED);
        var result = ExportResult.confirmationRequired(List.of(CesnetProcess.warning("uuid:unit", record)));
        var mapper = new JacksonProvider().getContext(ProArcResponse.class);
        var json = mapper.readTree(mapper.writeValueAsString(new ProArcResponse<>(result)));
        var response = json.has("response") ? json.get("response") : json;
        var data = response.get("data").get(0);
        assertEquals(0, response.get("status").asInt());
        assertTrue(data.get("confirmationRequired").asBoolean());
        assertTrue(result.getExportId() == null && result.getExportIds() == null);
        var warning = data.get("warnings").get(0);
        assertEquals("uuid:unit", warning.get("pid").asText());
        assertEquals("12", warning.get("packageId").asText());
        assertEquals("UPLOADED", warning.get("state").asText());
        assertTrue(warning.get("message").asText().contains("čeká na zvalidování"));
    }
}
