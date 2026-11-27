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
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class PreservationServiceTest {
    static class Memory implements PreservationService.Repository {
        ArchivInfo record;
        int writes;
        @Override public ArchivInfo read(String pid) { return record; }
        @Override public void save(String pid, ArchivInfo value) { record = value; writes++; }
    }
    static class Provider implements PreservationProvider {
        int uploads;
        RemotePackage remote = remote("12", 1, ArchivInfo.State.UPLOADED);
        @Override public RemotePackage submit(File file, JSONObject metadata, ArchivInfo record, RegistrationListener listener) throws IOException {
            uploads++;
            listener.registered(new RemotePackage(remote.id(), remote.version(), ArchivInfo.State.UPLOADING, null, null));
            return remote;
        }
        @Override public RemotePackage check(String id) { return remote; }
        @Override public void close() { }
    }
    static PreservationProvider.RemotePackage remote(String id, int version, ArchivInfo.State state) {
        return new PreservationProvider.RemotePackage(id, version, state, "1", state == ArchivInfo.State.STORED ? "2" : "1");
    }
    static ArchivInfo request(String operation) {
        ArchivInfo record = new ArchivInfo();
        record.setProvider("cesnet"); record.setEndpoint("https://example.test/api"); record.setGroup("group");
        record.setOperationId(operation); record.setCatalogId("verbis"); return record;
    }

    @Test void pendingCheckDoesNotWriteAndCatalogFailureIsRetriedWithoutAnotherUpload() throws Exception {
        Memory memory = new Memory(); Provider provider = new Provider(); AtomicInteger catalog = new AtomicInteger();
        PreservationService service = new PreservationService(memory, record -> provider, (pid, record) -> {
            if (catalog.incrementAndGet() == 1) throw new IOException("Catalog offline");
        });
        service.submit("uuid:unit", new File("unused"), new JSONObject(), request("one"));
        int writes = memory.writes;
        service.check("uuid:unit");
        assertEquals(writes, memory.writes);
        assertEquals(0, catalog.get());
        provider.remote = remote("12", 1, ArchivInfo.State.STORED);
        service.check("uuid:unit");
        assertEquals(0, catalog.get());
        assertThrows(IOException.class, () -> service.updateCatalog("uuid:unit"));
        assertEquals(ArchivInfo.State.STORED, memory.record.getState());
        assertEquals(ArchivInfo.CatalogState.PENDING, memory.record.getCatalogState());
        assertFalse(PreservationIndex.pending(memory.record), "Catalog failure must not restart CESNET checks");
        service.updateCatalog("uuid:unit"); service.updateCatalog("uuid:unit");
        assertEquals(2, catalog.get()); assertEquals(1, provider.uploads);
        assertEquals(ArchivInfo.CatalogState.DONE, memory.record.getCatalogState());
    }

    @Test void storedPackageWithoutCatalogFinishesWithoutCatalogWrite() throws Exception {
        for (String catalogId : new String[]{null, "", "   "}) {
            Memory memory = new Memory(); Provider provider = new Provider();
            provider.remote = remote("12", 1, ArchivInfo.State.STORED);
            PreservationService service = new PreservationService(memory, record -> provider,
                    (pid, record) -> fail("No catalog is configured"));
            ArchivInfo request = request("one"); request.setCatalogId(catalogId);
            service.submit("uuid:unit", new File("unused"), new JSONObject(), request);
            service.check("uuid:unit");
            service.updateCatalog("uuid:unit");
            assertEquals(ArchivInfo.State.STORED, memory.record.getState());
            assertFalse(PreservationIndex.pending(memory.record));
        }
    }

    @Test void checkUsesCurrentCatalogConfigurationInsteadOfUploadConfiguration() throws Exception {
        for (String catalogId : new String[]{null, "", "   ", "current-catalog"}) {
            Memory memory = new Memory(); Provider provider = new Provider();
            List<String> catalogIds = new ArrayList<>();
            PreservationService service = new PreservationService(memory, record -> provider,
                    (pid, record) -> catalogIds.add(record.getCatalogId()));
            service.submit("uuid:unit", new File("unused"), new JSONObject(), request("one"));
            provider.remote = remote("12", 1, ArchivInfo.State.STORED);
            service.check("uuid:unit", catalogId);
            assertEquals(catalogId, memory.record.getCatalogId());
            assertFalse(PreservationIndex.pending(memory.record));
            service.updateCatalog("uuid:unit");
            assertEquals(catalogId == null || catalogId.isBlank() ? List.of() : List.of(catalogId), catalogIds);
        }
    }

    @Test void storedPackageFromOldIndexIsSavedToClearPendingWithoutRemoteCheck() throws Exception {
        Memory memory = new Memory();
        memory.record = request("one");
        memory.record.setPackageId("12");
        memory.record.setState(ArchivInfo.State.STORED);
        PreservationService service = new PreservationService(memory, record -> {
            fail("Stored package must not be checked remotely"); return null;
        }, (pid, record) -> fail("No catalog is configured"));
        service.check("uuid:unit", null);
        assertEquals(1, memory.writes);
        assertFalse(PreservationIndex.pending(memory.record));
        assertFalse(PreservationService.needsCatalogUpdate(memory.record));
    }

    @Test void confirmedRepeatReplacesCurrentPackageAndSameOperationDoesNotUploadTwice() throws Exception {
        Memory memory = new Memory(); Provider provider = new Provider();
        PreservationService service = new PreservationService(memory, record -> provider, (pid, record) -> { });
        provider.remote = remote("12", 1, ArchivInfo.State.STORED);
        service.submit("uuid:unit", new File("unused"), new JSONObject(), request("one"));
        service.submit("uuid:unit", new File("unused"), new JSONObject(), request("one"));
        provider.remote = remote("23", 1, ArchivInfo.State.UPLOADED);
        assertThrows(IOException.class, () -> service.submit("uuid:unit", new File("unused"), new JSONObject(), request("two")));
        assertEquals("12", memory.record.getPackageId());
        service.submit("uuid:unit", new File("unused"), new JSONObject(), request("two"), true);
        assertEquals(2, provider.uploads); assertNotNull(memory.record);
        assertNull(memory.record.getPreviousPackageId());
        assertEquals("23", memory.record.getPackageId());
        assertNull(memory.record.getStoredAt());
        assertEquals(ArchivInfo.CatalogState.PENDING, memory.record.getCatalogState());
        assertThrows(IOException.class, () -> service.submit("uuid:unit", new File("unused"), new JSONObject(), request("three")));
        provider.remote = remote("34", 1, ArchivInfo.State.UPLOADED);
        service.submit("uuid:unit", new File("unused"), new JSONObject(), request("three"), true);
        assertNotNull(memory.record);
        assertEquals("34", memory.record.getPackageId());
    }

    @Test void registrationSurvivesInterruptedTransferAndRetryReusesId() throws Exception {
        Memory memory = new Memory(); AtomicInteger calls = new AtomicInteger();
        PreservationService service = new PreservationService(memory, record -> new Provider() {
            @Override public RemotePackage submit(File file, JSONObject metadata, ArchivInfo record, RegistrationListener listener) throws IOException {
                if (calls.incrementAndGet() == 1) {
                    listener.registered(remote("12", 1, ArchivInfo.State.UPLOADING));
                    throw new IOException("Connection lost");
                }
                assertEquals("12", record.getPackageId());
                return remote("12", 1, ArchivInfo.State.UPLOADED);
            }
        }, (pid, record) -> { });
        assertThrows(IOException.class, () -> service.submit("uuid:unit", new File("unused"), new JSONObject(), request("one")));
        assertEquals("12", memory.record.getPackageId());
        service.submit("uuid:unit", new File("unused"), new JSONObject(), request("one"));
        assertNotNull(memory.record); assertEquals(ArchivInfo.State.UPLOADED, memory.record.getState());
    }

    @Test void onlyReplacementIsCheckedAndWrittenToCatalogAfterValidation() throws Exception {
        Memory memory = new Memory();
        List<String> checked = new ArrayList<>();
        List<String> catalogIds = new ArrayList<>();
        Provider provider = new Provider() {
            @Override public RemotePackage check(String id) { checked.add(id); return remote; }
        };
        PreservationService service = new PreservationService(memory, record -> provider,
                (pid, record) -> catalogIds.add(record.getPackageId()));
        service.submit("uuid:unit", new File("unused"), new JSONObject(), request("one"));
        provider.remote = remote("23", 1, ArchivInfo.State.UPLOADED);
        service.submit("uuid:unit", new File("unused"), new JSONObject(), request("two"), true);
        service.check("uuid:unit");
        assertTrue(catalogIds.isEmpty());
        provider.remote = remote("23", 1, ArchivInfo.State.STORED);
        service.check("uuid:unit");
        service.check("uuid:unit");
        service.updateCatalog("uuid:unit");
        service.updateCatalog("uuid:unit");
        assertEquals(List.of("23", "23"), checked);
        assertEquals(List.of("23"), catalogIds);
        assertNotNull(memory.record);
    }
}
