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
package cz.cas.lib.proarc.common.process.external.cesnet;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.DaoFactory;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.IOException;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.*;

class CesnetCheckProcessTest {
    @TempDir
    Path temp;
    MemoryManager manager;
    UserProfile user;

    @BeforeEach
    void setup() throws Exception {
        Files.writeString(temp.resolve("proarc.cfg"), "");
        AppConfiguration config = AppConfigurationFactory.getInstance().create(
                new HashMap<>(Map.of(AppConfiguration.PROPERTY_APP_HOME, temp.toString())));
        manager = new MemoryManager(config);
        user = new UserProfile();
        user.setId(1);
        user.setAllObjectsFunction(true);
    }

    CesnetCheckProcess process(CesnetCheckProcess.Checker checker, CesnetCheckProcess.CatalogUpdater updater) {
        return new CesnetCheckProcess(manager, user, checker, token -> {
        }, updater);
    }

    @Test
    void checkWithoutCatalogPermissionHasItsOwnBatchAndLogAndDoesNotScheduleCatalog() throws Exception {
        CesnetCheckProcess process = process((log, candidate) -> {
            log.accept("uuid:one: STORED; aktualizace katalogu je vypnuta (katalog není nakonfigurován)");
            return 1;
        }, pid -> fail("Unexpected catalog write"));
        Batch batch = process.plan();
        assertEquals(Batch.EXTERNAL_CESNET_CHECK, batch.getProfileId());
        assertEquals(Batch.State.EXTERNAL_PLANNED, batch.getState());
        process.run(batch, next -> fail("Unexpected catalog process"));
        assertEquals(Batch.State.EXTERNAL_DONE, batch.getState());
        assertTrue(batch.getLog().contains("uuid:one: STORED"));
        assertTrue(batch.getLog().contains("Počet zkontrolovaných objektů: 1"));
        assertEquals(1, manager.batches.size());
    }

    @Test
    void missingPidsRequireAllObjectsPermission() throws Exception {
        CesnetCheckProcess process = process((log, candidate) -> {
            fail("Unexpected check without allObjectsFunction");
            return 0;
        }, pid -> fail("Unexpected catalog write"));
        Batch batch = process.plan();
        var params = batch.getParamsAsObject();
        params.setPids(null);
        batch.setParamsFromObject(params);
        user.setAllObjectsFunction(false);
        process.run(batch, next -> fail("Unexpected catalog process"));
        assertEquals(Batch.State.EXTERNAL_FAILED, batch.getState());
        assertTrue(batch.getLog().contains("Přístup odepřen: je vyžadováno oprávnění allObjectsFunction"));
    }

    @Test
    void catalogPermissionIsCheckedInSeparateBatchWithoutFailingSuccessfulCheck() throws Exception {
        List<String> written = new ArrayList<>();
        CesnetCheckProcess process = process((log, candidate) -> {
            log.accept("uuid:one: STORED");
            candidate.accept("uuid:one");
            return 1;
        }, written::add);
        Batch check = process.plan();
        List<Batch> queued = new ArrayList<>();
        process.run(check, queued::add);
        assertEquals(Batch.State.EXTERNAL_DONE, check.getState());
        Batch catalog = queued.get(0);
        assertEquals(Batch.EXTERNAL_CESNET_CATALOG, catalog.getProfileId());
        assertEquals(catalog.getId(), check.getParamsAsObject().getNextBatchId());
        assertEquals(check.getId(), catalog.getParamsAsObject().getSourceBatchId());
        process.updateCatalog(catalog);
        assertEquals(Batch.State.EXTERNAL_FAILED, catalog.getState());
        assertTrue(catalog.getLog().contains("je vyžadováno oprávnění importToCatalogFunction"));
        assertTrue(written.isEmpty());
        assertEquals(Batch.State.EXTERNAL_DONE, check.getState());
        user.setImportToCatalogFunction(true);
        process.updateCatalog(catalog);
        assertEquals(Batch.State.EXTERNAL_DONE, catalog.getState());
        assertEquals(List.of("uuid:one"), written);
    }

    @Test
    void packageAndFatalErrorsRetainEarlierLogsInFailedProcess() throws Exception {
        CesnetCheckProcess process = process((log, candidate) -> {
            log.accept("uuid:one: STORED");
            log.accept("CHYBA: uuid:two: unavailable");
            throw new IOException("Solr unavailable");
        }, pid -> fail("Unexpected catalog write"));
        Batch batch = process.plan();
        process.run(batch, next -> fail());
        assertEquals(Batch.State.EXTERNAL_FAILED, batch.getState());
        assertTrue(batch.getLog().contains("uuid:one: STORED"));
        assertTrue(batch.getLog().contains("uuid:two: unavailable"));
        assertTrue(batch.getLog().contains("Solr unavailable"));
    }

    @Test
    void tokenFailureIsLoggedAndDoesNotRunCheck() throws Exception {
        CesnetCheckProcess process = new CesnetCheckProcess(manager, user,
                (log, candidate) -> {
                    fail("Unexpected check");
                    return 0;
                },
                token -> {
                    throw new IOException("Missing CESNET token");
                }, pid -> fail());
        Batch batch = process.plan();
        process.run(batch, next -> fail());
        assertEquals(Batch.State.EXTERNAL_FAILED, batch.getState());
        assertTrue(batch.getLog().contains("Missing CESNET token"));
    }

    @Test
    void catalogFailureRetainsLogsAndContinuesWithOtherPackages() throws Exception {
        user.setImportToCatalogFunction(true);
        List<String> written = new ArrayList<>();
        CesnetCheckProcess process = process((log, candidate) -> {
            candidate.accept("uuid:bad");
            candidate.accept("uuid:good");
            return 2;
        }, pid -> {
            if (pid.equals("uuid:bad")) throw new IOException("Catalog offline");
            written.add(pid);
        });
        List<Batch> queued = new ArrayList<>();
        Batch check = process.plan();
        process.run(check, queued::add);
        Batch catalog = queued.get(0);
        process.updateCatalog(catalog);
        assertEquals(Batch.State.EXTERNAL_DONE, check.getState());
        assertEquals(Batch.State.EXTERNAL_FAILED, catalog.getState());
        assertEquals(List.of("uuid:good"), written);
        assertTrue(catalog.getLog().contains("uuid:bad: Catalog offline"));
        assertTrue(catalog.getLog().contains("uuid:good: synchronizace katalogu byla dokončena"));
    }

    @Test
    void stoppedCheckKeepsStoppedStateAndDoesNotStartCatalogProcess() throws Exception {
        CesnetCheckProcess process = process((log, candidate) -> {
            candidate.accept("uuid:one");
            manager.batches.get(1).setState(Batch.State.STOPPED);
            log.accept("uuid:one: STORED");
            return 1;
        }, pid -> fail());
        Batch batch = process.plan();
        process.run(batch, next -> fail("Check was stopped"));
        assertEquals(Batch.State.STOPPED, batch.getState());
        assertEquals(1, manager.batches.size());
    }

    static class MemoryManager extends BatchManager {
        final Map<Integer, Batch> batches = new LinkedHashMap<>();

        MemoryManager(AppConfiguration config) {
            super(config, (DaoFactory) Proxy.newProxyInstance(DaoFactory.class.getClassLoader(),
                    new Class<?>[]{DaoFactory.class}, (proxy, method, args) -> {
                        throw new AssertionError("Unexpected database access");
                    }));
        }

        @Override
        public Batch update(Batch batch) {
            if (batch.getId() == null) batch.setId(batches.size() + 1);
            batches.put(batch.getId(), batch);
            return batch;
        }

        @Override
        public Batch get(int id) {
            return batches.get(id);
        }
    }
}
