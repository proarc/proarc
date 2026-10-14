package cz.cas.lib.proarc.common.process.internal;

import cz.cas.lib.proarc.common.actions.CatalogRecord;
import cz.cas.lib.proarc.common.catalog.updateCatalog.CatalogUpdateResult;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.InternalExternalProcess;
import cz.cas.lib.proarc.common.storage.akubra.SolrSearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.akubra.AkubraStorage;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import java.util.List;
import java.util.Locale;
import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class CatalogUpdateBatchTest {
    @Mocked BatchManager manager;
    @Mocked AppConfiguration config;
    @Mocked AkubraStorage storage;
    @Mocked SolrSearchView search;
    @Mocked UserUtil userUtil;
    @Mocked UserManager users;
    @Mocked UserProfile user;
    @Mocked ValidationProcess validation;
    @Mocked CatalogRecord catalog;

    @Test
    void successfulAndConflictingWritesHaveIndependentBatchResults() throws Exception {
        for (boolean warning : new boolean[]{false, true}) {
            Batch batch = batch();
            SearchViewItem item = new SearchViewItem("uuid:one");
            item.setModel("model:ndkperiodical");
            CatalogUpdateResult operationResult = warning ? CatalogUpdateResult.review("Původní odkaz: old\nNový odkaz: new")
                    : CatalogUpdateResult.success("Shodný odkaz již existuje.");
            new Expectations() {{
                manager.get(7); this.result = batch;
                manager.update(batch); this.result = batch;
                UserUtil.getDefaultManger(); this.result = users;
                users.find(1); this.result = user;
                user.hasImportToCatalogFunction(); this.result = true;
                AkubraStorage.getInstance(null); this.result = storage;
                storage.getSearch(Locale.ROOT); this.result = search;
                search.find(List.of("uuid:one")); this.result = List.of(item);
                config.getCatalogUpdateModels(); this.result = List.of("model:ndkperiodical");
                validation.validate(ValidationProcess.Type.UPDATE_CATALOG_RECORD); this.result = new ValidationProcess.Result();
                catalog.updateWithResult("library", "uuid:one"); this.result = operationResult;
            }};
            InternalExternalProcess process = InternalExternalProcess.prepare(config, null, batch, manager, user, null, Locale.ROOT);
            assertSame(batch, process.start());
            assertEquals(warning ? Batch.State.INTERNAL_WARNING : Batch.State.INTERNAL_DONE, batch.getState(), batch.getLog());
            assertTrue(batch.getLog().contains("Katalog: library"));
            assertFalse(batch.getLog().contains("Zdrojová exportní dávka"));
            assertTrue(batch.getLog().contains(operationResult.message()));
        }
    }

    @Test
    void permissionRevokedBeforeExecutionPreventsTheWrite() throws Exception {
        Batch batch = batch();
        new Expectations() {{
            manager.get(7); result = batch;
            manager.update(batch); result = batch;
            UserUtil.getDefaultManger(); result = users;
            users.find(1); result = user;
            user.hasImportToCatalogFunction(); result = false;
        }};
        CatalogUpdateProcess.run(config, null, manager, user, batch, batch.getParamsAsObject(), Locale.ROOT);
        assertEquals(Batch.State.INTERNAL_FAILED, batch.getState());
        new Verifications() {{ catalog.updateWithResult(anyString, anyString); times = 0; }};
    }

    @Test
    void cancelledOrAlreadyClaimedJobsNeverWriteAgain() throws Exception {
        for (Batch.State state : List.of(Batch.State.STOPPED, Batch.State.INTERNAL_RUNNING, Batch.State.INTERNAL_DONE)) {
            Batch batch = batch();
            batch.setState(state);
            new Expectations() {{ manager.get(7); result = batch; }};
            CatalogUpdateProcess.run(config, null, manager, user, batch, batch.getParamsAsObject(), Locale.ROOT);
            assertEquals(state, batch.getState());
        }
        new Verifications() {{ catalog.updateWithResult(anyString, anyString); times = 0; }};
    }

    private Batch batch() {
        Batch batch = new Batch();
        batch.setId(7);
        batch.setUserId(1);
        batch.setProfileId(Batch.INTERNAL_UPDATE_CATALOG_RECORDS);
        batch.setState(Batch.State.INTERNAL_PLANNED);
        BatchParams params = new BatchParams(List.of("uuid:one"));
        params.setCatalogId("library");
        batch.setParamsFromObject(params);
        return batch;
    }
}
