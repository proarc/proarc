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
package cz.cas.lib.proarc.common.storage.akubra;

import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;
import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.common.SolrInputDocument;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import static org.junit.jupiter.api.Assertions.*;

class SolrLogFeederTest {
    @Mocked SolrClient client;
    @Mocked UserUtil userUtil;
    @Mocked UserManager users;

    private final SolrLogMetadata snapshot = new SolrLogMetadata("model:ndkpage", Set.of("device:scanner"), Set.of("ABA001"), Set.of("software:scanner"));

    private SolrLogFeeder feeder() {
        return new SolrLogFeeder(client) {
            @Override public SolrLogMetadata metadataFor(String pid) { return snapshot; }
        };
    }

    @ParameterizedTest
    @ValueSource(strings = {"INGEST", "UPDATE", "DEACTIVATE", "RESTORE", "PURGE"})
    void recordsObjectSnapshotAndTheActingUsersOrganization(String operation) throws Exception {
        UserProfile actor = UserProfile.create("actor", "password", "Actor");
        actor.setOrganization("Library A");
        new Expectations() {{ UserUtil.getDefaultManger(); result = users; users.find("actor"); result = actor; }};
        Instant before = Instant.now();
        feeder().feedLogChange("actor", "uuid:page", "MODS", operation);
        new Verifications() {{
            SolrInputDocument document;
            client.add(document = withCapture()); times = 1;
            assertEquals("actor", document.getFieldValue("user"));
            assertEquals("Library A", document.getFieldValue("organization"));
            assertEquals("model:ndkpage", document.getFieldValue("model"));
            assertEquals(Set.of("device:scanner"), Set.copyOf(document.getFieldValues("device")));
            assertEquals(Set.of("software:scanner"), Set.copyOf(document.getFieldValues("software")));
            assertEquals(Set.of("ABA001"), Set.copyOf(document.getFieldValues("sigla")));
            assertEquals(operation, document.getFieldValue("operation"));
            assertEquals("uuid:page", document.getFieldValue("pid"));
            assertFalse(Instant.parse((String) document.getFieldValue("date")).isBefore(before.minusMillis(1)));
            assertNull(document.getFieldValue("pageCount"));
            client.commit(); times = 1;
        }};
    }

    @Test
    void exportRecordsCountsAndBatchIdentityAndKeepsSeparateExports() throws Exception {
        UserProfile actor = UserProfile.create("actor", "password", "Actor");
        actor.setOrganization("Library B");
        new Expectations() {{ UserUtil.getDefaultManger(); result = users; users.find("actor"); result = actor; }};
        SolrLogFeeder feeder = feeder();
        feeder.feedExportLog("uuid:root", actor, snapshot, 3_000_000_000L, 3, "EXPORT_NDK");
        feeder.feedExportLog("uuid:root", actor, snapshot, 0, 1, "EXPORT_NDK");
        feeder.feedExportLog("uuid:root", actor, snapshot, 3_000_000_000L, 3, "EXPORT_NDK");
        new Verifications() {{
            List<SolrInputDocument> documents = new ArrayList<>();
            client.add(withCapture(documents)); times = 3;
            SolrInputDocument first = documents.get(0);
            assertEquals("EXPORT", first.getFieldValue("operation"));
            assertEquals("actor", first.getFieldValue("user"));
            assertEquals(Set.of("software:scanner"), Set.copyOf(first.getFieldValues("software")));
            assertEquals(null, first.getFieldValue("stream"));
            assertEquals("EXPORT_NDK", first.getFieldValue("exportType"));
            assertEquals("Library B", first.getFieldValue("organization"));
            assertEquals(3_000_000_000L, first.getFieldValue("pageCount"));
            assertEquals(3L, first.getFieldValue("otherModelCount"));;
            assertNotEquals(first.getFieldValue("source"), documents.get(1).getFieldValue("source"));
            assertEquals(0L, documents.get(1).getFieldValue("pageCount"));
            assertEquals(first.getFieldValue("pid"), documents.get(2).getFieldValue("pid"));
        }};
    }

    @Test
    void articleExportStoresDoiAndUsesSeparateEventIdentity() throws Exception {
        new Expectations() {{ UserUtil.getDefaultManger(); result = users; }};
        var metadata = new SolrLogMetadata("model:ndkarticle", Set.of(), Set.of(), Set.of(), Set.of("10.1234/article"));
        feeder().feedArticleExportLog("uuid:article", null, metadata, "exportProfile.crossref");
        new Verifications() {{
            SolrInputDocument document; client.add(document = withCapture());
            assertEquals("EXPORT", document.getFieldValue("operation"));
            assertEquals("exportProfile.crossref", document.getFieldValue("exportType"));
            assertEquals(Set.of("10.1234/article"), Set.copyOf(document.getFieldValues("doi")));
            assertEquals("UNKNOWN", document.getFieldValue("user"));
        }};
    }

    @Test
    void keepsExistingUserWithoutOrganization() throws Exception {
        UserProfile actor = UserProfile.create("actor", "password", "Actor");
        new Expectations() {{ UserUtil.getDefaultManger(); result = users; users.find("actor"); result = actor; }};
        feeder().feedIngestLog("uuid:root", "actor");
        new Verifications() {{
            SolrInputDocument document; client.add(document = withCapture());
            assertEquals("actor", document.getFieldValue("user"));
            assertNull(document.getFieldValue("organization"));
        }};
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {" ", "Upgrade MODS metadata to 3.8", "deleted-user", "UNKNOWN"})
    void normalizesMissingUsersAndLogMessages(String actor) throws Exception {
        new Expectations() {{ UserUtil.getDefaultManger(); result = users; }};
        feeder().feedLogChange(actor, "uuid:root", "MODS", "UPDATE");
        new Verifications() {{
            SolrInputDocument document; client.add(document = withCapture());
            assertEquals("UNKNOWN", document.getFieldValue("user"));
            assertNull(document.getFieldValue("organization"));
        }};
    }

    @Test
    void rejectsInexactUserLookupMatch() throws Exception {
        UserProfile actor = UserProfile.create("actor", "password", "Actor");
        actor.setOrganization("Library A");
        new Expectations() {{ UserUtil.getDefaultManger(); result = users; users.find("act%"); result = actor; }};
        feeder().feedIngestLog("uuid:root", "act%");
        new Verifications() {{
            SolrInputDocument document; client.add(document = withCapture());
            assertEquals("UNKNOWN", document.getFieldValue("user"));
            assertNull(document.getFieldValue("organization"));
        }};
    }

    @Test
    void exportRevalidatesUserAndDoesNotUseOrganizationFromDeletedProfile() throws Exception {
        UserProfile deleted = UserProfile.create("deleted", "password", "Deleted");
        deleted.setOrganization("Old library");
        new Expectations() {{ UserUtil.getDefaultManger(); result = users; users.find("deleted"); result = null; }};
        feeder().feedExportLog("uuid:root", deleted, snapshot, 10, 1, "EXPORT_NDK");
        new Verifications() {{
            SolrInputDocument document; client.add(document = withCapture());
            assertEquals("UNKNOWN", document.getFieldValue("user"));
            assertNull(document.getFieldValue("organization"));
            assertEquals(10L, document.getFieldValue("pageCount"));
        }};
    }

    @Test
    void purgeUsesSnapshotCapturedBeforeDeletingTheObject() throws Exception {
        new SolrLogFeeder(client).feedPurgeLog("uuid:deleted", "actor", snapshot);
        new Verifications() {{
            SolrInputDocument document;
            client.add(document = withCapture()); times = 1;
            assertEquals("PURGE", document.getFieldValue("operation"));
            assertEquals(snapshot.model(), document.getFieldValue("model"));
        }};
    }

    @Test
    void writesRealUtcEvenWhenServerUsesPragueTime() throws Exception {
        TimeZone original = TimeZone.getDefault();
        try {
            TimeZone.setDefault(TimeZone.getTimeZone("Europe/Prague"));
            Instant before = Instant.now();
            feeder().feedIngestLog("uuid:root", "actor");
            new Verifications() {{
                SolrInputDocument document;
                client.add(document = withCapture());
                Instant date = Instant.parse((String) document.getFieldValue("date"));
                assertTrue(date.isAfter(before.minusSeconds(1)) && date.isBefore(Instant.now().plusSeconds(1)));
            }};
        } finally { TimeZone.setDefault(original); }
    }

    @Test
    void propagatesLoggingFailure() throws Exception {
        new Expectations() {{ client.add((SolrInputDocument) any); result = new java.io.IOException("unavailable"); }};
        assertThrows(DigitalObjectException.class, () -> feeder().feedIngestLog("uuid:root", "actor"));
    }
}
