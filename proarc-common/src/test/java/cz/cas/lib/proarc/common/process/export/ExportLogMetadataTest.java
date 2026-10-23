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
package cz.cas.lib.proarc.common.process.export;

import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.SearchView;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.storage.akubra.SolrLogMetadata;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import mockit.Expectations;
import mockit.Mocked;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class ExportLogMetadataTest {
    @Mocked SearchView search;

    private SolrLogMetadata metadata(String model, String title) {
        return new SolrLogMetadata(model, Set.of("scanner"), Set.of("ABA001"), Set.of("software"),
                Set.of("10.1234/issue"), Set.of("urn:nbn:cz:issue"), title);
    }

    private SearchViewItem parent(String pid, String model) {
        var parent = new SearchViewItem(); parent.setPid(pid); parent.setModel(model); return parent;
    }

    @Test
    void preparesIssueTitleThroughVolumeAndAvoidsCyclesWithoutChangingOtherMetadata() throws Exception {
        var issue = metadata("model:ndkperiodicalissue", "2, 23.10.2026");
        var volume = parent("volume", "model:ndkperiodicalvolume");
        var periodical = parent("periodical", "model:ndkperiodical");
        Map<String, SolrLogMetadata> objects = Map.of("issue", issue,
                "periodical", metadata("model:ndkperiodical", "Journal"));
        new Expectations() {{
            search.findReferrers("issue"); result = List.of(volume);
            search.findReferrers("volume"); result = List.of(volume, periodical);
        }};
        var result = new ExportLogMetadata(objects::get, search).resolve("issue");
        assertEquals("Journal, 2, 23.10.2026", result.label());
        assertEquals(issue.model(), result.model()); assertEquals(issue.urnNbns(), result.urnNbns());
        assertEquals(issue.dois(), result.dois()); assertEquals(issue.devices(), result.devices());
        assertEquals(issue.software(), result.software()); assertEquals(issue.siglas(), result.siglas());
        assertEquals("2, 23.10.2026", issue.label());
    }

    @Test
    void nonIssuesUseTheirOwnMetadataWithoutParentLookupsAndReadEachObjectOnce() throws Exception {
        var book = metadata("model:ndkmonographunit", "Book");
        AtomicInteger reads = new AtomicInteger();
        new Expectations() {{ search.findReferrers(anyString); times = 0; }};
        var resolver = new ExportLogMetadata(pid -> { reads.incrementAndGet(); return book; }, search);
        assertSame(book, resolver.resolve("book")); assertSame(book, resolver.resolve("book"));
        assertEquals(1, reads.get());
    }

    @Test
    void missingPeriodicalRetainsIssueNumberAndDate() throws Exception {
        var issue = metadata("model:ndkperiodicalissue", "2, 23.10.2026");
        new Expectations() {{ search.findReferrers("issue"); result = List.of(); }};
        assertEquals(issue, new ExportLogMetadata(pid -> issue, search).resolve("issue"));
    }

    @Test
    void reportsFailedMetadataPreparationBeforeTheWriterIsCalled() throws Exception {
        var issue = metadata("model:ndkperiodicalissue", "2, 23.10.2026");
        IOException failure = new IOException("Search unavailable");
        new Expectations() {{ search.findReferrers("issue"); result = failure; }};
        var resolver = new ExportLogMetadata(pid -> issue, search);
        DigitalObjectException result = assertThrows(DigitalObjectException.class, () -> resolver.resolve("issue"));
        assertEquals("issue", result.getPid()); assertSame(failure, result.getCause());
    }
}
