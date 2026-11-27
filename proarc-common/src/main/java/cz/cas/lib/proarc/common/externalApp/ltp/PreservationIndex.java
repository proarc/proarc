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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import org.apache.solr.common.SolrInputDocument;

/** Indexes only the current package, including when reading legacy submission histories. */
public final class PreservationIndex {
    public static final String RECORDS = "ltpRecords";
    public static final String ID = "ltpId";
    public static final String PROVIDER_ID = "ltpProviderId";
    public static final String STATES = "ltpProviderState";
    public static final String PENDING = "ltpPending";
    private static final ObjectMapper JSON = new ObjectMapper();
    private PreservationIndex() { }

    public static ArchivInfo read(String json) throws IOException {
        if (json == null || json.isBlank()) return null;
        JsonNode value = JSON.readTree(json);
        // Older index entries used an array; only its last submission is current.
        if (value.isArray()) value = value.isEmpty() ? null : value.get(value.size() - 1);
        return value == null || value.isNull() ? null : JSON.treeToValue(value, ArchivInfo.class);
    }

    public static void addFields(SolrInputDocument doc, ArchivInfo record, boolean insert) throws IOException {
        String id = record == null ? null : record.getPackageId();
        add(doc, RECORDS, record == null ? null : JSON.writeValueAsString(record), insert);
        add(doc, ID, id, insert);
        add(doc, PROVIDER_ID, id == null ? null : record.getProvider() + ":" + id, insert);
        add(doc, STATES, record == null ? null : record.getProvider() + ":" + record.getState(), insert);
        add(doc, PENDING, record != null && pending(record), insert);
    }

    public static boolean pending(ArchivInfo record) {
        return record.getState() != ArchivInfo.State.FAILED && record.getState() != ArchivInfo.State.STORED;
    }

    private static void add(SolrInputDocument doc, String field, Object value, boolean insert) {
        doc.addField(field, insert ? value : java.util.Collections.singletonMap("set", value));
    }
}
