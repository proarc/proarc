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
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import cz.incad.kramerius.resourceindex.ProcessingIndexFeeder;
import java.io.IOException;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrServerException;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.UUID;
import java.util.function.Function;
import org.apache.solr.common.SolrInputDocument;

import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_DATE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_DEVICE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_DOI;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_EXPORT_TYPE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_MODEL;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_OPERATION;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_ORGANIZATION;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_OTHER_MODEL_COUNT;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_PAGE_COUNT;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_PID;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_SIGLA;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_SOFTWARE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_SOURCE;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_STREAM;
import static cz.cas.lib.proarc.common.storage.akubra.SolrUtils.FIELD_USER;

public class SolrLogFeeder extends ProcessingIndexFeeder {
    public static final String UNKNOWN_USER = "UNKNOWN";
    private static final DateTimeFormatter LOG_DATE = new DateTimeFormatterBuilder().appendInstant(3).toFormatter();
    private Function<String, ProArcObject> objectResolver;

    public SolrLogFeeder(SolrClient solrClient) {
        super(solrClient);
    }

    public void setObjectResolver(Function<String, ProArcObject> objectResolver) {
        this.objectResolver = objectResolver;
    }

    public SolrLogMetadata metadataFor(String pid) throws DigitalObjectException {
        return objectResolver == null ? SolrLogMetadata.EMPTY : SolrLogMetadata.from(objectResolver.apply(pid));
    }

    public void feedLogChange(String user, String pid, String stream, String operation) throws DigitalObjectException {
        feedLogChange(user, pid, stream, operation, metadataFor(pid), null, null, null);
    }

    private void feedLogChange(String user, String pid, String stream, String operation, SolrLogMetadata metadata,
            Long pageCount, Long otherModelCount, String exportType) throws DigitalObjectException {
        try {
            UserManager users = UserUtil.getDefaultManger();
            UserProfile profile = users == null || user == null || user.isBlank() || UNKNOWN_USER.equals(user)
                    ? null : users.find(user);
            String organization;
            // Some storage callers supply a log message instead of the actor. Never index it as a user.
            if (profile == null || !user.equals(profile.getUserName())) {
                user = UNKNOWN_USER;
                organization = null;
            } else {
                organization = profile.getOrganization();
            }
            SolrInputDocument sdoc = new SolrInputDocument();
            sdoc.addField(FIELD_SOURCE, UUID.randomUUID().toString());
            sdoc.addField(FIELD_USER, user);
            sdoc.addField(FIELD_DATE, LOG_DATE.format(Instant.now()));
            sdoc.addField(FIELD_PID, pid);
            if (stream != null && !stream.isEmpty()) sdoc.addField(FIELD_STREAM, stream);
            sdoc.addField(FIELD_OPERATION, operation);
            sdoc.addField(FIELD_MODEL, metadata.model());
            sdoc.addField(FIELD_ORGANIZATION, organization);
            if (!metadata.devices().isEmpty()) sdoc.addField(FIELD_DEVICE, metadata.devices());
            if (!metadata.software().isEmpty()) sdoc.addField(FIELD_SOFTWARE, metadata.software());
            if (!metadata.siglas().isEmpty()) sdoc.addField(FIELD_SIGLA, metadata.siglas());
            if (!metadata.dois().isEmpty()) sdoc.addField(FIELD_DOI, metadata.dois());
            if (pageCount != null) sdoc.addField(FIELD_PAGE_COUNT, pageCount);
            if (otherModelCount != null) sdoc.addField(FIELD_OTHER_MODEL_COUNT, otherModelCount);
            if (exportType != null) sdoc.addField(FIELD_EXPORT_TYPE, exportType);
            feedDescriptionDocument(sdoc);
            commit();
        } catch (SolrServerException | IOException ex) {
            throw new DigitalObjectException(pid, "Nepodarilo se zaindexovat objekt " + pid + " do SOLRu.");
        }
    }

    public void feedExportLog(String pid, UserProfile user, SolrLogMetadata metadata, long pageCount,
            long otherModelCount, String exportType) throws DigitalObjectException {
        feedLogChange(user == null ? null : user.getUserName(), pid, null, "EXPORT", metadata,
                pageCount, otherModelCount, exportType);
    }

    public void feedIngestLog(String pid, String user) throws DigitalObjectException {
        feedLogChange(user, pid, null, "INGEST");
    }

    public void feedUpdateLog(String user, String pid, String dsId) throws DigitalObjectException {
        feedLogChange(user, pid, dsId, "UPDATE");
    }

    public void feedDeleteLog(String pid, String user) throws DigitalObjectException {
        feedLogChange(user, pid, null, "DEACTIVATE");
    }

    public void feedArticleExportLog(String pid, UserProfile user, SolrLogMetadata metadata,
            String exportType) throws DigitalObjectException {
        feedLogChange(user == null ? null : user.getUserName(), pid, null, "EXPORT", metadata,
                null, null, exportType);
    }

    public void feedDeleteLog(String pid, String user, SolrLogMetadata metadata) throws DigitalObjectException {
        feedLogChange(user, pid, null, "DEACTIVATE", metadata, null, null, null);
    }

    public void feedRestoreLog(String pid, String user) throws DigitalObjectException {
        feedLogChange(user, pid, null, "RESTORE");
    }

    public void feedPurgeLog(String pid, String user) throws DigitalObjectException {
        feedLogChange(user, pid, null, "PURGE");
    }

    public void feedPurgeLog(String pid, String user, SolrLogMetadata metadata) throws DigitalObjectException {
        feedLogChange(user, pid, null, "PURGE", metadata, null, null, null);
    }
}
