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
package cz.cas.lib.proarc.common.jobs;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.externalApp.ltp.PreservationIndex;
import cz.cas.lib.proarc.common.externalApp.ltp.PreservationService;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.InternalExternalDispatcher;
import cz.cas.lib.proarc.common.process.InternalExternalProcess;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfigurationFactory;
import cz.cas.lib.proarc.common.user.UserUtil;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.Consumer;
import java.util.logging.Logger;
import org.apache.commons.configuration2.Configuration;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.HttpSolrClient;
import org.apache.solr.client.solrj.util.ClientUtils;
import org.apache.solr.common.SolrDocumentList;
import org.quartz.DisallowConcurrentExecution;
import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;

import static org.quartz.CronScheduleBuilder.cronSchedule;
import static org.quartz.JobBuilder.newJob;
import static org.quartz.TriggerBuilder.newTrigger;

/**
 * Weekly reconciliation; cursor pagination remains stable as completed records leave the result set.
 */
@DisallowConcurrentExecution
public class PreservationCheckJob implements Job, ProArcJob {
    private static final Logger LOG = Logger.getLogger(PreservationCheckJob.class.getName());

    @Override
    public String getType() {
        return "PreservationCheckJob";
    }

    @Override
    public void initJob(Scheduler scheduler, String jobId, Configuration config) throws SchedulerException {
        scheduler.scheduleJob(newJob(PreservationCheckJob.class).withIdentity(jobId, "preservation").build(),
                newTrigger().withIdentity(jobId, "preservation")
                        .withSchedule(cronSchedule(config.getString("schedule", "0 0 3 ? * SUN"))
                                .withMisfireHandlingInstructionDoNothing()).build());
    }

    @Override
    public void execute(JobExecutionContext context) throws JobExecutionException {
        try {
            AppConfiguration config = JobHandler.getInstance().getAppConfig();
            AkubraConfiguration akubra = AkubraConfigurationFactory
                    .getInstance().defaultInstance(config.getConfigHome());
            scheduleChecks(config, akubra);
        } catch (Exception ex) {
            throw new JobExecutionException(ex);
        }
    }

    public static synchronized int checkAll(AppConfiguration config, AkubraConfiguration akubra) throws IOException {
        return checkAll(config, akubra, null);
    }

    public static synchronized int checkAll(AppConfiguration config, AkubraConfiguration akubra, Integer credentialOwnerId) throws IOException {
        return checkAll(config, akubra, credentialOwnerId, message -> LOG.info(message), pid -> {
        });
    }

    public static synchronized int checkAll(AppConfiguration config, AkubraConfiguration akubra, Integer credentialOwnerId,
                                            Consumer<String> log, Consumer<String> catalogCandidate) throws IOException {
        return checkAll(config, akubra, credentialOwnerId, log, catalogCandidate, List.of());
    }

    public static synchronized int checkAll(AppConfiguration config, AkubraConfiguration akubra, Integer credentialOwnerId,
                                            Consumer<String> log, Consumer<String> catalogCandidate,
                                            List<String> selectedPids) throws IOException {
        PreservationService service = PreservationService.cesnet(config, credentialOwnerId);
        int count = 0;
        try (var solr = new HttpSolrClient.Builder(akubra.getSolrSearchHost()).build()) {
            String cursor = "*";
            while (!Thread.currentThread().isInterrupted()) {
                SolrQuery query = new SolrQuery(PreservationIndex.PENDING + ":true");
                query.setRows(100);
                query.setFields("pid");
                if (selectedPids != null && !selectedPids.isEmpty()) {
                    query.addFilterQuery("pid:(" + selectedPids.stream()
                            .map(pid -> "\"" + ClientUtils.escapeQueryChars(pid) + "\"")
                            .collect(java.util.stream.Collectors.joining(" OR ")) + ")");
                }
                // pid is the uniqueKey of searchCore and is required for cursorMark.
                query.setSort("pid", SolrQuery.ORDER.asc);
                query.set("cursorMark", cursor);
                var response = solr.query(query);
                SolrDocumentList results = response.getResults();
                for (var result : results) {
                    String pid = String.valueOf(result.getFieldValue("pid"));
                    try {
                        service.check(pid, config.getLtpCesnetConfiguration().getString("catalogId"));
                        var record = service.archivInfo(pid);
                        log.accept(pid + ": " + (record == null ? "Bez záznamu LTP" : switch (record.getState()) {
                            case REGISTERING -> "Probíhá registrace";
                            case UPLOADING -> "Probíhá nahrávání";
                            case UPLOADED -> "Nahráno, čeká na validaci";
                            case STORED -> "Archivováno a zvalidováno";
                            case FAILED -> "Archivace selhala";
                            case null -> "Neznámý stav archivace";
                        })
                                + (record != null && (record.getCatalogId() == null || record.getCatalogId().isBlank())
                                ? "; aktualizace katalogu je vypnuta (katalog není nakonfigurován)" : ""));
                        if (PreservationService.needsCatalogUpdate(record)) catalogCandidate.accept(pid);
                        count++;
                    } catch (Exception ex) {
                        log.accept("CHYBA: Kontrola LTP selhala pro " + pid + ": " + ex.getMessage());
                    }
                }
                String next = response.getNextCursorMark();
                if (cursor.equals(next)) break;
                cursor = next;
            }
            return count;
        } catch (SolrServerException ex) {
            throw new IOException("Nelze vyhledat nedokončené archivace LTP", ex);
        }
    }

    /**
     * Scheduled checks use the same durable process/log workflow as manual checks.
     */
    private static void scheduleChecks(AppConfiguration config, AkubraConfiguration akubra) throws Exception {
        Map<Integer, List<String>> owners = new LinkedHashMap<>();
        try (var solr = new HttpSolrClient.Builder(akubra.getSolrSearchHost()).build()) {
            String cursor = "*";
            while (!Thread.currentThread().isInterrupted()) {
                SolrQuery query = new SolrQuery(PreservationIndex.PENDING + ":true");
                query.setRows(100);
                query.setFields("pid", PreservationIndex.RECORDS);
                query.setSort("pid", SolrQuery.ORDER.asc);
                query.set("cursorMark", cursor);
                var response = solr.query(query);
                for (var result : response.getResults()) {
                    Object json = result.getFieldValue(PreservationIndex.RECORDS);
                    var record = PreservationIndex.read(json == null ? null : json.toString());
                    if (record == null || record.getCredentialOwnerId() == null) continue;
                    owners.computeIfAbsent(record.getCredentialOwnerId(), id -> new ArrayList<>())
                            .add(String.valueOf(result.getFieldValue("pid")));
                }
                String next = response.getNextCursorMark();
                if (cursor.equals(next)) break;
                cursor = next;
            }
        }
        var manager = BatchManager.getInstance();
        for (var entry : owners.entrySet()) {
            var user = UserUtil.getDefaultManger().find(entry.getKey());
            if (user == null) continue;
            // Keep each filter below Solr's boolean-clause limit, even for large repositories.
            for (int offset = 0; offset < entry.getValue().size(); offset += 100) {
                List<String> pids = new ArrayList<>(entry.getValue().subList(offset, Math.min(offset + 100, entry.getValue().size())));
                var batch = manager.add("CESNET scheduled check", user,
                        Batch.EXTERNAL_CESNET_CHECK,
                        Batch.State.EXTERNAL_PLANNED, false,
                        Batch.PRIORITY_MEDIUM,
                        new BatchParams(pids));
                try {
                    InternalExternalDispatcher.getDefault().addInternalExternalProcess(
                            InternalExternalProcess.prepare(config, akubra, batch, manager,
                                    user, "Scheduled CESNET check", Locale.forLanguageTag("cs")));
                } catch (Exception ex) {
                    BatchUtils.finishedExternalWithError(manager, batch, batch.getFolder(), ex);
                }
            }
        }
    }
}
