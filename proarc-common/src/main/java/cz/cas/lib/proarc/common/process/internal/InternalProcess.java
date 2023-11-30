/*
 * Copyright (C) 2023 Lukas Sykora
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.process.internal;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.fedora.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.process.export.ExportDispatcher;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Locale;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.common.dao.BatchUtils.finishedExportWithError;

/**
 * Other process
 * {@link #start() runs} the run other process than Export and Import
 *
 * @author Lukas Sykora
 */
public final class InternalProcess implements Runnable {

    private static final Logger LOG = Logger.getLogger(InternalProcess.class.getName());

    private final BatchManager batchManager;
    private final AppConfiguration config;
    private final AkubraConfiguration akubraConfiguration;
    private final UserProfile user;
    private final InternalOptions options;

    InternalProcess(InternalOptions options, BatchManager batchManager, AppConfiguration config, AkubraConfiguration akubraConfiguration, UserProfile user) {
        this.batchManager = batchManager;
        this.config = config;
        this.akubraConfiguration = akubraConfiguration;
        this.user = user;
        this.options = options;
    }

    /**
     * Prepares a new other process.
     * to run with {@link #start} immediately or later with {@link ExportDispatcher}.
     */
    public static InternalProcess prepare(AppConfiguration config, AkubraConfiguration akubraConfig, Batch batch, BatchManager batchManager, UserProfile user, String log, Locale locale) throws IOException {
        InternalOptions options = new InternalOptions(config, batch, log, locale);
        InternalProcess process = new InternalProcess(options, batchManager, config, akubraConfig, user);
        return process;
    }

    @Override
    public void run() {
        start();
    }

    /**
     * Starts the process.
     *
     * @return the import batch
     */
    public Batch start() {
        Batch batch = options.getBatch();
        if (batch == null) {
            throw new IllegalStateException("Batch is null!");
        }
        String profileId = batch.getProfileId();
        BatchParams params = batch.getParamsAsObject();
        if (params == null) {
            return finishedExportWithError(batchManager, batch, batch.getFolder(), new Exception("Batch params are null."));
        }
        try {
            batch = BatchUtils.startWaitingExportBatch(batchManager, batch);
            switch (profileId) {
                case Batch.INTERNAL_PERO:
                    return peroProcess(batch, params);
                default:
                    return finishedExportWithError(batchManager, batch, batch.getFolder(), new Exception("Unknown export profile."));
            }
        } catch (InterruptedException ex) {
            // rollback files on batch resume
            return null;
        } catch (Throwable t) {
            return BatchUtils.finishedExportWithError(batchManager, batch, batch.getFolder(), t);
        }
    }

    private Batch peroProcess(Batch batch, BatchParams params) throws InterruptedException {
        // TODO
        throw new InterruptedException("test");
//        return batch;
    }

    public static void resumeAll(BatchManager ibm, InternalDispatcher dispatcher, AppConfiguration config, AkubraConfiguration akubraConfiguration) {
        List<Batch> batches2schedule = ibm.findWaitingInternalBatches();
        for (Batch batch : batches2schedule) {
            try {
                InternalProcess resume = InternalProcess.resume(batch, ibm, config, akubraConfiguration);
                dispatcher.addInternalProcess(resume);
            } catch (Exception ex) {
                BatchUtils.finishedExportWithError(ibm, batch, batch.getFolder(), ex);
            }
        }
    }

    public static InternalProcess resume(Batch batch, BatchManager ibm, AppConfiguration config, AkubraConfiguration akubraConfiguration) throws IOException {
        UserManager users = UserUtil.getDefaultManger();
        UserProfile user = users.find(batch.getUserId());
        // if necessary reset old computed batch items
        InternalProcess process = InternalProcess.prepare(config, akubraConfiguration, batch, ibm, user, "Resume export", new Locale("cs", "CZ"));
        return process;
    }

    public static final class InternalOptions {

        private File targetFolder;
        private AppConfiguration configuration;
        private Batch batch;
        private String log;
        private Locale locale;

        public InternalOptions(AppConfiguration config, Batch batch, String log, Locale locale) {
            this.configuration = config;
            this.batch = batch;
            this.log = log;
            this.locale = locale;
        }

        public File getTargetFolder() {
            return targetFolder;
        }

        public void setTargetFolder(File targetFolder) {
            this.targetFolder = targetFolder;
        }

        public AppConfiguration getConfiguration() {
            return configuration;
        }

        public void setConfiguration(AppConfiguration configuration) {
            this.configuration = configuration;
        }

        public Batch getBatch() {
            return batch;
        }

        public void setBatch(Batch batch) {
            this.batch = batch;
        }

        public String getLog() {
            return log;
        }

        public void setLog(String log) {
            this.log = log;
        }

        public Locale getLocale() {
            return locale;
        }

        public void setLocale(Locale locale) {
            this.locale = locale;
        }
    }
}
