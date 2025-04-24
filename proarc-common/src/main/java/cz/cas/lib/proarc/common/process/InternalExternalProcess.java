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
package cz.cas.lib.proarc.common.process;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.dao.Batch;
import cz.cas.lib.proarc.common.dao.BatchParams;
import cz.cas.lib.proarc.common.dao.BatchUtils;
import cz.cas.lib.proarc.common.process.export.ExportDispatcher;
import cz.cas.lib.proarc.common.process.external.PdfaProcess;
import cz.cas.lib.proarc.common.process.external.PeroProcess;
import cz.cas.lib.proarc.common.process.external.ThumbnailPdfProcess;
import cz.cas.lib.proarc.common.process.internal.ValidationProcess;
import cz.cas.lib.proarc.common.storage.akubra.AkubraConfiguration;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Locale;
import java.util.logging.Logger;

import static cz.cas.lib.proarc.common.dao.BatchUtils.finishedExternalSuccessfully;
import static cz.cas.lib.proarc.common.dao.BatchUtils.finishedExternalWithError;
import static cz.cas.lib.proarc.common.dao.BatchUtils.finishedInternalSuccessfully;
import static cz.cas.lib.proarc.common.dao.BatchUtils.finishedInternalWithError;

/**
 * Other process
 * {@link #start() runs} the run other process than Export and Import
 *
 * @author Lukas Sykora
 */
public final class InternalExternalProcess implements Runnable {

    private static final Logger LOG = Logger.getLogger(InternalExternalProcess.class.getName());

    private final BatchManager batchManager;
    private final AppConfiguration config;
    private final AkubraConfiguration akubraConfiguration;
    private final UserProfile user;
    private final InternalExternalOptions options;

    InternalExternalProcess(InternalExternalOptions options, BatchManager batchManager, AppConfiguration config, AkubraConfiguration akubraConfiguration, UserProfile user) {
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
    public static InternalExternalProcess prepare(AppConfiguration config, AkubraConfiguration akubraConfig, Batch batch, BatchManager batchManager, UserProfile user, String log, Locale locale) throws IOException {
        InternalExternalOptions options = new InternalExternalOptions(config, batch, log, locale);
        InternalExternalProcess process = new InternalExternalProcess(options, batchManager, config, akubraConfig, user);
        return process;
    }

    /**
     * Prepares a new other process.
     * to run with {@link #start} immediately or later with {@link ExportDispatcher}.
     */
    public static InternalExternalProcess prepare(AppConfiguration config, AkubraConfiguration akubraConfig, Batch batch, BatchManager batchManager, UserProfile user, String log, Locale locale, File folder) throws IOException {
        InternalExternalOptions options = new InternalExternalOptions(config, batch, log, locale, folder);
        InternalExternalProcess process = new InternalExternalProcess(options, batchManager, config, akubraConfig, user);
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
            switch (profileId) {
                case Batch.EXTERNAL_PERO:
                case Batch.EXTERNAL_PDFA:
                case Batch.EXTERNAL_THUMBNAIL:
                    return finishedExternalWithError(batchManager, batch, batch.getFolder(), new Exception("Batch params are null."));
                case Batch.INTERNAL_VALIDATION:
                    return finishedInternalWithError(batchManager, batch, batch.getFolder(), new Exception("Batch params are null."));
                default:
                    return finishedInternalWithError(batchManager, batch, batch.getFolder(), new Exception("Batch params are null."));
            }
        }
        try {
            switch (profileId) {
                case Batch.EXTERNAL_PERO:
                    batch = BatchUtils.startWaitingExternalBatch(batchManager, batch);
                    return peroProcess(batch, params);
                case Batch.EXTERNAL_PDFA:
                    batch = BatchUtils.startWaitingExternalBatch(batchManager, batch);
                    return pdfaProcess(batch, params);
                case Batch.EXTERNAL_THUMBNAIL:
                    batch = BatchUtils.startWaitingExternalBatch(batchManager, batch);
                    return thumbnailProcess(batch, params);
                case Batch.INTERNAL_VALIDATION:
                    batch = BatchUtils.startWaitingInternalBatch(batchManager, batch);
                    return validationProcess(batch, params);
                default:
                    return finishedInternalWithError(batchManager, batch, batch.getFolder(), new Exception("Unknown profile."));
            }
        } catch (Throwable t) {
            return BatchUtils.finishedInternalWithError(batchManager, batch, batch.getFolder(), t);
        }
    }

    private Batch peroProcess(Batch batch, BatchParams params) {
        try {
            PeroProcess peroProcess = new PeroProcess(config, akubraConfiguration);
            PeroProcess.Result result = peroProcess.generateAlto(params.getPids(), options.getFolder());
            if (result.getException() != null) {
                batch = finishedExternalWithError(this.batchManager, batch, batch.getFolder(), result.getException());
                throw result.getException();
            } else {
                batch = finishedExternalSuccessfully(this.batchManager, batch, batch.getFolder());
            }
            return batch;
        } catch (Exception ex) {
            return finishedExternalWithError(this.batchManager, batch, batch.getFolder(), ex);
        }
    }

    private Batch pdfaProcess(Batch batch, BatchParams params) {
        try {
            PdfaProcess pdfaProcess = new PdfaProcess(config, akubraConfiguration);
            PdfaProcess.Result result = pdfaProcess.generatePdfA(params.getPids(), options.getFolder());
            if (result.getException() != null) {
                batch = finishedExternalWithError(this.batchManager, batch, batch.getFolder(), result.getException());
                throw result.getException();
            } else {
                batch = finishedExternalSuccessfully(this.batchManager, batch, batch.getFolder());
            }
            return batch;
        } catch (Exception ex) {
            return finishedExternalWithError(this.batchManager, batch, batch.getFolder(), ex);
        }
    }

    private Batch thumbnailProcess(Batch batch, BatchParams params) {
        try {
            ThumbnailPdfProcess thumbnailProcess = new ThumbnailPdfProcess(config, akubraConfiguration);
            ThumbnailPdfProcess.Result result = thumbnailProcess.generateThumbnail(params.getPids(), options.getFolder());
            if (result.getException() != null) {
                batch = finishedExternalWithError(this.batchManager, batch, batch.getFolder(), result.getException());
                throw result.getException();
            } else {
                batch = finishedExternalSuccessfully(this.batchManager, batch, batch.getFolder());
            }
            return batch;
        } catch (Exception ex) {
            return finishedExternalWithError(this.batchManager, batch, batch.getFolder(), ex);
        }
    }

    private Batch validationProcess(Batch batch, BatchParams params) {
        try {
            ValidationProcess validationProcess = new ValidationProcess(config, akubraConfiguration, params.getPids(), options.getLocale());
            ValidationProcess.Result result = validationProcess.validate(ValidationProcess.Type.VALIDATION);
            if (result.isStatusOk(false)) {
                batch = finishedInternalSuccessfully(this.batchManager, batch, batch.getFolder());
            } else {
                batch = finishedInternalWithError(this.batchManager, batch, batch.getFolder(), result.getMessages());
            }
            validationProcess.indexResult(batch);
            return batch;
        } catch (Exception ex) {
            return finishedInternalWithError(this.batchManager, batch, batch.getFolder(), ex);
        }
    }

    public static void resumeAll(BatchManager ibm, InternalExternalDispatcher dispatcher, AppConfiguration config, AkubraConfiguration akubraConfiguration) {
        List<Batch> batches2schedule = ibm.findWaitingInternalBatches();
        for (Batch batch : batches2schedule) {
            try {
                InternalExternalProcess resume = InternalExternalProcess.resume(batch, ibm, config, akubraConfiguration);
                dispatcher.addInternalExternalProcess(resume);
            } catch (Exception ex) {
                BatchUtils.finishedExportWithError(ibm, batch, batch.getFolder(), ex);
            }
        }
    }

    public static InternalExternalProcess resume(Batch batch, BatchManager ibm, AppConfiguration config, AkubraConfiguration akubraConfiguration) throws IOException {
        UserManager users = UserUtil.getDefaultManger();
        UserProfile user = users.find(batch.getUserId());
        // if necessary reset old computed batch items
        InternalExternalProcess process = InternalExternalProcess.prepare(config, akubraConfiguration, batch, ibm, user, "Resume internal process", new Locale("cs", "CZ"));
        return process;
    }

    public static final class InternalExternalOptions {

        private AppConfiguration configuration;
        private Batch batch;
        private String log;
        private Locale locale;
        private File folder;

        public InternalExternalOptions(AppConfiguration config, Batch batch, String log, Locale locale) {
            this.configuration = config;
            this.batch = batch;
            this.log = log;
            this.locale = locale;
        }

        public InternalExternalOptions(AppConfiguration config, Batch batch, String log, Locale locale, File folder) {
            this.configuration = config;
            this.batch = batch;
            this.log = log;
            this.locale = locale;
            this.folder = folder;
        }

        public File getFolder() {
            return folder;
        }

        public void setFolder(File folder) {
            this.folder = folder;
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
