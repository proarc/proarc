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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.common.externalApp.kramerius;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.process.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.storage.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.storage.ProArcObject;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class KUtils {

    public static final String KRAMERIUS_PROCESS_FINISHED = "FINISHED";
    public static final String KRAMERIUS_PROCESS_WARNING = "WARNING";
    public static final String KRAMERIUS_PROCESS_FAILED = "FAILED";
    public static final String KRAMERIUS_PROCESS_ERROR = "ERROR";
    public static final String KRAMERIUS_PROCESS_PLANNED = "PLANNED";
    public static final String KRAMERIUS_PROCESS_RUNNING = "RUNNING";
    public static final String KRAMERIUS_PROCESS_NOT_RUNNING = "NOT_RUNNING";

    public static final String KRAMERIUS_BATCH_NO_BATCH_V5 = "NO_BATCH";
    public static final String KRAMERIUS_BATCH_STARTED_V5 = "BATCH_STARTED";
    public static final String KRAMERIUS_BATCH_FINISHED_V5 = "BATCH_FINISHED";
    public static final String KRAMERIUS_BATCH_FAILED_V5 = "BATCH_FAILED";

    public static final String KRAMERIUS_BATCH_RUNNING_V7 = "RUNNING";
    public static final String KRAMERIUS_BATCH_PLANNED_V7 = "PLANNED";
    public static final String KRAMERIUS_BATCH_FINISHED_V7 = "FINISHED";
    public static final String KRAMERIUS_BATCH_FAILED_V7 = "FAILED";
    public static final String KRAMERIUS_BATCH_KILLED_V7 = "KILLED";

    public static final String EXPORT_KRAMERIUS = "kramerius";
    public static final String EXPORT_NDK = "ndk";

    private KUtils() {
    }

    public static DigitalObjectHandler findHandler(String pid, String krameriusInstanceId)
            throws DigitalObjectNotFoundException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        ProArcObject proArcObject = dom.find2(pid, null, krameriusInstanceId);
        return dom.createHandler(proArcObject);
    }

    public static String getPidAsFile(String value) {
        if (value.startsWith("uuid:")) {
            return value.substring(5);
        }
        return value;
    }

    public static String transformKrameriusModel(AppConfiguration config, String model) {
        String newModel = config.getKramerius4Export().getReverseModelMap().get(model);
        return newModel == null || newModel.isEmpty() ? model : newModel;
    }

    public static String getExpectedSourcePath(AppConfiguration appConfig, String instanceId, String pid) {
        return appConfig.getConfigHome().toPath()
                .resolve("krameriusEdit")
                .resolve(instanceId)
                .resolve(getPidAsFile(pid) + ".xml")
                .toString();
    }

    public static File getFile(AppConfiguration appConfig, String instanceId, String pid) throws IOException {
        Path configHome = appConfig.getConfigHome().toPath();
        if (!Files.isDirectory(configHome)) {
            throw new IOException("Config home does not exist: " + configHome);
        }
        Path instanceFolder = configHome.resolve("krameriusEdit").resolve(instanceId);
        Files.createDirectories(instanceFolder);
        return instanceFolder.resolve(getPidAsFile(pid) + ".xml").toFile();
    }

    public static String getExpectedDestinationPath(KrameriusOptions.KrameriusInstance instance, String pid) {
        String filePid = getPidAsFile(pid);
        return Path.of(instance.getExportFoxmlFolder())
                .resolve("k7_edit_" + filePid)
                .resolve(filePid + ".xml")
                .toString();
    }

    public static File getExportFile(KrameriusOptions.KrameriusInstance instance, String pid) throws IOException {
        String filePid = getPidAsFile(pid);
        Path exportRoot = Path.of(instance.getExportFoxmlFolder());
        Files.createDirectories(exportRoot);
        Path exportFolder = exportRoot.resolve("k7_edit_" + filePid);
        if (Files.exists(exportFolder)) {
            MetsUtils.deleteFolder(exportFolder.toFile());
        }
        Files.createDirectories(exportFolder);
        return exportFolder.resolve(filePid + ".xml").toFile();
    }


    public static class RedirectedResult {

        private final String pid;
        private String status;
        private String message;
        private String url;

        public RedirectedResult(String pid) {
            this.pid = pid;
        }

        public String getPid() {
            return pid;
        }

        public String getStatus() {
            return status;
        }

        public void setStatus(String status) {
            this.status = status;
        }

        public String getMessage() {
            return message;
        }

        public void setMessage(String message) {
            this.message = message;
        }

        public String getUrl() {
            return url;
        }

        public void setUrl(String url) {
            this.url = url;
        }
    }


    public static class ImportResult {

        private final String pid;
        private final String instance;
        private String status;
        private String reason;

        public ImportResult(String pid, String instance) {
            this(pid, instance, null, null);
        }

        public ImportResult(String pid, String instance, String status, String reason) {
            this.pid = pid;
            this.instance = instance;
            this.status = status;
            this.reason = reason;
        }

        public String getPid() {
            return pid;
        }

        public String getInstance() {
            return instance;
        }

        public String getStatus() {
            return status;
        }

        public String getReason() {
            return reason;
        }

        public void setStatus(String status) {
            this.status = status;
        }

        public void setReason(String reason) {
            this.reason = reason;
        }
    }

    public static class ImportState {

        private final String processState;
        private final String batchState;

        public ImportState(String processState, String batchState) {
            this.processState = processState;
            this.batchState = batchState;
        }

        public String getProcessState() {
            return processState;
        }

        public String getBatchState() {
            return batchState;
        }
    }
}
