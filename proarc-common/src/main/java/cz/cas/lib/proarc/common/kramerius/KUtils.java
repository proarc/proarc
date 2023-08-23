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
package cz.cas.lib.proarc.common.kramerius;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.export.mets.MetsUtils;
import cz.cas.lib.proarc.common.fedora.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.fedora.FedoraObject;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectManager;
import java.io.File;
import java.io.IOException;

public class KUtils {

    public static final String KRAMERIUS_PROCESS_FINISHED = "FINISHED";
    public static final String KRAMERIUS_PROCESS_WARNING = "WARNING";
    public static final String KRAMERIUS_PROCESS_FAILED = "FAILED";
    public static final String KRAMERIUS_PROCESS_ERROR = "ERROR";
    public static final String KRAMERIUS_PROCESS_PLANNED = "PLANNED";
    public static final String KRAMERIUS_PROCESS_RUNNING = "RUNNING";

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

    public static DigitalObjectHandler findHandler(String pid, String krameriusInstanceId)
            throws DigitalObjectNotFoundException {
        DigitalObjectManager dom = DigitalObjectManager.getDefault();
        FedoraObject fedoraObject = dom.find2(pid, null, krameriusInstanceId);
        return dom.createHandler(fedoraObject);
    }

    public static String getPidAsFile(String value) {
        if (value.startsWith("uuid:")) {
            return value.substring(5);
        }
        return value;
    }

    public static String transformKrameriusModel(AppConfiguration config, String model) {
        String newModel = config.getKramerius4Export().getReverseModelMap().get(model);
        if (newModel == null || newModel.isEmpty()) {
            return model;
        } else {
            return newModel;
        }
    }

    public static String getExpectedSourcePath(AppConfiguration appConfig, String instanceId, String pid) {
        return appConfig.getConfigHome().getAbsolutePath() + File.separator + "krameriusEdit" + File.separator + instanceId + File.separator + getPidAsFile(pid) + ".xml";
    }

    public static File getFile(AppConfiguration appConfig, String instanceId, String pid) throws IOException {
        File configHome = appConfig.getConfigHome();
        if (configHome.exists()) {
            File krameriusEdit = new File(configHome, "krameriusEdit");
            if (!krameriusEdit.exists()) {
                if (!krameriusEdit.mkdir()) {
                    throw new IOException("KrameriusEdit folder can not be created (" + krameriusEdit.getAbsolutePath() + ")");
                }
            }
            File k7Instance = new File(krameriusEdit, instanceId);
            if (!k7Instance.exists()) {
                if (!k7Instance.mkdir()) {
                    throw new IOException("KrameriusEdit folder can not be created (" + k7Instance.getAbsolutePath() + ")");
                }
            }
            File pidFoxml = new File(k7Instance, getPidAsFile(pid) + ".xml");
            return pidFoxml;
        } else {
            throw new IOException("Config home does not exists!");
        }
    }
    public static String getExpectedDestinationPath(KrameriusOptions.KrameriusInstance instance, String pid) {
        if (instance.getExportFoxmlFolder().endsWith("/") || instance.getExportFoxmlFolder().endsWith("\\")) {
            return instance.getExportFoxmlFolder() + "k7_edit_" + getPidAsFile(pid) + File.separator + getPidAsFile(pid) + ".xml";
        } else {
            return instance.getExportFoxmlFolder() + File.separator + "k7_edit_" + getPidAsFile(pid) + File.separator + getPidAsFile(pid) + ".xml";
        }
    }

    public static File getExportFile(AppConfiguration appConfiguration, KrameriusOptions.KrameriusInstance instance, String pid) throws IOException {
        File exportRoot = new File(instance.getExportFoxmlFolder());
        if (!exportRoot.exists()) {
            if (!exportRoot.mkdir()) {
                throw new IOException("Kramerius export folder can not be created (" + exportRoot.getAbsolutePath() + ")");
            }
        }
        File exportFolder = new File(exportRoot, "k7_edit_" + getPidAsFile(pid));
        if (exportFolder.exists()) {
            MetsUtils.deleteFolder(exportFolder);
        }

        if (!exportFolder.exists()) {
            if (!exportFolder.mkdir()) {
                throw new IOException("Kramerius export folder can not be created (" + exportFolder.getAbsolutePath() + ")");
            } else {
                File pidFoxml = new File(exportFolder, getPidAsFile(pid) + ".xml");
                return pidFoxml;
            }
        } else {
            throw new IOException("Old export folder still exists (" + exportFolder.getAbsolutePath() + ")");
        }
    }


    public static class RedirectedResult {

        private String pid;
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

        private String pid;
        private String instance;
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

        private String processState;
        private String batchState;

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
