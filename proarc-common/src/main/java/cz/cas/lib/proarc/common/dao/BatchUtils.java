package cz.cas.lib.proarc.common.dao;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.process.export.ExportResultLog;
import cz.cas.lib.proarc.common.process.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.process.BatchManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.StringWriter;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

public class BatchUtils {

    private static final Logger LOG = Logger.getLogger(BatchUtils.class.getName());

    public static Batch addNewBatch(BatchManager batchManager, List<String> pids, UserProfile user, String processProfile, Batch.State state, Batch.State overWriteState, BatchParams params) {
        Batch batch = findBatchWithParams(batchManager, getPid(pids), processProfile, overWriteState);
        if (batch == null) {
            return batchManager.add(getPid(pids), user, processProfile, state, params);
        } else {
            batch.setState(state);
            batch.setLog(null);
            batch.setUserId(user.getId());
            batch.setProfileId(processProfile);
            batch.setParamsFromObject(params);
            //batch.setTimestamp(new Timestamp(System.currentTimeMillis()));
            return batchManager.update(batch);
        }
    }

    private static Batch findBatchWithParams(BatchManager batchManager, String pid, String processProfile, Batch.State overWriteState) {
        List<Batch> batches = batchManager.findBatch(pid, processProfile, overWriteState);
        if (!batches.isEmpty()) {
            return batches.get(0);
        } else {
            return null;
        }
    }

    public static Batch finishedSuccessfully(BatchManager batchManager, Batch batch, String path, String message, Batch.State state) {
        batch.setState(state);
        batch.setFolder(path);
        batch.setLog(message != null && !message.isEmpty() ? message : null);
        return batchManager.update(batch);
    }

    private static Batch finishedWithWarning(BatchManager batchManager, Batch batch, String path, String warnings, Batch.State state) {
        batch.setState(state);
        batch.setFolder(path);
        batch.setLog(warnings);
        return batchManager.update(batch);
    }

    public static Batch finishedWithError(BatchManager batchManager, Batch batch, String path, String exception, Batch.State state) {
        batch.setState(state);
        batch.setFolder(path);
        batch.setLog(exception);
        return batchManager.update(batch);
    }

    public static Batch addNewExportBatch(BatchManager batchManager, List<String> pids, UserProfile user, String exportProfile, BatchParams params) {
        return addNewBatch(batchManager, pids, user, exportProfile, Batch.State.EXPORT_PLANNED, Batch.State.EXPORT_FAILED, params);
    }

    public static Batch startWaitingExportBatch(BatchManager batchManager, Batch batch) {
        batch.setState(Batch.State.EXPORTING);
        return batchManager.update(batch);
    }

    public static Batch finishedExportWithError(BatchManager batchManager, Batch batch, String path, Throwable throwable) {
        return finishedWithError(batchManager, batch, path, BatchManager.toString(throwable), Batch.State.EXPORT_FAILED);
    }

    public static Batch finishedExportWithError(BatchManager batchManager, Batch batch, List<MetsExportException.MetsExportExceptionElement> exceptions) {
        StringBuilder builder = new StringBuilder();
        for (MetsExportException.MetsExportExceptionElement exception : exceptions) {

            builder.append(exception.getMessage());
            if (exception.getEx() != null) {
                builder.append(" - ");
                builder.append(BatchManager.toString(exception.getEx()));
                builder.append("\n");
            }
        }
        return BatchUtils.finishedExportWithError(batchManager, batch, batch.getFolder(), builder.toString());
    }

    public static Batch finishedExportWithError(BatchManager batchManager, Batch batch, String path, List<ExportResultLog.ExportResult> exportResults) {
        if (exportResults.isEmpty() || exportResults.get(0) == null ||
                exportResults.get(0).getError() == null || exportResults.get(0).getError().isEmpty() ||
                exportResults.get(0).getError().get(0) == null || exportResults.get(0).getError().get(0).getDetails() == null) {
            return finishedWithError(batchManager, batch, path, "NullPointer in getting reason.", Batch.State.EXPORT_FAILED);
        }
        return finishedWithError(batchManager, batch, path, exportResults.get(0).getError().get(0).getDetails(), Batch.State.EXPORT_FAILED);
    }

    public static Batch finishedExportWithError(BatchManager batchManager, Batch batch, String path, ExportResultLog.ResultError resultError) {
        if (resultError == null || resultError.getDetails() == null) {
            return finishedWithError(batchManager, batch, path, "NullPointer in getting reason.", Batch.State.EXPORT_FAILED);
        }
        return finishedWithError(batchManager, batch, path, resultError.getDetails(), Batch.State.EXPORT_FAILED);
    }

    public static Batch finishedExportWithError(BatchManager batchManager, Batch batch, String path, String message) {
        return finishedWithError(batchManager, batch, path, message, Batch.State.EXPORT_FAILED);
    }

    public static Batch finishedExportWithWarning(BatchManager batchManager, Batch batch, String path, List<MetsExportException.MetsExportExceptionElement> exceptions) {
        return finishedExportWithWarning(batchManager, batch, path, exceptions, Batch.State.EXPORT_FAILED);
    }

    public static Batch finishedExportWithWarning(BatchManager batchManager, Batch batch, String path, List<MetsExportException.MetsExportExceptionElement> exceptions, Batch.State state) {
        if (!exceptions.isEmpty() && exceptions.get(0) != null) {
            MetsExportException.MetsExportExceptionElement exceptionElement = exceptions.get(0);
            if (exceptionElement.isWarning()) {
                return finishedWithWarning(batchManager, batch, path, exceptionElement.getMessage(), state);
            } else {
                StringWriter writer = new StringWriter();
                if (exceptionElement.getMessage() != null) {
                    writer.append(exceptionElement.getMessage());
                } else if (exceptionElement.getEx() != null) {
                    if (!writer.toString().isEmpty()) {
                        writer.append("\n");
                    }
                    writer.append(BatchManager.toString(exceptionElement.getEx()));
                }
                return finishedWithWarning(batchManager, batch, path, writer.toString().isEmpty() ? null : writer.toString(), state);
            }
        }
        return finishedWithWarning(batchManager, batch, path, null, state);
    }

    public static Batch finishedExportWithWarning(BatchManager batchManager, Batch batch, String path, String message) {
        return finishedWithWarning(batchManager, batch, path, message, Batch.State.EXPORT_FAILED);
    }

    public static Batch finishedExportSuccessfully(BatchManager batchManager, Batch batch, String path) {
        return finishedSuccessfully(batchManager, batch, path, null, Batch.State.EXPORT_DONE);
    }

    public static Batch finishedExportSuccessfully(BatchManager batchManager, Batch batch, String path, String message) {
        return finishedSuccessfully(batchManager, batch, path, message, Batch.State.EXPORT_DONE);
    }

    public static Batch addNewUploadBatch(BatchManager batchManager, String pid, UserProfile user, String exportProfile, BatchParams params) {
        return addNewBatch(batchManager, Collections.singletonList(pid), user, exportProfile, Batch.State.UPLOADING, Batch.State.UPLOAD_FAILED, params);
    }

    public static Batch finishedUploadWithError(BatchManager batchManager, Batch batch, String path, Exception exception) {
        return finishedWithError(batchManager, batch, path, BatchManager.toString(exception), Batch.State.UPLOAD_FAILED);
    }

    public static Batch finishedUploadSuccessfully(BatchManager batchManager, Batch batch, String path) {
        return finishedSuccessfully(batchManager, batch, path, null, Batch.State.UPLOAD_DONE);
    }

    public static Batch addNewInternalBatch(BatchManager batchManager, String pid, UserProfile user, String exportProfile, BatchParams params) {
        return addNewBatch(batchManager, Collections.singletonList(pid), user, exportProfile, Batch.State.INTERNAL_PLANNED, Batch.State.INTERNAL_FAILED, params);
    }

    public static Batch finishedInternalWithError(BatchManager batchManager, Batch batch, String path, Exception exception) {
        return finishedWithError(batchManager, batch, path, BatchManager.toString(exception), Batch.State.INTERNAL_FAILED);
    }

    public static Batch finishedInternalSuccessfully(BatchManager batchManager, Batch batch, String path) {
        return finishedSuccessfully(batchManager, batch, path, null, Batch.State.INTERNAL_DONE);
    }

    public static Batch startWaitingInternalBatch(BatchManager batchManager, Batch batch) {
        batch.setState(Batch.State.INTERNAL_RUNNING);
        return batchManager.update(batch);
    }

    public static String getPid(List<String> pids) {
        if (!pids.isEmpty()) {
            return pids.get(0);
        }
        return "missing pid";
    }

    public static void finishedExportingBatch(BatchManager ibm, AppConfiguration config) {
        List<Batch> batches2finished = ibm.findExportingBatches();
        for (Batch batch : batches2finished) {
            finishedExportWithError(ibm, batch, batch.getFolder(), new Exception("Application has been stopped."));
        }
    }

    public static void finishedUploadingBatch(BatchManager ibm, AppConfiguration config) {
        List<Batch> batches2finished = ibm.findUploadingBatches();
        for (Batch batch : batches2finished) {
            finishedUploadWithError(ibm, batch, batch.getFolder(), new Exception("Application has been stopped."));
        }
    }

    public static void finishedInternalRunningBatch(BatchManager ibm, AppConfiguration config) {
        List<Batch> batches2finished = ibm.findInternalRunningBatches();
        for (Batch batch : batches2finished) {
            finishedInternalWithError(ibm, batch, batch.getFolder(), new Exception("Application has been stopped."));
        }
    }
}
