package cz.cas.lib.proarc.common.dao;

import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.export.ExportResultLog;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.io.StringWriter;
import java.util.List;

public class BatchUtils {

    public static Batch addNewBatch(ImportBatchManager batchManager, List<String> pids, UserProfile user, String processProfile, Batch.State state) {
        Batch batch = findBatchWithParams(batchManager, getPid(pids), processProfile);
        if (batch == null) {
            return batchManager.add(getPid(pids), user, processProfile, state);
        } else {
            batch.setState(state);
            batch.setLog(null);
            //batch.setTimestamp(new Timestamp(System.currentTimeMillis()));
            return batchManager.update(batch);
        }
    }

    private static Batch findBatchWithParams(ImportBatchManager batchManager, String pid, String processProfile) {
        List<Batch> batches = batchManager.findBatch(pid, processProfile, Batch.State.EXPORT_FAILED);
        if (!batches.isEmpty()) {
            return batches.get(0);
        } else {
            return null;
        }
    }

    public static Batch finishedSuccessfully(ImportBatchManager batchManager, Batch batch, String path, String message, Batch.State state) {
        batch.setState(state);
        batch.setFolder(path);
        batch.setLog(message != null && !message.isEmpty() ? message : null);
        return batchManager.update(batch);
    }

    private static Batch finishedWithWarning(ImportBatchManager batchManager, Batch batch, String path, String warnings, Batch.State state) {
        batch.setState(state);
        batch.setFolder(path);
        batch.setLog(warnings);
        return batchManager.update(batch);
    }

    public static Batch finishedWithError(ImportBatchManager batchManager, Batch batch, String path, String exception, Batch.State state) {
        batch.setState(state);
        batch.setFolder(path);
        batch.setLog(exception);
        return batchManager.update(batch);
    }

    public static Batch addNewExportBatch(ImportBatchManager batchManager, List<String> pids, UserProfile user, String exportProfile) {
        return addNewBatch(batchManager, pids, user, exportProfile, Batch.State.EXPORTING);
    }

    public static Batch finishedExportWithError(ImportBatchManager batchManager, Batch batch, String path, Exception exception) {
        return finishedWithError(batchManager, batch, path, ImportBatchManager.toString(exception), Batch.State.EXPORT_FAILED);
    }

    public static Batch finishedExportWithError(ImportBatchManager batchManager, Batch batch, String path, List<ExportResultLog.ExportResult> exportResults) {
        if (exportResults.isEmpty() || exportResults.get(0) == null ||
                exportResults.get(0).getError() == null || exportResults.get(0).getError().isEmpty() ||
                exportResults.get(0).getError().get(0) == null || exportResults.get(0).getError().get(0).getDetails() == null) {
            return finishedWithError(batchManager, batch, path, "NullPointer in getting reason.", Batch.State.EXPORT_FAILED);
        }
        return finishedWithError(batchManager, batch, path, exportResults.get(0).getError().get(0).getDetails(), Batch.State.EXPORT_FAILED);
    }

    public static Batch finishedExportWithError(ImportBatchManager batchManager, Batch batch, String path, ExportResultLog.ResultError resultError) {
        if (resultError == null || resultError.getDetails() == null) {
            return finishedWithError(batchManager, batch, path, "NullPointer in getting reason.", Batch.State.EXPORT_FAILED);
        }
        return finishedWithError(batchManager, batch, path, resultError.getDetails(), Batch.State.EXPORT_FAILED);
    }

    public static Batch finishedExportWithError(ImportBatchManager batchManager, Batch batch, String path, String message) {
        return finishedWithError(batchManager, batch, path, message, Batch.State.EXPORT_FAILED);
    }

    public static Batch finishedExportWithWarning(ImportBatchManager batchManager, Batch batch, String path, List<MetsExportException.MetsExportExceptionElement> exceptions) {
        return finishedExportWithWarning(batchManager, batch, path, exceptions, Batch.State.EXPORT_FAILED);
    }

    public static Batch finishedExportWithWarning(ImportBatchManager batchManager, Batch batch, String path, List<MetsExportException.MetsExportExceptionElement> exceptions, Batch.State state) {
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
                    writer.append(ImportBatchManager.toString(exceptionElement.getEx()));
                }
                return finishedWithWarning(batchManager, batch, path, writer.toString().isEmpty() ? null : writer.toString(), state);
            }
        }
        return finishedWithWarning(batchManager, batch, path, null, state);
    }

    public static Batch finishedExportWithWarning(ImportBatchManager batchManager, Batch batch, String path, String message) {
        return finishedWithWarning(batchManager, batch, path, message, Batch.State.EXPORT_FAILED);
    }

    public static Batch finishedExportSuccessfully(ImportBatchManager batchManager, Batch batch, String path) {
        return finishedSuccessfully(batchManager, batch, path, null, Batch.State.EXPORT_DONE);
    }

    public static Batch finishedExportSuccessfully(ImportBatchManager batchManager, Batch batch, String path, String message) {
        return finishedSuccessfully(batchManager, batch, path, message, Batch.State.EXPORT_DONE);
    }

    public static String getPid(List<String> pids) {
        if (!pids.isEmpty()) {
            return pids.get(0);
        }
        return "missing pid";
    }

    public static void finishedExportingBatch(ImportBatchManager ibm, AppConfiguration config) {
        List<Batch> batches2finished = ibm.findExportingBatches();
        for (Batch batch : batches2finished) {
            finishedExportWithError(ibm, batch, batch.getFolder(), new Exception("Application has been stopped."));
        }
    }
}
