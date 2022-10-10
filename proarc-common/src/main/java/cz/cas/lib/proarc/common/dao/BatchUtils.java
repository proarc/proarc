package cz.cas.lib.proarc.common.dao;

import cz.cas.lib.proarc.common.export.ExportResultLog;
import cz.cas.lib.proarc.common.export.mets.MetsExportException;
import cz.cas.lib.proarc.common.imports.ImportBatchManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import java.util.List;

public class BatchUtils {

    public static Batch addNewBatch(ImportBatchManager batchManager, List<String> pids, UserProfile user, String exportProfile, Batch.State state) {
        return batchManager.add(getPid(pids), user, exportProfile, state);
    }

    public static Batch finishedSuccessfully(ImportBatchManager batchManager, Batch batch, String path, Batch.State state) {
        batch.setState(state);
        batch.setFolder(path);
        batch.setLog(null);
        return batchManager.update(batch);
    }

    public static Batch finishedWithWarning(ImportBatchManager batchManager, Batch batch, String path, List<MetsExportException.MetsExportExceptionElement> exceptions, Batch.State state) {
        batch.setState(state);
        batch.setFolder(path);
        if (!exceptions.isEmpty() && exceptions.get(0) != null) {
            MetsExportException.MetsExportExceptionElement exceptionElement = exceptions.get(0);
            if (exceptionElement.isWarning()) {
                batch.setLog(exceptionElement.getMessage());
            } else {
                batch.setLog(ImportBatchManager.toString(exceptionElement.getEx()));
            }
        }
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

    public static Batch finishedExportWithWarning(ImportBatchManager batchManager, Batch batch, String path, List<MetsExportException.MetsExportExceptionElement> exceptions) {
        return finishedWithWarning(batchManager, batch, path, exceptions, Batch.State.EXPORT_FAILED);
    }



    public static Batch finishedExportSuccessfully(ImportBatchManager batchManager, Batch batch, String path) {
        return finishedSuccessfully(batchManager, batch, path, Batch.State.EXPORT_DONE);
    }

    public static String getPid(List<String> pids) {
        if (!pids.isEmpty()) {
            return pids.get(0);
        }
        return "missing pid";
    }
}
