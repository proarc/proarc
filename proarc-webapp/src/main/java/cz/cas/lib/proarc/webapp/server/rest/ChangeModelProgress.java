package cz.cas.lib.proarc.webapp.server.rest;

import java.util.ArrayList;
import java.util.List;

/** Result of a model conversion across the input roots visited so far. */
public final class ChangeModelProgress {

    private final int totalObjects;
    private final int convertedObjects;
    private final String failedPid;
    private final List<String> unprocessedPids;
    private final List<String> unprocessedRootPids;
    private String status;
    private String validation;

    public ChangeModelProgress(int totalObjects, int convertedObjects, String failedPid,
            List<String> unprocessedPids, List<String> unprocessedRootPids) {
        this.totalObjects = totalObjects;
        this.convertedObjects = convertedObjects;
        this.failedPid = failedPid;
        this.unprocessedPids = new ArrayList<>(unprocessedPids);
        this.unprocessedRootPids = new ArrayList<>(unprocessedRootPids);
    }

    public int getTotalObjects() {
        return totalObjects;
    }

    public int getConvertedObjects() {
        return convertedObjects;
    }

    public String getFailedPid() {
        return failedPid;
    }

    public List<String> getUnprocessedPids() {
        return unprocessedPids;
    }

    public List<String> getUnprocessedRootPids() {
        return unprocessedRootPids;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getValidation() {
        return validation;
    }

    public void setValidation(String validation) {
        this.validation = validation;
    }
}
