/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.common.process.export.cejsh;

import cz.cas.lib.proarc.common.process.export.ExportException;
import cz.cas.lib.proarc.common.process.export.ExportResultLog;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import java.io.File;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public class CejshStatusHandler {

    private static final Logger LOG = Logger.getLogger(CejshStatusHandler.class.getName());
    final Level logLevel;
    private ExportResultLog reslog = new ExportResultLog();
    private ExportResultLog.ExportResult currentPkg;
    private File targetFolder;

    public CejshStatusHandler() {
        this(Level.INFO);
    }

    public CejshStatusHandler(Level logLevel) {
        this.logLevel = logLevel;
    }

    /**
     * Starts to log next events for a new export hierarchy.
     * @param elm an export hierarchy root element
     */
    public void startInput(DigitalObjectElement elm) {
        LOG.log(logLevel, elm.toLog());
        currentPkg = new ExportResultLog.ExportResult();
        currentPkg.setInputPid(elm.getPid());
        reslog.getExports().add(currentPkg);
    }

    /**
     * Stops logging for the current root element.
     * @param elm an export hierarchy root
     */
    public void finishInput(DigitalObjectElement elm) {
        currentPkg.setEnd();
        if (currentPkg.getStatus() == null) {
            currentPkg.setStatus(currentPkg.getError().isEmpty() ? ExportResultLog.ResultStatus.OK : ExportResultLog.ResultStatus.FAILED);
        }
        currentPkg = null;
    }

    public void error(DigitalObjectElement elm, String msg, Throwable ex) {
        error(elm, msg, null, ex);
    }

    public void error(DigitalObjectElement elm, String msg, String details, Throwable ex) {
        StringBuilder logMsg = new StringBuilder();
        if (elm != null) {
            logMsg.append(elm.toLog()).append("\n");
        }
        if (msg != null) {
            logMsg.append("Message: ").append(msg).append("\n");
        }
        if (details != null) {
            logMsg.append("Details: ").append(details).append("\n");
        }
        String pid = elm == null ? null : elm.getPid();
        LOG.log(Level.SEVERE, logMsg.toString(), ex != null ? ex : new IllegalStateException());
        ExportResultLog.ExportResult result = getCurrentResult();
        result.setEnd();
        result.setStatus(ExportResultLog.ResultStatus.FAILED);
        result.getError().add(new ExportResultLog.ResultError(pid, msg, details, ex));
    }

    public void error(ExportException ex) {
        error(ex.getElement(), ex.getMessage(), ex.getDetails(), ex);
    }

    public void error(String pid, String msg, Throwable ex) {
        String details = pid == null ? msg : pid + "\n" + msg;
        LOG.log(Level.SEVERE, details, ex);
        ExportResultLog.ExportResult result = getCurrentResult();
        result.setEnd();
        result.setStatus(ExportResultLog.ResultStatus.FAILED);
        result.getError().add(new ExportResultLog.ResultError(pid, msg, details, ex));
    }

    private ExportResultLog.ExportResult getCurrentResult() {
        if (currentPkg == null) {
            LOG.log(Level.WARNING, "Call startInput first!");
            currentPkg = new ExportResultLog.ExportResult();
            reslog.getExports().add(currentPkg);
        }
        return currentPkg != null ? currentPkg : new ExportResultLog.ExportResult();
    }

    public void ok(DigitalObjectElement parent, List<DigitalObjectElement> articles, String path) {
        LOG.log(logLevel,"Exported {1} article(s) to {2} for {0}.", new Object[]{
            parent.toLog(), articles.size(), path});
        if (currentPkg != null) {
            currentPkg.setEnd();
            currentPkg.setStatus(ExportResultLog.ResultStatus.OK);
        }
    }

    public boolean isOk() {
        for (ExportResultLog.ExportResult er : reslog.getExports()) {
            if (er.getStatus() == ExportResultLog.ResultStatus.FAILED) {
                return false;
            }
        }
        return true;
    }

    public ExportResultLog getReslog() {
        return reslog;
    }

    public File getTargetFolder() {
        return targetFolder;
    }

    public void setTargetFolder(File targetFolder) {
        this.targetFolder = targetFolder;
    }

}
