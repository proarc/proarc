/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.urnnbn;

import cz.cas.lib.proarc.common.fedora.SearchViewItem;
import cz.cas.lib.proarc.common.object.DigitalObjectElement;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * It handles warnings, errors and registered URN:NBNs.
 *
 * @author Jan Pokorsky
 */
public class UrnNbnStatusHandler {

    public static final UrnNbnStatusHandler DEFAULT = new UrnNbnStatusHandler() {

        @Override
        public void log(LogType logType, DigitalObjectElement elm, Status status, String msg, String urnNbn) {
        }

        @Override
        public void log(LogType logType, String pid, Status status, String msg, String urnNbn) {
        }

        @Override
        public void ok(DigitalObjectElement elm, String urnNbn) {
        }

    };

    private Map<String, PidResult> pids = new LinkedHashMap<String, PidResult>();

    public void error(DigitalObjectElement elm, Throwable t) {
        error(elm, null, t);
    }

    @Deprecated
    public void error(String pid, Throwable t) {
        error(null, pid, t);
    }

    private void error(DigitalObjectElement elm, String pid, Throwable t) {
        StringWriter dump = new StringWriter();
        PrintWriter pw = new PrintWriter(dump);
        t.printStackTrace(pw);
        pw.close();
        if (elm != null) {
            error(elm, Status.EXCEPTION, dump.toString());
        } else {
            error(pid, Status.EXCEPTION, dump.toString());
        }
    }

    @Deprecated
    public void error(String pid, Status status, String msg) {
        log(LogType.ERROR, pid, status, msg, null);
    }

    public void error(DigitalObjectElement elm, Status status, String msg) {
        log(LogType.ERROR, elm, status, msg, null);
    }

    public void warning(DigitalObjectElement elm, Status status, String msg, String urnNbn) {
        log(LogType.WARNING, elm, status, msg, urnNbn);
    }

    @Deprecated
    public void warning(String pid, Status status, String msg, String urnNbn) {
        log(LogType.WARNING, pid, status, msg, urnNbn);
    }

    public void ok(DigitalObjectElement elm, String urnNbn) {
        getEntry(elm.getPid()).setPid(elm.getItem()).ok(urnNbn);
    }

    public void log(LogType logType, DigitalObjectElement elm, Status status, String msg, String urnNbn) {
        getEntry(elm.getPid())
                .setPid(elm.getItem())
                .getLogs(logType)
                    .add(new StatusEntry(status, msg, urnNbn));
    }

    public void log(LogType logType, String pid, Status status, String msg, String urnNbn) {
        PidResult entry = getEntry(pid);
        if (entry.getPid() == null) {
            entry.setPid(new SearchViewItem(pid));
        }
        entry.getLogs(logType).add(new StatusEntry(status, msg, urnNbn));
    }

    public Map<String, PidResult> getPids() {
        return pids;
    }

    private PidResult getEntry(String pid) {
        PidResult pe = pids.get(pid);
        if (pe == null) {
            pe = new PidResult();
            pids.put(pid, pe);
        }
        return pe;
    }

    public enum Status {
        EXCEPTION, MISSING_DATASTREAM, MISSING_MIX,
        MISSING_PARENT, NO_PAGE_FOUND, NOT_PROCESSED,
        UNEXPECTED_PARENT,
        URNNBN_EXISTS, XML_REQUEST_NOT_VALID,
        URNNBN_DONT_EXISTS
    }

    public enum LogType { ERROR, REGISTERED, WARNING}

    public static class PidResult {

        private Map<LogType, List<StatusEntry>> logs = new HashMap<LogType, List<StatusEntry>>();
        private String urnNbn;
        private SearchViewItem pid;

        List<StatusEntry> getLogs(LogType type) {
            List<StatusEntry> entries = logs.get(type);
            if (entries == null) {
                entries = new ArrayList<StatusEntry>();
                logs.put(type, entries);
            }
            return entries;
        }

        public PidResult ok(String urnNbn) {
            this.urnNbn = urnNbn;
            return this;
        }

        public String getUrnNbn() {
            return urnNbn;
        }

        public SearchViewItem getPid() {
            return pid;
        }

        public PidResult setPid(SearchViewItem pid) {
            this.pid = pid;
            return this;
        }

        public List<StatusEntry> getErrors() {
            List<StatusEntry> errors = logs.get(LogType.ERROR);
            return errors != null ? errors : Collections.<StatusEntry>emptyList();
        }

        public List<StatusEntry> getWarnings() {
            List<StatusEntry> warnings = logs.get(LogType.WARNING);
            return warnings != null ? warnings : Collections.<StatusEntry>emptyList();
        }

    }

    public static class StatusEntry {

        private Status status;
        private String message;
        private String log;
        private String urnNbn;
        private List<String> params;

        public StatusEntry(Status status, String message, String urnNbn) {
            this.status = status;
            this.message = message;
            this.urnNbn = urnNbn;
        }

        public StatusEntry(Status status, String message) {
            this(status, message, null);
        }

        public Status getStatus() {
            return status;
        }

        public String getMessage() {
            return message;
        }

        public String getUrnNbn() {
            return urnNbn;
        }
    }

}
