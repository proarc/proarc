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
package cz.cas.lib.proarc.common.export;

import cz.cas.lib.proarc.common.xml.ProarcXmlUtils;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

/**
 * Describes result of an export.
 *
 * @author Jan Pokorsky
 */
@XmlRootElement(name = "exports", namespace = ProarcXmlUtils.NS_EXPORT)
@XmlAccessorType(value = XmlAccessType.FIELD)
public class ExportResultLog {

    @XmlElement(namespace = ProarcXmlUtils.NS_EXPORT)
    @XmlSchemaType(name = "dateTime")
    private Date begin;

    @XmlElement(namespace = ProarcXmlUtils.NS_EXPORT)
    @XmlSchemaType(name = "dateTime")
    private Date end;

    @XmlElement(name = "export", namespace = ProarcXmlUtils.NS_EXPORT)
    private List<ExportResult> exports;

    public ExportResultLog() {
        begin = new Date();
    }

    public Date getBegin() {
        return begin;
    }

    public void setBegin(Date begin) {
        this.begin = begin;
    }

    public Date getEnd() {
        return end;
    }

    public void setEnd(Date end) {
        this.end = end;
    }

    public List<ExportResult> getExports() {
        if (exports == null) {
            exports = new ArrayList<ExportResult>();
        }
        return exports;
    }

    @XmlType(namespace = ProarcXmlUtils.NS_EXPORT)
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class ExportResult {

        @XmlElement(namespace = ProarcXmlUtils.NS_EXPORT)
        @XmlSchemaType(name = "dateTime")
        private Date begin;

        @XmlElement(namespace = ProarcXmlUtils.NS_EXPORT)
        @XmlSchemaType(name = "dateTime")
        private Date end;

        @XmlElement(namespace = ProarcXmlUtils.NS_EXPORT)
        private ResultStatus status;

        @XmlElement(namespace = ProarcXmlUtils.NS_EXPORT)
        private String inputPid;

        @XmlElement(namespace = ProarcXmlUtils.NS_EXPORT)
        private List<ResultError> error;

        public ExportResult() {
            begin = new Date();
        }

        public Date getBegin() {
            return begin;
        }

        public void setBegin(Date begin) {
            this.begin = begin;
        }

        public Date getEnd() {
            return end;
        }

        public void setEnd() {
            setEnd(new Date());
        }

        public void setEnd(Date end) {
            this.end = end;
        }

        public ResultStatus getStatus() {
            return status;
        }

        public void setStatus(ResultStatus status) {
            this.status = status;
        }

        public String getInputPid() {
            return inputPid;
        }

        public void setInputPid(String inputPid) {
            this.inputPid = inputPid;
        }

        public List<ResultError> getError() {
            if (error == null) {
                error = new ArrayList<ResultError>();
            }
            return error;
        }

        public void setError(List<ResultError> error) {
            this.error = error;
        }
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class ResultError {

        @XmlAttribute
        private String message;
        @XmlAttribute
        private String pid;
        @XmlAttribute
        private Boolean warning;
        @XmlValue
        private String details;

        static String toDetails(List<String> details) {
            StringBuilder sb = null;
            for (String s : details) {
                if (sb == null) {
                    sb = new StringBuilder(200);
                    sb.append(s);
                } else {
                    sb.append("\n").append(s);
                }
            }
            return sb == null ? null : sb.toString();
        }

        public ResultError(String pid, String message, List<String> details) {
            this(pid, message, toDetails(details));
        }

        public ResultError(String pid, String message, String details) {
            this(pid, message, details, null);
        }

        public ResultError(String pid, String details) {
            this(pid, null, details, null);
        }

        public ResultError(String pid, Throwable ex) {
            this(pid, null, ex);
        }

        public ResultError(String pid, String message, Throwable ex) {
            this(pid, message, null, ex);
        }

        public ResultError(String pid, String message, String details, Throwable ex) {
            this.pid = pid;
            this.message = message;
            this.details = details;
            if (ex != null) {
                this.details = details == null
                        ? ExportUtils.toString(ex)
                        : details + '\n' + ExportUtils.toString(ex);
            }
        }

        public ResultError() {
        }

        public String getMessage() {
            return message;
        }

        public void setMessage(String message) {
            this.message = message;
        }

        public String getPid() {
            return pid;
        }

        public void setPid(String pid) {
            this.pid = pid;
        }

        public String getDetails() {
            return details;
        }

        public void setDetails(String log) {
            this.details = log;
        }

        public boolean isWarning() {
            return warning == null ? false : warning;
        }

        public void setWarning(boolean warning) {
            this.warning = warning;
        }
    }

    public enum ResultStatus {
        OK, FAILED
    }

}
