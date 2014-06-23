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
        private String pid;
        @XmlAttribute
        private Boolean warning;
        @XmlValue
        private String log;

        public ResultError(String pid, String log) {
            this(pid, log, null);
        }

        public ResultError(String pid, Throwable ex) {
            this(pid, null, ex);
        }

        public ResultError(String pid, String log, Throwable ex) {
            this.pid = pid;
            this.log = log;
            if (ex != null) {
                this.log = ExportUtils.toString(ex);
            }
        }

        public ResultError() {
        }

        public String getPid() {
            return pid;
        }

        public void setPid(String pid) {
            this.pid = pid;
        }

        public String getLog() {
            return log;
        }

        public void setLog(String log) {
            this.log = log;
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
