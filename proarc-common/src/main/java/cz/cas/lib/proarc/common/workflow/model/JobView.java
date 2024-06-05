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
package cz.cas.lib.proarc.common.workflow.model;

import cz.cas.lib.proarc.common.device.Device;
import java.sql.Timestamp;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.NONE)
public class JobView extends Job {

    @XmlElement(name = WorkflowModelConsts.JOB_OWNERNAME)
    private String userName;
    @XmlElement(name = WorkflowModelConsts.JOB_PROFILELABEL)
    private String profileLabel;
    @XmlElement(name = WorkflowModelConsts.JOB_PROFILEHINT)
    private String profileHint;
    // properties of the physical document
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_BARCODE)
    private String barcode;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_DETAIL)
    private String detail;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_FIELD001)
    private String field001;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_ISSUE)
    private String issue;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_SIGLA)
    private String sigla;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_SIGNATURE)
    private String signature;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_VOLUME)
    private String volume;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_YEAR)
    private String year;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_DIGOBJ_PID)
    private String pid;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_EDITION)
    private String edition;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_RAW_PATH)
    private String rawPath;
    @XmlElement(name = WorkflowModelConsts.JOB_TASK_NAME)
    private String taskName;
    @XmlElement(name = WorkflowModelConsts.JOB_TASK_HINT)
    private String taskHint;
    @XmlElement(name = WorkflowModelConsts.JOB_TASK_LABEL)
    private String taskLabel;
    @XmlElement(name = WorkflowModelConsts.JOB_TASK_CHANGE_DATE)
    private Timestamp taskDate;
    @XmlElement(name = WorkflowModelConsts.JOB_TASK_CHANGE_USER)
    private String taskUser;
    @XmlElement(name = WorkflowModelConsts.JOB_TASK_CHANGE_USERNAME)
    private String taskUsername;
    @XmlElement(name = WorkflowModelConsts.JOB_DEVICE_ID)
    private String deviceId;
    @XmlElement(name = WorkflowModelConsts.JOB_DEVICE_LABEL)
    private String deviceLabel;

    public String getRawPath() {
        return rawPath;
    }

    public void setRawPath(String rawPath) {
        this.rawPath = rawPath;
    }

    public String getEdition() {
        return edition;
    }

    public void setEdition(String edition) {
        this.edition = edition;
    }

    public String getPid() {
        return pid;
    }

    public void setPid(String pid) {
        this.pid = pid;
    }

    public String getProfileLabel() {
        return profileLabel;
    }

    public void setProfileLabel(String profileLabel) {
        this.profileLabel = profileLabel;
    }

    public String getProfileHint() {
        return profileHint;
    }

    public void setProfileHint(String profileHint) {
        this.profileHint = profileHint;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    /**
     * The conversion to and from ISO timedate loses precision of the timestamp.
     * Use this instead of {@link #getTimestamp() } for updates.
     */
    @XmlElement(name = WorkflowModelConsts.JOB_TIMESTAMP)
    public long getTimestampAsLong() {
        return getTimestamp().getTime();
    }

    public String getBarcode() {
        return barcode;
    }

    public void setBarcode(String barcode) {
        this.barcode = barcode;
    }

    public String getDetail() {
        return detail;
    }

    public void setDetail(String detail) {
        this.detail = detail;
    }

    public String getField001() {
        return field001;
    }

    public void setField001(String field001) {
        this.field001 = field001;
    }

    public String getIssue() {
        return issue;
    }

    public void setIssue(String issue) {
        this.issue = issue;
    }

    public String getSigla() {
        return sigla;
    }

    public void setSigla(String sigla) {
        this.sigla = sigla;
    }

    public String getSignature() {
        return signature;
    }

    public void setSignature(String signature) {
        this.signature = signature;
    }

    public String getVolume() {
        return volume;
    }

    public void setVolume(String volume) {
        this.volume = volume;
    }

    public String getYear() {
        return year;
    }

    public void setYear(String year) {
        this.year = year;
    }

    public String getTaskName() {
        return taskName;
    }

    public void setTaskName(String taskName) {
        this.taskName = taskName;
    }

    public String getTaskHint() {
        return taskHint;
    }

    public void setTaskHint(String taskHint) {
        this.taskHint = taskHint;
    }

    public String getTaskLabel() {
        return taskLabel;
    }

    public void setTaskLabel(String taskLabel) {
        this.taskLabel = taskLabel;
    }

    public Timestamp getTaskDate() {
        return taskDate;
    }

    public void setTaskDate(Timestamp taskDate) {
        this.taskDate = taskDate;
    }

    public String getTaskUser() {
        return taskUser;
    }

    public void setTaskUser(String taskUser) {
        this.taskUser = taskUser;
    }

    public String getTaskUsername() {
        return taskUsername;
    }

    public void setTaskUsername(String taskUsername) {
        this.taskUsername = taskUsername;
    }

    public void setDevice(Device device) {
        if (device != null) {
            this.deviceId = device.getId();
            this.deviceLabel = device.getLabel();
        }
    }

    public String getDeviceId() {
        return deviceId;
    }

    public void setDeviceId(String deviceId) {
        this.deviceId = deviceId;
    }

    public String getDeviceLabel() {
        return deviceLabel;
    }

    public void setDeviceLabel(String deviceLabel) {
        this.deviceLabel = deviceLabel;
    }
}
