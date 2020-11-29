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

}
