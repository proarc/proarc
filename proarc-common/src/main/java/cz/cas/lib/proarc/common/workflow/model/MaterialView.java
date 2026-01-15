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

import cz.cas.lib.proarc.common.workflow.profile.Way;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import java.math.BigDecimal;

/**
 * The view for all kinds of materials.
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class MaterialView extends Material {

    @XmlElement(name = WorkflowModelConsts.MATERIAL_PROFILELABEL)
    private String profileLabel;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_JOB_ID)
    private BigDecimal jobId;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_TASKID)
    private BigDecimal taskId;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_WAY)
    private Way way;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_PATH)
    private String path;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_PID)
    private String pid;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_BARCODE)
    private String barcode;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_FIELD001)
    private String field001;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_RDCZID)
    private BigDecimal rdczId;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_CATALOG)
    private String source;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_SIGNATURE)
    private String signature;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_METADATA)
    private String metadata;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_DETAIL)
    private String detail;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_ISSUE)
    private String issue;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_SIGLA)
    private String sigla;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_VOLUME)
    private String volume;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_YEAR)
    private String year;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_EDITION)
    private String edition;

    public String getEdition() {
        return edition;
    }

    public void setEdition(String edition) {
        this.edition = edition;
    }

    public String getProfileLabel() {
        return profileLabel;
    }

    public void setProfileLabel(String profileLabel) {
        this.profileLabel = profileLabel;
    }

    public BigDecimal getJobId() {
        return jobId;
    }

    public void setJobId(BigDecimal jobId) {
        this.jobId = jobId;
    }

    public BigDecimal getTaskId() {
        return taskId;
    }

    public void setTaskId(BigDecimal taskId) {
        this.taskId = taskId;
    }

    public Way getWay() {
        return way;
    }

    public void setWay(Way way) {
        this.way = way;
    }

    public String getWayAsString() {
        return way == null ? null : way.name();
    }

    public void setWayAsString(String way) {
        this.way = Way.fromString(way);
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getPid() {
        return pid;
    }

    public void setPid(String pid) {
        this.pid = pid;
    }

    public String getBarcode() {
        return barcode;
    }

    public void setBarcode(String barcode) {
        this.barcode = barcode;
    }

    public String getField001() {
        return field001;
    }

    public void setField001(String field001) {
        this.field001 = field001;
    }

    public BigDecimal getRdczId() {
        return rdczId;
    }

    public void setRdczId(BigDecimal rdczId) {
        this.rdczId = rdczId;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getMetadata() {
        return metadata;
    }

    public void setMetadata(String metadata) {
        this.metadata = metadata;
    }

    public String getSignature() {
        return signature;
    }

    public void setSignature(String signature) {
        this.signature = signature;
    }

    public String getDetail() {
        return detail;
    }

    public void setDetail(String detail) {
        this.detail = detail;
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
