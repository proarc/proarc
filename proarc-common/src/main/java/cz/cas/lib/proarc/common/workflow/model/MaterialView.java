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

import java.math.BigDecimal;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

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
    private String way;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_PATH)
    private String path;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_PID)
    private String pid;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_BARCODE)
    private String barcode;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_FIELD001)
    private String field001;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_RDCZID)
    private String rdczId;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_CATALOG)
    private String source;
    @XmlElement(name = WorkflowModelConsts.MATERIAL_METADATA)
    private String metadata;

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

    public String getWay() {
        return way;
    }

    public void setWay(String way) {
        this.way = way;
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

    public String getRdczId() {
        return rdczId;
    }

    public void setRdczId(String rdczId) {
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
    
}
