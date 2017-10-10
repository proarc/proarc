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
import java.util.Locale;
import javax.xml.bind.annotation.XmlElement;

/**
 * The filter to view materials.
 *
 * @author Jan Pokorsky
 */
public class MaterialFilter {

    @XmlElement(name = WorkflowModelConsts.MATERIALFILTER_ID)
    private BigDecimal id;
    @XmlElement(name = WorkflowModelConsts.MATERIALFILTER_JOBID)
    private BigDecimal jobId;
    @XmlElement(name = WorkflowModelConsts.MATERIALFILTER_TASKID)
    private BigDecimal taskId;
    @XmlElement(name = WorkflowModelConsts.MATERIALFILTER_TYPE)
    private MaterialType type;
    @XmlElement(name = WorkflowModelConsts.MATERIALFILTER_SORTBY)
    private String sortBy;
    @XmlElement(name = WorkflowModelConsts.MATERIALFILTER_OFFSET)
    private int offset = 0;
    private int maxCount = 100;
    private Locale locale;

    public BigDecimal getId() {
        return id;
    }

    public void setId(BigDecimal id) {
        this.id = id;
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

    public MaterialType getType() {
        return type;
    }

    public void setType(MaterialType type) {
        this.type = type;
    }

    public String getSortBy() {
        return sortBy;
    }

    public void setSortBy(String sortBy) {
        this.sortBy = sortBy;
    }

    public int getOffset() {
        return offset;
    }

    public void setOffset(int offset) {
        this.offset = offset;
    }

    public int getMaxCount() {
        return maxCount;
    }

    public void setMaxCount(int maxCount) {
        this.maxCount = maxCount;
    }

    public Locale getLocale() {
        return locale;
    }

    public void setLocale(Locale locale) {
        this.locale = locale;
    }

}
