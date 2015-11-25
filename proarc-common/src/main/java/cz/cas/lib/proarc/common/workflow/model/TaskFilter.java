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

import cz.cas.lib.proarc.common.workflow.model.Task.State;
import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class TaskFilter {

    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_CREATED)
    private List<String> created;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_MODIFIED)
    private List<String> modified;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_ID)
    private BigDecimal id;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_JOBID)
    private BigDecimal jobId;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_OWNERID)
    private BigDecimal userId;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_PRIORITY)
    private Integer priority;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_PROFILENAME)
    private List<String> profileName;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_STATE)
    private State state;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_SORTBY)
    private String sortBy;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_OFFSET)
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

    public List<String> getCreated() {
        return created == null ? Collections.<String>emptyList() : created;
    }

    public void setCreated(List<String> created) {
        this.created = created;
    }

    public List<String> getModified() {
        return modified == null ? Collections.<String>emptyList() : modified;
    }

    public void setModified(List<String> modified) {
        this.modified = modified;
    }

    public BigDecimal getUserId() {
        return userId;
    }

    public void setUserId(BigDecimal userId) {
        this.userId = userId;
    }

    public Integer getPriority() {
        return priority;
    }

    public void setPriority(Integer priority) {
        this.priority = priority;
    }

    public List<String> getProfileName() {
        return profileName;
    }

    public void setProfileName(List<String> profileName) {
        this.profileName = profileName;
    }

    public State getState() {
        return state;
    }

    public void setState(State state) {
        this.state = state;
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
