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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class TaskFilter {

    private static final Pattern SORTBY_MODIFIED_PATTERN = Pattern.compile(
            "([-+]?)(" + WorkflowModelConsts.TASK_FILTER_MODIFIED + ")");

    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_CREATED)
    private List<String> created;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_MODIFIED)
    private List<String> modified;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_ID)
    private BigDecimal id;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_JOBID)
    private BigDecimal jobId;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_JOBLABEL)
    private String jobLabel;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_OWNERID)
    private List<BigDecimal> userId;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_PRIORITY)
    private List<Integer> priority;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_PROFILENAME)
    private List<String> profileName;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_STATE)
    private List<State> state;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_SORTBY)
    private String sortBy;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_OFFSET)
    private int offset = 0;
    private int maxCount = 100;
    private Locale locale;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_BARCODE)
    private String barcode;

    private List<BigDecimal> ids;

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

    public String getJobLabel() {
        return jobLabel;
    }

    public void setJobLabel(String jobLabel) {
        this.jobLabel = jobLabel;
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

    public List<BigDecimal> getUserId() {
        return userId != null ? userId : Collections.<BigDecimal>emptyList();
    }

    public void setUserId(List<BigDecimal> userId) {
        this.userId = userId;
    }

    public List<Integer> getPriority() {
        return priority != null ? priority : Collections.<Integer>emptyList();
    }

    public void setPriority(List<Integer> priority) {
        this.priority = priority;
    }

    public List<String> getProfileName() {
        return profileName != null ? profileName : Collections.<String>emptyList();
    }

    public void setProfileName(List<String> profileName) {
        this.profileName = profileName;
    }

    public List<String> getStateAsString() {
        if (state != null) {
            ArrayList<String> stateNames = new ArrayList<String>(state.size());
            for (State s : state) {
                stateNames.add(s.name());
            }
            return stateNames;
        } else {
            return Collections.emptyList();
        }
    }

    public List<State> getState() {
        return state != null ? state : Collections.<State>emptyList();
    }

    public void setState(List<State> state) {
        this.state = state;
    }

    public String getSortBy() {
        return sortBy;
    }

    public void setSortBy(String sortBy) {
        if (sortBy != null) {
            Matcher matcher = SORTBY_MODIFIED_PATTERN.matcher(sortBy);
            if (matcher.matches()) {
                sortBy = matcher.group(1) + WorkflowModelConsts.TASK_TIMESTAMP;
            }
        }
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

    public String getBarcode() {
        return barcode;
    }

    public void setBarcode(String barcode) {
        this.barcode = barcode;
    }

    public List<BigDecimal> getIds() {
        return ids;
    }

    public void setIds(List<BigDecimal> ids) {
        this.ids = ids;
    }
}
