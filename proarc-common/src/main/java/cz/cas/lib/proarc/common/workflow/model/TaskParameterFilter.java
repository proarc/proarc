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

import jakarta.xml.bind.annotation.XmlElement;
import java.math.BigDecimal;
import java.util.Locale;

/**
 *
 * @author Jan Pokorsky
 */
public class TaskParameterFilter {

    private BigDecimal taskId;
    private BigDecimal jobId;
    private String profileName;
    @XmlElement(name = WorkflowModelConsts.TASK_FILTER_OFFSET)
    private int offset = 0;
    private int maxCount = 100;
    private Locale locale;

    @XmlElement(name = WorkflowModelConsts.PARAMETERPROFILE_TASKID)
    public BigDecimal getTaskId() {
        return taskId;
    }

    public void setTaskId(BigDecimal taskId) {
        this.taskId = taskId;
    }

    public BigDecimal getJobId() {
        return jobId;
    }

    public void setJobId(BigDecimal jobId) {
        this.jobId = jobId;
    }

    public String getProfileName() {
        return profileName;
    }

    public void setProfileName(String profileName) {
        this.profileName = profileName;
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
