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

/**
 *
 * @author Jan Pokorsky
 */
public class TaskParameter {

    private BigDecimal taskId;
    /**
     * The name of a parameter in workflow profile.
     */
    private String paramRef;
    private String value;

    public BigDecimal getTaskId() {
        return taskId;
    }

    public TaskParameter addTaskId(BigDecimal taskId) {
        setTaskId(taskId);
        return this;
    }

    public void setTaskId(BigDecimal taskId) {
        this.taskId = taskId;
    }

    public String getParamRef() {
        return paramRef;
    }

    public TaskParameter addParamRef(String paramRef) {
        setParamRef(paramRef);
        return this;
    }

    public void setParamRef(String paramRef) {
        this.paramRef = paramRef;
    }

    public String getValue() {
        return value;
    }

    public TaskParameter addValue(String value) {
        setValue(value);
        return this;
    }

    public void setValue(String value) {
        this.value = value;
    }

}
