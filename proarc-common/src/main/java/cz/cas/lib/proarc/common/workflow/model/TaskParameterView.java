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

import cz.cas.lib.proarc.common.workflow.profile.DisplayType;
import cz.cas.lib.proarc.common.workflow.profile.ParamDefinition;
import cz.cas.lib.proarc.common.workflow.profile.ValueMapSource;
import java.math.BigDecimal;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;

/**
 *
 * @author Jan Pokorsky
 */
@XmlAccessorType(XmlAccessType.NONE)
public class TaskParameterView extends TaskParameter {

    @XmlElement(name = WorkflowModelConsts.PARAMETER_JOBID)
    private BigDecimal jobId;
    @XmlElement(name = WorkflowModelConsts.PARAMETER_PROFILELABEL)
    private String profileLabel;
    @XmlElement(name = WorkflowModelConsts.PARAMETER_PROFILEHINT)
    private String profileHint;
    @XmlTransient
    private ParamDefinition profile;
    @XmlTransient
    private String taskProfileName;

    public BigDecimal getJobId() {
        return jobId;
    }

    public void setJobId(BigDecimal jobId) {
        this.jobId = jobId;
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

    public ParamDefinition getProfile() {
        return profile;
    }

    public void setProfile(ParamDefinition profile) {
        this.profile = profile;
    }

    public String getTaskProfileName() {
        return taskProfileName;
    }

    public void setTaskProfileName(String taskProfileName) {
        this.taskProfileName = taskProfileName;
    }

    @XmlElement(name = WorkflowModelConsts.PARAMETER_REQUIRED)
    public Boolean getRequired() {
        return profile == null ? null : profile.isRequired();
    }

    @XmlElement(name = WorkflowModelConsts.PARAMETER_DISPLAYTYPE)
    public DisplayType getDisplayType() {
        return profile == null ? null : profile.getDisplayType();
    }

    @XmlElement(name = WorkflowModelConsts.PARAMETER_OPTION_VALUE_FIELD)
    public String getOptionValueField() {
        return profile == null ? null : profile.getOptionValueField();
    }

    @XmlElement(name = WorkflowModelConsts.PARAMETER_OPTION_DISPLAY_FIELD)
    public String getOptionDisplayField() {
        return profile == null ? null : profile.getOptionDisplayField();
    }

    @XmlElement(name = WorkflowModelConsts.PARAMETER_VALUEMAPID)
    public String getValueMapId() {
        return profile == null || profile.getDatasource() == null ? null
                : profile.getDatasource().getId();
    }

    @XmlElement(name = WorkflowModelConsts.PARAMETER_VALUEMAPTYPE)
    public ValueMapSource getValueMapType() {
        return profile == null || profile.getDatasource() == null ? null
                : profile.getDatasource().getSource();
    }
}
