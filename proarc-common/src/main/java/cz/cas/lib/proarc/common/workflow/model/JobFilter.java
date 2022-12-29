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

import cz.cas.lib.proarc.common.workflow.model.Job.State;
import java.math.BigDecimal;
import java.sql.Timestamp;
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
public class JobFilter {

    private static final Pattern SORTBY_MODIFIED_PATTERN = Pattern.compile(
            "([-+]?)(" + WorkflowModelConsts.JOB_FILTER_MODIFIED + ")");

    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_CREATED)
    private List<String> created;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_ID)
    private BigDecimal id;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_LABEL)
    private String label;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_BARCODE)
    private String materialBarcode;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_FINANCED)
    private String financed;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_DETAIL)
    private String materialDetail;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_FIELD001)
    private String materialField001;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_ISSUE)
    private String materialIssue;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_SIGLA)
    private String materialSigla;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_SIGNATURE)
    private String materialSignature;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_VOLUME)
    private String materialVolume;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_YEAR)
    private String materialYear;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MODIFIED)
    private List<String> modified;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_PRIORITY)
    private Integer priority;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_OWNERID)
    private BigDecimal userId;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_PARENTID)
    private BigDecimal parentId;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_PROFILENAME)
    private String profileName;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_STATE)
    private State state;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_DIGOBJ_PID)
    private String pid;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_MATERIAL_EDITION)
    private String materialEdition;
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
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_SORTBY)
    private String sortBy;
    @XmlElement(name = WorkflowModelConsts.JOB_FILTER_OFFSET)
    private int offset = 0;


    private int maxCount = 100;
    private Locale locale;

    public String getMaterialEdition() {
        return materialEdition;
    }

    public void setMaterialEdition(String materialEdition) {
        this.materialEdition = materialEdition;
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

    public BigDecimal getId() {
        return id;
    }

    public void setId(BigDecimal id) {
        this.id = id;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public BigDecimal getUserId() {
        return userId;
    }

    public void setUserId(BigDecimal userId) {
        this.userId = userId;
    }

    public String getMaterialBarcode() {
        return materialBarcode;
    }

    public void setMaterialBarcode(String materialBarcode) {
        this.materialBarcode = materialBarcode;
    }

    public String getMaterialDetail() {
        return materialDetail;
    }

    public void setMaterialDetail(String materialDetail) {
        this.materialDetail = materialDetail;
    }

    public String getMaterialField001() {
        return materialField001;
    }

    public void setMaterialField001(String materialField001) {
        this.materialField001 = materialField001;
    }

    public String getMaterialIssue() {
        return materialIssue;
    }

    public void setMaterialIssue(String materialIssue) {
        this.materialIssue = materialIssue;
    }

    public String getMaterialSigla() {
        return materialSigla;
    }

    public void setMaterialSigla(String materialSigla) {
        this.materialSigla = materialSigla;
    }

    public String getMaterialSignature() {
        return materialSignature;
    }

    public void setMaterialSignature(String materialSignature) {
        this.materialSignature = materialSignature;
    }

    public String getMaterialVolume() {
        return materialVolume;
    }

    public void setMaterialVolume(String materialVolume) {
        this.materialVolume = materialVolume;
    }

    public String getMaterialYear() {
        return materialYear;
    }

    public void setMaterialYear(String materialYear) {
        this.materialYear = materialYear;
    }

    public BigDecimal getParentId() {
        return parentId;
    }

    public void setParentId(BigDecimal parentId) {
        this.parentId = parentId;
    }

    public Integer getPriority() {
        return priority;
    }

    public void setPriority(Integer priority) {
        this.priority = priority;
    }

    public String getProfileName() {
        return profileName;
    }

    public void setProfileName(String profileName) {
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
        if (sortBy != null) {
            Matcher matcher = SORTBY_MODIFIED_PATTERN.matcher(sortBy);
            if (matcher.matches()) {
                sortBy = matcher.group(1) + WorkflowModelConsts.JOB_TIMESTAMP;
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

    public String getFinanced() {
        return financed;
    }

    public void setFinanced(String financed) {
        this.financed = financed;
    }

    public String getRawPath() {
        return rawPath;
    }

    public void setRawPath(String rawPath) {
        this.rawPath = rawPath;
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

    public void setTaskName(String taskName) {
        this.taskName = taskName;
    }

    public String getTaskName() {
        return taskName;
    }

    public String getPid() {
        return pid;
    }

    public void setPid(String pid) {
        this.pid = pid;
    }
}
