/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.common.dao;

import java.sql.Timestamp;

/**
 * View to display {@link Batch batches} and their owners.
 *
 * @author Jan Pokorsky
 */
public class BatchView {

    private Integer id;
    private String folder;
    private String title;
    private Timestamp timestamp;
    private Timestamp create;
    private Timestamp updated;
    private Timestamp itemUpdated;
    private String state;
    private Integer userId;
    private String userName;
    private String parentPid;
    private String profileId;
    private String log;
    private Integer pageCount;
    private Integer estimateItemNumber;
    private String priority;

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getFolder() {
        return folder;
    }

    public void setFolder(String folder) {
        this.folder = folder;
    }

    public Timestamp getCreate() {
        return create;
    }

    public String getLog() {
        return log;
    }

    public void setLog(String log) {
        this.log = log;
    }

    public void setCreate(Timestamp create) {
        this.create = create;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public void setStateAsString(String state) {
        setState(state);
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public Integer getUserId() {
        return userId;
    }

    public void setUserId(Integer userId) {
        this.userId = userId;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String username) {
        this.userName = username;
    }

    public String getParentPid() {
        return parentPid;
    }

    public void setParentPid(String parentPid) {
        this.parentPid = parentPid;
    }

    public String getProfileId() {
        return profileId;
    }

    public void setProfileId(String profileId) {
        this.profileId = profileId;
    }

    public Integer getPageCount() {
        return pageCount;
    }

    public void setPageCount(Integer pageCount) {
        this.pageCount = pageCount;
    }

    public Integer getEstimateItemNumber() {
        return estimateItemNumber;
    }

    public void setEstimateItemNumber(Integer estimateItemNumber) {
        this.estimateItemNumber = estimateItemNumber;
    }

    public String getPriority() {
        return priority;
    }

    public void setPriority(String priority) {
        if (priority == null) {
            priority = Batch.PRIORITY_MEDIUM;
        }
        this.priority = priority;
    }

    public Timestamp getItemUpdated() {
        return itemUpdated;
    }

    public void setItemUpdated(Timestamp itemUpdated) {
        this.itemUpdated = itemUpdated;
    }

    public Timestamp getUpdated() {
        return updated;
    }

    public void setUpdated(Timestamp updated) {
        this.updated = updated;
    }

    @Override
    public String toString() {
        return "BatchView{" + "id=" + id + ", folder=" + folder + ", title=" + title
                + ", timestamp=" + timestamp + ", create=" + create
                + ", state=" + state + ", userId=" + userId + ", username=" + userName
                + ", parentPid=" + parentPid + ", profileId=" + profileId + '}';
    }


}
