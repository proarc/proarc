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
 * The batch describes group of {@link BatchItem items} related to some task
 * e.g. import.
 *
 * @author Jan Pokorsky
 */
public class Batch {

    public enum State {

        EMPTY, LOADING, LOADING_FAILED, LOADED, INGESTING, INGESTING_FAILED, INGESTED
    }
    
    private Integer id;
    private String folder;
    private String title;
    private String parentPid;
    private Timestamp create;
    private Timestamp timestamp;
    private State state;
    private Integer userId;
    private Integer estimateItemNumber;
    // user input fields
    private String device;
    private boolean generateIndices;
    private String log;
//    private String model;

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Timestamp getCreate() {
        return create;
    }

    public void setCreate(Timestamp create) {
        this.create = create;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }

    public String getFolder() {
        return folder;
    }

    public void setFolder(String folderPath) {
        this.folder = folderPath;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public State getState() {
        return state;
    }

    public void setState(State state) {
        this.state = state;
    }

    public String getStateAsString() {
        return state == null ? null : state.name();
    }

    public void setStateAsString(String state) {
        this.state = state == null ? null : State.valueOf(state);
    }

    public Integer getUserId() {
        return userId;
    }

    public void setUserId(Integer userId) {
        this.userId = userId;
    }

    public Integer getEstimateItemNumber() {
        return estimateItemNumber;
    }

    public void setEstimateItemNumber(Integer estimateItemNumber) {
        this.estimateItemNumber = estimateItemNumber;
    }

    public String getParentPid() {
        return parentPid;
    }

    public void setParentPid(String parentPid) {
        this.parentPid = parentPid;
    }

    public String getDevice() {
        return device;
    }

    public void setDevice(String device) {
        this.device = device;
    }

    public boolean isGenerateIndices() {
        return generateIndices;
    }

    public void setGenerateIndices(boolean generateIndices) {
        this.generateIndices = generateIndices;
    }

    public String getLog() {
        return log;
    }

    public void setLog(String log) {
        this.log = log;
    }

    @Override
    public String toString() {
        return "Batch{" + "id=" + id + ", folder=" + folder + ", title=" + title + ", parentPid=" + parentPid + ", create=" + create + ", timestamp=" + timestamp + ", state=" + state + ", userId=" + userId + ", estimateItemNumber=" + estimateItemNumber + ", device=" + device + ", generateIndices=" + generateIndices + ", log=" + log + '}';
    }

}
