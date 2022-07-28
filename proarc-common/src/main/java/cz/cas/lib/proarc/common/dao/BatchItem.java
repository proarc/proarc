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
 *
 * @author Jan Pokorsky
 */
public class BatchItem {

    public enum Type {
        DATASTREAM, FILE, OBJECT;
    }

    public enum FileState {
        OK, SKIPPED;
    }

    public enum ObjectState {
        EXCLUDED, LOADING, LOADING_FAILED, LOADED, INGESTING_FAILED, INGESTED, STOPPED;
    }

    public enum StreamState {
        INVALID, MISSING, UNKNOWN, VERIFIED;
    }

    private Integer id;
    private Integer batchId;
    private String pid;
    private String dsId;
    private String file;
    private String state;
    private Type type;
    private String log;
    private Timestamp timestamp;

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getBatchId() {
        return batchId;
    }

    public void setBatchId(Integer batchId) {
        this.batchId = batchId;
    }

    public String getPid() {
        return pid;
    }

    public void setPid(String pid) {
        this.pid = pid;
    }

    public String getDsId() {
        return dsId;
    }

    public void setDsId(String dsId) {
        this.dsId = dsId;
    }

    public String getFile() {
        return file;
    }

    public void setFile(String file) {
        this.file = file;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }

    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }

    public String getTypeAsString() {
        return type == null ? null : type.name();
    }

    public void setTypeAsString(String type) {
        setType(type == null ? null : Type.valueOf(type));
    }

    public String getLog() {
        return log;
    }

    public void setLog(String log) {
        this.log = log;
    }

    @Override
    public String toString() {
        return "BatchItem{" + "id=" + id + ", batchId=" + batchId + ", pid=" + pid + ", dsId=" + dsId + ", file=" + file + ", state=" + state + ", type=" + type + ", log=" + log + ", timestamp=" + timestamp + '}';
    }


}
