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
import java.sql.Timestamp;

/**
 *
 * @author Jan Pokorsky
 */
public class Task {

    private Timestamp created;
    private BigDecimal id;
    private BigDecimal jobId;
    private String note;
    private BigDecimal ownerId;
    private Integer priority;
    private State state;
    /** The name of a task type in workflow profile. */
    private String typeRef;
    private Timestamp timestamp;

    public Timestamp getCreated() {
        return created;
    }

    public Task addCreated(Timestamp created) {
        setCreated(created);
        return this;
    }

    public void setCreated(Timestamp created) {
        this.created = created;
    }

    public BigDecimal getId() {
        return id;
    }

    public Task addId(BigDecimal id) {
        setId(id);
        return this;
    }

    public void setId(BigDecimal id) {
        this.id = id;
    }

    public BigDecimal getJobId() {
        return jobId;
    }

    public Task addJobId(BigDecimal jobId) {
        setJobId(jobId);
        return this;
    }

    public void setJobId(BigDecimal jobId) {
        this.jobId = jobId;
    }

    public String getNote() {
        return note;
    }

    public Task addNote(String note) {
        setNote(note);
        return this;
    }

    public void setNote(String note) {
        this.note = note;
    }

    public BigDecimal getOwnerId() {
        return ownerId;
    }

    public Task addOwnerId(BigDecimal ownerId) {
        setOwnerId(ownerId);
        return this;
    }

    public void setOwnerId(BigDecimal ownerId) {
        this.ownerId = ownerId;
    }

    public Integer getPriority() {
        return priority;
    }

    public Task addPriority(Integer priority) {
        setPriority(priority);
        return this;
    }

    public void setPriority(Integer priority) {
        this.priority = priority;
    }

    public State getState() {
        return state;
    }

    public Task setState(State state) {
        this.state = state;
        return this;
    }

    public String getStateAsString() {
        return state == null ? null : state.name();
    }

    public Task setStateAsString(String state) {
        return setState(State.valueOf(state));
    }

    public String getTypeRef() {
        return typeRef;
    }

    public Task addTypeRef(String typeRef) {
        setTypeRef(typeRef);
        return this;
    }

    public void setTypeRef(String typeRef) {
        this.typeRef = typeRef;
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public Task addTimestamp(Timestamp timestamp) {
        setTimestamp(timestamp);
        return this;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }

    public enum State {
        WAITING, READY, STARTED, FINISHED, CANCELED
    }

}
