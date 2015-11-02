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
public class Job {

    private Timestamp created;
    private BigDecimal id;
    private String financed;
    private String label;
    private String note;
    private BigDecimal ownerId;
    private int priority;
    private String profileName;
    private State state;
    private Timestamp timestamp;

    public Timestamp getCreated() {
        return created;
    }

    public Job addCreated(Timestamp created) {
        setCreated(created);
        return this;
    }

    public void setCreated(Timestamp created) {
        this.created = created;
    }

    public BigDecimal getId() {
        return id;
    }

    public Job addId(BigDecimal id) {
        setId(id);
        return this;
    }

    public void setId(BigDecimal id) {
        this.id = id;
    }

    public String getFinanced() {
        return financed;
    }

    public Job addFinanced(String financed) {
        setFinanced(financed);
        return this;
    }

    public void setFinanced(String financed) {
        this.financed = financed;
    }

    public String getLabel() {
        return label;
    }

    public Job addLabel(String label) {
        setLabel(label);
        return this;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getNote() {
        return note;
    }

    public Job addNote(String note) {
        setNote(note);
        return this;
    }

    public void setNote(String note) {
        this.note = note;
    }

    public BigDecimal getOwnerId() {
        return ownerId;
    }

    public Job addOwnerId(BigDecimal ownerId) {
        setOwnerId(ownerId);
        return this;
    }

    public void setOwnerId(BigDecimal ownerId) {
        this.ownerId = ownerId;
    }

    public int getPriority() {
        return priority;
    }

    public Job addPriority(int priority) {
        setPriority(priority);
        return this;
    }

    public void setPriority(int priority) {
        this.priority = priority;
    }

    public String getProfileName() {
        return profileName;
    }

    public Job addProfileName(String profileName) {
        setProfileName(profileName);
        return this;
    }

    public void setProfileName(String profileName) {
        this.profileName = profileName;
    }

    public State getState() {
        return state;
    }

    public Job setState(State state) {
        this.state = state;
        return this;
    }

    public String getStateAsString() {
        return state == null ? null : state.name();
    }

    public Job setStateAsString(String state) {
        return setState(State.fromValue(state));
    }

    public Timestamp getTimestamp() {
        return timestamp;
    }

    public Job addTimestamp(Timestamp timestamp) {
        setTimestamp(timestamp);
        return this;
    }

    public void setTimestamp(Timestamp timestamp) {
        this.timestamp = timestamp;
    }

    public enum State {

        OPEN,
        FINISHED,
        CANCELED
        ;

        public static State fromValue(String s) {
            for (State state : values()) {
                if (state.name().equals(s)) {
                    return state;
                }
            }
            return null;
        }
    }
}
