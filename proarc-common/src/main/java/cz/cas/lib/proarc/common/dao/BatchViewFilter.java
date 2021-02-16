/*
 * Copyright (C) 2014 Jan Pokorsky
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

import cz.cas.lib.proarc.common.dao.Batch.State;
import java.sql.Timestamp;
import java.util.Set;

/**
 * Helps to filter {@link BatchView} queries.
 *
 * @author Jan Pokorsky
 */
public class BatchViewFilter {

    private Integer userId;
    private Integer batchId;
    private Set<State> state;
    private String filePattern;
    private Timestamp createdFrom;
    private Timestamp createdTo;
    private Timestamp modifiedFrom;
    private Timestamp modifiedTo;
    private String profile;
    private String sortBy;
    private int offset = 0;
    private int maxCount = Integer.MAX_VALUE;

    public Integer getUserId() {
        return userId;
    }

    public Integer getBatchId() {
        return batchId;
    }

    public Set<State> getState() {
        return state;
    }

    public String getFilePattern() {
        return filePattern;
    }

    public Timestamp getCreatedFrom() {
        return createdFrom;
    }

    public Timestamp getCreatedTo() {
        return createdTo;
    }

    public Timestamp getModifiedFrom() {
        return modifiedFrom;
    }

    public Timestamp getModifiedTo() {
        return modifiedTo;
    }

    public String getSortBy() {
        return sortBy;
    }

    public int getOffset() {
        return offset;
    }

    public int getMaxCount() {
        return maxCount;
    }

    public BatchViewFilter setUserId(Integer userId) {
        this.userId = userId;
        return this;
    }

    public BatchViewFilter setBatchId(Integer batchId) {
        this.batchId = batchId;
        return this;
    }

    public BatchViewFilter setState(Set<State> state) {
        this.state = state;
        return this;
    }

    /**
     * The file pattern to search batches with matching folders or batch item files.
     */
    public BatchViewFilter setFilePattern(String filePattern) {
        this.filePattern = filePattern;
        return this;
    }

    public BatchViewFilter setCreatedFrom(Timestamp createdFrom) {
        this.createdFrom = createdFrom;
        return this;
    }

    public BatchViewFilter setCreatedTo(Timestamp createdTo) {
        this.createdTo = createdTo;
        return this;
    }

    public BatchViewFilter setModifiedFrom(Timestamp modifiedFrom) {
        this.modifiedFrom = modifiedFrom;
        return this;
    }

    public BatchViewFilter setModifiedTo(Timestamp modifiedTo) {
        this.modifiedTo = modifiedTo;
        return this;
    }

    public BatchViewFilter setSortBy(String sortBy) {
        this.sortBy = sortBy;
        return this;
    }

    public BatchViewFilter setOffset(int offset) {
        this.offset = offset;
        return this;
    }

    public BatchViewFilter setMaxCount(int maxCount) {
        this.maxCount = maxCount;
        return this;
    }

    public String getProfile() {
        return profile;
    }

    public BatchViewFilter setProfile(String profile) {
        this.profile = profile;
        return this;
    }
}
