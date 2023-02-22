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
package cz.cas.lib.proarc.common.object;


/**
 * The description metadata of a digital object.
 *
 * @author Jan Pokorsky
 */
public class DescriptionMetadata<T> {

    private String pid;
    private Integer batchId;
    private String krameriusInstanceId;
    private long timestamp;
    private String editor;
    private String model;
    private T data;
    private String content;
    private boolean ignoreValidation = false;
    private DigitalObjectHandler parentHandler;
    private String standard;

    /**
     * an alternative id of a digital object from the workflow
     * a digital object that exists only in the workflow may not have a pid
     */
    private Long worfklowJobId;

    public DescriptionMetadata() {
    }

    public Integer getBatchId() {
        return batchId;
    }

    public void setBatchId(Integer batchId) {
        this.batchId = batchId;
    }

    public String getKrameriusInstanceId() {
        return krameriusInstanceId;
    }

    public void setKrameriusInstanceId(String krameriusInstanceId) {
        this.krameriusInstanceId = krameriusInstanceId;
    }

    public T getData() {
        return data;
    }

    public void setData(T data) {
        this.data = data;
    }

    public String getEditor() {
        return editor;
    }

    public void setEditor(String editor) {
        this.editor = editor;
    }

    public boolean isIgnoreValidation() {
        return ignoreValidation;
    }

    public void setIgnoreValidation(boolean ignoreValidation) {
        this.ignoreValidation = ignoreValidation;
    }

    public String getPid() {
        return pid;
    }

    public void setPid(String pid) {
        this.pid = pid;
    }

    public long getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(long timestamp) {
        this.timestamp = timestamp;
    }

    public Long getWorfklowJobId() {
        return worfklowJobId;
    }

    public void setWorfklowJobId(Long worfklowJobId) {
        this.worfklowJobId = worfklowJobId;
    }

    public void setParentHandler(DigitalObjectHandler parentHandler) {
        this.parentHandler = parentHandler;
    }

    public String getModel() {
        return model;
    }

    public void setModel(String model) {
        this.model = model;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public String getStandard() {
        return standard;
    }

    public void setStandard(String standard) {
        this.standard = standard;
    }
}
