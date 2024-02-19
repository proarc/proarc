/*
 * Copyright (C) 2012 Jan Pokorsky
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
package cz.cas.lib.proarc.common.storage;

/**
 *
 * @author Jan Pokorsky
 */
public class DigitalObjectException extends Exception {
    
    private static final long serialVersionUID = 1L;

    private final String pid;
    private final Integer batchId;
    private String message;
    private final String dsId;

    public DigitalObjectException(String pid) {
        this(pid, (Throwable) null);
    }

    public DigitalObjectException(String pid, String message) {
        this(pid, message, null);
    }
    
    public DigitalObjectException(String pid, Throwable cause) {
        this(pid, cause == null ? null : cause.getMessage(), cause);
    }

    public DigitalObjectException(String pid, String message, Throwable cause) {
        this(pid, null, null, message, cause);
    }

    public DigitalObjectException(String pid, Integer batchId, String dsId, String message, Throwable cause) {
        super(buildMsg(pid, batchId, dsId, message), cause);
        this.message = message;
        this.pid = pid;
        this.batchId = batchId;
        this.dsId = dsId;
    }

    public String getPid() {
        return pid;
    }

    public Integer getBatchId() {
        return batchId;
    }

    public String getDsId() {
        return dsId;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public String getMyMessage() {
        return message;
    }

    private static String buildMsg(String pid, Integer batchId, String dsId, String message) {
        StringBuilder sb = new StringBuilder("PID: ").append(pid);
        if (batchId != null) {
            sb.append(", batchId: ").append(batchId);
        }
        if (dsId != null) {
            sb.append(", dsId: ").append(dsId);
        }
        if (message != null) {
            sb.append(", ").append(message);
        }
        return sb.toString();
    }

}
