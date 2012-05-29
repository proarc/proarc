/*
 * Copyright (C) 2011 Jan Pokorsky
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.server.rest;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Wrapper suitable as a RestDataSource response
 *
 * @author Jan Pokorsky
 */
@XmlRootElement(name="response")
@XmlAccessorType(XmlAccessType.FIELD)
public class SmartGwtResponse<T> {

    public static final int STATUS_FAILURE = -1;
    public static final int STATUS_LOGIN_INCORRECT = -5;
    public static final int STATUS_LOGIN_REQUIRED = -7;
    public static final int STATUS_LOGIN_SUCCESS = -8;
    public static final int STATUS_MAX_LOGIN_ATTEMPTS_EXCEEDED = -6;
    public static final int STATUS_SERVER_TIMEOUT = -100;
    public static final int STATUS_SUCCESS = 0;
    public static final int STATUS_TRANSPORT_ERROR = -90;
    public static final int STATUS_VALIDATION_ERROR = -4;

    private int status;
    private Integer startRow;
    private Integer endRow;
    private Integer totalRows;
    private List<T> data;
    
    /**
     * errors holder; see RestDataSource doc
     * <br/> validation format: status:STATUS_VALIDATION_ERROR, errors:[{fieldname:errormsg}] or errors:[fieldname:[{errormsg}]]
     * <br/> failure format: status:STATUS_FAILURE, errors:errormsg
     */
    private Map<String, List<ErrorMessage>> errors;

    public SmartGwtResponse() {
    }

    public SmartGwtResponse(T singletonDataItem) {
        this(STATUS_SUCCESS, 0, 0, 1, Collections.singletonList(singletonDataItem));
    }

    public SmartGwtResponse(List<T> data) {
        this(STATUS_SUCCESS, 0, Math.max(0, data.size() - 1), data.size(), data);
    }

    public SmartGwtResponse(int status, Integer startRow, Integer endRow, Integer totalRows, List<T> data) {
        this.status = status;
        this.startRow = startRow;
        this.endRow = endRow;
        this.totalRows = totalRows;
        this.data = (data != null) ? data : Collections.<T>emptyList();
    }

    public static <T> ErrorBuilder<T> asError() {
        return new ErrorBuilder<T>();
    }

    public static final class ErrorBuilder<T> {

        private Map<String, List<ErrorMessage>> errors = new HashMap<String, List<ErrorMessage>>();

        private ErrorBuilder() {
        }

        public ErrorBuilder error(String fieldName, String message) {
            List<ErrorMessage> msgs = errors.get(fieldName);
            if (msgs == null) {
                msgs = new ArrayList<ErrorMessage>();
                errors.put(fieldName, msgs);
            }
            msgs.add(new ErrorMessage(message));
            return this;
        }

        public SmartGwtResponse<T> build() {
            SmartGwtResponse<T> result = new SmartGwtResponse<T>();
            result.errors = errors;
            result.status = STATUS_VALIDATION_ERROR;
            return result;
        }
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class ErrorMessage {
        
        private String errorMessage;

        public ErrorMessage(String errorMessage) {
            this.errorMessage = errorMessage;
        }

        public ErrorMessage() {
        }

        public String getErrorMessage() {
            return errorMessage;
        }
        
    }

}
