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

import java.lang.reflect.Array;
import java.util.Collections;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;

/**
 * Wrapper suitable as a RestDataSource response
 *
 * @author Jan Pokorsky
 */
@XmlRootElement(name="response")
@XmlAccessorType(XmlAccessType.FIELD)
//@XmlSeeAlso(ImportFolder.class)
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
    @XmlElementWrapper(name="data")
//    @XmlElement(name="record")
    @XmlAnyElement
    private List<T> data;
//    private T[] data;
    /**
     * errors holder; see RestDataSource doc
     * <br/> validation format: status:STATUS_VALIDATION_ERROR, errors:[{fieldname:errormsg}] or errors:[fieldname:[{errormsg}]]
     * <br/> failure format: status:STATUS_FAILURE, errors:errormsg
     */
    private List<Object> errors;

    public SmartGwtResponse() {
    }

    public SmartGwtResponse(int status, Integer startRow, Integer endRow, Integer totalRows, List<T> data) {
        this.status = status;
        this.startRow = startRow;
        this.endRow = endRow;
        this.totalRows = totalRows;
//        this.data = data.toArray((T[]) Array.newInstance(data.get(0).getClass(), data.size()));
        this.data = (data != null) ? data : Collections.<T>emptyList();
    }

}
