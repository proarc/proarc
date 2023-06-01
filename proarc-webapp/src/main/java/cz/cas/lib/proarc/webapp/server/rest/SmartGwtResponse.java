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
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.webapp.server.rest.JacksonProvider.DefaultAdapter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Logger;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import static cz.cas.lib.proarc.webapp.server.rest.v1.DigitalObjectResourceV1.STATUS_DONT_BE_IGNORED;
import static cz.cas.lib.proarc.webapp.server.rest.v1.DigitalObjectResourceV1.STATUS_LOCKED;

/**
 * Wrapper suitable as a RestDataSource response
 *
 * @author Jan Pokorsky
 */
@XmlRootElement(name="response")
@XmlAccessorType(XmlAccessType.FIELD)
public class SmartGwtResponse<T>{

    private static final Logger LOG = Logger.getLogger(SmartGwtResponse.class.getName());

    public static final int STATUS_FAILURE = -1;
    public static final int STATUS_LOGIN_INCORRECT = -5;
    public static final int STATUS_LOGIN_REQUIRED = -7;
    public static final int STATUS_LOGIN_SUCCESS = -8;
    public static final int STATUS_MAX_LOGIN_ATTEMPTS_EXCEEDED = -6;
    public static final int STATUS_SERVER_TIMEOUT = -100;
    public static final int STATUS_SUCCESS = 0;
    public static final int STATUS_TRANSPORT_ERROR = -90;
    public static final int STATUS_VALIDATION_ERROR = -4;
    public static final int STATUS_OBJECT_LOCKED = -41;


    private int status;
    private Integer startRow;
    private Integer endRow;
    private Integer totalRows;
    @XmlTransient
    private List<T> typedData;
    /** The JAXB mapping for {@link #typedData} and {@link #errdata}. */
    private Object data;
    private String errorMessage;
    private String errorStackTrace;
    @XmlTransient
    private String errdata;
    /**
     * errors holder; see RestDataSource doc
     * <br/> validation format: status:STATUS_VALIDATION_ERROR,
     * errors:[{fieldname:{errorMessage:"msg"}}] or errors:[fieldname:[{errorMessage:"msg1"},{errorMessage:"msg2"}]]
     * <br/> failure format: status:STATUS_FAILURE, errors:errormsg
     */
    @XmlJavaTypeAdapter(ErrorAdapter.class)
    private Map<String, List<ErrorMessage>> errors;

    public SmartGwtResponse() {
    }

    public SmartGwtResponse(T singletonDataItem) {
        this(STATUS_SUCCESS, 0, 0, 1, singletonDataItem != null
                ? Collections.singletonList(singletonDataItem)
                : Collections.<T>emptyList());
    }

    public SmartGwtResponse(List<T> data) {
        this(STATUS_SUCCESS, 0, Math.max(0, data.size() - 1), data.size(), data);
    }

    public SmartGwtResponse(int status, Integer startRow, Integer endRow, Integer totalRows, List<T> data) {
        this.status = status;
        this.startRow = startRow;
        this.endRow = endRow;
        this.totalRows = totalRows;
        setTypedData(data);
    }

    /**
     * Builds response as an unrecoverable error with status {@link #STATUS_FAILURE}.
     * @param <T> data type
     * @param msg error message send as data
     * @return the response
     */
    public static <T> SmartGwtResponse<T> asError(String msg) {
        SmartGwtResponse<T> result = new SmartGwtResponse<T>();
        result.setErrorData(msg);
        result.setErrorMessage(msg);
        return result;
    }

    /**
     * Builds response as an unrecoverable error with status {@link #STATUS_FAILURE}.
     * @param <T> data type
     * @param msg error message send as data
     * @param session session context
     * @return the response
     */
    public static <T> SmartGwtResponse<T> asError(String msg, SessionContext session) {
        LOG.severe(session.getIp() + "{" + session.getUser().getUserName() + "} : " + msg);
        return asError(msg);
    }

    /**
     * @see #asError(java.lang.String)
     */
    public static <T> SmartGwtResponse<T> asError(Throwable t) {
        return asError(t.getMessage(), t);
    }

    /**
     * @see #asError(java.lang.String)
     */
    public static <T> SmartGwtResponse<T> asError(String msg, Throwable t) {
        StringWriter errorDataSW = new StringWriter();
        PrintWriter errorDataPW = new PrintWriter(errorDataSW);
        StringWriter stackTraceSW = new StringWriter();
        PrintWriter stackTracePW = new PrintWriter(stackTraceSW);
        if (msg == null || msg.isEmpty()) {
            msg = t.getMessage();
        }
        if (msg != null && !msg.isEmpty()) {
            errorDataPW.println(msg);
            errorDataPW.println();
        }
        t.printStackTrace(errorDataPW);
        t.printStackTrace(stackTracePW);
        errorDataPW.close();
        stackTracePW.close();
        return asError(errorDataSW.toString(), t.getMessage(), errorDataSW.toString());
    }

    private static <T> SmartGwtResponse<T> asError(String errorData, String message, String stackTrace) {
        SmartGwtResponse<T> result = new SmartGwtResponse<T>();
        result.setErrorData(errorData);
        result.setErrorInfo(message, stackTrace);
        return result;
    }

    public static <T> SmartGwtResponse<T> asError(String fieldName, String message) {
        return SmartGwtResponse.<T>asError().error(fieldName, message).build();
    }

    public static <T> ErrorBuilder<T> asError() {
        return new ErrorBuilder<T>();
    }

    public List<T> getData() {
        return typedData;
    }

    public String getDataAsError() {
        return errdata;
    }

    private void setTypedData(List<T> data) {
        this.typedData = (data != null) ? data : Collections.<T>emptyList();
        this.data = typedData;
        this.errdata = null;
    }

    private void setErrorData(String msg) {
        this.status = STATUS_FAILURE;
        this.errdata = msg;
        this.data = errdata;
        this.typedData = null;
    }

    private void setErrorInfo(String message, String stackTrace) {
        this.errorMessage = message;
        this.errorStackTrace = stackTrace;
    }

    public Integer getEndRow() {
        return endRow;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    public String getErrorStackTrace() {
        return errorStackTrace;
    }

    public Map<String, List<ErrorMessage>> getErrors() {
        return errors;
    }

    public Integer getStartRow() {
        return startRow;
    }

    public int getStatus() {
        return status;
    }

    public Integer getTotalRows() {
        return totalRows;
    }

    public static final class ErrorBuilder<T> {

        private Map<String, List<ErrorMessage>> errors = new LinkedHashMap<String, List<ErrorMessage>>();

        private ErrorBuilder() {
        }

        public ErrorBuilder<T> error(String fieldName, String message) {
            List<ErrorMessage> msgs = errors.get(fieldName);
            if (msgs == null) {
                msgs = new ArrayList<ErrorMessage>();
                errors.put(fieldName, msgs);
            }
            msgs.add(new ErrorMessage(message));
            return this;
        }

        public SmartGwtResponse<T> build() {
            return build(null);
        }

        public SmartGwtResponse<T> build(String type) {
            if (type == null) {
                type="";
            }
            SmartGwtResponse<T> result = new SmartGwtResponse<T>();
            result.errors = errors;
            switch (type) {
                case STATUS_LOCKED:
                    result.status = STATUS_OBJECT_LOCKED;
                    break;
                case STATUS_DONT_BE_IGNORED:
                    result.status = STATUS_VALIDATION_ERROR;
                    result.data = "cantIgnore";
                    break;
                default:
                    result.status = STATUS_VALIDATION_ERROR;
                    break;
            }
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

    /**
     * The JAXB mapping of Map to DOM Element that complies with the SmartGWT response schema.
     */
    public static class ErrorAdapter extends XmlAdapter<Element, Map<String, List<ErrorMessage>>> {

        public ErrorAdapter() {
        }

        @Override
        public Map<String, List<ErrorMessage>> unmarshal(Element v) throws Exception {
            // not required yet
            throw new UnsupportedOperationException();
        }

        @Override
        public Element marshal(Map<String, List<ErrorMessage>> v) throws Exception {
            if (v == null) {
                return null;
            }
            List<Element> errFields = errorsAsElements(v);
            if (errFields.isEmpty()) {
                return null;
            } else {
                Document doc = errFields.get(0).getOwnerDocument();
                Element errorsElm = doc.createElement("errors");
                for (Element errField : errFields) {
                    errorsElm.appendChild(errField);
                }
                return errorsElm;
            }
        }

        /**
         * Gets list of {@code  <fieldName><errorMessage>error</errorMessage></fieldName>} elements
         * where fieldName is replaced with real names.
         * @param errors maps field names to lists of errors
         * @return the list of DOM elements
         */
        private static List<Element> errorsAsElements(Map<String, List<ErrorMessage>> errors) {
            try {
                Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
                List<Element> errFields = new ArrayList<Element>();
                for (Entry<String, List<ErrorMessage>> entry : errors.entrySet()) {
                    String field = entry.getKey();
                    Element fieldElm = doc.createElement(field);
                    errFields.add(fieldElm);
                    for (ErrorMessage errmsg : entry.getValue()) {
                        Element errMsgElm = doc.createElement("errorMessage");
                        errMsgElm.setTextContent(errmsg.getErrorMessage());
                        fieldElm.appendChild(errMsgElm);
                    }
                }
                return errFields;
            } catch (ParserConfigurationException ex) {
                throw new IllegalStateException(ex);
            }
        }

    }

    /**
     * JSON JAXB mapping helper class. It removes errors XML adapter as Jackson can
     * serialize rather Map than DOM elements.
     */
    public static abstract class AnnotatedSmartGwtResponse<T> extends SmartGwtResponse<T> {

        @XmlJavaTypeAdapter(SgwtErrorAdapter.class)
        private Map<String, List<ErrorMessage>> errors;

    }

    private static class SgwtErrorAdapter extends DefaultAdapter<Map<String, List<ErrorMessage>>> {}

}
