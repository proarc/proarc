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
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.common.dao.ConcurrentModificationException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectConcurrentModificationException;
import cz.cas.lib.proarc.common.fedora.DigitalObjectNotFoundException;
import cz.cas.lib.proarc.common.json.JsonUtils;
import java.io.IOException;
import java.io.StringWriter;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Helper class to throw HTTP errors in different formats.
 *
 * @author Jan Pokorsky
 */
public class RestException extends WebApplicationException {
    
    private static final long serialVersionUID = 1L;
    private static final Logger LOG = Logger.getLogger(RestException.class.getName());

    private RestException(Response response) {
        super(response);
    }

    public static RestException plainText(Status status, String message) {
        return new RestException(plainText(Response.status(status), message).build());
    }

    public static RestException plainNotFound(String message) {
        return new RestException(plainText(Response.status(Status.NOT_FOUND), message).build());
    }

    public static RestException plainNotFound(String name, String value) {
        return new RestException(
                plainText(Response.status(Status.NOT_FOUND),
                        String.format("%s: %s not found!", name, value))
                .build());
    }

    public static RestException json(Status status, int errorCode, String errorKey, String message) {
        return new RestException(json(Response.status(status), errorCode, errorKey, message).build());
    }

    private static ResponseBuilder plainText(ResponseBuilder rb, String message) {
        return rb.type(MediaType.TEXT_PLAIN_TYPE).entity(message);
    }
    
    private static ResponseBuilder json(ResponseBuilder rb, int errorCode, String errorKey, String message) {
        try {
            StringWriter sw = new StringWriter();
            JsonUtils.defaultObjectMapper().writeValue(sw, new ErrorResponse(errorCode, errorKey, message));
            return rb.type(MediaType.APPLICATION_JSON_TYPE).entity(sw.toString());
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

    @XmlRootElement(name = "response")
    @XmlAccessorType(XmlAccessType.FIELD)
    private static class ErrorResponse {

        private int errorCode;
        private String errorKey;
        private String message;

        public ErrorResponse(int errorCode, String errorKey, String message) {
            this.errorCode = errorCode;
            this.errorKey = errorKey;
            this.message = message;
        }

        public ErrorResponse() {
        }

    }

    @Provider
    public static final class DigitalObjectConcurrentModificationMapper implements ExceptionMapper<DigitalObjectConcurrentModificationException> {

        @Override
        public Response toResponse(DigitalObjectConcurrentModificationException ex) {
            LOG.log(Level.INFO, null, ex);
            return plainText(Response.status(Status.CONFLICT), ex.getMessage()).build();
        }

    }

    @Provider
    public static final class DigitalObjectNotFoundExceptionMapper implements ExceptionMapper<DigitalObjectNotFoundException> {

        @Override
        public Response toResponse(DigitalObjectNotFoundException ex) {
            LOG.log(Level.INFO, null, ex);
            return plainText(Response.status(Status.NOT_FOUND), "Not found! " + ex.getMessage()).build();
        }

    }

    @Provider
    public static final class ConcurrentModificationExceptionMapper implements ExceptionMapper<ConcurrentModificationException> {

        @Override
        public Response toResponse(ConcurrentModificationException ex) {
            LOG.log(Level.INFO, null, ex);
            return plainText(Response.status(Status.CONFLICT), ex.getMessage()).build();
        }

    }

}
