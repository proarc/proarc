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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.server.rest;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

/**
 *
 * @author Jan Pokorsky
 */
public class NotFoundException extends WebApplicationException {

    public NotFoundException(String name, String value) {
        this(String.format("%s: %s not found!", name, value));
    }
    
    public NotFoundException(String msg) {
        super(Response.status(Response.Status.NOT_FOUND)
                .entity(msg)
                .type(MediaType.TEXT_PLAIN_TYPE)
                .build());
    }

}
