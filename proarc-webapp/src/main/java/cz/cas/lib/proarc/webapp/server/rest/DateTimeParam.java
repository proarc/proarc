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
package cz.cas.lib.proarc.webapp.server.rest;

import jakarta.ws.rs.core.Response;
import java.sql.Timestamp;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.ISODateTimeFormat;

/**
 * Parameter reader for ISO Date and Time.
 *
 * @author Jan Pokorsky
 * @see <a href='http://en.wikipedia.org/wiki/ISO_8601'>ISO 8601</a>
 */
public final class DateTimeParam {
    
    private static final Logger LOG = Logger.getLogger(DateTimeParam.class.getName());
    private static final DateTimeFormatter FMT = ISODateTimeFormat.dateOptionalTimeParser();
    private final DateTime dateTime;

    public DateTimeParam(String txt) {
        try {
            dateTime = FMT.parseDateTime(txt);
        } catch (Exception e) {
            LOG.log(Level.SEVERE, txt, e);
            throw RestException.plainText(Response.Status.BAD_REQUEST, e.getMessage());
        }
    }

    public Date toDate() {
        return dateTime.toDate();
    }

    public Timestamp toTimestamp() {
        return new Timestamp(dateTime.getMillis());
    }

}
