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
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.TemporalAccessor;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Parameter reader for ISO Date and Time.
 *
 * @author Jan Pokorsky
 * @see <a href="http://en.wikipedia.org/wiki/ISO_8601">ISO 8601</a>
 */
public final class DateTimeParam {

    private static final Logger LOG = Logger.getLogger(DateTimeParam.class.getName());

    /**
     * ISO parser supporting:
     * yyyy-MM-dd
     * yyyy-MM-ddTHH:mm
     * yyyy-MM-ddTHH:mm:ss
     * yyyy-MM-ddTHH:mm:ssZ
     */
    private static final DateTimeFormatter FMT = new DateTimeFormatterBuilder()
            .append(DateTimeFormatter.ISO_LOCAL_DATE)
            .optionalStart()
            .appendLiteral('T')
            .append(DateTimeFormatter.ISO_LOCAL_TIME)
            .optionalStart()
            .appendOffsetId()
            .optionalEnd()
            .optionalEnd()
            .toFormatter();

    private final Instant instant;

    public DateTimeParam(String txt) {
        try {
            TemporalAccessor parsed = FMT.parseBest(
                    txt,
                    OffsetDateTime::from,
                    LocalDateTime::from,
                    LocalDate::from
            );

            if (parsed instanceof OffsetDateTime odt) {
                instant = odt.toInstant();
            } else if (parsed instanceof LocalDateTime ldt) {
                instant = ldt.atZone(ZoneId.systemDefault()).toInstant();
            } else if (parsed instanceof LocalDate ld) {
                instant = ld.atStartOfDay(ZoneId.systemDefault()).toInstant();
            } else {
                throw new IllegalArgumentException("Unsupported date format: " + txt);
            }

        } catch (Exception e) {
            LOG.log(Level.SEVERE, txt, e);
            throw RestException.plainText(Response.Status.BAD_REQUEST, e.getMessage());
        }
    }

    public Date toDate() {
        return Date.from(instant);
    }

    public Timestamp toTimestamp() {
        return Timestamp.from(instant);
    }
}