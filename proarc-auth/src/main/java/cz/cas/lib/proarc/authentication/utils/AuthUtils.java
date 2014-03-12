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
package cz.cas.lib.proarc.authentication.utils;

import cz.cas.lib.proarc.authentication.ProarcAuthFilter;
import java.io.IOException;
import java.io.InputStream;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.MediaType;
import org.apache.commons.io.IOUtils;

/**
 *
 * @author Jan Pokorsky
 */
public final class AuthUtils {

    /**
     * Writes authentication required status to the HTTP response.
     * @param response response
     * @throws IOException failure
     * @see <a href='http://www.smartclient.com/smartgwt/javadoc/com/smartgwt/client/docs/Relogin.html'>SmartGWT Relogin</a>
     */
    public static void setLoginRequiredResponse(HttpServletResponse response) throws IOException {
        response.setStatus(HttpServletResponse.SC_FORBIDDEN);
        response.setContentType(MediaType.TEXT_HTML);
        InputStream res = ProarcAuthFilter.class.getResourceAsStream("loginRequiredMarker.html");
        try {
            IOUtils.copy(res, response.getOutputStream());
            res.close();
        } finally {
            IOUtils.closeQuietly(res);
        }
    }

    /**
     * Writes authentication OK status to the HTTP response.
     * @param response response
     * @throws IOException failure
     * @see <a href='http://www.smartclient.com/smartgwt/javadoc/com/smartgwt/client/docs/Relogin.html'>SmartGWT Relogin</a>
     */
    public static void  setLoginSuccesResponse(HttpServletResponse response) throws IOException {
        response.setStatus(HttpServletResponse.SC_OK);
        response.setContentType(MediaType.TEXT_HTML);
        InputStream res = ProarcAuthFilter.class.getResourceAsStream("loginSuccessMarker.html");
        try {
            IOUtils.copy(res, response.getOutputStream());
            res.close();
        } finally {
            IOUtils.closeQuietly(res);
        }
    }
}
