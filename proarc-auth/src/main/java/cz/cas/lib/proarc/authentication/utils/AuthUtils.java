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

import cz.cas.lib.proarc.authentication.Authenticators;
import cz.cas.lib.proarc.authentication.ProarcAuthFilter;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.ws.rs.core.MediaType;
import java.io.IOException;
import java.io.InputStream;

/**
 *
 * @author Jan Pokorsky
 */
public final class AuthUtils {

    /**
     * The HTTP header in unauthorized response to choose a login form/type on client.
     * Expect values like proarc, ....
     */
    public static final String HEADER_AUTHENTICATE_TYPE = "ProArc-Authenticate";

    /**
     * Writes the authentication required status to the HTTP response.
     *
     * @param response response
     * @throws IOException failure
     * @see #HEADER_AUTHENTICATE_TYPE
     * @see <a href='http://www.smartclient.com/smartgwt/javadoc/com/smartgwt/client/docs/Relogin.html'>SmartGWT Relogin</a>
     */
    public static void setLoginRequiredResponse(HttpServletResponse response) throws IOException {
        response.setHeader(HEADER_AUTHENTICATE_TYPE, Authenticators.getInstance().getLoginType());
        response.setStatus(HttpServletResponse.SC_FORBIDDEN);
        response.setContentType(MediaType.TEXT_HTML);
        InputStream res = ProarcAuthFilter.class.getResourceAsStream("loginRequiredMarker.html");
        try {
            res.transferTo(response.getOutputStream());
        } finally {
            closeQuietly(res);
        }
    }

    /**
     * Writes the authentication OK status to the HTTP response.
     *
     * @param response response
     * @throws IOException failure
     * @see <a href='http://www.smartclient.com/smartgwt/javadoc/com/smartgwt/client/docs/Relogin.html'>SmartGWT Relogin</a>
     */
    public static void setLoginSuccesResponse(HttpServletResponse response) throws IOException {
        response.setStatus(HttpServletResponse.SC_OK);
        response.setContentType(MediaType.TEXT_HTML);
        InputStream res = ProarcAuthFilter.class.getResourceAsStream("loginSuccessMarker.html");
        try {
            res.transferTo(response.getOutputStream());
        } finally {
            closeQuietly(res);
        }
    }

    public static void closeQuietly(AutoCloseable closeable) {
        if (closeable != null) {
            try {
                closeable.close();
            } catch (Exception ignored) {
            }
        }
    }
}
