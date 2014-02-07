/*
 * Copyright (C) 2013 Pavel Stastny
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
package cz.cas.lib.proarc.authentication.utils;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class AddressUtils {

    public static final String RETURNS_URL_PARAM="url";
    public static final String ERROR_PARAM="error";
    
    /**
     * Redirects to login page with encoded return url parameter
     * @throws IOException
     */
    public static void redirectToLogin(HttpServletRequest httpReq, HttpServletResponse httpResp) throws IOException {
        httpResp.sendRedirect("proarclogin?"+RETURNS_URL_PARAM+"="+returnURLParam(httpReq));
    }

    /**
     * Redirects to login page with ecnoded url parameter and error flag indicated first login failed
     * @throws IOException
     */
    public static void redirectToLogin(HttpServletRequest httpReq, HttpServletResponse httpResp, boolean errorOccured) throws IOException {
        httpResp.sendRedirect("proarclogin?"+RETURNS_URL_PARAM+"="+returnURLParam(httpReq)+"&"+ERROR_PARAM+"="+errorOccured);
    }
        
    
    /**
     * Redirect to home
     * @throws IOException
     */
    public static void redirectToHome(HttpServletRequest httpReq, HttpServletResponse httpResp) throws IOException {
        httpResp.sendRedirect("index.html");
    }
    /**
     * Returns encoded return url parameter
     */
    public static String returnURLParam(HttpServletRequest httpReq) {
        String addr = httpReq.getRequestURL().toString();
        String queryString = httpReq.getQueryString();
        if (queryString != null) {
            addr += "?" + queryString;
        }
        return addr;
    }
}
