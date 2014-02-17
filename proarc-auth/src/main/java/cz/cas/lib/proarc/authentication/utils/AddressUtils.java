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
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.stringtemplate.v4.STGroup;
import org.stringtemplate.v4.STGroupString;

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
     * Sends forbidden error
     * @param httpResp
     * @throws IOException
     */
    public static void forbiddenResource(HttpServletResponse httpResp, String content) throws IOException {
        // forbidden code with content;
        httpResp.setContentType("text/html");
        httpResp.getWriter().println(content);
        httpResp.setStatus(HttpServletResponse.SC_FORBIDDEN);
    }

    /**
     * Redirects to login page with ecnoded url parameter and error flag indicated first login failed
     * @throws IOException
     */
    public static void redirectToLogin(HttpServletRequest httpReq, HttpServletResponse httpResp, boolean errorOccured) throws IOException {
        String url = httpReq.getParameter("url");
        String redirectingURL = "proarclogin?";
        if (url != null) {
            redirectingURL = redirectingURL+RETURNS_URL_PARAM+"="+url;
        } else {
            redirectingURL = redirectingURL+RETURNS_URL_PARAM+"="+returnURLParam(httpReq);
        }
        if (errorOccured) {
            redirectingURL = redirectingURL+"&"+ERROR_PARAM+"=login";
        }
        httpResp.sendRedirect(redirectingURL);
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
     * @throws UnsupportedEncodingException 
     */
    public static String returnURLParam(HttpServletRequest httpReq) throws UnsupportedEncodingException {
        String addr = httpReq.getRequestURL().toString();
        String queryString = httpReq.getQueryString();
        if (queryString != null) {
            addr += "?" + queryString;
        }
        return URLEncoder.encode(addr, "UTF-8");
    }
}
