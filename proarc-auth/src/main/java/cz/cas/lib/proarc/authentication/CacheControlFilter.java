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
package cz.cas.lib.proarc.authentication;

import java.io.IOException;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Cache headers controller
 * @author pavels
 */
public class CacheControlFilter implements Filter {

    public static Logger LOGGER = Logger.getLogger(CacheControlFilter.class
            .getName());

    @Override
    public void destroy() {
    }

    @Override
    public void doFilter(ServletRequest arg0, ServletResponse arg1,FilterChain arg2) throws IOException, ServletException {
        try {
            HttpServletRequest httpServletRequest = (HttpServletRequest) arg0;
            String requestURI = httpServletRequest.getRequestURI();
            if (checkURI(requestURI)) {
                Date currentDate = new Date();
                HttpServletResponse httpServletResponse = (HttpServletResponse) arg1;
                httpServletResponse.setDateHeader("Date", currentDate.getTime());
                // no cache headers
                httpServletResponse.setHeader("Pragma", "no-cache");
                httpServletResponse.setHeader("Cache-control","no-cache, no-store, must-revalidate");
            }
            arg2.doFilter(arg0, arg1);
        } catch (Exception ex) {
            LOGGER.log(Level.SEVERE, ex.getMessage(), ex);
        }
    }
        
    /**
     * Returns true if given uri should not be cached
     * @param requestURI
     * @return
     */
    private boolean checkURI(String requestURI) {
        String[] shouldNotBeCached = new String[] {"index.html",".js"};   
        for (String cRes : shouldNotBeCached) {
            if (requestURI.endsWith(cRes)) return true;
        }
        return false;
    }

    @Override
    public void init(FilterConfig arg0) throws ServletException {
    }
}
