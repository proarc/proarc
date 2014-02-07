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

import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Basic authentication interface.    
 * 
 * The Class should implement one authentication mechanism (basic, form, etc..). 
 * 
 * @author pavels
 * @see ChainAuthenticator
 */
public interface Authenticator {

    
    /** Login name key */
    public static final String LOGINNAME = "loginname";
        
    /** Password key */ 
    public static final String PASSWORD = "password";
    
    /**
     * Main authenticate method
     * @param loginProperties Login properties map
     * @param request TODO
     * @param response TODO
     * @return
     * @see ChainAuthenticator#authenticate(Map, HttpServletRequest, HttpServletResponse, ProarcPrincipal)
     */
    public boolean authenticate(Map<String, String> loginProperties, HttpServletRequest request, HttpServletResponse response, ProarcPrincipal principal);
}
