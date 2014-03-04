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

import cz.cas.lib.proarc.authentication.desa.DESAAuthenticator;
import cz.cas.lib.proarc.authentication.proarc.ProArcAuthenticator;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Login servlet
 * @author pavels
 */
public class ProarcHTTPServlet extends HttpServlet {

    protected ChainAuthenticator chain;

    @Override
    public void init() throws ServletException {
        super.init();
        this.chain = new ChainAuthenticator(Arrays.asList(
                new DESAAuthenticator(), new ProArcAuthenticator()));
    }

    /**
     * Form get method
     */
    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp)
            throws ServletException, IOException {
        resp.setContentType("text/html; charset=utf-8");
        resp.setStatus(HttpServletResponse.SC_OK);
    }


    /**
     * Form post method
     */
    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp)
            throws ServletException, IOException {
        String username = req.getParameter("j_username");
        String password = req.getParameter("j_password");
        String code = req.getParameter("j_code");
        Map<String, String> loginProperties = new HashMap<String, String>();
        {
            loginProperties.put(Authenticator.LOGINNAME, username);
            loginProperties.put(Authenticator.PASSWORD, password);
            loginProperties.put(DESAAuthenticator.KOD_PUVODCE, code);
        }
        
        ProarcPrincipal proarcPrincipal = new ProarcPrincipal(username);
        if (this.chain.authenticate(loginProperties, req, resp, proarcPrincipal)) {
            // store principal to session    
            req.getSession(true).setAttribute(ProarcAuthFilter.SESSION_KEY,proarcPrincipal);
            resp.setStatus(HttpServletResponse.SC_OK);
        } else {
            resp.setStatus(HttpServletResponse.SC_FORBIDDEN);
        }
    }

}
