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

import cz.cas.lib.proarc.authentication.utils.AuthUtils;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * Login/logout servlet.
 *
 * @author pavels
 */
public class ProarcLoginServlet extends HttpServlet {

    /**
     * The login post method.
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
        }

        ProarcPrincipal proarcPrincipal = new ProarcPrincipal(username);
        ServletOutputStream outputStream = resp.getOutputStream();
        ChainAuthenticator chain = new ChainAuthenticator(Authenticators.getInstance().getAuthenticators());
        if (chain.authenticate(loginProperties, req, resp, proarcPrincipal)) {
            // store principal to session    
            req.getSession(true).setAttribute(ProarcAuthFilter.SESSION_KEY, proarcPrincipal);
            AuthUtils.setLoginSuccesResponse(resp);
        } else {
            AuthUtils.setLoginRequiredResponse(resp);
        }
        outputStream.flush();
    }

    /**
     * Logout the session.
     */
    @Override
    protected void doDelete(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        HttpSession session = req.getSession(false);
        if (session != null) {
            session.invalidate();
        }
        resp.setStatus(HttpServletResponse.SC_NO_CONTENT);
    }

}
