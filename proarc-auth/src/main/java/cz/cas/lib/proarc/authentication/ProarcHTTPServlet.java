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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.stringtemplate.v4.ST;
import org.stringtemplate.v4.STGroup;
import org.stringtemplate.v4.STGroupString;

import cz.cas.lib.proarc.authentication.desa.DESAAuthenticator;
import cz.cas.lib.proarc.authentication.proarc.ProArcAuthenticator;
import cz.cas.lib.proarc.authentication.utils.IOUtils;
import static cz.cas.lib.proarc.authentication.utils.AddressUtils.*;

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
        PrintWriter writer = resp.getWriter();
        String error = req.getParameter("error");
        String redirectURLAddress = req.getParameter("url");
        String render = htmlTemplate(error != null && error.equals("login"), redirectURLAddress).render();
        writer.write(render);
    }

    /**
     * Render login html
     * @param error true, if the first login failed
     * @return Rendered html
     * @throws IOException
     * @throws UnsupportedEncodingException
     */
    public ST htmlTemplate(boolean error, String redirectURL) throws IOException,
            UnsupportedEncodingException {
        URL urlRes = IOUtils.class.getClassLoader()
                .getResource("loginfile.stg");
        InputStream isStream = urlRes.openStream();
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        IOUtils.copyStreams(isStream, bos);
        String str = new String(bos.toByteArray(), "UTF-8");
        STGroup stGroup = new STGroupString(str, str, '$', '$');
        ST html = stGroup.getInstanceOf("html");
        html.add("error", error);
        html.add("url", redirectURL);
        return html;
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
            String retUrl = req.getParameter(RETURNS_URL_PARAM);
            if (retUrl != null) {
                String decoded = URLDecoder.decode(retUrl, "UTF-8");
                resp.sendRedirect(decoded);
            } else {
                redirectToHome(req, resp);
            }
        } else {
            redirectToLogin(req, resp, true);
        }
    }
}
