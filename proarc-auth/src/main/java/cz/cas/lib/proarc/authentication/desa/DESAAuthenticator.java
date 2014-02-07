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
package cz.cas.lib.proarc.authentication.desa;

import java.io.File;
import java.net.PasswordAuthentication;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.ws.BindingProvider;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;

import cz.cas.lib.proarc.authentication.AbstractAuthenticator;
import cz.cas.lib.proarc.authentication.Authenticator;
import cz.cas.lib.proarc.authentication.ProarcPrincipal;
import cz.cas.lib.proarc.common.config.AppConfiguration;
import cz.cas.lib.proarc.common.config.AppConfigurationException;
import cz.cas.lib.proarc.common.config.AppConfigurationFactory;
import cz.cas.lib.proarc.common.export.desa.DesaServices;
import cz.cas.lib.proarc.common.export.desa.DesaServices.DesaConfiguration;
import cz.cas.lib.proarc.common.export.desa.sip2desa.api.AuthenticateUserFault;
import cz.cas.lib.proarc.common.export.desa.sip2desa.api.AuthenticateUserRequest;
import cz.cas.lib.proarc.common.export.desa.sip2desa.api.AuthenticateUserResponse;
import cz.cas.lib.proarc.common.export.desa.sip2desa.api.SIPSubmission;
import cz.cas.lib.proarc.common.export.desa.sip2desa.api.SIPSubmissionService;

/**
 * DESA authentication
 * @author pavels
 */
public class DESAAuthenticator extends AbstractAuthenticator {
    
    public static Logger LOGGER = Logger.getLogger(DESAAuthenticator.class.getName());
    
    public static final String KOD_PUVODCE = "kod";
    private static final String OK_STATUS = "OK";

    private SIPSubmission port;
    
    SIPSubmission initializeDesa(final String url, final String user,final String password) {
        // TODO: change it
        java.net.Authenticator.setDefault(new java.net.Authenticator() {
            @Override
            protected PasswordAuthentication getPasswordAuthentication() {
                return new PasswordAuthentication(user, password.toCharArray());
            }
        });
        SIPSubmissionService desaService = new SIPSubmissionService();
        SIPSubmission desaPort = desaService.getSIPSubmissionSOAP();
        final Map<String, Object> context = ((BindingProvider) desaPort).getRequestContext();
        context.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY, url);
        context.put(BindingProvider.USERNAME_PROPERTY, user);
        context.put(BindingProvider.PASSWORD_PROPERTY, password);
        return desaPort;
    }

    public DESAAuthenticator(String url, String wsUser, String wsPswd) {
        this.port = initializeDesa(url, wsUser, wsPswd);
    }

    public DESAAuthenticator() {
        try {
            AppConfiguration appConfig = AppConfigurationFactory.getInstance().defaultInstance();
            DesaServices desaServices = appConfig.getDesaServices();
            List<DesaConfiguration> configurations = desaServices.getConfigurations();
            if (!configurations.isEmpty()) {
                DesaConfiguration desConf = configurations.get(0);
                HashMap<String,String> desaConfig = desConf.toDESAAuthenticationConfig();
                this.port = initializeDesa(desaConfig.get("desa.webservice"), desaConfig.get("desa.user"), desaConfig.get("desa.password"));
            }
        } catch (AppConfigurationException e) {
            LOGGER.log(Level.WARNING,e.getMessage(),e);
        }
        
   }

    boolean authenticateReq(String tUser, String tPass, Integer code) {
        try {
            AuthenticateUserRequest req = new AuthenticateUserRequest();
            req.setLogin(tUser);
            req.setPassword(tPass);
            req.setProducerID(code);
            AuthenticateUserResponse response = port.authenticateUser(req);
            return response.getStatus().equals(OK_STATUS);
        } catch (AuthenticateUserFault e) {
            LOGGER.log(Level.SEVERE, e.getMessage(), e);
            return false;
        }
    }

    @Override
    public boolean authenticate(Map<String, String> loginProperties, HttpServletRequest request, HttpServletResponse response, ProarcPrincipal principal) {
        String user = loginProperties.get(LOGINNAME);
        String pswd = loginProperties.get(PASSWORD);
        String kod = loginProperties.get(KOD_PUVODCE);
        if (isNullString(kod) || isNullString(user) || isNullString(pswd)) {
            return false;
        }
        boolean authenticated = authenticateReq(user, pswd,
                Integer.valueOf(kod));
        if (authenticated) {
            associateUserProfile(principal, user);
        }
        
        return authenticated;
    }

    boolean isNullString(String str) {
        return str == null || str.trim().equals("");
    }
}
