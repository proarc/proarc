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
package cz.cas.lib.proarc.authentication.proarc;

import cz.cas.lib.proarc.authentication.Authenticator;
import cz.cas.lib.proarc.authentication.ProarcPrincipal;
import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


/**
 * ProArc authentication
 * @author pavels
 */
public class ProArcAuthenticator implements Authenticator  {

    public static final Logger LOGGER = Logger
            .getLogger(ProArcAuthenticator.class.getName());

    @Override
    public AuthenticatedState authenticate(Map<String, String> loginProperties,
            HttpServletRequest request, HttpServletResponse response, ProarcPrincipal principal) {

        String login = loginProperties.get(Authenticator.LOGINNAME);
        String passwd = loginProperties.get(Authenticator.PASSWORD);
        try {
            UserManager userManger = UserUtil.getDefaultManger();
            UserProfile authenticated = userManger.authenticate(login, passwd);
            if (authenticated != null) {
                principal.associateUserProfile(authenticated);
                return AuthenticatedState.AUTHENTICATED;
            } else {
                return AuthenticatedState.FORBIDDEN;
            }
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, login, e);
            return AuthenticatedState.FORBIDDEN;
        }
    }

}
