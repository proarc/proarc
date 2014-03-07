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

import cz.cas.lib.proarc.authentication.Authenticator.AuthenticatedState;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * This chain collects all implementations of the {@link Authenticator} and tries to authenticate given principal.
 * @author pavels
 */
public class ChainAuthenticator {

    private List<? extends Authenticator> auths = new ArrayList<Authenticator>();

    public ChainAuthenticator(List<? extends Authenticator> auths) {
        super();
        this.auths = auths;
    }

    /**
     * Main point of authentication. 
     * 
     * Method iterates over {@link Authenticator} and tries to authenticate given principal. 
     * 
     * @param loginProperties Login properties map
     * @param request TODO
     * @param response TODO
     * @param proarcPrincipal TODO
     * @return
     */
    public boolean authenticate(Map<String, String> loginProperties, HttpServletRequest request, HttpServletResponse response, ProarcPrincipal proarcPrincipal) {
        for (Authenticator auth : this.auths) {
            AuthenticatedState authState = auth.authenticate(loginProperties, request, response, proarcPrincipal);
            switch(authState) {
                    case AUTHENTICATED: return true;
                    case FORBIDDEN: return false;
                    case IGNORED: break; // ignored -> the next authenticator should continue
            }
        }
        return false;
    }
}
