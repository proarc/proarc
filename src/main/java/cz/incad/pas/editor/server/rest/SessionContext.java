/*
 * Copyright (C) 2012 Jan Pokorsky
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.server.rest;

import cz.cas.lib.proarc.common.user.UserProfile;
import java.security.Principal;
import java.util.List;
import java.util.Locale;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.core.HttpHeaders;

/**
 * Information about current request session.
 * 
 * @author Jan Pokorsky
 */
public final class SessionContext {

    private String user;
    private String ip;

    public SessionContext(String user, String ip) {
        this.user = user;
        this.ip = ip;
    }

    public static SessionContext from(HttpServletRequest request) {
        Principal userPrincipal = request.getUserPrincipal();
        String name = userPrincipal.getName();
        String remoteAddr = request.getRemoteAddr();
        return new SessionContext(name, remoteAddr);
    }

    public static SessionContext from(UserProfile user, String clientIp) {
        return new SessionContext(user.getUserName(), clientIp);
    }

    /**
     * Gets session context as Fedora log in JSON format.
     * @return
     */
    public String asFedoraLog() {
        // use Jackson for complex JSON; String.format is enough for now
        return String.format("{\"proarc\":{\"user\":\"%s\",\"ip\":\"%s\"}}", user, ip);
    }

    public Locale getLocale(HttpHeaders httpHeaders) {
        List<Locale> acceptableLanguages = httpHeaders.getAcceptableLanguages();
        Locale locale = acceptableLanguages.isEmpty() ? Locale.ENGLISH : acceptableLanguages.get(0);
        return locale;
    }
}
