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

import cz.cas.lib.proarc.common.user.UserManager;
import cz.cas.lib.proarc.common.user.UserProfile;
import cz.cas.lib.proarc.common.user.UserUtil;

public abstract  class AbstractAuthenticator implements Authenticator {

    // XXX move to ProArcAuthenticator; null profile cannot occur!
    public void associateUserProfile(ProarcPrincipal principal, String user) {
        UserManager userManager = UserUtil.getDefaultManger();
        // NOTE: UserProfile.validateAsNew(UserProfile.java:197) only lower
        // case supports but ws
        String proarcValidUserName = user;
        UserProfile userProfile = userManager.find(proarcValidUserName);
//        if (userProfile == null) {
//            userProfile = new UserProfile();
//            // not proarc user
//            userProfile.setProarcuser(false);
//            userProfile.setUserName(proarcValidUserName);
//            userProfile.setCreated(new Date());
//            userProfile.setDisplayName(user);
//            userProfile.setForename(user);
//            userProfile.setSurname("-ws user-");
//            userManager.add(userProfile);
//        }
        if (userProfile != null) {
            principal.associateUserProfile(userProfile);
        }
    }

}
