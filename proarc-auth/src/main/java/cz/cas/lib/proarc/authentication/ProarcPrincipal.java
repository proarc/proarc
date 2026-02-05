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

import java.security.Principal;

import cz.cas.lib.proarc.common.user.UserProfile;

/**
 * Basic ProArc principal
 *
 * @author pavels
 */
public class ProarcPrincipal implements Principal {

    private String uname;
    private UserProfile associatedUserProfile;

    public ProarcPrincipal(String uname) {
        super();
        this.uname = uname;
    }

    @Override
    public String getName() {
        return this.uname;
    }

    @Override
    public String toString() {
        return "ProarcPrincipal{" + "uname=" + uname + ", associatedUserProfile=" + associatedUserProfile + '}';
    }

    /**
     * Returns associated user profile
     *
     * @return
     */
    public UserProfile getAssociatedUserProfile() {
        return this.associatedUserProfile;
    }

    /**
     * Associate user profile
     *
     * @param uProfile
     */
    public void associateUserProfile(UserProfile uProfile) {
        this.associatedUserProfile = uProfile;
        this.uname = uProfile.getUserName();
    }

    /**
     * Break user's profile association
     */
    public void disassociateUserProfile() {
        this.associatedUserProfile = null;
    }
}
