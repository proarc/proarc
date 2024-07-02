/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.common.dao;

import cz.cas.lib.proarc.common.user.UserProfile;
import java.util.List;

/**
 * DAO for {@link UserProfile}.
 *
 * @author Jan Pokorsky
 */
public interface UserDao extends Dao {

    UserProfile find(int userId);

    List<UserProfile> find(String userName, String passwd, String remoteName, String remoteType, String organization);

    UserProfile create();

    void update(UserProfile user) throws ConcurrentModificationException;

    void delete(Integer userId);
}
