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

import cz.cas.lib.proarc.common.user.Group;
import java.util.List;

/**
 * DAO for {@link Group}.
 *
 * @author Jan Pokorsky
 */
public interface GroupDao extends Dao {

    Group create();

    List<Group> find(Integer userId, String grpName, String grpRemoteName, String grpRemoteType);

    Group find(int id);

    void update(Group group) throws ConcurrentModificationException;

}
