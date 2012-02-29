/*
 * Copyright (C) 2011 Jan Pokorsky
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
package cz.incad.pas.editor.server.user;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Jan Pokorsky
 */
final class UserManagerMemoryImpl implements UserManager {

    static final UserManagerMemoryImpl INSTANCE = new UserManagerMemoryImpl();
    /** memory storage for now */
    private final Map<String, UserProfile> map = new HashMap<String, UserProfile>();

    private UserManagerMemoryImpl() {
//        map.put("datel", new UserProfile(1, "/mnt/share/tmp/kramerius.lib.cas.cz/", "datel", "Datel Urputný"));
        map.put("datel", new UserProfile(1, "/mnt/share/tmp/kramerius.lib.cas.cz/incad.demo.content/", "datel", "Datel Urputný"));
//        map.put("datel", new UserProfile(1, "/fast/paseditor/import/", "datel", "Datel Urputný"));
    }

    @Override
    public Collection<UserProfile> findAll() {
        return map.values();
    }

    @Override
    public UserProfile find(String userName) throws IllegalArgumentException {
        UserProfile up;
        if (userName != null) {
            synchronized (map) {
                up = map.get(userName);
            }
            if (up != null) {
                return up;
            }
        }
        throw new IllegalArgumentException("User not found.");
    }

    @Override
    public UserProfile find(int userId) throws IllegalArgumentException {
        synchronized (map) {
            for (UserProfile up : map.values()) {
                if (up.getId() == userId) {
                    return up;
                }
            }
        }
        throw new IllegalArgumentException("User not found.");
    }

}
