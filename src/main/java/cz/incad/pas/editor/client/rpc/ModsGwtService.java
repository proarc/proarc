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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.client.rpc;

import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import cz.fi.muni.xkremser.editor.client.mods.ModsCollectionClient;

/**
  * The service allowing to persist MODS data.
 *
 * @author Jan Pokorsky
 */
@RemoteServiceRelativePath("modsgwt")
public interface ModsGwtService extends RemoteService {

    /**
     * Reads data from storage.
     *
     * @param pid an identifier to lookup MODS.
     * @return MODS
     */
    ModsGwtRecord read(String pid);

    /**
     * Writes data to storage.
     *
     * @param pid an identifier of passed MODS data or {@code null} in case of a new record.
     * @param mods MODS data
     * @return the identifier of persisted data
     */
    String write(String pid, ModsGwtRecord mods);
    
    String getXml(ModsCollectionClient modsCollection);

}
