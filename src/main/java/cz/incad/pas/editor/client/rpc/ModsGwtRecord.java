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

import com.google.gwt.user.client.rpc.IsSerializable;
import cz.fi.muni.xkremser.editor.client.mods.ModsCollectionClient;

/**
 * MODS transport object.
 *
 * @author Jan Pokorsky
 */
public class ModsGwtRecord implements IsSerializable {

    private ModsCollectionClient mods;
    private long timestamp;
    private int xmlHash;

    private ModsGwtRecord() {
    }

    public ModsGwtRecord(ModsCollectionClient mods, long timestamp, int xmlHash) {
        this.mods = mods;
        this.timestamp = timestamp;
        this.xmlHash = xmlHash;
    }

    public ModsCollectionClient getMods() {
        return mods;
    }

    public void setMods(ModsCollectionClient mods) {
        this.mods = mods;
    }

    /**
     * Gets time of the last persisted modification.
     * @return time stamp
     */
    public long getTimestamp() {
        return timestamp;
    }

    /**
     * Gets hash code of origin XML that is parsed as {@link ModsCollectionClient}.
     * It is useful to detect modifications in model as ModsCollectionClient does not
     * support such detection.
     *
     * @return hash code
     */
    public int getXmlHash() {
        return xmlHash;
    }

}
