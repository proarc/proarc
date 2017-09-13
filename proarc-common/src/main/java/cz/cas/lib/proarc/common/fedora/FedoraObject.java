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
package cz.cas.lib.proarc.common.fedora;

import com.yourmediashelf.fedora.generated.management.DatastreamProfile;
import java.util.List;

/**
 *
 * @author Jan Pokorsky
 */
public interface FedoraObject {

    String getPid();

    /**
     * Gets available datastream profiles.
     * @param dsId datastream ID. {@code null} stands for all streams.
     * @return the list of profiles
     * @throws DigitalObjectException failure
     */
    List<DatastreamProfile> getStreamProfile(String dsId) throws DigitalObjectException;

    XmlStreamEditor getEditor(DatastreamProfile datastream);
    
    void register(XmlStreamEditor editor);
    
    void setLabel(String label);

    void flush() throws DigitalObjectException;

    String asText() throws DigitalObjectException;

    void purgeDatastream(String datastream, String logMessage) throws DigitalObjectException;

}
