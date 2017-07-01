/*
 * Copyright (C) 2017 Lukas Sykora
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
package cz.cas.lib.proarc.common.export.mets;

import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/**
 * Created by lsykora on 6/22/2017.
 */
public class NdkExportOptionsTest {

    public NdkExportOptionsTest() {
    }

    @Test
    public void testGetOptions() {
        Configuration config = new BaseConfiguration();

        String creator = "KNAV";
        config.addProperty(NdkExportOptions.PROP_NDK_AGENT_CREATOR, creator);

        String archivist = "ProArc";
        config.addProperty(NdkExportOptions.PROP_NDK_AGENT_ARCHIVIST, archivist);

        NdkExportOptions result = NdkExportOptions.getOptions(config);

        assertEquals("creator", creator, result.getCreator());
        assertEquals("archivist", archivist, result.getArchivist());
    }

    @Test
    public void testFromEmpyConfig() {
        Configuration config = new BaseConfiguration();
        NdkExportOptions result = NdkExportOptions.getOptions(config);
        assertNull("The default creator must be empty", result.getCreator());
        assertNull("The default archivist must be empty", result.getArchivist());
    }

}
