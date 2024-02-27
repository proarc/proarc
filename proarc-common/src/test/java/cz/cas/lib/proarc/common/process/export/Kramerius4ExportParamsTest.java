/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.common.process.export;

import cz.cas.lib.proarc.common.storage.BinaryEditor;
import java.util.Arrays;
import java.util.HashSet;
import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class Kramerius4ExportParamsTest {

    public Kramerius4ExportParamsTest() {
    }

    @Test
    public void testFrom() {
        Configuration config = new BaseConfiguration();
        String[] excludes = {"ID1", "ID2", "ID3"};
        config.addProperty(Kramerius4ExportOptions.PROP_EXCLUDE_DATASTREAM_ID, excludes);

        config.addProperty(Kramerius4ExportOptions.PROP_RENAME_PREFIX + ".ID1", "NEWID1");
        config.addProperty(Kramerius4ExportOptions.PROP_RENAME_PREFIX + ".ID2", "NEWID2");

        String policy = "policy:public";
        config.addProperty(Kramerius4ExportOptions.PROP_POLICY, policy);

        Kramerius4ExportOptions result = Kramerius4ExportOptions.from(config);
        assertEquals(new HashSet<String>(Arrays.asList(excludes)), result.getExcludeDatastreams());
        assertEquals("NEWID1", result.getDsIdMap().get("ID1"));
        assertEquals("NEWID2", result.getDsIdMap().get("ID2"));
        assertEquals(policy, result.getPolicy());
    }

    @Test
    public void testFromEmpyConfig() {
        Configuration config = new BaseConfiguration();
        Kramerius4ExportOptions result = Kramerius4ExportOptions.from(config);
        assertTrue(result.getExcludeDatastreams().isEmpty());
        assertEquals(1, result.getDsIdMap().size());
        assertEquals("IMG_FULL", result.getDsIdMap().get(BinaryEditor.RAW_ID));
        assertNull(result.getPolicy());
    }
}
