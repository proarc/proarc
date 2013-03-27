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
package cz.incad.pas.editor.server.export;

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
public class Kramerius4ExportOptionsTest {

    public Kramerius4ExportOptionsTest() {
    }

    @Test
    public void testFrom() {
        System.out.println("from");
        Configuration config = new BaseConfiguration();
        String[] excludes = {"ID1", "ID2", "ID3"};
        config.addProperty(Kramerius4ExportOptions.PROP_EXCLUDE_DATASTREAM_ID, excludes);

        config.addProperty(Kramerius4ExportOptions.PROP_RENAME_PREFIX + ".ID1", "NEWID1");
        config.addProperty(Kramerius4ExportOptions.PROP_RENAME_PREFIX + ".ID2", "NEWID2");

        Kramerius4ExportOptions result = Kramerius4ExportOptions.from(config);
        assertEquals(new HashSet<String>(Arrays.asList(excludes)), result.getExcludeDatastreams());
        assertEquals("NEWID1", result.getDsIdMap().get("ID1"));
        assertEquals("NEWID2", result.getDsIdMap().get("ID2"));
    }

}
