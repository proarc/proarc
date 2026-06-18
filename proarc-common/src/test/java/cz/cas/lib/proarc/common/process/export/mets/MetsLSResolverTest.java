/*
 * Copyright (C) 2014 Robert Simonovsky
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

package cz.cas.lib.proarc.common.process.export.mets;

import java.io.InputStream;
import org.junit.jupiter.api.Test;
import org.w3c.dom.ls.LSInput;

import static org.junit.jupiter.api.Assertions.assertNotNull;

public class MetsLSResolverTest {

    @Test
    public void checkLSResolvers() {
        MetsLSResolver metsLSResolver = MetsLSResolver.getInstance();
        for (String resource : MetsLSResolver.URL_MAP.keySet()) {
            InputStream is = metsLSResolver.getClass().getResourceAsStream(MetsLSResolver.URL_MAP.get(resource));
            assertNotNull(is);
        }
    }

    @Test
    public void testResolveResource() throws Exception {
        MetsLSResolver resolver = MetsLSResolver.getInstance();
        LSInput input = resolver.resolveResource(null, null, null, "http://www.openarchives.org/OAI/2.0/oai_dc.xsd", null);
        assertNotNull(input);
        InputStream stream = input.getByteStream();
        assertNotNull(stream);
        stream.close();
    }
}
