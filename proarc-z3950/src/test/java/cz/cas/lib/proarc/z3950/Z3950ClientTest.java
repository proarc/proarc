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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.z3950;

import java.io.StringWriter;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Assume;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.core.JdkVersion;
import org.w3c.dom.Document;

/**
 * See src/test/resources/log4j.properties to configure jzkit logging
 *
 * @author Jan Pokorsky
 */
public class Z3950ClientTest {
    private String host;
    private String port;
    private String base;

    public Z3950ClientTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() throws Exception {
        host = System.getProperty("Z3950ClientTest.host");
        port = System.getProperty("Z3950ClientTest.port");
        base = System.getProperty("Z3950ClientTest.base");
        Assume.assumeNotNull(host, port, base);
        assertEquals(JdkVersion.JAVA_17, JdkVersion.getMajorJavaVersion());
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testSearch() throws Exception {
        String query = System.getProperty("Z3950ClientTest.testSearch.query");
        Assume.assumeNotNull(query);
        Z3950Client client = new Z3950Client(host, Integer.parseInt(port), base);
        try {
            Iterable<byte[]> search = client.search(query);
            assertNotNull(search);
            assertTrue(search.iterator().hasNext());
            Transformer t = TransformerFactory.newInstance().newTransformer();
            for (byte[] content : search) {
                Document marcxml = Z3950Client.toMarcXml(content, "cp1250");
                StringWriter dump = new StringWriter();
                t.transform(new DOMSource(marcxml), new StreamResult(dump));
                System.out.println("MarcXML:\n" + dump);
//                LOG.info("MarcXML:\n" + dump);
            }
        } finally {
            client.close();
        }

    }

}
