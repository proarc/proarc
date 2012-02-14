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
package cz.incad.pas.editor.server.xml;

import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.transform.stream.StreamSource;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class TransformersTest {

    private static final Logger LOG = Logger.getLogger(TransformersTest.class.getName());

    public TransformersTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

//    @Test
//    public void testMarcAsMods2() throws Exception {
//        InputStream xmlIS = TransformersTest.class.getResourceAsStream("oaiMarcSample.xml");
//        assertNotNull(xmlIS);
//        StreamSource streamSource = new StreamSource(xmlIS);
//        Transformers mt = new Transformers();
//
//        try {
//            Source marcxml = mt.transform(streamSource, Transformers.Format.OaimarcAsMarc21slim);
//            assertNotNull(marcxml);
//            marcxml = mt.dump2Temp(marcxml, "testMarcAsMods2.xml");
//            byte[] contents = mt.transformAsBytes(marcxml, Format.MarcxmlAsMods34);
//            System.out.println(new String(contents, "UTF-8"));
//        } finally {
//            try {
//                xmlIS.close();
//            } catch (IOException ex) {
//                LOG.log(Level.SEVERE, null, ex);
//            }
//        }
//    }
    
    @Test
    public void testMarcAsMods() throws Exception {
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("marcxmlSample.xml");// from test
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers();

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.MarcxmlAsMods34);
            assertNotNull(contents);
            System.out.println(new String(contents, "UTF-8"));
        } finally {
            try {
                xmlIS.close();
            } catch (IOException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        }
    }

    @Test
    public void testOaiMarcAsMarc() throws Exception {
        InputStream xmlIS = TransformersTest.class.getResourceAsStream("oaiMarcSample.xml");
        assertNotNull(xmlIS);
        StreamSource streamSource = new StreamSource(xmlIS);
        Transformers mt = new Transformers();

        try {
            byte[] contents = mt.transformAsBytes(streamSource, Transformers.Format.OaimarcAsMarc21slim);
            assertNotNull(contents);
            System.out.println(new String(contents, "UTF-8"));
        } finally {
            try {
                xmlIS.close();
            } catch (IOException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        }
    }

}
