/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.server.rest;

import java.util.Collections;
import java.util.Map;
import javax.ws.rs.core.MediaType;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Jan Pokorsky
 */
public class SmartGwtResponseTest {

    private static ObjectMapper mapper;

    public SmartGwtResponseTest() {
    }

    @BeforeClass
    public static void setUpClass() {
        mapper = new JacksonProvider().locateMapper(SmartGwtResponse.class, MediaType.APPLICATION_JSON_TYPE);
        mapper.configure(DeserializationConfig.Feature.UNWRAP_ROOT_VALUE, true);
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testMapData() throws Exception {
        SmartGwtResponse<Map<String, String>> response = new SmartGwtResponse<Map<String,String>>(Collections.singletonMap("key", "value"));
        String json = mapper.writeValueAsString(response);
//        System.out.println(json);
        assertEquals(("{'response':{'status':0,'startRow':0,'endRow':0,'totalRows':1,"
                + "'data':[{'key':'value'}]}}").replaceAll("'", "\""), json);
    }

    @Test
    public void testValidationError() throws Exception {
        SmartGwtResponse<Long> response = SmartGwtResponse.<Long>asError()
                .error("field1", "ERROR_F1_1")
                .error("field1", "ERROR_F1_2")
                .error("field2", "ERROR_F2")
                .build();
        String json = mapper.writeValueAsString(response);
//        System.out.println(json);
        assertEquals(("{'response':{'status':-4,'errors':{"
                + "'field1':[{'errorMessage':'ERROR_F1_1'},{'errorMessage':'ERROR_F1_2'}],"
                + "'field2':[{'errorMessage':'ERROR_F2'}]"
                + "}}}").replaceAll("'", "\""), json);
    }

    @Test
    public void testError() throws Exception {
        SmartGwtResponse<Long> response = SmartGwtResponse.asError("ERROR");
        String json = mapper.writeValueAsString(response);
//        System.out.println(json);
        assertEquals("{'response':{'status':-1,'data':'ERROR'}}".replaceAll("'", "\""), json);
    }

    @Test
    public void testExceptionError() throws Exception {
        final IllegalStateException ex = new IllegalStateException("exMsg");
        SmartGwtResponse<Long> response = SmartGwtResponse.asError("ERROR", ex);
        String json = mapper.writeValueAsString(response);
//        System.out.println(json);
        String error = response.getDataAsError();
        assertTrue(error, error.startsWith("ERROR\n\n" + ex.getClass().getName()));
    }

}
