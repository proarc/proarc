/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.urnnbn;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 *
 * @author Jan Pokorsky
 */
public class ResolverClientTest {

    private ResolverClient client;

    public ResolverClientTest() {
    }

    @BeforeAll
    public static void setUpClass() {
    }

    @AfterAll
    public static void tearDownClass() {
    }

    @BeforeEach
    public void setUp() {
        String serviceUrl = System.getProperty("proarc-urnnbn.ResolverClient.url");
        String user = System.getProperty("proarc-urnnbn.ResolverClient.user");
        String passwd = System.getProperty("proarc-urnnbn.ResolverClient.passwd");
        String registrar = "";
        assertNotNull(serviceUrl);
        assertNotNull(user);
        assertNotNull(passwd);
        client = new ResolverClient(serviceUrl, registrar, null, user, passwd);
    }

    @AfterEach
    public void tearDown() {
    }

    @Test
    public void testGetDigitalInstances() {
        client.setDebug(true);
        String response = client.getDigitalInstances();
        assertNotNull(response);
    }

}