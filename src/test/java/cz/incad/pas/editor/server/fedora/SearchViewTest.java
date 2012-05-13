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
package cz.incad.pas.editor.server.fedora;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraCredentials;
import cz.incad.pas.editor.server.fedora.SearchView.Item;
import java.util.List;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assume;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * @author Jan Pokorsky
 */
public class SearchViewTest {

    private static FedoraClient client;

    public SearchViewTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
        try {
            client = new FedoraClient(new FedoraCredentials("http://localhost:8080/fedora", "fedoraAdmin", "fedoraAdmin"));
            client.getServerVersion();
        } catch (Exception ex) {
//            Logger.getLogger(RemoteStorageTest.class.getName()).log(Level.SEVERE, null, ex);
            Assume.assumeNoException(ex);
        }
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

    @Test
    public void testFindLastCreated() throws Exception {
        // XXX needs some assertions; it tests no exception now
        String user = "";
        SearchView instance = new SearchView(client);
        List<Item> result = instance.findLastCreated(0, user);
        System.out.println(result);
    }
}
