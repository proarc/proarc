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
package cz.cas.lib.proarc.common.fedora;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.FedoraCredentials;
import com.yourmediashelf.fedora.client.response.FindObjectsResponse;
import com.yourmediashelf.fedora.generated.foxml.DigitalObject;
import cz.cas.lib.proarc.common.fedora.LocalStorage.LocalObject;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import javax.xml.transform.stream.StreamSource;
import static org.junit.Assert.*;
import org.junit.Assume;

/**
 *
 * @author Jan Pokorsky
 */
public class FedoraTestSupport {

    private FedoraClient client;
    private RemoteStorage storage;

    /**
     * Skips unit test in case the fedora is unreachable.
     * @throws Exception
     */
    public FedoraTestSupport() {
        client = fedoraClientSetup();
        storage = new RemoteStorage(client);
        RemoteStorage.setInstance(storage);
    }

    private static FedoraClient fedoraClientSetup() {
        String user = System.getProperty("proarc-common.FedoraTestSupport.user");
        String passwd = System.getProperty("proarc-common.FedoraTestSupport.passwd");
        String url = System.getProperty("proarc-common.FedoraTestSupport.url");
        Assume.assumeNotNull(url, user, passwd);
        FedoraClient client = null;
        try {
            client = new FedoraClient(new FedoraCredentials(url, user, passwd));
            client.getServerVersion();
        } catch (Exception ex) {
            Assume.assumeNoException(ex);
        }
        return client;
    }

    public FedoraClient getClient() {
        return client;
    }

    public RemoteStorage getRemoteStorage() {
        return storage;
    }

    public void ingest(URL... foxml) throws Exception {
        for (URL u : foxml) {
            ingestFromUrl(u);
        }
    }

    public String getTestUser() {
        return "junit";
    }

    private void ingestFromUrl(URL foxml) throws Exception {
        assertNotNull(foxml);
        DigitalObject dobj = FoxmlUtils.unmarshal(new StreamSource(foxml.toExternalForm()), DigitalObject.class);
        LocalObject object = new LocalStorage().create(dobj);
        storage.ingest(object, "junit");
    }

    /**
     * Prunes all fedora objects with owner 'junit'
     */
    public void cleanUp() throws Exception {
//        client.debug(true);
        FindObjectsResponse response = FedoraClient.findObjects()
                .pid().query("ownerId='junit'")
                .maxResults(5000)
                .execute(client);
//        String cursor = response.getCursor();
//        String expirationDate = response.getExpirationDate();
//        List<String> pids = response.getPids();
//        String token = response.getToken();
//        System.out.printf("cursor: %s, expiration: %s, token: %s,\n size: %s, pids: %s\n",
//                cursor, expirationDate, token, pids.size(), pids);

        int count = 0;
        while (true) {
            count += cleanUp(response);
            if (!response.hasNext()) {
                break;
            }
            response = FedoraClient.findObjects().sessionToken(response.getToken()).execute(client);
        }
        System.out.println("purged: " + count + " objects");
    }

    private int cleanUp(FindObjectsResponse response) throws Exception {
        List<String> pids = response.getPids();
        for (String pid : pids) {
            FedoraClient.purgeObject(pid).logMessage("junit cleanup").execute(client);
        }
        return pids.size();
    }

    public static void assertItem(List<Item> items, String... pids) {
        assertItem(items, Arrays.asList(pids));
    }
    
    public static void assertItem(List<Item> items, List<String> pids) {
        for (String pid : pids) {
            assertNotNull(pid, find(items, pid));
        }
    }

    public static void assertNoItem(List<Item> items, String... pids) {
        assertNoItem(items, Arrays.asList(pids));
    }

    public static void assertNoItem(List<Item> items, List<String> pids) {
        for (String pid : pids) {
            assertNull(pid, find(items, pid));
        }
    }

    static Item find(List<Item> items, String pid) {
        for (Item item : items) {
            if (pid.equals(item.getPid())) {
                return item;
            }
        }
        return null;
    }

}
