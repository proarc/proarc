/*
 * Copyright (C) 2018 Martin Rumanek
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

package cz.cas.lib.proarc.common.export.mockrepository;

import com.yourmediashelf.fedora.client.FedoraClient;
import com.yourmediashelf.fedora.client.request.GetDatastreamDissemination;
import com.yourmediashelf.fedora.client.response.FedoraResponse;
import cz.cas.lib.proarc.common.fedora.SearchView;
import cz.cas.lib.proarc.common.xml.SimpleNamespaceContext;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;
import mockit.Mock;
import mockit.MockUp;
import mockit.Mocked;
import org.apache.commons.lang.StringUtils;
import org.w3c.dom.Document;

public class MockSearchView extends MockUp<SearchView> {

    @Mocked
    FedoraClient client;

    @Mock
    List<SearchView.Item> findReferrers(String pid) {
        //child (has) parent
        Map<String, String> relations = new HashMap<>();
        relations.put("uuid:b0ebac65-e9fe-417d-a71b-58e74fe707a4", "uuid:26342028-12c8-4446-9217-d3c9f249bd13");
        relations.put("uuid:d5d5e950-3668-4458-8fdb-aeb7028f4fcc", "uuid:b0ebac65-e9fe-417d-a71b-58e74fe707a4");
        relations.put("uuid:26342028-12c8-4446-9217-d3c9f249bd13", null);
        relations.put("uuid:acd66301-4e75-4d12-9d98-b323ff5beee9", null);

        if (relations.containsKey(pid)) {
            return relations.get(pid) == null ? Collections.EMPTY_LIST :Collections.singletonList(new SearchView.Item(relations.get(pid)));
        } else {
            throw new IllegalArgumentException("Unknown parent for " + pid + " (fake risearch)");
        }
    }

    @Mock
    public List<SearchView.Item> find(String... pids) {
        return Arrays.stream(pids).map(pid -> {
            SearchView.Item item = new SearchView.Item(pid);

            new MockFedoraClient();
            GetDatastreamDissemination datastream = FedoraClient.getDatastreamDissemination(pid, "RELS-EXT");
            try {
                FedoraResponse response = datastream.execute(client);
                //IOUtils.toString(response.getEntityInputStream());
                DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                factory.setNamespaceAware(true);
                DocumentBuilder builder = factory.newDocumentBuilder();
                Document document = builder.parse(response.getEntityInputStream());
                XPath xPath = XPathFactory.newInstance().newXPath();
                SimpleNamespaceContext namespaces = new SimpleNamespaceContext().add("fedora-model", "info:fedora/fedora-system:def/model#")
                        .add("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#");
                xPath.setNamespaceContext(namespaces);
                String value  = (String) xPath.compile("//fedora-model:hasModel/@rdf:resource").evaluate(document, XPathConstants.STRING);
                item.setModel(StringUtils.removeStart(value, "info:fedora/"));
            } catch (Exception e) {
                e.printStackTrace();
            }

            return item;
        }).collect(Collectors.toList());
    }

    @Mock
    public List<SearchView.Item> findSortedChildren(String parentPid) {
        return find(parentPid);
    }





}
