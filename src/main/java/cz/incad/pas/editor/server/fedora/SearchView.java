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
import com.yourmediashelf.fedora.client.FedoraClientException;
import com.yourmediashelf.fedora.client.request.RiSearch;
import com.yourmediashelf.fedora.client.response.FindObjectsResponse;
import com.yourmediashelf.fedora.client.response.RiSearchResponse;
import cz.incad.pas.editor.server.fedora.relation.RelationResource;
import cz.incad.pas.editor.server.json.JsonUtils;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Scanner;
import java.util.logging.Logger;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import org.codehaus.jackson.map.ObjectMapper;

/**
 *
 * @author Jan Pokorsky
 */
public final class SearchView {

    private static final Logger LOG = Logger.getLogger(SearchView.class.getName());

    private static final String QUERY_LAST_CREATED = readQuery("lastCreated.itql");
    private static final String QUERY_FIND_MEMBERS = readQuery("findMembers.itql");
    private static final String QUERY_FIND_PIDS = readQuery("findPids.itql");

    private final FedoraClient fedora;
    private final int maxLimit;

    SearchView(FedoraClient fedora) {
        this(fedora, 100);
    }

    SearchView(FedoraClient fedora, int maxLimit) {
        this.fedora = fedora;
        this.maxLimit = maxLimit;
    }

    public void findQuery(String query) throws FedoraClientException {
        FindObjectsResponse response = FedoraClient.findObjects().query(query).resultFormat("xml")
                .pid().title().label().identifier().cDate().mDate().type().state()
                .execute(null);
        response.getExpirationDate();
        List<String> pids = response.getPids();
        throw new UnsupportedOperationException();
    }

    public List<Item> find(String... pids) throws FedoraClientException, IOException {
        StringBuilder expr = new StringBuilder(256);
        for (int i = 0; i < pids.length; i++) {
            if (i > 0) {
                expr.append("\n  or ");
            }
            expr.append(String.format(
                    "$pid <http://mulgara.org/mulgara#is> <info:fedora/%s>",
                    pids[i]));
        }
        String query = QUERY_FIND_PIDS.replace("${pids.expression}", expr);
        LOG.info(query);
        RiSearch search = buildSearch(query);
        return consumeSearch(search.execute(fedora));
    }

    public List<Item> findChildren(String pid) throws FedoraClientException, IOException {
        String query = QUERY_FIND_MEMBERS.replace("${parent}", RelationResource.fromPid(pid).getResource());
        RiSearch search = buildSearch(query);
        return consumeSearch(search.execute(fedora));
    }

    public List<Item> findLastCreated(int offset, String user) throws FedoraClientException, IOException {
        return findLastCreated(offset, user, 100);
    }
    
    public List<Item> findLastCreated(int offset, String user, int limit) throws FedoraClientException, IOException {
        String query = QUERY_LAST_CREATED.replace("${offset}", String.valueOf(offset));
        RiSearch search = buildSearch(query);

        if (limit > 0) {
            limit = Math.min(limit, maxLimit);
            search.limit(limit);
        }
        return consumeSearch(search.execute(fedora));
    }

    private List<Item> consumeSearch(RiSearchResponse response) throws IOException {
        String json = response.getEntity(String.class);
        ObjectMapper om = JsonUtils.defaultObjectMapper();
        Result result = om.readValue(json, Result.class);
        return replaceUriWithPid(result.results);
    }
    
    private static List<Item> replaceUriWithPid(List<Item> items) {
        for (Item item : items) {
            item.pid = RelationResource.toPid(item.pid);
            item.model = RelationResource.toPid(item.model);
            item.state = RelationResource.toPid(item.state);
        }
        return items;
    }

    private static RiSearch buildSearch(String query) {
        RiSearch search = FedoraClient.riSearch(query).distinct(true)
                .type("tuples").lang("itql")
//                .format("json")
                .xParam("format", "json");
        return search;
    }

    private static String readQuery(String file) {
        InputStream is = SearchView.class.getResourceAsStream(file);
        if (is == null) {
            throw new IllegalStateException(file + " not found!");
        }
        Scanner scanner = new Scanner(is, "UTF-8").useDelimiter("\\A");
        String content = scanner.next();
        scanner.close();
        return content;
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Item {

        private String pid;
        private String model;
        private String owner;
        private String label;
        private String state;
        private String created;
        private String modified;

        public String getCreated() {
            return created;
        }

        public void setCreated(String created) {
            this.created = created;
        }

        public String getLabel() {
            return label;
        }

        public void setLabel(String label) {
            this.label = label;
        }

        public String getModel() {
            return model;
        }

        public void setModel(String model) {
            this.model = model;
        }

        public String getModified() {
            return modified;
        }

        public void setModified(String modified) {
            this.modified = modified;
        }

        public String getOwner() {
            return owner;
        }

        public void setOwner(String owner) {
            this.owner = owner;
        }

        public String getPid() {
            return pid;
        }

        public void setPid(String pid) {
            this.pid = pid;
        }

        public String getState() {
            return state;
        }

        public void setState(String state) {
            this.state = state;
        }
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    static class Result {

        private List<Item> results;

    }

}
