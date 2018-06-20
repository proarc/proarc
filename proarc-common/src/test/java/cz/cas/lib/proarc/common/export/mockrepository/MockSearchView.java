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

import cz.cas.lib.proarc.common.fedora.SearchView;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import mockit.Mock;
import mockit.MockUp;

public class MockSearchView extends MockUp<SearchView> {
    @Mock
    List<SearchView.Item> findReferrers(String pid) {
        //child (has) parent
        Map<String, String> relations = new HashMap<>();
        relations.put("uuid:b0ebac65-e9fe-417d-a71b-58e74fe707a4", "uuid:26342028-12c8-4446-9217-d3c9f249bd13");
        relations.put("uuid:d5d5e950-3668-4458-8fdb-aeb7028f4fcc", "uuid:b0ebac65-e9fe-417d-a71b-58e74fe707a4");
        relations.put("uuid:26342028-12c8-4446-9217-d3c9f249bd13", null);
        relations.put("uuid:acd66301-4e75-4d12-9d98-b323ff5beee9", null);

        if (relations.containsKey(pid)) {
            return Collections.singletonList(new SearchView.Item(relations.get(pid)));
        } else {
            throw new IllegalArgumentException("Unknown parent for " + pid + " (fake risearch)");
        }
    }

}
