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
package cz.cas.lib.proarc.common.mods.custom;

import static org.junit.Assert.assertArrayEquals;

import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cas.lib.proarc.common.mods.ModsUtils;
import cz.cas.lib.proarc.common.mods.custom.NameMapper.NameItem;
import cz.cas.lib.proarc.common.mods.custom.NameMapper.NameItem.NameRole;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;

/**
 * 
 * @author Jan Pokorsky
 */
public class NameMapperTest {

    public NameMapperTest() {
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

    // @Test
    public void testRead() throws Exception {
        ModsType mods = ModsUtils.unmarshal(NameMapperTest.class.getResource("monograph_mods.xml"), ModsType.class);
        NameItem[] expectedAll = { new NameItem(0, "FamilyAuthor1", "GivenAuthor1", NameRole.AUTHOR), new NameItem(1, "FamilyContributor1", "GivenContributor1", NameRole.CONTRIBUTOR), new NameItem(2, "FamilyAuthor2", "GivenAuthor2", NameRole.AUTHOR), new NameItem(3, "FamilyXXX", "GivenXXX", NameRole.OTHER), new NameItem(4, "FamilyContributor2", "GivenContributor2", NameRole.CONTRIBUTOR), };
        NameItem[] expectedAuthors = { expectedAll[0], expectedAll[2], };
        NameItem[] expectedContributors = { expectedAll[1], expectedAll[4], };
        NameItem[] expectedOthers = { expectedAll[3], };

        NameMapper instance = new NameMapper();
        List<NameItem> all = instance.map(mods);
        assertArrayEquals(expectedAll, all.toArray());
        List<NameItem> authors = NameMapper.filter(all, true, NameRole.AUTHOR);
        assertArrayEquals(expectedAuthors, authors.toArray());
        List<NameItem> contributors = NameMapper.filter(all, true, NameRole.CONTRIBUTOR);
        assertArrayEquals(expectedContributors, contributors.toArray());
        List<NameItem> others = NameMapper.filter(all, false, NameRole.AUTHOR, NameRole.CONTRIBUTOR);
        assertArrayEquals(expectedOthers, others.toArray());
    }

    @Test
    public void testWriteUpdate() throws Exception {
        ModsType mods = ModsUtils.unmarshal(NameMapperTest.class.getResource("monograph_mods.xml"), ModsType.class);
        MapperUtils.normalize(mods);
        NameItem[] expectedAll = { new NameItem(0, "FamilyAuthor1", "GivenAuthor1", NameRole.AUTHOR), new NameItem(1, "FamilyContributor1", "GivenContributor1", NameRole.CONTRIBUTOR), new NameItem(2, "FamilyAuthor2", "GivenAuthor2", NameRole.AUTHOR), new NameItem(3, "FamilyXXX", "GivenXXX", NameRole.OTHER), new NameItem(4, "FamilyContributor2", "GivenContributor2", NameRole.CONTRIBUTOR), };
        NameItem[] expectedAuthors = { expectedAll[0], expectedAll[2], };
        NameItem[] expectedContributors = { expectedAll[1], expectedAll[4], };
        NameItem[] expectedOthers = { expectedAll[3], };

        NameMapper instance = new NameMapper();
        // read
        List<NameItem> all = instance.map(mods);
        assertArrayEquals(expectedAll, all.toArray());
        List<NameItem> authors = NameMapper.filter(all, true, NameRole.AUTHOR);
        assertArrayEquals(expectedAuthors, authors.toArray());
        List<NameItem> contributors = NameMapper.filter(all, true, NameRole.CONTRIBUTOR);
        assertArrayEquals(expectedContributors, contributors.toArray());

        // authors: remove [0], update[1], append new
        NameItem update = authors.get(1);
        update.setFamily("FamilyAuthor2Updated");
        update.setGiven(null);
        authors.remove(0);
        authors.add(0, new NameItem("FamilyAuthorInserted", null, NameRole.AUTHOR));
        authors.add(new NameItem("FamilyAuthorAdded", "GivenAuthorAdded", NameRole.AUTHOR));
        contributors.clear();

        List<NameItem> others = NameMapper.filter(all, false, NameRole.AUTHOR, NameRole.CONTRIBUTOR);
        assertArrayEquals(expectedOthers, others.toArray());
        List<NameItem> merged = MapperUtils.mergeList(authors, contributors, others);
        // write update
        instance.map(mods, merged);
        String xml = ModsUtils.toXml(mods, true);
        System.out.println(xml);

        // read
        ModsType result = ModsUtils.unmarshal(xml, ModsType.class);
        all = instance.map(result);
        NameItem[] expectedUpdatesAll = { new NameItem(0, "FamilyAuthorInserted", null, NameRole.AUTHOR), new NameItem(1, "FamilyAuthor2Updated", null, NameRole.AUTHOR), new NameItem(2, "FamilyAuthorAdded", "GivenAuthorAdded", NameRole.AUTHOR), new NameItem(3, "FamilyXXX", "GivenXXX", NameRole.OTHER), };
        NameItem[] expectedUpdatesAuthors = { expectedUpdatesAll[0], expectedUpdatesAll[1], expectedUpdatesAll[2], };
        NameItem[] expectedUpdatesContributors = {};
        NameItem[] expectedUpdatesOthers = { expectedUpdatesAll[3], };
        assertArrayEquals(expectedUpdatesAll, all.toArray());
        authors = NameMapper.filter(all, true, NameRole.AUTHOR);
        assertArrayEquals(expectedUpdatesAuthors, authors.toArray());
        contributors = NameMapper.filter(all, true, NameRole.CONTRIBUTOR);
        assertArrayEquals(expectedUpdatesContributors, contributors.toArray());
    }

}
