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

import cz.cas.lib.proarc.common.mods.custom.LanguageMapper.LanguageItem;
import cz.cas.lib.proarc.mods.CodeOrText;
import cz.cas.lib.proarc.mods.LanguageDefinition;
import cz.cas.lib.proarc.mods.LanguageTermDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.ObjectFactory;
import java.util.Arrays;
import java.util.List;
import org.hamcrest.core.Is;
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
public class LanguageMapperTest {

    private ObjectFactory factory = new ObjectFactory();

    public LanguageMapperTest() {
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

    @Test
    public void testRead() {
        ModsDefinition mods = new ModsDefinition();
        mods.getLanguage().add(language("cze"));
        mods.getLanguage().add(otherLanguage("other1"));
        mods.getLanguage().add(language("ger"));
        LanguageMapper instance = new LanguageMapper();
        List<LanguageItem> result = instance.map(mods);
        List<LanguageItem> expect = Arrays.asList(
                new LanguageItem(0, "cze"),
                new LanguageItem(2, "ger")
                );
        assertThat(result, Is.is(expect));
    }

    @Test
    public void testWrite() {
        ModsDefinition mods = new ModsDefinition();
        mods.getLanguage().add(language("cze"));
        mods.getLanguage().add(otherLanguage("other1"));
        mods.getLanguage().add(language("ger"));
        List<LanguageItem> updates = Arrays.asList(
                new LanguageItem(0, "cze-update"), // update
//                new LanguageItem(2, "ger") // delete
                new LanguageItem(null, "eng") // add
                );
        LanguageMapper instance = new LanguageMapper();
        instance.map(mods, updates);
        List<LanguageItem> result = instance.map(mods);
        List<LanguageItem> expect = Arrays.asList(
                new LanguageItem(0, "cze-update"),
                new LanguageItem(1, "eng")
                );
        assertThat(result, Is.is(expect));
    }

    private LanguageDefinition language(String lang) {
        LanguageDefinition l = factory.createLanguageDefinition();
        LanguageTermDefinition term = factory.createLanguageTermDefinition();
        term.setType(CodeOrText.CODE);
        term.setAuthority("iso639-2b");
        term.setValue(lang);
        l.getLanguageTerm().add(term);
        return l;
    }

    private LanguageDefinition otherLanguage(String part) {
        LanguageDefinition l = factory.createLanguageDefinition();
        l.setObjectPart(part);
        return l;
    }
}
