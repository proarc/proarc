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
package cz.incad.pas.editor.server.mods.custom;

import cz.fi.muni.xkremser.editor.server.mods.CodeOrText;
import cz.fi.muni.xkremser.editor.server.mods.LanguageType;
import cz.fi.muni.xkremser.editor.server.mods.LanguageType.LanguageTerm;
import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.ObjectFactory;
import cz.incad.pas.editor.server.mods.custom.LanguageMapper.LanguageItem;
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
        ModsType mods = new ModsType();
        mods.getModsGroup().add(language("cze"));
        mods.getModsGroup().add(otherLanguage("other1"));
        mods.getModsGroup().add(language("ger"));
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
        ModsType mods = new ModsType();
        mods.getModsGroup().add(language("cze"));
        mods.getModsGroup().add(otherLanguage("other1"));
        mods.getModsGroup().add(language("ger"));
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

    private LanguageType language(String lang) {
        LanguageType l = factory.createLanguageType();
        LanguageTerm term = factory.createLanguageTypeLanguageTerm();
        term.setType(CodeOrText.CODE);
        term.setAuthority("iso639-2b");
        term.setValue(lang);
        l.getLanguageTerm().add(term);
        return l;
    }

    private LanguageType otherLanguage(String part) {
        LanguageType l = factory.createLanguageType();
        l.setObjectPart(part);
        return l;
    }
}
