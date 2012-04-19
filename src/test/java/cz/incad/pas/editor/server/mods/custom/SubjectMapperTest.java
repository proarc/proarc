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

import cz.fi.muni.xkremser.editor.server.mods.ModsType;
import cz.fi.muni.xkremser.editor.server.mods.ObjectFactory;
import cz.fi.muni.xkremser.editor.server.mods.SubjectType;
import java.util.Arrays;
import java.util.List;
import javax.xml.bind.JAXBElement;
import org.hamcrest.core.Is;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.BeforeClass;

/**
 *
 * @author Jan Pokorsky
 */
public class SubjectMapperTest {

    ObjectFactory factory = new ObjectFactory();

    public SubjectMapperTest() {
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
    public void testGetKeywords() {
        ModsType mods = new ModsType();
        SubjectType subject = new SubjectType();
        mods.getModsGroup().add(subject);
        subject.getTopicOrGeographicOrTemporal().add(factory.createSubjectTypeTopic("keyword[0]"));
        subject.getTopicOrGeographicOrTemporal().add(factory.createSubjectTypeTopic("keyword[1]"));
        subject.getTopicOrGeographicOrTemporal().add(factory.createSubjectTypeGenre("genre[2]"));
        subject.getTopicOrGeographicOrTemporal().add(factory.createSubjectTypeTopic("keyword[3]"));
        SubjectMapper instance = new SubjectMapper(mods);
        List<String> result = instance.getKeywords();
        List<String> expect = Arrays.asList("keyword[0]", "keyword[1]", "keyword[3]");
        assertThat(result, Is.is(expect));
    }

    @Test
    public void testSetKeywords() {
        ModsType mods = new ModsType();
        SubjectType subject = new SubjectType();
        mods.getModsGroup().add(subject);
        subject.getTopicOrGeographicOrTemporal().add(factory.createSubjectTypeTopic("keyword[0]"));
        subject.getTopicOrGeographicOrTemporal().add(factory.createSubjectTypeTopic("keyword[1]"));
        subject.getTopicOrGeographicOrTemporal().add(factory.createSubjectTypeGenre("genre[2]"));
        subject.getTopicOrGeographicOrTemporal().add(factory.createSubjectTypeTopic("keyword[3]"));
        List<String> keywords = Arrays.asList("keyword[new]","keyword[0] - updated", "keyword[3] - updated");
        SubjectMapper instance = new SubjectMapper(mods);
        instance.setKeywords(keywords);
        List<String> result = instance.getKeywords();
        List<String> expect = Arrays.asList("keyword[new]","keyword[0] - updated", "keyword[3] - updated");
        assertThat(result, Is.is(expect));
    }

}
