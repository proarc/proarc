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
import java.util.Collections;
import java.util.List;
import javax.xml.bind.JAXBElement;


/**
 * keywords from first mods/subject/topic*
 * <p/><b>NOTE: KNAV Kramerius 4 format</b>
 * @see <a href='http://code.google.com/p/kramerius/source/browse/trunk/import-cmdtool/src/main/resources/model_periodical_MODS.xsl'>model_periodical_MODS.xsl</a>
 *
 * @author Jan Pokorsky
 */
final class SubjectMapper {

    private final ModsType mods;
    private final ObjectFactory factory = new ObjectFactory();

    public SubjectMapper(ModsType mods) {
        this.mods = mods;
    }

    public List<String> getKeywords() {
        SubjectType subjectNode = getSubjectNode(false);
        List<String> keywords = Collections.emptyList();
        if (subjectNode != null) {
            List<JAXBElement<?>> group = subjectNode.getTopicOrGeographicOrTemporal();
            keywords = MapperUtils.find(group, String.class, ObjectFactory._SubjectTypeTopic_QNAME);
        }
        return keywords;
    }

    public void setKeywords(List<String> keywords) {
        keywords = MapperUtils.noNull(keywords);
        SubjectType subjectNode = getSubjectNode(true);
        List<JAXBElement<?>> group = subjectNode.getTopicOrGeographicOrTemporal();
        List<JAXBElement<?>> oldies = MapperUtils.findAny(group, ObjectFactory._SubjectTypeTopic_QNAME);
        group.removeAll(oldies);
        group.addAll(MapperUtils.toJaxb(keywords, ObjectFactory._SubjectTypeTopic_QNAME));
    }

    private SubjectType getSubjectNode(boolean create) {
        SubjectType subjectNode = MapperUtils.findFirst(mods.getModsGroup(), SubjectType.class);
        if (create && subjectNode == null) {
            subjectNode = factory.createSubjectType();
        }
        return subjectNode;
    }

}
