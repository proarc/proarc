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

import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.ObjectFactory;
import cz.cas.lib.proarc.mods.StringPlusLanguagePlusAuthority;
import cz.cas.lib.proarc.mods.SubjectDefinition;
import java.util.Collections;
import java.util.List;


/**
 * keywords from first mods/subject/topic*
 * <p/><b>NOTE: KNAV Kramerius 4 format</b>
 *
 * @author Jan Pokorsky
 * @see <a href='https://github.com/ceskaexpedice/kramerius/blob/master/import-cmdtool/src/main/resources/model_periodical_MODS.xsl'>model_periodical_MODS.xsl</a>
 */
final class SubjectMapper {

    private final ModsDefinition mods;
    private final ObjectFactory factory = new ObjectFactory();

    public SubjectMapper(ModsDefinition mods) {
        this.mods = mods;
    }

    public List<String> getKeywords() {
        SubjectDefinition subjectNode = getSubjectNode(false);
        List<String> keywords = Collections.emptyList();
        if (subjectNode != null) {
            keywords = MapperUtils.toStringPlusLanguageValue(subjectNode.getTopic());
        }
        return keywords;
    }

    public void setKeywords(List<String> keywords) {
        keywords = MapperUtils.noNull(keywords);
        SubjectDefinition subjectNode = getSubjectNode(true);
        List<StringPlusLanguagePlusAuthority> oldies = subjectNode.getTopic();
        oldies.clear();
        oldies.addAll(MapperUtils.toStringPlusLanguagePlusAuthority(keywords));
    }

    private SubjectDefinition getSubjectNode(boolean create) {
        SubjectDefinition subjectNode = mods.getSubject().stream().findFirst().orElse(null);
        if (create && subjectNode == null) {
            subjectNode = factory.createSubjectDefinition();
        }
        return subjectNode;
    }

}
