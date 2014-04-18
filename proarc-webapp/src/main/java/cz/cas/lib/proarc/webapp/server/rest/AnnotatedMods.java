/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.server.rest;

import cz.cas.lib.proarc.mods.AbstractDefinition;
import cz.cas.lib.proarc.mods.AccessConditionDefinition;
import cz.cas.lib.proarc.mods.CopyInformationDefinition;
import cz.cas.lib.proarc.mods.LanguageDefinition;
import cz.cas.lib.proarc.mods.LocationDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.mods.NameDefinition;
import cz.cas.lib.proarc.mods.NoteDefinition;
import cz.cas.lib.proarc.mods.OriginInfoDefinition;
import cz.cas.lib.proarc.mods.PartDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionDefinition;
import cz.cas.lib.proarc.mods.PhysicalDescriptionNote;
import cz.cas.lib.proarc.mods.PhysicalLocationDefinition;
import cz.cas.lib.proarc.mods.RecordInfoDefinition;
import cz.cas.lib.proarc.mods.RelatedItemDefinition;
import cz.cas.lib.proarc.mods.StringPlusLanguage;
import cz.cas.lib.proarc.mods.SubjectDefinition;
import cz.cas.lib.proarc.mods.SubjectNameDefinition;
import cz.cas.lib.proarc.mods.SubjectTitleInfoDefinition;
import cz.cas.lib.proarc.mods.TableOfContentsDefinition;
import cz.cas.lib.proarc.mods.Text;
import cz.cas.lib.proarc.mods.TitleInfoDefinition;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Helper class to annotate {@link cz.cas.lib.proarc.mods} properties.
 *
 * @see JacksonProvider
 *
 * @author Jan Pokorsky
 */
@XmlRootElement(name = "mods")
public class AnnotatedMods extends ModsDefinition {

    public static class AnnotatedAbstractDefinition extends AbstractDefinition {

        @XmlAttribute(name = "xlinkType")
        protected String xlinkType;
    }

    public static class AnnotatedAccessConditionDefinition extends AccessConditionDefinition {

        @XmlAttribute(name = "xlinkType")
        protected String xlinkType;
        @XmlAttribute(name = "xmlLang")
        protected String xmlLang;
    }

    public static class AnnotatedCopyInformationDefinitionNote extends CopyInformationDefinition.Note {

        @XmlAttribute(name = "xlinkType")
        protected String xlinkType;
    }

    public static class AnnotatedLanguageDefinition extends LanguageDefinition {

        @XmlAttribute(name = "xmlLang")
        protected String xmlLang;
    }

    public static class AnnotatedLocationDefinition extends LocationDefinition {

        @XmlAttribute(name = "xmlLang")
        protected String xmlLang;
    }

    public static class AnnotatedNameDefinition extends NameDefinition {

        @XmlAttribute(name = "xlinkRole")
        protected String xlinkRole;
        @XmlAttribute(name = "xlinkType")
        protected String xlinkType;
        @XmlAttribute(name = "xmlLang")
        protected String xmlLang;
    }

    public static class AnnotatedNoteDefinition extends NoteDefinition {

        @XmlAttribute(name = "xlinkType")
        protected String xlinkType;
    }

    public static class AnnotatedOriginInfoDefinition extends OriginInfoDefinition {

        @XmlAttribute(name = "xmlLang")
        protected String xmlLang;
    }

    public static class AnnotatedPartDefinition extends PartDefinition {

        @XmlAttribute(name = "xmlLang")
        protected String xmlLang;
    }

    public static class AnnotatedPhysicalDescriptionDefinition extends PhysicalDescriptionDefinition {

        @XmlAttribute(name = "xmlLang")
        protected String xmlLang;
    }

    public static class AnnotatedPhysicalDescriptionNote extends PhysicalDescriptionNote {

        @XmlAttribute(name = "xlinkType")
        protected String xlinkType;
    }

    public static class AnnotatedPhysicalLocationDefinition extends PhysicalLocationDefinition {

        @XmlAttribute(name = "xlinkType")
        protected String xlinkType;
    }

    public static class AnnotatedRecordInfoDefinition extends RecordInfoDefinition {

        @XmlAttribute(name = "xmlLang")
        protected String xmlLang;
    }

    public static class AnnotatedRelatedItemDefinition extends RelatedItemDefinition {

        @XmlAttribute(name = "xlinkType")
        protected String xlinkType;
    }

    public static class AnnotatedStringPlusLanguage extends StringPlusLanguage {

        @XmlAttribute(name = "xmlLang")
        protected String xmlLang;
    }

    public static class AnnotatedSubjectDefinition extends SubjectDefinition {

        @XmlAttribute(name = "xmlLang")
        protected String xmlLang;
    }

    public static class AnnotatedSubjectNameDefinition extends SubjectNameDefinition {

        @XmlAttribute(name = "xmlLang")
        protected String xmlLang;
        @XmlAttribute(name = "xlinkRole")
        protected String xlinkRole;
        @XmlAttribute(name = "xlinkType")
        protected String xlinkType;
    }

    public static class AnnotatedSubjectTitleInfoDefinition extends SubjectTitleInfoDefinition {

        @XmlAttribute(name = "xmlLang")
        protected String xmlLang;
        @XmlAttribute(name = "xlinkTitle")
        protected String xlinkTitle;
        @XmlAttribute(name = "xlinkType")
        protected String xlinkType;
    }

    public static class AnnotatedTableOfContentsDefinition extends TableOfContentsDefinition {

        @XmlAttribute(name = "xlinkType")
        protected String xlinkType;
    }

    public static class AnnotatedText extends Text {

        @XmlAttribute(name = "xlinkType")
        protected String xlinkType;
    }

    public static class AnnotatedTitleInfoDefinition extends TitleInfoDefinition {

        @XmlAttribute(name = "xmlLang")
        protected String xmlLang;
        @XmlAttribute(name = "xlinkTitle")
        protected String xlinkTitle;
        @XmlAttribute(name = "xlinkType")
        protected String xlinkType;
    }

}
