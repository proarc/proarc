/*
 * Copyright (C) 2011 Jan Pokorsky
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
package cz.incad.pas.editor.client.widget;

import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import cz.incad.pas.editor.client.ds.DcRecordDataSource;
import cz.incad.pas.editor.client.ds.DcRecordDataSource.QName;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public class DCEditor extends DynamicForm {
    
    private static final Logger LOG = Logger.getLogger(DCEditor.class.getName());

    public DCEditor() {
//        TextItem textItem = new TextItem("karel", "Prdel");
//        textItem.setCanFocus(true);
//        setCanFocus(true);


//        addItemChangedHandler(null);
//        getRecordList();
//        setAutoFocus(true);
//        valuesHaveChanged();

    }

    @Override
    protected void onDraw() {
        setFields(
//                textItem,
//                new TextItem("karel2", "Prdel2"),
                createDCListFormItem(DcRecordDataSource.FIELD_TITLE, "Titles",
                    "<i>Title</i><p/>The name given to the resource. Typically, a Title will be a name by which the resource is formally known."),
//                new TextItem("karel3", "Prdel3"),
                createDCListFormItem(DcRecordDataSource.FIELD_CREATOR, "Creators",
                    "<i>Creator</i><p/>An entity primarily responsible for making the content of the resource. Examples of a Creator include a person, an organization, or a service. Typically the name of the Creator should be used to indicate the entity."),
                createDCListFormItem(DcRecordDataSource.FIELD_SUBJECT, "Subjects",
                    "<i>Subject and Keywords</i><p/>The topic of the content of the resource. Typically, a Subject will be expressed as keywords or key phrases or classification codes that describe the topic of the resource. Recommended best practice is to select a value from a controlled vocabulary or formal classification scheme."),
                createDCListFormItem(DcRecordDataSource.FIELD_DESCRIPTION, "Descriptions",
                    "An account of the content of the resource. Description may include but is not limited to: an abstract, table of contents, reference to a graphical representation of content or a free-text account of the content."),
                createDCListFormItem(DcRecordDataSource.FIELD_PUBLISHER, "Publishers",
                    "The entity responsible for making the resource available. Examples of a Publisher include a person, an organization, or a service. Typically, the name of a Publisher should be used to indicate the entity."),
                createDCListFormItem(DcRecordDataSource.FIELD_CONTRIBUTOR, "Contributors",
                    "An entity responsible for making contributions to the content of the resource. Examples of a Contributor include a person, an organization or a service. Typically, the name of a Contributor should be used to indicate the entity."),
                createDCListFormItem(DcRecordDataSource.FIELD_DATE, "Dates",
                    "A date associated with an event in the life cycle of the resource. Typically, Date will be associated with the creation or availability of the resource. Recommended best practice for encoding the date value is defined in a profile of ISO 8601 [Date and Time Formats, W3C Note, http://www.w3.org/TR/NOTE- datetime] and follows the YYYY-MM-DD format.",
                    false),
                createDCListFormItem(DcRecordDataSource.FIELD_TYPE, "Types",
                    "The nature or genre of the content of the resource. Type includes terms describing general categories, functions, genres, or aggregation levels for content. Recommended best practice is to select a value from a controlled vocabulary (for example, the DCMIType vocabulary ). To describe the physical or digital manifestation of the resource, use the FORMAT element."),
                createDCListFormItem(DcRecordDataSource.FIELD_FORMAT, "Formats",
                    "The physical or digital manifestation of the resource. Typically, Format may include the media-type or dimensions of the resource. Examples of dimensions include size and duration. Format may be used to determine the software, hardware or other equipment needed to display or operate the resource."),
                createDCListFormItem(DcRecordDataSource.FIELD_IDENTIFIER, "Identifiers",
                    "<i>Resource Identifier</i><p/>An unambiguous reference to the resource within a given context. Recommended best practice is to identify the resource by means of a string or number conforming to a formal identification system. Examples of formal identification systems include the Uniform Resource Identifier (URI) (including the Uniform Resource Locator (URL), the Digital Object Identifier (DOI) and the International Standard Book Number (ISBN).",
                    false),
                createDCListFormItem(DcRecordDataSource.FIELD_SOURCE, "Sources",
                    "<i>Source</i><p/>A Reference to a resource from which the present resource is derived. The present resource may be derived from the Source resource in whole or part. Recommended best practice is to reference the resource by means of a string or number conforming to a formal identification system."),
                createDCListFormItem(DcRecordDataSource.FIELD_LANGUAGE, "Languages",
                    "<i>Language</i><p/>A language of the intellectual content of the resource. Recommended best practice for the values of the Language element is defined by RFC 3066 [RFC 3066, http://www.ietf.org/rfc/ rfc3066.txt] which, in conjunction with ISO 639 [ISO 639, http://www.oasis- open.org/cover/iso639a.html]), defines two- and three-letter primary language tags with optional subtags. Examples include \"en\" or \"eng\" for English, \"akk\" for Akkadian, and \"en-GB\" for English used in the United Kingdom.",
                    false),
                createDCListFormItem(DcRecordDataSource.FIELD_RELATION, "Relations",
                    "A reference to a related resource. Recommended best practice is to reference the resource by means of a string or number conforming to a formal identification system."),
                createDCListFormItem(DcRecordDataSource.FIELD_COVERAGE, "Coverage",
                    "<i>Coverage</i><p/>The extent or scope of the content of the resource. Coverage will typically include spatial location (a place name or geographic co-ordinates), temporal period (a period label, date, or date range) or jurisdiction (such as a named administrative entity). Recommended best practice is to select a value from a controlled vocabulary (for example, the Thesaurus of Geographic Names [Getty Thesaurus of Geographic Names, http://www. getty.edu/research/tools/vocabulary/tgn/]). Where appropriate, named places or time periods should be used in preference to numeric identifiers such as sets of co-ordinates or date ranges."),
                createDCListFormItem(DcRecordDataSource.FIELD_RIGHTS, "Rights",
                    "<i>Rights Management</i><p/>Information about rights held in and over the resource. Typically a Rights element will contain a rights management statement for the resource, or reference a service providing such information. Rights information often encompasses Intellectual Property Rights (IPR), Copyright, and various Property Rights. If the rights element is absent, no assumptions can be made about the status of these and other rights with respect to the resource.")
        );
    }


    private static ListFormItem createDCListFormItem(QName name, String title, String help) {
        return createDCListFormItem(name, title, help, true);
    }

    private static ListFormItem createDCListFormItem(QName name, String title, String help, boolean addLang) {
        ArrayList<ListGridField> fields = new ArrayList<ListGridField>(2);
        // text content field
        ListGridField fieldTextContent = new ListGridField(DcRecordDataSource.FIELD_XML_TEXT_CONTENT, title);
        fieldTextContent.setRequired(true);
        fields.add(fieldTextContent);

        if (addLang) {
            ComboBoxItem langComboBoxItem = new ComboBoxItem();
            langComboBoxItem.setAddUnknownValues(true);
            langComboBoxItem.setCompleteOnTab(true);
            langComboBoxItem.setBrowserSpellCheck(false);
    //        langComboBoxItem.setDefaultValue("cs");
            langComboBoxItem.setShowAllOptions(true);
            ListGridField fieldLang = new ListGridField(DcRecordDataSource.FIELD_XML_LANG, "Language");
    //        Map<String, String> langMap = new HashMap<String, String>();
    //        langMap.put("cs", "Czech");
    //        langMap.put("de", "German");
    //        langMap.put("en", "English");
    //        langFormItem.setValueMap(langMap);
            fieldLang.setEditorType(langComboBoxItem);
            fieldLang.setValueMap("cs", "de", "en");
            fieldLang.setWidth(100);
            fieldLang.setDefaultValue("cs");
            fieldLang.setEmptyCellValue("cs - default");
            fields.add(fieldLang);
        }

        ListFormItem editorTitle = new ListFormItem(name.getQualifiedName(), title);
        editorTitle.setFields(fields.toArray(new ListGridField[fields.size()]));
        editorTitle.setShowTitle(false);
        editorTitle.setShowHint(false);
        editorTitle.setHelp(help);
//        editorTitle.setCanFocus(true);
        return editorTitle;
    }

}
