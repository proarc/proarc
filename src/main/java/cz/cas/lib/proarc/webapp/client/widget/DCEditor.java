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
package cz.cas.lib.proarc.webapp.client.widget;

import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.DcRecordDataSource;
import cz.cas.lib.proarc.webapp.client.ds.DcRecordDataSource.QName;
import java.util.ArrayList;
import java.util.logging.Logger;

/**
 * Dublin Core editor.
 *
 * @author Jan Pokorsky
 */
public class DCEditor extends DynamicForm {
    
    private static final Logger LOG = Logger.getLogger(DCEditor.class.getName());

    private final ClientMessages i18n;

    public DCEditor(ClientMessages i18n) {
        this.i18n = i18n;
    }

    @Override
    protected void onDraw() {
        setFields(
                createDCListFormItem(DcRecordDataSource.FIELD_TITLE, i18n.DCEditor_Titles_Title(),
                    i18n.DCEditor_Titles_Hint()),
                createDCListFormItem(DcRecordDataSource.FIELD_CREATOR, i18n.DCEditor_Creators_Title(),
                    i18n.DCEditor_Creators_Hint()),
                createDCListFormItem(DcRecordDataSource.FIELD_SUBJECT, i18n.DCEditor_Subjects_Title(),
                    i18n.DCEditor_Subjects_Hint()),
                createDCListFormItem(DcRecordDataSource.FIELD_DESCRIPTION, i18n.DCEditor_Descriptions_Title(),
                    i18n.DCEditor_Descriptions_Hint()),
                createDCListFormItem(DcRecordDataSource.FIELD_PUBLISHER, i18n.DCEditor_Publishers_Title(),
                    i18n.DCEditor_Publishers_Hint()),
                createDCListFormItem(DcRecordDataSource.FIELD_CONTRIBUTOR, i18n.DCEditor_Contributors_Title(),
                    i18n.DCEditor_Contributors_Hint()),
                createDCListFormItem(DcRecordDataSource.FIELD_DATE, i18n.DCEditor_Dates_Title(),
                    i18n.DCEditor_Dates_Hint(),
                    false),
                createDCListFormItem(DcRecordDataSource.FIELD_TYPE, i18n.DCEditor_Types_Title(),
                    i18n.DCEditor_Types_Hint()),
                createDCListFormItem(DcRecordDataSource.FIELD_FORMAT, i18n.DCEditor_Formats_Title(),
                    i18n.DCEditor_Formats_Hint()),
                createDCListFormItem(DcRecordDataSource.FIELD_IDENTIFIER, i18n.DCEditor_Identifiers_Title(),
                    i18n.DCEditor_Identifiers_Hint(),
                    false),
                createDCListFormItem(DcRecordDataSource.FIELD_SOURCE, i18n.DCEditor_Sources_Title(),
                    i18n.DCEditor_Sources_Hint()),
                createDCListFormItem(DcRecordDataSource.FIELD_LANGUAGE, i18n.DCEditor_Languages_Title(),
                    i18n.DCEditor_Languages_Hint(),
                    false),
                createDCListFormItem(DcRecordDataSource.FIELD_RELATION, i18n.DCEditor_Relations_Title(),
                    i18n.DCEditor_Relations_Hint()),
                createDCListFormItem(DcRecordDataSource.FIELD_COVERAGE, i18n.DCEditor_Coverage_Title(),
                    i18n.DCEditor_Coverage_Hint()),
                createDCListFormItem(DcRecordDataSource.FIELD_RIGHTS, i18n.DCEditor_Rights_Title(),
                    i18n.DCEditor_Rights_Hint())
        );
    }


    private ListFormItem createDCListFormItem(QName name, String title, String help) {
        return createDCListFormItem(name, title, help, true);
    }

    private ListFormItem createDCListFormItem(QName name, String title, String help, boolean addLang) {
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
            ListGridField fieldLang = new ListGridField(DcRecordDataSource.FIELD_XML_LANG,
                    i18n.DCEditor_Language_Title());
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
