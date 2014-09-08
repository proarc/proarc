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
package cz.cas.lib.proarc.webapp.client.widget;

import com.google.gwt.core.client.GWT;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.presenter.ModsBatchEditor;
import java.util.Collections;

/**
 * UI to display the batch copy options.
 *
 * @author Jan Pokorsky
 * @see ModsBatchEditor
 */
public class CopyPageMetadataWidget {

    private final ClientMessages i18n;
    private final VLayout formPanel;
    private CheckboxItem allowPageIndexes;
    private CheckboxItem allowPageNumbers;
    private CheckboxItem allowPageTypes;
    private final DynamicForm form;

    public CopyPageMetadataWidget() {
        this.i18n = GWT.create(ClientMessages.class);
        form = createForm();
        formPanel = new VLayout(10);
        formPanel.setAutoHeight();
        formPanel.setMembers(form);
    }

    private DynamicForm createForm() {
        DynamicForm form = new DynamicForm();
        form.setWrapItemTitles(false);
//        form.setAutoWidth();
        form.setAutoHeight();
        form.setBrowserSpellCheck(false);
        allowPageIndexes = new CheckboxItem("fillPageIndexes", i18n.PageMetadataEditor_CheckboxPageIndices_Title());
        allowPageIndexes.setStartRow(true);
        allowPageIndexes.setColSpan("*");
        allowPageIndexes.setShowTitle(false);
        allowPageNumbers = new CheckboxItem("fillPageNumbers", i18n.PageMetadataEditor_CheckboxPageNubers_Title());
        allowPageNumbers.setStartRow(true);
        allowPageNumbers.setColSpan("*");
        allowPageNumbers.setShowTitle(false);
        allowPageTypes = new CheckboxItem("fillPageTypes", i18n.PageMetadataEditor_CheckboxPageTypes_Title());
        allowPageTypes.setStartRow(true);
        allowPageTypes.setColSpan("*");
        allowPageTypes.setShowTitle(false);
        form.setFields(allowPageTypes, allowPageIndexes, allowPageNumbers);
        return form;
    }

    public boolean getAllowPageIndexes() {
        return allowPageIndexes.getValueAsBoolean();
    }

    public boolean getAllowPageNumbers() {
        return allowPageNumbers.getValueAsBoolean();
    }

    public boolean getAllowPageTypes() {
        return allowPageTypes.getValueAsBoolean();
    }

    public Canvas getPanel() {
        return formPanel;
    }

    /**
     * Resets form.
     */
    public void initAll() {
        form.clearValues();
        allowPageIndexes.setValue(true);
        allowPageNumbers.setValue(true);
        allowPageTypes.setValue(true);
    }

    public boolean validate() {
        boolean valid = getAllowPageIndexes() || getAllowPageNumbers() || getAllowPageTypes();
        if (!valid) {
            form.setErrors(
                    Collections.singletonMap(allowPageTypes.getName(),
                            i18n.CopyPageMetadataWidget_NoOption_Msg()),
                    true);
        }
        return valid;
    }

}
