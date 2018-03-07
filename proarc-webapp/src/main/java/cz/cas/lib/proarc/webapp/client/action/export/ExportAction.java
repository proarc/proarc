/*
 * Copyright (C) 2017 Jakub Kremlacek
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

package cz.cas.lib.proarc.webapp.client.action.export;

import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.widget.Dialog;
import java.util.List;


/**
 * Wrapper class for export actions, enables customization of export options via overriding createExportFormOptions() and setRequestOptions().
 *
 * @author Jakub Kremlacek
 */
public abstract class ExportAction extends AbstractAction {

    protected final ClientMessages i18n;

    private List<FormItem> formItems;

    public ExportAction(ClientMessages i18n, String title, String icon, String tooltip) {
        super(title, icon, tooltip);
        this.i18n = i18n;
    }

    /**
     * Enables export options dialog form customization by the specific export variant
     */
    protected List<FormItem> createExportFormOptions() {
        return null;
    }

    /**
     * Fills options selected in FormItems created by createOtherExportFormOptions
     *
     * @param record record that will be passed to the backend
     */
    protected void setRequestOptions(Record record) {}

    protected final void dsAddData(DataSource dataSource, Record record, DSCallback dsCallback, DSRequest dsRequest) {
        DynamicForm optionsForm = createOptionsForm();

        if (optionsForm == null) {
            dataSource.addData(record, dsCallback, dsRequest);
            return;
        }

        Dialog d = new Dialog(i18n.ExportAction_Request_Title());
        d.getDialogContentContainer().addMember(optionsForm);

        IButton okButton = d.addOkButton((ClickEvent eventX) -> {
            setRequestOptions(record);
            dataSource.addData(record, dsCallback, dsRequest);
            d.destroy();
            SC.say(i18n.ExportAction_Sent_Msg());
        });

        d.addCancelButton(() -> d.destroy());
        d.setWidth(400);
        d.show();

        okButton.focus();
    }

    private final DynamicForm createOptionsForm() {
        DynamicForm f = new DynamicForm();
        f.setAutoHeight();
        f.setLayoutAlign(Alignment.CENTER);
        f.setWidth(350);

        formItems = createExportFormOptions();

        if (formItems == null || formItems.isEmpty()) {
            return null;
        }

        FormItem fields[] = new FormItem[formItems.size()];

        for (int i = 0; i < formItems.size(); i++) {
            fields[i] = formItems.get(i);
            formItems.get(i);
        }

        f.setFields(fields);

        return f;
    }
}
