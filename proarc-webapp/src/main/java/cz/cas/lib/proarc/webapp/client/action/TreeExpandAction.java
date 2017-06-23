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

package cz.cas.lib.proarc.webapp.client.action;

import com.smartgwt.client.data.Record;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource;
import cz.cas.lib.proarc.webapp.client.widget.Dialog;
import cz.cas.lib.proarc.webapp.client.widget.DigitalObjectTreeView;

/**
 * Action for expanding tree in treeView
 *
 * @author Jakub Kremlacek
 */
public final class TreeExpandAction extends AbstractAction {

    private final DigitalObjectTreeView treeView;
    private DynamicForm optionsForm;
    private final ClientMessages i18n;

    public TreeExpandAction(
            ClientMessages i18n,
            DigitalObjectTreeView treeView
            ) {
        this(i18n, null, treeView);
    }

    public TreeExpandAction(
            ClientMessages i18n,
            String icon,
            DigitalObjectTreeView treeView
    ) {
        super(
                i18n.DigitalObjectEditor_ExpandTree_Title(),
                icon == null ? "[SKIN]/actions/next.png" : icon,
                i18n.DigitalObjectEditor_ExpandTree_Hint()
        );

        this.treeView = treeView;
        this.i18n = i18n;
    }

    @Override
    public void performAction(ActionEvent event) {
        Record[] records = Actions.getSelection(event);

        if (records.length == 0) return;

        optionsForm = createExpandOptionsForm();

        optionsForm.clearValues();
        final Dialog d = new Dialog(i18n.TreeExpandAction_Window_Title());
        d.getDialogLabelContainer().setContents(i18n.TreeExpandAction_Window_Msg());
        d.getDialogContentContainer().setMembers(optionsForm);
        d.addYesButton((ClickEvent eventX) -> {
            d.destroy();

            String pid = records[0].getAttribute(RelationDataSource.FIELD_PID);
            treeView.expandNode(pid);
        });
        d.addNoButton(new Dialog.DialogCloseHandler() {
            @Override
            public void onClose() {
                d.destroy();
            }
        });
        d.setWidth(400);
        d.show();
    }

    @Override
    public boolean accept(ActionEvent event) {
        Object[] selection = Actions.getSelection(event);

        return selection != null && selection.length == 1;
    }

    private DynamicForm createExpandOptionsForm() {
        DynamicForm f = new DynamicForm();
        f.setAutoHeight();
        return f;
    }
}
