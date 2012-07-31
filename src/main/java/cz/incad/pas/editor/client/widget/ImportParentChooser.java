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
package cz.incad.pas.editor.client.widget;

import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.ds.RelationDataSource;

public final class ImportParentChooser extends VLayout {

    private final PasEditorMessages i18nPas;
    private ImportParentHandler handler;
    private final DigitalObjectSearchView foundView;
    private final DigitalObjectTreeView treeView;
    
    public ImportParentChooser(PasEditorMessages i18nPas) {
        super(4);
        this.i18nPas = i18nPas;
        setLayoutMargin(4);
        setWidth100();
        setHeight100();

        foundView = new DigitalObjectSearchView(i18nPas);
        treeView = new DigitalObjectTreeView(i18nPas);

        foundView.getGrid().setSelectionType(SelectionStyle.SINGLE);
        foundView.getGrid().addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                final ListGridRecord selectedRecord = foundView.getGrid().getSelectedRecord();
                if (selectedRecord != null) {
                    String pid = selectedRecord.getAttribute(RelationDataSource.FIELD_PID);
                    treeView.setRoot(pid);
                }
            }
        });

        treeView.getTree().addSelectionUpdatedHandler(new SelectionUpdatedHandler() {

            @Override
            public void onSelectionUpdated(SelectionUpdatedEvent event) {
                handler.onParentSelectionUpdated();
            }
        });

        addMember(foundView.asWidget());
        addMember(treeView.asWidget());
    }

    public void setHandler(ImportParentHandler handler) {
        this.handler = handler;
    }

    public void setDataSource(final String parentPid) {
        treeView.loadModels();
        foundView.onShow(parentPid == null);
        treeView.setRoot(parentPid);
    }

    public Record getSelectedParent() {
        return treeView.getTree().getSelectedRecord();
    }

    public interface ImportParentHandler {
        void onParentSelectionUpdated();
    }

}
