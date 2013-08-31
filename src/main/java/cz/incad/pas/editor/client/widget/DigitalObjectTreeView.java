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

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeGridField;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.action.Actions;
import cz.incad.pas.editor.client.action.RefreshAction;
import cz.incad.pas.editor.client.action.Selectable;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.client.ds.RelationDataSource;
import cz.incad.pas.editor.client.ds.RestConfig;
import java.util.Map;

/**
 * Shows hierarchy of digital objects for the particular digital object.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectTreeView implements Selectable<Record>, RefreshAction.Refreshable {

    private final Canvas rootWidget;
    private final TreeGrid treeSelector;
    private final ClientMessages i18n;
    private String rootPid;
    private final ToolStrip toolbar;

    public DigitalObjectTreeView(ClientMessages i18n) {
        this.i18n = i18n;
        treeSelector = createTreeSelector();

        VLayout vLayout = new VLayout();
        toolbar = createToolbar();
        vLayout.addMember(toolbar);
        vLayout.addMember(treeSelector);
        rootWidget = vLayout;
    }

    public Canvas asWidget() {
        return rootWidget;
    }

    public TreeGrid getTree() {
        return treeSelector;
    }

    public ToolStrip getToolbar() {
        return toolbar;
    }

    private TreeGrid createTreeSelector() {
        TreeGrid treeGrid = new TreeGrid();
        treeGrid.setCanSort(false);
        treeGrid.setDataSource(RelationDataSource.getInstance());
        TreeGridField parentId = new TreeGridField(RelationDataSource.FIELD_PARENT);
        TreeGridField label = new TreeGridField(RelationDataSource.FIELD_LABEL,
                i18n.DigitalObjectSearchView_ListHeaderLabel_Title());
        TreeGridField model = new TreeGridField(RelationDataSource.FIELD_MODEL,
                i18n.DigitalObjectSearchView_ListHeaderModel_Title(), 150);
        model.setAlign(Alignment.CENTER);
        TreeGridField pid = new TreeGridField(RelationDataSource.FIELD_PID,
                i18n.DigitalObjectSearchView_ListHeaderPid_Title(), 100);
        pid.setAlign(Alignment.CENTER);
        TreeGridField created = new TreeGridField(RelationDataSource.FIELD_CREATED,
                i18n.DigitalObjectSearchView_ListHeaderCreated_Title(), 100);
        created.setAlign(Alignment.CENTER);
        TreeGridField modified = new TreeGridField(RelationDataSource.FIELD_MODIFIED,
                i18n.DigitalObjectSearchView_ListHeaderModified_Title(), 100);
        modified.setAlign(Alignment.CENTER);
        TreeGridField owner = new TreeGridField(RelationDataSource.FIELD_OWNER,
                i18n.DigitalObjectSearchView_ListHeaderOwner_Title(), 100);
        treeGrid.setFields(parentId, label, model, pid, created, modified, owner);
        treeGrid.setTitleField(RelationDataSource.FIELD_LABEL);
        treeGrid.setShowConnectors(true);
        treeGrid.setEmptyMessage(i18n.DigitalObjectTreeView_EmptySelection_Msg());
        treeGrid.setAlternateRecordStyles(true);
        treeGrid.setSelectionType(SelectionStyle.SINGLE);
        treeGrid.setContextMenu(Actions.createMenu());
        return treeGrid;
    }

    private ToolStrip createToolbar() {
        ToolStrip toolbar = Actions.createToolStrip();
        return toolbar;
    }

    public void setRoot(String pid) {
        if (rootPid == null ? pid == null : rootPid.equals(pid)) {
            return ;
        }
        rootPid = pid;
        if (pid == null) {
            treeSelector.setData((Record[]) null);
            return ;
        }
        treeSelector.fetchData(new Criteria(RelationDataSource.FIELD_ROOT, pid), new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    treeSelector.selectRecord(0);
                }
            }
        });
    }

    public void setModels(Map<?, ?> valueMap) {
        ListGridField field = treeSelector.getField(RelationDataSource.FIELD_MODEL);
        field.setValueMap(valueMap);
    }

    @Override
    public void refresh() {
        treeSelector.invalidateCache();
    }

    @Override
    public Record[] getSelection() {
        return MetaModelDataSource.addModelObjectField(
                RelationDataSource.FIELD_MODEL, treeSelector.getSelectedRecords(true));
    }

}
