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
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeGridField;
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.action.Actions;
import cz.incad.pas.editor.client.action.RefreshAction;
import cz.incad.pas.editor.client.action.Selectable;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.client.ds.RelationDataSource;
import cz.incad.pas.editor.client.ds.RestConfig;
import java.util.HashMap;

/**
 * Shows hierarchy of digital objects for the particular digital object.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectTreeView implements Selectable<Record>, RefreshAction.Refreshable {

    private final Canvas rootWidget;
    private final TreeGrid treeSelector;
    private final PasEditorMessages i18nPas;
    private String rootPid;
    private final ToolStrip toolbar;

    public DigitalObjectTreeView(PasEditorMessages i18nPas) {
        this.i18nPas = i18nPas;
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
        TreeGridField model = new TreeGridField(RelationDataSource.FIELD_MODEL,
                i18nPas.DigitalObjectSearchView_ListHeaderModel_Title());
//        model.setOptionDataSource(MetaModelDataSource.getInstance());
//        model.setValueField(MetaModelDataSource.FIELD_PID);
//        model.setDisplayField(MetaModelDataSource.FIELD_DISPLAY_NAME);
        treeGrid.setFields(
                new TreeGridField(RelationDataSource.FIELD_LABEL,
                        i18nPas.DigitalObjectSearchView_ListHeaderLabel_Title()),
                model,
                new TreeGridField(RelationDataSource.FIELD_PID,
                        i18nPas.DigitalObjectSearchView_ListHeaderPid_Title()),
                new TreeGridField(RelationDataSource.FIELD_CREATED,
                        i18nPas.DigitalObjectSearchView_ListHeaderCreated_Title()),
                new TreeGridField(RelationDataSource.FIELD_MODIFIED,
                        i18nPas.DigitalObjectSearchView_ListHeaderModified_Title()),
                new TreeGridField(RelationDataSource.FIELD_OWNER,
                        i18nPas.DigitalObjectSearchView_ListHeaderOwner_Title())
                );
        treeGrid.setTitleField(RelationDataSource.FIELD_LABEL);
        treeGrid.setShowConnectors(true);
        treeGrid.setEmptyMessage(i18nPas.ImportParentChooser_EmptySelection_Title());
        treeGrid.setAlternateRecordStyles(true);
        treeGrid.setSelectionType(SelectionStyle.SINGLE);
        treeGrid.setContextMenu(Actions.createMenu());
        return treeGrid;
    }

    private ToolStrip createToolbar() {
        ToolStrip toolbar = Actions.createToolStrip();
        toolbar.addMember(Actions.asIconButton(new RefreshAction(i18nPas), this));
        return toolbar;
    }

    public void setRoot(String pid) {
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

    /**
     * workaround for broken TreeGridField.setOptionDataSource implementation.
     * @see <a href='http://forums.smartclient.com/showpost.php?p=59004&postcount=9'>SmartGWT solution</a>
     */
    public void loadModels() {
        MetaModelDataSource.getInstance().fetchData(null, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RestConfig.isStatusOk(response)) {
                    Record[] data = response.getData();
                    ListGridField field = treeSelector.getField(RelationDataSource.FIELD_MODEL);
                    HashMap<String, String> modelMap = new HashMap<String, String>();
                    for (Record record : data) {
                        MetaModelDataSource.MetaModelRecord m = new MetaModelDataSource.MetaModelRecord(record);
                        modelMap.put(m.getId(), m.getDisplayName());
                    }
                    field.setValueMap(modelMap);
                }
            }
        });
    }

    @Override
    public void refresh() {
        treeSelector.invalidateCache();
        loadModels();
        setRoot(rootPid);
    }

    @Override
    public Record[] getSelection() {
        return treeSelector.getSelectedRecords(true);
    }

}
