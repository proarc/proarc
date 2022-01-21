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
package cz.cas.lib.proarc.webapp.client.widget;

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.i18n.SmartGwtMessages;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.tree.Tree;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeGridField;
import com.smartgwt.client.widgets.tree.TreeNode;
import com.smartgwt.client.widgets.tree.events.DataArrivedEvent;
import com.smartgwt.client.widgets.tree.events.DataArrivedHandler;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction;
import cz.cas.lib.proarc.webapp.client.action.Selectable;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource;
import cz.cas.lib.proarc.webapp.client.ds.SearchDataSource;
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
    private final SmartGwtMessages i18nSmartGwt;
    private String rootPid;
    private final ToolStrip toolbar;
    private TreeNode openingNode = null;

    public DigitalObjectTreeView(ClientMessages i18n) {
        this.i18n = i18n;
        this.i18nSmartGwt = ClientUtils.createSmartGwtMessages();
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
        ListGridField isLocked = new ListGridField(SearchDataSource.FIELD_IS_LOCKED,
                "locked", 100);
        isLocked.setCellFormatter(new CellFormatter() {
            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return value == null || "0".equals(value)
                        ? i18nSmartGwt.dialog_NoButtonTitle()
                        : i18nSmartGwt.dialog_YesButtonTitle();
            }
        });
        TreeGridField export = new TreeGridField(SearchDataSource.FIELD_EXPORT,
                i18n.DigitalObjectSearchView_ListHeaderExport_Title(), 100);
        export.setCellFormatter(new CellFormatter() {

            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return value == null || "0".equals(value)
                        ? i18nSmartGwt.dialog_NoButtonTitle()
                        : i18nSmartGwt.dialog_YesButtonTitle();
            }
        });
        ListGridField organization = new ListGridField(SearchDataSource.FIELD_ORGANIZATION,
                i18n.DigitalObjectSearchView_ListHeaderOrganization_Title(), 100);
        organization.setHidden(true);
        organization.setValueMap(Organization.getMap());
        ListGridField user = new ListGridField(SearchDataSource.FIELD_USER,
                i18n.UsersView_ListHeader_Proccesor_Title(), 100);
        user.setHidden(true);
        user.setCellFormatter(new CellFormatter() {
            @Override
            public String format(Object value, ListGridRecord listGridRecord, int i, int i1) {
                return value == null || "all".equals(value)
                        ? i18n.UsersView_ListHeader_Proccesors_All_Title()
                        : value.toString();
                }
            });
        ListGridField status = new ListGridField(SearchDataSource.FIELD_STATUS,
                i18n.DigitalObjectEditor_AdministrationEditor_Status_Title(), 100);
        status.setValueMap(DigitalObjectState.getMap(i18n));
        status.setHidden(true);
        ListGridField ndkExport = new ListGridField(SearchDataSource.FIELD_NDK_EXPORT,
                i18n.DigitalObjectEditor_AdministrationEditor_Ndk_Export_Title(), 100);
        ndkExport.setHidden(true);
        ndkExport.setCellFormatter(new CellFormatter() {

            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return value == null || "0".equals(value)
                        ? i18nSmartGwt.dialog_NoButtonTitle()
                        : i18nSmartGwt.dialog_YesButtonTitle();
            }
        });
        ListGridField archiveExport = new ListGridField(SearchDataSource.FIELD_ARCHIVE_EXPORT,
                i18n.DigitalObjectEditor_AdministrationEditor_Archive_Export_Title(), 100);
        archiveExport.setHidden(true);
        archiveExport.setCellFormatter(new CellFormatter() {

            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return value == null || "0".equals(value)
                        ? i18nSmartGwt.dialog_NoButtonTitle()
                        : i18nSmartGwt.dialog_YesButtonTitle();
            }
        });
        ListGridField krameriusExport = new ListGridField(SearchDataSource.FIELD_KRAMERIUS_EXPORT,
                i18n.DigitalObjectEditor_AdministrationEditor_Kramerius_Export_Title(), 100);
        krameriusExport.setHidden(true);
        krameriusExport.setCellFormatter(new CellFormatter() {

            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return value == null || "0".equals(value)
                        ? i18nSmartGwt.dialog_NoButtonTitle()
                        : i18nSmartGwt.dialog_YesButtonTitle();
            }
        });
        ListGridField crossrefExport = new ListGridField(SearchDataSource.FIELD_CROSSREF_EXPORT,
                i18n.DigitalObjectEditor_AdministrationEditor_Crossref_Export_Title(), 100);
        crossrefExport.setHidden(true);
        crossrefExport.setCellFormatter(new CellFormatter() {

            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                return value == null || "0".equals(value)
                        ? i18nSmartGwt.dialog_NoButtonTitle()
                        : i18nSmartGwt.dialog_YesButtonTitle();
            }
        });
        treeGrid.setFields(parentId, label, model, isLocked, pid, created, modified, owner, organization, user, status, export, ndkExport, archiveExport, krameriusExport, crossrefExport);
        treeGrid.setTitleField(RelationDataSource.FIELD_LABEL);
        treeGrid.setShowConnectors(true);
        treeGrid.setEmptyMessage(i18n.DigitalObjectTreeView_EmptySelection_Msg());
        treeGrid.setAlternateRecordStyles(true);
        treeGrid.setSelectionType(SelectionStyle.SINGLE);
        treeGrid.setContextMenu(Actions.createMenu());
        ListGridPersistance treeGridPersistence = new ListGridPersistance("DigitalObjectTreeView.digitalObjectList", treeGrid);
        treeGrid.setViewState(treeGridPersistence.getViewState());
        treeGrid.addDataArrivedHandler(new DataArrivedHandler() {

            @Override
            public void onDataArrived(DataArrivedEvent event) {
                selectAndExpandRootNode(event);

                if (openingNode != null) {
                    expandNode(event.getParentNode());
                }
            }
        });
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
            treeSelector.setData(new TreeNode[0]);
            return ;
        }
        treeSelector.fetchData(new Criteria(RelationDataSource.FIELD_ROOT, pid));
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
        return treeSelector.getSelectedRecords(true);
    }

    private void selectAndExpandRootNode(DataArrivedEvent event) {
        // first event contains parent node that is root node of TreeGrid
        //  and it is a synthetic node without attributes
        TreeNode parentNode = event.getParentNode();
        Tree tree = treeSelector.getTree();
        TreeNode[] children = tree.getChildren(parentNode);
        if (children.length > 0 && children[0].getAttribute(RelationDataSource.FIELD_PARENT) == null) {
            // select real root node and expand it
            TreeNode realRootNode = children[0];
            treeSelector.selectRecord(realRootNode);
            tree.openFolder(realRootNode);
        }
    }

    /**
     * recursively expands node via given pid
     *
     * @param pid id of node to be expanded
     */
    public void expandNode(String pid) {
        expandNode(null, pid);
    }

    /**
     * recursively expands node via given pid within specified subtree
     *
     * @param root subtree containing node with pid
     * @param pid id of node to be expanded
     */
    public void expandNode(TreeNode root, String pid) {
        if (root == null) {
            root = treeSelector.getTree().getRoot();
        }

        String id = root.getAttribute(RelationDataSource.FIELD_PID);

        if (id == pid) {
            openingNode = root;
            expandNode(root);
            return;
        }

        TreeNode[] children = treeSelector.getTree().getChildren(root);

        for(TreeNode child : children) {
            expandNode(child, pid);
        }
    }

    /**
     * recursively expands specific node
     *
     * @param node
     */
    public void expandNode(TreeNode node) {

        //is successor to calling node?
        if (!isSuccessor(node)) {
            openingNode = null;
            return;
        }

        //if is then open him else reset calling node
        treeSelector.openFolder(node);

        TreeNode[] children = treeSelector.getTree().getChildren(node);

        for (TreeNode child : children) {
            expandNode(child);
        }
    }

    private boolean isSuccessor(TreeNode child) {
        if (child == null) {
            return false;
        }
        
        if (child.equals(openingNode)) {
            return true;
        }

        TreeNode parent = treeSelector.getTree().getParent(child);

        while (parent != null) {
            if (parent.equals(openingNode)) return true;

            parent = treeSelector.getTree().getParent(parent);
        }

        return false;
    }
}
