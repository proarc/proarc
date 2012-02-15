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
package cz.incad.pas.editor.client;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.http.client.UrlBuilder;
import com.google.gwt.i18n.client.Dictionary;
import com.google.gwt.user.client.Window;
import com.smartgwt.client.types.Autofit;
import com.smartgwt.client.util.Page;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.LinkItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.tree.Tree;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeNode;
import com.smartgwt.client.widgets.tree.events.FolderClosedEvent;
import com.smartgwt.client.widgets.tree.events.FolderClosedHandler;
import com.smartgwt.client.widgets.tree.events.LeafClickEvent;
import com.smartgwt.client.widgets.tree.events.LeafClickHandler;
import cz.incad.pas.editor.client.ds.DcRecordDataSource;
import cz.incad.pas.editor.client.ds.DigitalObjectDataSource;
import cz.incad.pas.editor.client.ds.OcrDataSource;
import cz.incad.pas.editor.client.ds.RemoteMetadataDataSource;
import cz.incad.pas.editor.client.presenter.DigObjectEditorPresenter;
import cz.incad.pas.editor.client.presenter.ImportPresenter;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Editor implements EntryPoint {

    private static final Logger LOG = Logger.getLogger(Editor.class.getName());

    private PasEditorMessages i18nPas;

    @Override
    public void onModuleLoad() {
//        if (true) {
//            Canvas canvas = new Canvas();
//            canvas.setContents("test");
//            canvas.setWidth100();
//            canvas.setHeight100();
//            canvas.draw();
//            return;
//        }
        initLogging();
        
        ClientUtils.info(LOG, "onModuleLoad:\n module page: %s\n host page: %s"
                + "\n getModuleName: %s\n getPermutationStrongName: %s\n version: %s"
                + "\n Page.getAppDir: %s",
                GWT.getModuleBaseURL(), GWT.getHostPageBaseURL(),
                GWT.getModuleName(), GWT.getPermutationStrongName(), GWT.getVersion(),
                Page.getAppDir()
                );

        // !!!DO NOT REMOVE datasource init, otherwise SmartGWT does not render anything
        // It relates to ImportBatchItemEditor.createTabs somehow.
        OcrDataSource ocrDataSource = OcrDataSource.getInstance();
        DcRecordDataSource dcRecordDataSource = DcRecordDataSource.getInstance();
        RemoteMetadataDataSource instance = RemoteMetadataDataSource.getInstance();
        DigitalObjectDataSource instance1 = DigitalObjectDataSource.getInstance();

        i18nPas = GWT.create(PasEditorMessages.class);

////        tabSet.setBorder("2px solid blue");

        TreeGrid menu = createMenu();

        Canvas menuPlaces = createMenuPlaces(menu);

        final HLayout mainLayout = new HLayout();
//        mainLayout.setLayoutMargin(5);
        mainLayout.setWidth100();
        mainLayout.setHeight100();
        mainLayout.setMembers(menu, menuPlaces);

//        selectDefaultPlace(menu, "Import/History");
        selectDefaultPlace(menu, "Edit/New Object");
        
        Canvas mainHeader = createMainHeader();

        VLayout desktop = new VLayout(0);
        desktop.setWidth100();
        desktop.setHeight100();
        desktop.setMembers(mainHeader, mainLayout);
        desktop.draw();
    }

    private Canvas createMainHeader() {
        ToolStrip mainHeader = new ToolStrip();
        mainHeader.setWidth100();
//        mainHeader.setHeight(33);
        mainHeader.setHeight(40);

        mainHeader.addSpacer(6);

        Label headerItem = new Label(i18nPas.Editor_Header_Title());
        headerItem.setStyleName("pasMainTitle");
        headerItem.setWrap(false);
        headerItem.setIcon("24/cube_frame.png");
        mainHeader.addMember(headerItem);

        mainHeader.addFill();

        DynamicForm langForm = new DynamicForm();
        langForm.setNumCols(2);
        langForm.setFields(createLangLink("cs", "Česky"), createLangLink("en", "English"));
        langForm.setAutoWidth();
        langForm.setAutoHeight();
        mainHeader.addMember(langForm);

        return mainHeader;
    }

    private LinkItem createLangLink(final String locale, String title) {
        final LinkItem lang = new LinkItem();
        lang.setLinkTitle(title);
        lang.setShowTitle(false);
        lang.setWidth(45);
        lang.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                UrlBuilder urlBuilder = Window.Location.createUrlBuilder();
                urlBuilder.setParameter("locale", locale);
                String url = urlBuilder.buildString();
                Window.Location.replace(url);
            }
        });
        return lang;
    }

    private void selectDefaultPlace(TreeGrid menu, String menuPath) {
//        TreeNode find = tree.find("Import/New Batch");
        Tree tree = menu.getTree();
        TreeNode find = tree.find(menuPath);
        int menuItemIdx = menu.getRecordIndex(find);
        ClientUtils.info(LOG, "Found: %s, menuItemIdx: %s, name: %s",
                find, menuItemIdx, find != null ? find.getName() : null);
        menu.selectRecord(find);
        menu.rowClick(find, menuItemIdx, 0);
    }

    private TreeGrid createMenu() {
        final TreeGrid menu = new TreeGrid();
        menu.setHeight100();
        menu.setAutoFitData(Autofit.HORIZONTAL);
        menu.setShowResizeBar(true);
        menu.setLeaveScrollbarGap(false);
//        menu.setWidth(200);

        TreeNode[] trees = new TreeNode[] {
                createTreeNode("Import", i18nPas.MainMenu_Import_Title(),
                        createTreeNode("New Batch", i18nPas.MainMenu_Import_NewBatch_Title()),
                        createTreeNode("History", i18nPas.MainMenu_Import_Edit_Title())),
                createTreeNode("Edit", i18nPas.MainMenu_Edit_Title(),
                        createTreeNode("New Object", i18nPas.MainMenu_Edit_NewObject_Title()),
                        createTreeNode("Search", i18nPas.MainMenu_Edit_Edit_Title())),
                createTreeNode("Statistics", i18nPas.MainMenu_Statistics_Title()),
                createTreeNode("Users", i18nPas.MainMenu_Users_Title()),
                createTreeNode("Console", i18nPas.MainMenu_Console_Title()),
        };
        for (int i = 0; i < trees.length; i++) {
            TreeNode treeNode = trees[i];
            ClientUtils.fine(LOG, "TreeNode.array: %s, i: %s", treeNode.getName(), i);
        }
        Tree tree = new Tree();
        tree.setData(trees);
        tree.openAll();
        menu.setData(tree);
        menu.setShowHeader(false);
        menu.setShowOpener(false);
        menu.setShowOpenIcons(false);
        menu.setCanCollapseGroup(false);
        menu.addFolderClosedHandler(new FolderClosedHandler() {

            @Override
            public void onFolderClosed(FolderClosedEvent event) {
                event.cancel();
            }
        });
        return menu;
    }

    private Canvas createMenuPlaces(final TreeGrid menu) {
        final ImportPresenter importPresenter = new ImportPresenter(i18nPas);
//
        final DigObjectEditorPresenter objectEditorPresenter = new DigObjectEditorPresenter(i18nPas);

        final HLayout placesContainer = new HLayout();
        placesContainer.setHeight100();
        placesContainer.setWidth100();

        final Canvas empty = new Canvas();
        empty.setHeight100();
        empty.setWidth100();
        empty.setContents("Select action.");
        placesContainer.setMembers(empty);

        menu.addLeafClickHandler(new LeafClickHandler() {

            @Override
            public void onLeafClick(LeafClickEvent event) {
                ClientUtils.fine(LOG, "menu.getSelectedPaths: %s\nmenu.getSelectedRecord: %s",
                        menu.getSelectedPaths(), menu.getSelectedRecord());
                String name = event.getLeaf().getName();
                if ("New Batch".equals(name)) {
                    Canvas ui = importPresenter.getUI();
                    placesContainer.setMembers(ui);
                    importPresenter.bind();
                    importPresenter.importFolder();
                } else if ("History".equals(name)) {
                    Canvas ui = importPresenter.getUI();
                    placesContainer.setMembers(ui);
                    importPresenter.bind();
                    importPresenter.selectBatchFromHistory();
                } else if ("New Object".equals(name)) {
                    Canvas ui = objectEditorPresenter.getUI();
                    placesContainer.setMembers(ui);
                    objectEditorPresenter.newObject();
                    importPresenter.bind();
                    importPresenter.selectBatchFromHistory();
                } else if ("Search".equals(name)) {
                    Canvas ui = objectEditorPresenter.getUI();
                    placesContainer.setMembers(ui);
                    objectEditorPresenter.search();
                } else if ("Console".equals(name)) {
                    SC.showConsole();
//                } else if ("Users".equals(name)) {
//                    PageMetadataEditor.getInstance().showInWindow(ClientUtils.EMPTY_BOOLEAN_CALLBACK);
                } else {
                    placesContainer.setMembers(empty);
                }
            }
        });

        return placesContainer;
    }

    /**
     * Helper method to workaround GWT compiler issue with varargs.
     * DO NOT REMOVE till
     * {@link #createTreeNode(java.lang.String, java.lang.String, com.smartgwt.client.widgets.tree.TreeNode[])}
     * exists!
     */
    private TreeNode createTreeNode(String name, String displayName) {
        return createTreeNode(name, displayName, (TreeNode[]) null);
    }

    private TreeNode createTreeNode(String name, String displayName, TreeNode... children) {
        if (name == null) {
            throw new NullPointerException("name");
        }
        TreeNode treeNode = new TreeNode(name);
        if (displayName != null) {
            treeNode.setTitle(displayName);
        }
        if (children != null) {
            treeNode.setChildren(children);
        }
        return treeNode;
    }

    private void initLogging() {
        Dictionary levels = Dictionary.getDictionary("EditorLoggingConfiguration");
        for (String loggerName : levels.keySet()) {
            String levelValue = levels.get(loggerName);
            try {
                Level level = Level.parse(levelValue);
                Logger logger = Logger.getLogger(loggerName);
                logger.setLevel(level);
                Logger.getLogger("").info(ClientUtils.format(
                        "logger: '%s', levelValue: %s", loggerName, level));
            } catch (IllegalArgumentException ex) {
                Logger.getLogger("").log(Level.SEVERE,
                        ClientUtils.format("logger: '%s', levelValue: %s", loggerName, levelValue), ex);
            }
        }

        if (GWT.isProdMode()) {
            // XXX SmartGWT 3.0 ignores thrown exceptions in production mode.
            // Javascript stack traces are useless but messages can be valuable
            GWT.setUncaughtExceptionHandler(new GWT.UncaughtExceptionHandler() {

                @Override
                public void onUncaughtException(Throwable e) {
                    StringBuilder sb = new StringBuilder();
                    for (Throwable t = e; t != null; t = t.getCause()) {
                        sb.append("* ").append(t.getClass().getName()).append(": ")
                                .append(t.getLocalizedMessage()).append("\n");
                        for (StackTraceElement elm : t.getStackTrace()) {
                            sb.append("  ").append(elm.toString()).append("\n");
                        }
                    }

                    // visible in javascript console; Window.alert is too much intrusive.
                    LOG.log(Level.SEVERE, e.getMessage(), e);
//                    Window.alert(sb.toString());
                }
            });
        }
    }
}
