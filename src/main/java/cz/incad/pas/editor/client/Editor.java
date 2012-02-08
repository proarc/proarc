package cz.incad.pas.editor.client;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.Dictionary;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.tree.Tree;
import com.smartgwt.client.widgets.tree.TreeGrid;
import com.smartgwt.client.widgets.tree.TreeNode;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.SchemaSet;
import com.smartgwt.client.data.XMLTools;
import com.smartgwt.client.data.XSDLoadCallback;
import com.smartgwt.client.data.fields.DataSourceDateField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.data.fields.DataSourceBooleanField;
import com.smartgwt.client.i18n.SmartGwtMessages;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SectionItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Autofit;
import com.smartgwt.client.types.TextAreaWrap;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.Page;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.fields.CanvasItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.layout.LayoutSpacer;
import com.smartgwt.client.widgets.tree.TreeGridField;
import com.smartgwt.client.widgets.tree.events.FolderClosedEvent;
import com.smartgwt.client.widgets.tree.events.FolderClosedHandler;
import com.smartgwt.client.widgets.tree.events.LeafClickEvent;
import com.smartgwt.client.widgets.tree.events.LeafClickHandler;
import cz.incad.pas.editor.client.ds.DcRecordDataSource;

import cz.incad.pas.editor.client.ds.OcrDataSource;
import cz.incad.pas.editor.client.presenter.DigObjectEditorPresenter;
import cz.incad.pas.editor.client.presenter.ImportPresenter;
import cz.incad.pas.editor.client.presenter.ModsFullEditor;
import cz.incad.pas.editor.client.widget.Wizard;
import cz.incad.pas.editor.client.widget.PageMetadataEditor;
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
        
        LOG.info(ClientUtils.format("onModuleLoad:\n module page: %s\n host page: %s"
                + "\n getModuleName: %s\n getPermutationStrongName: %s\n version: %s"
                + "\n Page.getAppDir: %s",
                GWT.getModuleBaseURL(), GWT.getHostPageBaseURL(),
                GWT.getModuleName(), GWT.getPermutationStrongName(), GWT.getVersion(),
                Page.getAppDir()
                ));

        i18nPas = GWT.create(PasEditorMessages.class);

////        tabSet.setBorder("2px solid blue");

        TreeGrid menu = createMenu();

        Canvas menuPlaces = createMenuPlaces(menu);

        final HLayout mainLayout = new HLayout();
        mainLayout.setContents("Editor.mainLayout");
        mainLayout.setShowEdges(true);
        mainLayout.setMargin(4);
        mainLayout.setWidth100();
        mainLayout.setHeight100();
        mainLayout.setMembers(menu, menuPlaces);

        // !!!DO NOT REMOVE datasource init, otherwise SmartGWT does not render anything
        // It relates to ImportBatchItemEditor.createTabs somehow.
        OcrDataSource ocrDataSource = OcrDataSource.getInstance();
        DcRecordDataSource dcRecordDataSource = DcRecordDataSource.getInstance();

//        selectDefaultPlace(menu, "Import/History");
        selectDefaultPlace(menu, "Edit/New Object");
        
        mainLayout.draw();
    }

    private void selectDefaultPlace(TreeGrid menu, String menuPath) {
//        TreeNode find = tree.find("Import/New Batch");
        Tree tree = menu.getTree();
        TreeNode find = tree.find(menuPath);
        int menuItemIdx = menu.getRecordIndex(find);
        System.out.println("### Found: " + find + ", menuItemIdx: " + menuItemIdx + ", name: " + (find != null ? find.getName() : null));
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
            System.out.println("## TreeNode.array: " + treeNode.getName() + ", i: " + i);
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
        final ImportPresenter importPresenter = new ImportPresenter();
//
        final DigObjectEditorPresenter objectEditorPresenter = new DigObjectEditorPresenter();

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
                LOG.fine("menu.getSelectedPaths: " + menu.getSelectedPaths());
                LOG.fine("menu.getSelectedRecord: " + menu.getSelectedRecord());
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
    }
}
//package cz.incad.pas.editor.client;
//
//import cz.incad.pas.editor.shared.FieldVerifier;
//import com.google.gwt.core.client.EntryPoint;
//import com.google.gwt.core.client.GWT;
//import com.google.gwt.event.dom.client.ClickEvent;
//import com.google.gwt.event.dom.client.ClickHandler;
//import com.google.gwt.event.dom.client.KeyCodes;
//import com.google.gwt.event.dom.client.KeyUpEvent;
//import com.google.gwt.event.dom.client.KeyUpHandler;
//import com.google.gwt.user.client.rpc.AsyncCallback;
//import com.google.gwt.user.client.ui.Button;
//import com.google.gwt.user.client.ui.DialogBox;
//import com.google.gwt.user.client.ui.HTML;
//import com.google.gwt.user.client.ui.Label;
//import com.google.gwt.user.client.ui.RootPanel;
//import com.google.gwt.user.client.ui.TextBox;
//import com.google.gwt.user.client.ui.VerticalPanel;
//
///**
// * Entry point classes define <code>onModuleLoad()</code>.
// */
//public class Editor implements EntryPoint {
//  /**
//   * The message displayed to the user when the server cannot be reached or
//   * returns an error.
//   */
//  private static final String SERVER_ERROR = "An error occurred while "
//      + "attempting to contact the server. Please check your network "
//      + "connection and try again.";
//
//  /**
//   * Create a remote service proxy to talk to the server-side Greeting service.
//   */
//  private final GreetingServiceAsync greetingService = GWT.create(GreetingService.class);
//
//  private final Messages messages = GWT.create(Messages.class);
//
//  /**
//   * This is the entry point method.
//   */
//  public void onModuleLoad() {
//    final Button sendButton = new Button( messages.sendButton() );
//    final TextBox nameField = new TextBox();
//    nameField.setText( messages.nameField() );
//    final Label errorLabel = new Label();
//
//    // We can add style names to widgets
//    sendButton.addStyleName("sendButton");
//
//    // Add the nameField and sendButton to the RootPanel
//    // Use RootPanel.get() to get the entire body element
//    RootPanel.get("nameFieldContainer").add(nameField);
//    RootPanel.get("sendButtonContainer").add(sendButton);
//    RootPanel.get("errorLabelContainer").add(errorLabel);
//
//    // Focus the cursor on the name field when the app loads
//    nameField.setFocus(true);
//    nameField.selectAll();
//
//    // Create the popup dialog box
//    final DialogBox dialogBox = new DialogBox();
//    dialogBox.setText("Remote Procedure Call");
//    dialogBox.setAnimationEnabled(true);
//    final Button closeButton = new Button("Close");
//    // We can set the id of a widget by accessing its Element
//    closeButton.getElement().setId("closeButton");
//    final Label textToServerLabel = new Label();
//    final HTML serverResponseLabel = new HTML();
//    VerticalPanel dialogVPanel = new VerticalPanel();
//    dialogVPanel.addStyleName("dialogVPanel");
//    dialogVPanel.add(new HTML("<b>Sending name to the server:</b>"));
//    dialogVPanel.add(textToServerLabel);
//    dialogVPanel.add(new HTML("<br><b>Server replies:</b>"));
//    dialogVPanel.add(serverResponseLabel);
//    dialogVPanel.setHorizontalAlignment(VerticalPanel.ALIGN_RIGHT);
//    dialogVPanel.add(closeButton);
//    dialogBox.setWidget(dialogVPanel);
//
//    // Add a handler to close the DialogBox
//    closeButton.addClickHandler(new ClickHandler() {
//      public void onClick(ClickEvent event) {
//        dialogBox.hide();
//        sendButton.setEnabled(true);
//        sendButton.setFocus(true);
//      }
//    });
//
//    // Create a handler for the sendButton and nameField
//    class MyHandler implements ClickHandler, KeyUpHandler {
//      /**
//       * Fired when the user clicks on the sendButton.
//       */
//      public void onClick(ClickEvent event) {
//        sendNameToServer();
//      }
//
//      /**
//       * Fired when the user types in the nameField.
//       */
//      public void onKeyUp(KeyUpEvent event) {
//        if (event.getNativeKeyCode() == KeyCodes.KEY_ENTER) {
//          sendNameToServer();
//        }
//      }
//
//      /**
//       * Send the name from the nameField to the server and wait for a response.
//       */
//      private void sendNameToServer() {
//        // First, we validate the input.
//        errorLabel.setText("");
//        String textToServer = nameField.getText();
//        if (!FieldVerifier.isValidName(textToServer)) {
//          errorLabel.setText("Please enter at least four characters");
//          return;
//        }
//
//        // Then, we send the input to the server.
//        sendButton.setEnabled(false);
//        textToServerLabel.setText(textToServer);
//        serverResponseLabel.setText("");
//        greetingService.greetServer(textToServer, new AsyncCallback<String>() {
//          public void onFailure(Throwable caught) {
//            // Show the RPC error message to the user
//            dialogBox.setText("Remote Procedure Call - Failure");
//            serverResponseLabel.addStyleName("serverResponseLabelError");
//            serverResponseLabel.setHTML(SERVER_ERROR);
//            dialogBox.center();
//            closeButton.setFocus(true);
//          }
//
//          public void onSuccess(String result) {
//            dialogBox.setText("Remote Procedure Call");
//            serverResponseLabel.removeStyleName("serverResponseLabelError");
//            serverResponseLabel.setHTML(result);
//            dialogBox.center();
//            closeButton.setFocus(true);
//          }
//        });
//      }
//    }
//
//    // Add a handler to send the name to the server
//    MyHandler handler = new MyHandler();
//    sendButton.addClickHandler(handler);
//    nameField.addKeyUpHandler(handler);
//  }
//}
