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
package cz.incad.pas.editor.client.presenter;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.rpc.RPCResponse;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.tab.events.TabDeselectedEvent;
import com.smartgwt.client.widgets.tab.events.TabDeselectedHandler;
import com.smartgwt.client.widgets.tab.events.TabSelectedEvent;
import com.smartgwt.client.widgets.tab.events.TabSelectedHandler;
import cz.fi.muni.xkremser.editor.client.mods.ModsCollectionClient;
import cz.fi.muni.xkremser.editor.client.mods.ModsTypeClient;
import cz.fi.muni.xkremser.editor.client.view.ModsTab;
import cz.incad.pas.editor.client.ds.mods.PageDataSource;
import cz.incad.pas.editor.client.rpc.ModsGwtServiceAsync;
import cz.incad.pas.editor.client.widget.mods.ModsPage;
import java.util.Arrays;
import java.util.logging.Logger;

/**
 *
 * TODO: a) implement Save button
 * TODO: b) implement change detection instead of Save button
 * TODO: BUG: 2 Notes note1 and note2 are displayed as note2 and note1 in Notes tab
 *
 * @author Jan Pokorsky
 */
public final class ModsFullEditor {

    private static final Logger LOG = Logger.getLogger(ModsFullEditor.class.getName());
    private static final String TAB_FULL = "FULL_ModsFullEditor";
    private static final String TAB_SIMPLE = "SIMPLE_ModsFullEditor";
    private static final String TAB_XML = "XML_ModsFullEditor";

    private ModsTab modsTab;
    private ModsCollectionClient editedMods;
    private final VLayout modsContainer;
    private String pid;
    private TabSet tabSet;
    private ModsPage pageForm;
    private DynamicForm sourceForm;

    private ModsFullEditor(boolean tabbed) {
        modsContainer = new VLayout();
        modsContainer.setHeight100();
        modsContainer.setWidth100();
        if (tabbed) {
            Tab tabFull = new Tab(verticalTitle("Full"));
            tabFull.setID(TAB_FULL);
            tabFull.setPane(modsContainer);

            Tab tabSimple = new Tab(verticalTitle("Basic"));
            tabSimple.setID(TAB_SIMPLE);
            pageForm = new ModsPage();
            tabSimple.setPane(pageForm);

            Tab tabSource = new Tab(verticalTitle("XML"));
            tabSource.setID(TAB_XML);
            sourceForm = new DynamicForm();
            sourceForm.setCanEdit(false);
            sourceForm.setWidth100();
            sourceForm.setHeight100();
            sourceForm.setColWidths("*");
            sourceForm.setNumCols(1);
            TextAreaItem sourceItem = new TextAreaItem("source");
            sourceItem.setWidth("*");
            sourceItem.setHeight("*");
            sourceItem.setShowTitle(false);
//            sourceItem.setDisabled(true);
            sourceForm.setFields(sourceItem);
            tabSource.setPane(sourceForm);

            tabSet = new TabSet();
            tabSet.setTabs(tabSimple, tabFull, tabSource);
            tabSet.setTabBarPosition(Side.LEFT);
//            tabSet.selectTab(tabFull);

            tabSet.addTabSelectedHandler(new TabSelectedHandler() {

                @Override
                public void onTabSelected(TabSelectedEvent event) {
                    final Canvas tabPane = event.getTabPane();
                    if (modsContainer == tabPane) {
                        loadFull(editedMods);
                    } else if (pageForm == tabPane) {
                        loadBasic(editedMods);
                    } else if (sourceForm == tabPane) {
                        loadSource(editedMods);
                    }
                }
            });

            tabSet.addTabDeselectedHandler(new TabDeselectedHandler() {

                @Override
                public void onTabDeselected(TabDeselectedEvent event) {
                    final Canvas tabPane = event.getTabPane();
                    if (modsContainer == tabPane) {
                        ModsTypeClient mc = modsTab.getMods();
                        ModsCollectionClient mcc = new ModsCollectionClient();
                        mcc.setMods(Arrays.asList(mc));
                        editedMods = mcc;
                    } else if (pageForm == tabPane) {
                        // do not use pageForm.getValuesAsRecord()
//                        Record r = pageForm.getValuesAsRecord();
                        Record r = new Record(pageForm.getValues());
                        ModsCollectionClient modsCollection = PageDataSource.getInstance().convert(r);
                        editedMods = modsCollection;
                    } else if (sourceForm == tabPane) {
                        editedMods = (ModsCollectionClient) sourceForm.getValue(PageDataSource.FIELD_MODS_OBJECT);
                    }
                }
            });
        }
    }

    private static String verticalTitle(String title) {
        final String newLine = "<br>";
        StringBuilder sb = new StringBuilder(title.length() * (newLine.length() + 1));
        for (int i = 0; i < title.length(); i++) {
            sb.append(title.charAt(i));
            sb.append(newLine);
        }
        return sb.toString();
    }

    public ModsFullEditor() {
        this(true);
    }

    private void loadFull(Criteria pid) {
        PageDataSource.getInstance().fetchData(pid, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RPCResponse.STATUS_SUCCESS == response.getStatus()) {
                    modsContainer.setContents("");
                    ModsCollectionClient mc = (ModsCollectionClient) response.getAttributeAsObject(PageDataSource.FIELD_MODS_OBJECT);
                    loadFull(mc);
                } else {
                    modsContainer.setContents("Loading FAILED!");
                }
            }
        });
    }

    private void loadFull(ModsCollectionClient modsCollection) {
        modsTab = new ModsTab(1, modsCollection);
        VLayout modsLayout = modsTab.getModsLayout();
        modsContainer.setMembers(modsLayout);
    }

    private void loadBasic(Criteria pid) {
        pageForm.fetchData(pid);
    }

    private void loadBasic(ModsCollectionClient modsCollection) {
        Record[] data = PageDataSource.getInstance().convert(pid, modsCollection);
        pageForm.editRecord(data[0]);
        // XXX if it triggers fetch then use toMap
//        pageForm.setValues(data[0].toMap());
    }

    private void loadSource(final ModsCollectionClient modsCollection) {
        // implement this with PageDataSource.fetch(new Criteria("fetch=XML"))
        ModsGwtServiceAsync service = ModsGwtServiceAsync.Util.getInstance();
        service.getXml(modsCollection, new AsyncCallback<String>() {

            @Override
            public void onFailure(Throwable caught) {
                throw new UnsupportedOperationException("Not supported yet.");
            }

            @Override
            public void onSuccess(String result) {
                Record record = new Record();
                record.setAttribute("source", result);
                record.setAttribute(PageDataSource.FIELD_MODS_OBJECT, modsCollection);
                sourceForm.editRecord(record);
            }
        });
    }

    private void loadTabData(String tabId, Criteria pid) {
        if (TAB_FULL.equals(tabId)) {
            if (pid == null) {
                loadFull(editedMods);
            } else {
                loadFull(pid);
            }
        } else if (TAB_SIMPLE.equals(tabId)) {
            if (pid == null) {
                loadBasic(editedMods);
            } else {
                loadBasic(pid);
            }
        } else if (TAB_XML.equals(tabId)) {
            loadSource(editedMods);
        } else {
            // XXX load default form
            LOG.severe("Missing tab - unsupported state!");
        }
    }

    public void loadData(String pid) {
        this.pid = pid;
        Criteria pidCriteria = new Criteria(PageDataSource.FIELD_PID, pid);
        loadTabData(tabSet.getSelectedTab().getID(), pidCriteria);

        //        modsContainer.setMembers();
        //        modsContainer.setContents("Loading MODS data...");
        //        ModsGwtServiceAsync service = ModsGwtServiceAsync.Util.getInstance();
        //        service.read(pid, new AsyncCallback<ModsCollectionClient>() {
        //
        //            @Override
        //            public void onFailure(Throwable caught) {
        //                modsContainer.setContents("Loading FAILED!");
        //                GWT.log("data loading failed", caught);
        //            }
        //
        //            @Override
        //            public void onSuccess(ModsCollectionClient result) {
        //                modsTab = new ModsTab(1, result);
        //                VLayout modsLayout = modsTab.getModsLayout();
        //                modsContainer.setMembers(modsLayout);
        //            }
        //        });
    }

    public void saveData() {
        modsContainer.setMembers();
        modsContainer.setContents("Saving MODS data...");
        ModsTypeClient mc = this.modsTab.getMods();
        ModsCollectionClient mcc = new ModsCollectionClient();
        mcc.setMods(Arrays.asList(mc));
        ModsGwtServiceAsync service = ModsGwtServiceAsync.Util.getInstance();
        service.write(this.pid, mcc, new AsyncCallback<String>() {

            @Override
            public void onFailure(Throwable caught) {
                modsContainer.setContents("Save FAILED!");
                GWT.log("data saving failed", caught);
            }

            @Override
            public void onSuccess(String newOrExistingId) {
                ModsFullEditor.this.pid = newOrExistingId;
//                modsContainer.setMembers(content);
                loadData(newOrExistingId);
            }
        });
    }

//    public void newData() {
//        this.pid = null;
//        this.modsTab = new ModsTab(1);
//        this.modsContainer.setMembers(modsTab.getModsLayout());
//    }

    public Canvas getUI() {
//        modsTab = new ModsTab(1);
//        modsContainer.setMembers(modsTab.getModsLayout());
        return tabSet != null ? tabSet : modsContainer;
    }

}
