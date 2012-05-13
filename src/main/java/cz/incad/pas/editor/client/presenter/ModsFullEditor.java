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
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.ds.MetaModelDataSource;
import cz.incad.pas.editor.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.incad.pas.editor.client.ds.ModsCustomDataSource;
import cz.incad.pas.editor.client.ds.mods.PageDataSource;
import cz.incad.pas.editor.client.rpc.ModsGwtRecord;
import cz.incad.pas.editor.client.rpc.ModsGwtServiceAsync;
import cz.incad.pas.editor.client.widget.mods.MonographForm;
import cz.incad.pas.editor.client.widget.mods.MonographUnitForm;
import cz.incad.pas.editor.client.widget.mods.PageForm;
import cz.incad.pas.editor.client.widget.mods.PeriodicalForm;
import cz.incad.pas.editor.client.widget.mods.PeriodicalIssueForm;
import cz.incad.pas.editor.client.widget.mods.PeriodicalVolumeForm;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * TODO: introduce /object/mods(pid) returning text/plain
 * TODO: BUG: 2 Notes note1 and note2 are displayed as note2 and note1 in Notes tab
 *
 * @author Jan Pokorsky
 */
public final class ModsFullEditor {

    private static final Logger LOG = Logger.getLogger(ModsFullEditor.class.getName());
    private static final String TAB_FULL = "FULL_ModsFullEditor";
    private static final String TAB_CUSTOM = "CUSTOM_ModsFullEditor";
    private static final String TAB_XML = "XML_ModsFullEditor";

    private ModsTab modsTab;
    private ModsGwtRecord editedMods;
    private Record editedCustomMods;
    private final VLayout modsContainer;
    private String pid;
    private MetaModelRecord model;
    private TabSet tabSet;
    private DynamicForm customForm;
    private DynamicForm sourceForm;
    private boolean ignoreTabSelection = false;
    private boolean ignoreTabDeselection = false;
    private final PasEditorMessages i18nPas;

    private ModsFullEditor(boolean tabbed, PasEditorMessages i18nPas) {
        this.i18nPas = i18nPas;
        modsContainer = new VLayout();
        modsContainer.setHeight100();
        modsContainer.setWidth100();
        if (tabbed) {
            Tab tabFull = new Tab(verticalTitle(i18nPas.ModsFullEditor_TabFull_Title()));
            tabFull.setID(TAB_FULL);
            tabFull.setPane(modsContainer);

            Tab tabCustom = new Tab(verticalTitle(i18nPas.ModsFullEditor_TabSimple_Title()));
            tabCustom.setID(TAB_CUSTOM);

            Tab tabSource = new Tab(verticalTitle(i18nPas.ModsFullEditor_TabSource_Title()));
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
            sourceForm.setFields(sourceItem);
            tabSource.setPane(sourceForm);

            tabSet = new TabSet();
            tabSet.setTabs(tabCustom, tabFull, tabSource);
            tabSet.setTabBarPosition(Side.LEFT);

            tabSet.addTabSelectedHandler(new TabSelectedHandler() {

                @Override
                public void onTabSelected(TabSelectedEvent event) {
                    if (ignoreTabSelection) {
                        ignoreTabSelection = false;
                        return ;
                    }
                    if (editedCustomMods != null || editedMods != null) {
                        return ;
                    }
                    String tabId = event.getID();
                    ClientUtils.info(LOG, "onTabSelected tabId: %s", tabId);
                    Criteria pidCriteria = new Criteria(ModsCustomDataSource.FIELD_PID, pid);
                    loadTabData(tabId, pidCriteria);
                }
            });

            tabSet.addTabDeselectedHandler(new TabDeselectedHandler() {

                @Override
                public void onTabDeselected(final TabDeselectedEvent event) {
                    if (ignoreTabDeselection) {
                        ignoreTabDeselection = false;
                        return;
                    }
                    String tabId = event.getID();
                    if (TAB_FULL.equals(tabId)) {
                        event.cancel();
                        if (editedMods == null) {
                            return ;
                        }
                        saveFullData(new TabSwitchTask(event.getNewTab()));
                    } else if (TAB_CUSTOM.equals(tabId)) {
                        event.cancel();
                        saveCustomData(new TabSwitchTask(event.getNewTab()));
                    } else if (TAB_XML.equals(tabId)) {
                        editedMods = (ModsGwtRecord) sourceForm.getValue(PageDataSource.FIELD_MODS_TRANSPORT_OBJECT);
                    } else {
                        throw new IllegalStateException("ModsFullEditor.onTabDeselected: unknown tabId: " + tabId);
                    }
                    ClientUtils.info(LOG, "onTabDeselected tab: %s, mods: %s", tabId, editedMods );
                }
            });
        }
    }

    private final class TabSwitchTask implements Runnable {

        private final Tab newTab;

        public TabSwitchTask(Tab newTab) {
            this.newTab = newTab;
        }

        @Override
        public void run() {
            ignoreTabDeselection = true;
            tabSet.selectTab(newTab);
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

    public ModsFullEditor(PasEditorMessages i18nPas) {
        this(true, i18nPas);
    }

    private void loadFull(Criteria pid) {
        final String spid = pid.getAttribute(ModsCustomDataSource.FIELD_PID);
        ModsGwtServiceAsync service = ModsGwtServiceAsync.Util.getInstance();
        service.read(spid, new AsyncCallback<ModsGwtRecord>() {

            @Override
            public void onFailure(Throwable caught) {
                ClientUtils.severe(LOG, "read failed: " + caught.getMessage());
                LOG.log(Level.SEVERE, "fetchRequest.onFailure: " + spid, caught);
                modsContainer.setContents("Loading FAILED!");
            }

            @Override
            public void onSuccess(ModsGwtRecord modsTransport) {
                editedMods = modsTransport;
                ModsCollectionClient mc = modsTransport.getMods();
                ClientUtils.info(LOG, "loadFull mods: %s", mc);
                modsContainer.setContents("");
                loadFull(modsTransport);
            }
        });
    }

    private void loadFull(ModsGwtRecord modsRecord) {
        ModsCollectionClient modsCollection = modsRecord.getMods();
        modsTab = new ModsTab(1, modsCollection);
        VLayout modsLayout = modsTab.getModsLayout();
        modsContainer.setMembers(modsLayout);
    }

    private void loadCustom(Criteria pid, MetaModelRecord model) {
        Criteria criteria = new Criteria(MetaModelDataSource.FIELD_EDITOR, model.getEditorId());
        criteria.addCriteria(pid);
        ClientUtils.info(LOG, "loadCustom pid: %s, editor: %s", pid, model.getEditorId());
        ModsCustomDataSource.getInstance().fetchData(criteria, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                Record[] data = response.getData();
//                ClientUtils.severe(LOG, "Custom data: %s, %s", data.length, ClientUtils.dump(data[0].getJsObj()));
                editedCustomMods = data[0];
                Record rec = editedCustomMods.getAttributeAsRecord(ModsCustomDataSource.FIELD_DATA);
                if (rec != null) {
                    customForm.editRecord(rec);
                }
            }
        });
    }

    private void loadSource(final ModsGwtRecord modsRecord) {
        ClientUtils.info(LOG, "loadSource pid: %s, mods: %s", pid, modsRecord);
        if (modsRecord == null) {
            // XXX replace with rela service
            Record record = new Record();
            record.setAttribute("source", "Here will go XML for " + pid);
            sourceForm.editRecord(record);
            return ;
        }
        // implement this with PageDataSource.fetch(new Criteria("fetch=XML"))
        ModsGwtServiceAsync service = ModsGwtServiceAsync.Util.getInstance();
        service.getXml(modsRecord.getMods(), new AsyncCallback<String>() {

            @Override
            public void onFailure(Throwable caught) {
                throw new IllegalStateException("ModsFullEditor.loadSource", caught);
            }

            @Override
            public void onSuccess(String result) {
                Record record = new Record();
                record.setAttribute("source", result);
                record.setAttribute(PageDataSource.FIELD_MODS_TRANSPORT_OBJECT, modsRecord);
                sourceForm.editRecord(record);
            }
        });
    }

    private void loadTabData(String tabId, Criteria pid) {
        if (TAB_FULL.equals(tabId)) {
            loadFull(pid);
        } else if (TAB_CUSTOM.equals(tabId)) {
            loadCustom(pid, model);
        } else if (TAB_XML.equals(tabId)) {
            loadSource(editedMods);
        } else {
            throw new IllegalStateException("ModsFullEditor.loadTabData: unknown tabId: " + tabId);
        }
    }

    private DynamicForm createCustomForm(MetaModelRecord model) {
        DynamicForm form = null;
        final String editorId = model.getEditorId();
        if (MetaModelDataSource.EDITOR_PAGE.equals(editorId)) {
            form = new PageForm(i18nPas);
        } else if (MetaModelDataSource.EDITOR_PERIODICAL.equals(editorId)) {
            form = new PeriodicalForm(i18nPas);
        } else if (MetaModelDataSource.EDITOR_MONOGRAPH.equals(editorId)) {
            form = new MonographForm(i18nPas);
        } else if (MetaModelDataSource.EDITOR_PERIODICAL_VOLUME.equals(editorId)) {
            form = new PeriodicalVolumeForm(i18nPas);
        } else if (MetaModelDataSource.EDITOR_PERIODICAL_ISSUE.equals(editorId)) {
            form = new PeriodicalIssueForm(i18nPas);
        } else if (MetaModelDataSource.EDITOR_MONOGRAPH_UNIT.equals(editorId)) {
            form = new MonographUnitForm(i18nPas);
        } else {
            ClientUtils.warning(LOG, "Uknown model editor: %s, editor: %s", model.getId(), model.getEditorId());
        }
        return form;
    }

    public void loadData(String pid, MetaModelRecord model) {
        this.pid = pid;
        this.model = model;
        this.editedMods = null;
        this.editedCustomMods = null;
        // XXX custom forms should be cached
        this.customForm = createCustomForm(model);

        // as TabSet.getSelectedTab fires TabSelectedEvent!!!
        ignoreTabSelection = true;
        Tab selectedTab = tabSet.getSelectedTab();
        Tab customTab = tabSet.getTab(TAB_CUSTOM);
        boolean unknownModelEditor = customForm == null;
        if (unknownModelEditor) {
            tabSet.disableTab(TAB_CUSTOM);
        } else {
            tabSet.enableTab(TAB_CUSTOM);
        }
        ClientUtils.info(LOG, "customTab.setDisabled: %s", unknownModelEditor);
        if (customForm != customTab.getPane()) {
            // set new custom form
            customTab.setPane(customForm);
        }
        if (selectedTab == customTab && unknownModelEditor) {
            // unknown model, use full form
            ignoreTabSelection = ignoreTabDeselection = true;
            tabSet.selectTab(TAB_FULL);
            selectedTab = tabSet.getTab(TAB_FULL);
        } else if (!unknownModelEditor && selectedTab != customTab) {
            ignoreTabSelection = ignoreTabDeselection = true;
            tabSet.selectTab(TAB_CUSTOM);
            selectedTab = tabSet.getTab(TAB_CUSTOM);
        }

        Criteria pidCriteria = new Criteria(ModsCustomDataSource.FIELD_PID, pid);
        ClientUtils.info(LOG, "loadData tabId: %s", selectedTab.getID());
        loadTabData(selectedTab.getID(), pidCriteria);
    }

    public void save(Runnable callback) {
        if (!saveCustomData(callback)) {
            saveFullData(callback);
        }
    }

    private boolean saveFullData(Runnable callback) {
        if (editedMods == null) {
            return false;
        }
        ModsTypeClient mc = modsTab.getMods();
        ModsCollectionClient mcc = new ModsCollectionClient();
        mcc.setMods(Arrays.asList(mc));
        editedMods.setMods(mcc);
        saveFullData(pid, editedMods, callback);
        return true;
    }

    private void saveFullData(String pid, final ModsGwtRecord mods, final Runnable callback) {
        editedMods = null;
        ModsGwtServiceAsync service = ModsGwtServiceAsync.Util.getInstance();
        service.write(pid, mods, new AsyncCallback<String>() {

            @Override
            public void onFailure(Throwable caught) {
                editedMods = mods;
                modsContainer.setContents("Save FAILED!");
                LOG.log(Level.SEVERE, "data saving failed", caught);
            }

            @Override
            public void onSuccess(String newOrExistingId) {
                ModsFullEditor.this.pid = newOrExistingId;
                if (callback != null) {
                    callback.run();
                }
            }
        });
    }

    private boolean saveCustomData(final Runnable callback) {
        if (editedCustomMods == null) {
            return false;
        }
        // do not use customForm.getValuesAsRecord()
        Record r = new Record(customForm.getValues());
//        ClientUtils.severe(LOG, "saveCustomData: %s", ClientUtils.dump(r.getJsObj()));

        final Record toSave = editedCustomMods;
        editedCustomMods = null;
        toSave.setAttribute(ModsCustomDataSource.FIELD_DATA, r);
//        ClientUtils.severe(LOG, "saveCustomData.ready: %s", ClientUtils.dump(toSave.getJsObj()));
        ModsCustomDataSource.getInstance().updateData(toSave, new DSCallback() {

            @Override
            public void execute(DSResponse response, Object rawData, DSRequest request) {
                if (RPCResponse.STATUS_SUCCESS != response.getStatus()) {
                    editedCustomMods = toSave;
                    return ;
                }
                if (callback != null) {
                    callback.run();
                }
            }
        });
        return true;
    }

    public Canvas getUI() {
        return tabSet != null ? tabSet : modsContainer;
    }

}
