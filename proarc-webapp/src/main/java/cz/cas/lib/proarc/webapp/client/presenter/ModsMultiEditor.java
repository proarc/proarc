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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.client.presenter;

import com.google.gwt.event.shared.HandlerRegistration;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.Page;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.events.HasSubmitValuesHandlers;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.IconMenuButton;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import cz.cas.lib.proarc.common.object.chronicle.ChroniclePlugin;
import cz.cas.lib.proarc.common.object.collectionOfClippings.CollectionOfClippingsPlugin;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.action.AbstractAction;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.Actions.ActionSource;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.action.SaveAction;
import cz.cas.lib.proarc.webapp.client.action.Selectable;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource.DescriptionMetadata;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource.DescriptionSaveHandler;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource;
import cz.cas.lib.proarc.webapp.client.ds.WorkflowModsCustomDataSource;
import cz.cas.lib.proarc.webapp.client.event.EditorLoadEvent;
import cz.cas.lib.proarc.webapp.client.event.HasEditorLoadHandlers;
import cz.cas.lib.proarc.webapp.client.widget.AbstractDatastreamEditor;
import cz.cas.lib.proarc.webapp.client.widget.BatchDatastreamEditor;
import cz.cas.lib.proarc.webapp.client.widget.CatalogBrowser;
import cz.cas.lib.proarc.webapp.client.widget.DatastreamEditor;
import cz.cas.lib.proarc.webapp.client.widget.StatusView;
import java.util.HashSet;
import java.util.logging.Logger;

/**
 * Edits MODS data in multiple custom editor (all fields, selected fields, plain XML).
 *
 * @author Jan Pokorsky
 */
// XXX rename to DescriptionMultiEditor
public final class ModsMultiEditor extends AbstractDatastreamEditor implements
        BatchDatastreamEditor, Refreshable, Selectable<DigitalObject> {

    private static final Logger LOG = Logger.getLogger(ModsMultiEditor.class.getName());
    private static final HashSet<String> ACCEPT_BATCH_MODELS = new HashSet<>();
    static {
        ACCEPT_BATCH_MODELS.add("model:page");
        ACCEPT_BATCH_MODELS.add("model:ndkpage");
        ACCEPT_BATCH_MODELS.add("model:oldprintpage");
    }

    private final VLayout uiContainer;
    private final ModsCustomEditor modsCustomEditor;
    private final ModsXmlEditor modsSourceEditor;
    private final ModsBatchEditor modsBatchEditor;
    private final CatalogBrowser catalogBrowser;
    private DatastreamEditor activeEditor;
    private final ClientMessages i18n;
    private DigitalObject[] digitalObjects;
    private MenuItem customEditorButton;
    /**
     * The simplified simple form. See issue 321.
     */
    private MenuItem customEditorButton2;
    private final ActionSource actionSource;
    private HandlerRegistration submitCustomValuesRegistration;
    private final SubmitValuesHandler submitCustomValuesHandler = new SubmitValuesHandler() {
        @Override
        public void onSubmitValues(SubmitValuesEvent event) {
            save();
        }
    };

    public ModsMultiEditor(ClientMessages i18n) {
        this.i18n = i18n;
        uiContainer = new VLayout();
        modsCustomEditor = new ModsCustomEditor(i18n);
        modsSourceEditor = new ModsXmlEditor(i18n);
        modsBatchEditor = new ModsBatchEditor(i18n);
        catalogBrowser = new CatalogBrowser(i18n);
        catalogBrowser.setCompactUi(true);
        actionSource = new ActionSource(this);
        attachDatastreamEditor(modsCustomEditor);
    }

    /**
     * Forwards editor events.
     */
    private void attachDatastreamEditor(DatastreamEditor deditor) {
        if (deditor instanceof HasEditorLoadHandlers) {
            ((HasEditorLoadHandlers) deditor).addEditorLoadHandler((EditorLoadEvent evt) -> {
                fireEvent(evt);
            });
        }
    }

    @Override
    public void edit(DigitalObject digitalObject) {
        ClientUtils.fine(LOG, "edit %s", digitalObject);
        DigitalObject[] dobjs = digitalObject == null ? null : new DigitalObject[] { digitalObject };
        edit(dobjs);
    }

    @Override
    public void edit(DigitalObject[] items) {
        this.digitalObjects = items;
        if (items == null || items.length == 0) {
            // show nothing or throw exception!
        } else if (items.length == 1) {
            loadTabData(modsCustomEditor, items[0]);
        } else {
            String firstModelId = null;
            boolean unsupportedBatch = false;
            for (DigitalObject dobj : items) {
                if (firstModelId == null) {
                    firstModelId = dobj.getModelId();
                }
                String currentModelId = dobj.getModelId();
                if (!ACCEPT_BATCH_MODELS.contains(currentModelId) || !firstModelId.equals(currentModelId)) {
                    unsupportedBatch = true;
                    break;
                }
            }
            if (unsupportedBatch) {
                setActiveEditor(null);
            } else {
                loadBatch();
            }
        }
        actionSource.fireEvent();
    }

    @Override
    public void focus() {
        if (activeEditor != null) {
            activeEditor.focus();
        }
    }

    public void save(BooleanCallback callback) {
        callback = wrapSaveCallback(callback);
        if (activeEditor == modsCustomEditor) {
            saveCustomData(callback);
        } else if (activeEditor == modsBatchEditor) {
            saveBatchData(callback);
        } else if (activeEditor == catalogBrowser) {
            saveCatalogData(callback);
        } else if (activeEditor == modsSourceEditor) {
            saveXmlData(callback);
        } else {
            callback.execute(Boolean.TRUE);
        }
    }

    private void save() {
        save((Boolean value) -> {
            if (value != null && value) {
                StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
            }
        });
    }

    /**
     * Notifies other data sources to update its caches with object label.
     */
    private BooleanCallback wrapSaveCallback(final BooleanCallback callback) {
        return (Boolean value) -> {
            if (value != null && value) {
                RelationDataSource.getInstance().fireRelationChange(digitalObjects[0].getPid());
            }
            callback.execute(value);
        };
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getCapability(Class<T> clazz) {
        T c = null;
        if (Refreshable.class.equals(clazz) || BatchDatastreamEditor.class.equals(clazz)) {
            c = (T) this;
        }
        return c;
    }

    @Override
    public Canvas[] getToolbarItems() {
        SaveAction saveAction = new SaveAction(i18n) {

            @Override
            public void performAction(ActionEvent event) {
                save();
            }

            @Override
            public boolean accept(ActionEvent event) {
                return activeEditor != null;
            }
        };

        return new Canvas[] {
            createModsMenu(),
            Actions.asIconButton(saveAction, actionSource)
        };

    }

    private IconMenuButton createModsMenu() {
        IconMenuButton btnMods = Actions.asIconMenuButton(new AbstractAction(
                i18n.ModsMultiEditor_ActionMods_Title(), "[SKIN]/actions/edit.png", null) {

            @Override
            public void performAction(ActionEvent event) {
            }

            @Override
            public boolean accept(ActionEvent event) {
                DigitalObject[] selections = getSelection();
                return selections != null && selections.length == 1;
            }
        }, actionSource);

        Menu menuMods = Actions.createMenu();
        menuMods.addItem(Actions.asMenuItem(
                new SwitchAction(modsCustomEditor,
                        i18n.ModsMultiEditor_TabSimple_Title(),
                        Page.getAppDir() + "images/silk/16/application_form_edit.png",
                        i18n.ModsMultiEditor_TabSimple_Hint()
                ) {

                    @Override
                    public void performAction(ActionEvent event) {
                        modsCustomEditor.setFormPrefix("");
                        super.performAction(event);
                    }
                }, actionSource, false));
        customEditorButton = menuMods.getItem(0);

        menuMods.addItem(Actions.asMenuItem(
                new SwitchAction(modsCustomEditor,
                        i18n.ModsMultiEditor_TabSimpleSimple_Title(),
                        Page.getAppDir() + "images/silk/16/application_form_edit.png",
                        i18n.ModsMultiEditor_TabSimpleSimple_Hint()
                ) {

                    @Override
                    boolean accept(DigitalObject obj) {
                        return "model:bdmarticle".equals(obj.getModelId());
                    }

                    @Override
                    public void performAction(ActionEvent event) {
                        modsCustomEditor.setFormPrefix("simple:");
                        super.performAction(event);
                    }

                }, actionSource, false));
        customEditorButton2 = menuMods.getItem(1);
        menuMods.addItem(Actions.asMenuItem(
                new SwitchAction(modsCustomEditor,
                        i18n.ModsMultiEditor_TabComplex_Title(),
                        Page.getAppDir() + "images/silk/16/application_form_edit.png",
                        i18n.ModsMultiEditor_TabComplex_Hint()
                        ) {
                    @Override
                    boolean accept(DigitalObject obj) {
                        return ChroniclePlugin.MODEL_CHRONICLETITLE.equals(obj.getModelId())
                                || ChroniclePlugin.MODEL_CHRONICLEVOLUME.equals(obj.getModelId())
                                || ChroniclePlugin.MODEL_CHRONICLESUPPLEMENT.equals(obj.getModelId())
                                || CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_VOLUME.equals(obj.getModelId())
                                || CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_TITLE.equals(obj.getModelId());
                    }

                    @Override
                    public void performAction(ActionEvent event) {
                        modsCustomEditor.setFormPrefix("complex:");
                        super.performAction(event);
                    }

                }, actionSource, false));
        customEditorButton2 = menuMods.getItem(2);

        menuMods.addItem(Actions.asMenuItem(
                new SwitchAction(modsSourceEditor,
                        i18n.ModsMultiEditor_TabSource_Title(),
                        Page.getAppDir() + "images/oxygen/16/application_xml.png",
                        i18n.ModsMultiEditor_TabSource_Hint()
                ), actionSource, false));
        menuMods.addItem(Actions.asMenuItem(
                new SwitchAction(catalogBrowser,
                        i18n.ModsMultiEditor_TabCatalog_Title(),
                        "[SKIN]/DatabaseBrowser/data.png",
                        i18n.ModsMultiEditor_TabCatalog_Hint()
                ), actionSource, false));
        btnMods.setMenu(menuMods);
        return btnMods;
    }

    @Override
    public Canvas getUI() {
        return uiContainer;
    }

    @Override
    public void refresh() {
        if (activeEditor != null) {
            Refreshable refreshable = activeEditor.getCapability(Refreshable.class);
            if (refreshable != null) {
                refreshable.refresh();
            } else {
                loadTabData(activeEditor, digitalObjects[0]);
            }
        }
    }

    @Override
    public DigitalObject[] getSelection() {
        return digitalObjects;
    }

    private void loadTabData(DatastreamEditor tab, DigitalObject digitalObject) {
        setEnabledCustom2();
        if (tab == modsCustomEditor) {
            loadCustom(digitalObject);
        } else {
            setActiveEditor(tab);
            tab.edit(digitalObject);
        }
    }

    private void loadCustom(DigitalObject digitalObject) {
        modsCustomEditor.edit(digitalObject);
        if (modsCustomEditor.getCustomForm() != null) {
            setActiveEditor(modsCustomEditor);
            setEnabledCustom(true);
        } else if (!modsCustomEditor.getFormPrefix().isEmpty()) {
            modsCustomEditor.setFormPrefix("");
            loadCustom(digitalObject);
        } else {
            // unknown model, use source form
            setEnabledCustom(false);
            loadTabData(modsSourceEditor, digitalObject);
        }
    }

    private void loadBatch() {
        if (activeEditor != modsBatchEditor) {
            modsBatchEditor.refresh();
        }
        setActiveEditor(modsBatchEditor);
        modsBatchEditor.edit(digitalObjects);
    }

    private void setActiveEditor(DatastreamEditor newEditor) {
        if (newEditor != activeEditor) {
            if (newEditor != null) {
                uiContainer.setMembers(newEditor.getUI());
            } else {
                uiContainer.setMembers(new Canvas[0]);
            }
            if (submitCustomValuesRegistration != null) {
                submitCustomValuesRegistration.removeHandler();
            }
            if (newEditor instanceof HasSubmitValuesHandlers) {
                submitCustomValuesRegistration = ((HasSubmitValuesHandlers) newEditor)
                        .addSubmitValuesHandler(submitCustomValuesHandler);
            }
            activeEditor = newEditor;
        }
    }

    private void setEnabledCustom(boolean enabled) {
        if (customEditorButton != null) {
            customEditorButton.setEnabled(enabled);
        }
    }

    private void setEnabledCustom2() {
        if (customEditorButton2 != null) {
            DigitalObject[] selection = getSelection();
            boolean enable = selection != null && selection.length == 1
                    && "model:bdmarticle".equals(selection[0].getModelId());
            customEditorButton2.setEnabled(enable);
        }
    }

    private void saveCustomData(BooleanCallback callback) {
        modsCustomEditor.save(callback);
    }

    private void saveXmlData(BooleanCallback callback) {
        modsSourceEditor.save(callback, true, SaveAction.SaveValidation.ASK);
    }

    private void saveBatchData(BooleanCallback callback) {
        modsBatchEditor.save(callback);
    }

    private void saveCatalogData(final BooleanCallback callback) {
        String mods = catalogBrowser.getMods();
        DescriptionSaveHandler descriptionSaveHandler = new DescriptionSaveHandler() {

            @Override
            protected void onSave(DescriptionMetadata dm) {
                super.onSave(dm);
                callback.execute(Boolean.TRUE);
            }

            @Override
            protected void onError() {
                super.onError();
                callback.execute(Boolean.FALSE);
            }

        };

        if (digitalObjects != null && digitalObjects.length > 0 && digitalObjects[0] != null && digitalObjects[0].getWorkflowJobId() != null) {
            WorkflowModsCustomDataSource.getInstance().saveXmlDescription(digitalObjects[0], mods, descriptionSaveHandler);
        } else {
            ModsCustomDataSource.getInstance().saveXmlDescription(digitalObjects[0], mods, descriptionSaveHandler);
        }

    }

    private class SwitchAction extends AbstractAction {

        private final DatastreamEditor tab;
        private final String format;

        public SwitchAction(DatastreamEditor tab, String title, String icon, String tooltip) {
            this(tab, title, icon, tooltip, null);
        }

        public SwitchAction(DatastreamEditor tab, String title, String icon, String tooltip, String format) {
            super(title, icon, tooltip);
            this.tab = tab;
            this.format = format;
        }

        @Override
        public void performAction(ActionEvent event) {
            loadTabData(tab, digitalObjects[0]);
        }

        @Override
        public boolean accept(ActionEvent event) {
            DigitalObject[] selections = getSelection();
            return selections != null && selections.length == 1 && accept(selections[0]);
        }

        boolean accept(DigitalObject obj) {
            return acceptFormat(obj);
        }

        boolean acceptFormat(DigitalObject obj) {
            return format == null || format != null && format.equals(obj.getModel().getMetadataFormat());
        }

    }

}
