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
package cz.incad.pas.editor.client.presenter;

import com.google.gwt.event.shared.HandlerRegistration;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.Page;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.events.SubmitValuesEvent;
import com.smartgwt.client.widgets.form.events.SubmitValuesHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStripSeparator;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.action.AbstractAction;
import cz.incad.pas.editor.client.action.ActionEvent;
import cz.incad.pas.editor.client.action.Actions;
import cz.incad.pas.editor.client.action.Actions.ActionSource;
import cz.incad.pas.editor.client.action.RefreshAction.Refreshable;
import cz.incad.pas.editor.client.action.SaveAction;
import cz.incad.pas.editor.client.action.Selectable;
import cz.incad.pas.editor.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.incad.pas.editor.client.ds.RelationDataSource;
import cz.incad.pas.editor.client.widget.BatchDatastreamEditor;
import cz.incad.pas.editor.client.widget.DatastreamEditor;
import cz.incad.pas.editor.client.widget.StatusView;
import java.util.logging.Logger;

/**
 * Edits MODS data in multiple custom editor (all fields, selected fields, plain XML).
 *
 * @author Jan Pokorsky
 */
public final class ModsMultiEditor implements BatchDatastreamEditor, Refreshable, Selectable<DigitalObject> {

    private static final Logger LOG = Logger.getLogger(ModsMultiEditor.class.getName());

    private final VLayout uiContainer;
    private final ModsCustomEditor modsCustomEditor;
    private final ModsFullEditor modsFullEditor;
    private final ModsXmlEditor modsSourceEditor;
    private final ModsBatchEditor modsBatchEditor;
    private DatastreamEditor activeEditor;
    private final ClientMessages i18n;
    private DigitalObject[] digitalObjects;
    private Canvas customEditorButton;
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
        modsFullEditor = new ModsFullEditor(i18n);
        modsCustomEditor = new ModsCustomEditor(i18n);
        modsSourceEditor = new ModsXmlEditor();
        modsBatchEditor = new ModsBatchEditor(i18n);
        actionSource = new ActionSource(this);
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
            loadCustom(items[0]);
        } else {
            String modelId = "model:page";
            boolean unsupportedBatch = false;
            for (DigitalObject dobj : items) {
                if (!modelId.equals(dobj.getModelId())) {
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
        } else if (activeEditor == modsFullEditor) {
            saveFullData(callback);
        } else if (activeEditor == modsBatchEditor) {
            saveBatchData(callback);
        } else {
            callback.execute(Boolean.TRUE);
        }
    }

    private void save() {
        save(new BooleanCallback() {

            @Override
            public void execute(Boolean value) {
                if (value != null && value) {
                    StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
                }
            }
        });
    }

    /**
     * Notifies other data sources to update its caches with object label.
     */
    private BooleanCallback wrapSaveCallback(final BooleanCallback callback) {
        BooleanCallback bc = new BooleanCallback() {

            @Override
            public void execute(Boolean value) {
                RelationDataSource.getInstance().fireRelationChange(digitalObjects[0].getPid());
                callback.execute(value);
            }
        };
        return bc;
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
            customEditorButton = Actions.asIconButton(
                new SwitchAction(modsCustomEditor,
                        i18n.ModsMultiEditor_TabSimple_Title(),
                        Page.getAppDir() + "images/silk/16/application_form_edit.png",
                        i18n.ModsMultiEditor_TabSimple_Hint()
                ), actionSource),
            Actions.asIconButton(
                new SwitchAction(modsFullEditor,
                        i18n.ModsMultiEditor_TabFull_Title(),
                        Page.getAppDir() + "images/silk/16/container.png",
                        i18n.ModsMultiEditor_TabFull_Hint()
                ), actionSource),
            Actions.asIconButton(
                new SwitchAction(modsSourceEditor,
                        i18n.ModsMultiEditor_TabSource_Title(),
                        Page.getAppDir() + "images/oxygen/16/application_xml.png",
                        i18n.ModsMultiEditor_TabSource_Hint()
                ), actionSource),
            new ToolStripSeparator(),
            Actions.asIconButton(saveAction, actionSource)
        };

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
        if (tab == modsFullEditor) {
            loadFull(digitalObject);
        } else if (tab == modsSourceEditor) {
            loadSource(digitalObject);
        } else {
            loadCustom(digitalObject);
        }
    }

    private void loadCustom(DigitalObject digitalObject) {
        modsCustomEditor.edit(digitalObject);
        if (modsCustomEditor.getCustomForm() != null) {
            if (submitCustomValuesRegistration != null) {
                submitCustomValuesRegistration.removeHandler();
            }
            submitCustomValuesRegistration = modsCustomEditor.getCustomForm()
                    .addSubmitValuesHandler(submitCustomValuesHandler);
            setActiveEditor(modsCustomEditor);
            setEnabledCustom(true);
        } else {
            // unknown model, use full form
            setEnabledCustom(false);
            loadFull(digitalObject);
        }
    }

    private void loadFull(DigitalObject digitalObject) {
        setActiveEditor(modsFullEditor);
        modsFullEditor.edit(digitalObject);
    }

    private void loadSource(DigitalObject digitalObject) {
        setActiveEditor(modsSourceEditor);
        modsSourceEditor.edit(digitalObject);
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
            activeEditor = newEditor;
        }
    }

    private void setEnabledCustom(boolean enabled) {
        if (customEditorButton != null) {
            customEditorButton.setVisible(enabled);
        }
    }

    private void saveFullData(BooleanCallback callback) {
        modsFullEditor.save(callback);
    }

    private void saveCustomData(BooleanCallback callback) {
        modsCustomEditor.save(callback);
    }

    private void saveBatchData(BooleanCallback callback) {
        modsBatchEditor.save(callback);
    }

    private final class SwitchAction extends AbstractAction {

        private final DatastreamEditor tab;

        public SwitchAction(DatastreamEditor tab, String title, String icon, String tooltip) {
            super(title, icon, tooltip);
            this.tab = tab;
        }

        @Override
        public void performAction(ActionEvent event) {
            loadTabData(tab, digitalObjects[0]);
        }

        @Override
        public boolean accept(ActionEvent event) {
            DigitalObject[] selections = getSelection();
            return selections != null && selections.length == 1;
        }

    }

}
