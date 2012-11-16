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

import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.Page;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStripSeparator;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.action.AbstractAction;
import cz.incad.pas.editor.client.action.ActionEvent;
import cz.incad.pas.editor.client.action.Actions;
import cz.incad.pas.editor.client.action.RefreshAction.Refreshable;
import cz.incad.pas.editor.client.action.SaveAction;
import cz.incad.pas.editor.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.incad.pas.editor.client.widget.DatastreamEditor;
import cz.incad.pas.editor.client.widget.StatusView;
import java.util.logging.Logger;

/**
 * Edits MODS data in multiple custom editor (all fields, selected fields, plain XML).
 *
 * @author Jan Pokorsky
 */
public final class ModsMultiEditor implements DatastreamEditor, Refreshable {

    private static final Logger LOG = Logger.getLogger(ModsMultiEditor.class.getName());

    private final VLayout uiContainer;
    private final ModsCustomEditor modsCustomEditor;
    private final ModsFullEditor modsFullEditor;
    private final ModsXmlEditor modsSourceEditor;
    private DatastreamEditor activeEditor;
    private final ClientMessages i18n;
    private String pid;
    private MetaModelRecord model;
    private Canvas customEditorButton;

    public ModsMultiEditor(ClientMessages i18n) {
        this.i18n = i18n;
        uiContainer = new VLayout();
        modsFullEditor = new ModsFullEditor(i18n);
        modsCustomEditor = new ModsCustomEditor(i18n);
        modsSourceEditor = new ModsXmlEditor();
    }

    @Override
    public void edit(String pid, String batchId, MetaModelRecord model) {
        this.pid = pid;
        this.model = model;
        ClientUtils.fine(LOG, "edit pid: %s", pid);
        loadCustom(pid, model);
    }

    public void save(BooleanCallback callback) {
        if (activeEditor == modsCustomEditor) {
            saveCustomData(callback);
        } else if (activeEditor == modsFullEditor) {
            saveFullData(callback);
        } else {
            callback.execute(Boolean.TRUE);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getCapability(Class<T> clazz) {
        T c = null;
        if (Refreshable.class.equals(clazz)) {
            c = (T) this;
        }
        return c;
    }

    @Override
    public Canvas[] getToolbarItems() {
        SaveAction saveAction = new SaveAction(i18n) {

            @Override
            public void performAction(ActionEvent event) {
                save(new BooleanCallback() {

                    @Override
                    public void execute(Boolean value) {
                        if (value != null && value) {
                            StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
                        }
                    }
                });
            }
        };
        return new Canvas[] {
            customEditorButton = Actions.asIconButton(
                new SwitchAction(modsCustomEditor,
                        i18n.ModsMultiEditor_TabSimple_Title(),
                        Page.getAppDir() + "images/silk/16/application_form_edit.png",
                        i18n.ModsMultiEditor_TabSimple_Hint()
                ), this),
            Actions.asIconButton(
                new SwitchAction(modsFullEditor,
                        i18n.ModsMultiEditor_TabFull_Title(),
                        Page.getAppDir() + "images/silk/16/container.png",
                        i18n.ModsMultiEditor_TabFull_Hint()
                ), this),
            Actions.asIconButton(
                new SwitchAction(modsSourceEditor,
                        i18n.ModsMultiEditor_TabSource_Title(),
                        Page.getAppDir() + "images/oxygen/16/application_xml.png",
                        i18n.ModsMultiEditor_TabSource_Hint()
                ), this),
            new ToolStripSeparator(),
            Actions.asIconButton(saveAction, this)
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
                loadTabData(activeEditor, pid);
            }
        }
    }

    private void loadTabData(DatastreamEditor tab, String pid) {
        if (tab == modsFullEditor) {
            loadFull(pid);
        } else if (tab == modsSourceEditor) {
            loadSource(pid);
        } else {
            loadCustom(pid, model);
        }
    }

    private void loadCustom(String pid, MetaModelRecord model) {
        modsCustomEditor.edit(pid, null, model);
        if (modsCustomEditor.getCustomForm() != null) {
            setActiveEditor(modsCustomEditor);
            setEnabledCustom(true);
        } else {
            // unknown model, use full form
            setEnabledCustom(false);
            loadFull(pid);
        }
    }

    private void loadFull(final String pid) {
        setActiveEditor(modsFullEditor);
        modsFullEditor.edit(pid, null, model);
    }

    private void loadSource(String pid) {
        setActiveEditor(modsSourceEditor);
        modsSourceEditor.edit(pid, null, model);
    }

    private void setActiveEditor(DatastreamEditor newEditor) {
        if (newEditor != activeEditor) {
            uiContainer.setMembers(newEditor.getUI());
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

    private final class SwitchAction extends AbstractAction {

        private final DatastreamEditor tab;

        public SwitchAction(DatastreamEditor tab, String title, String icon, String tooltip) {
            super(title, icon, tooltip);
            this.tab = tab;
        }

        @Override
        public void performAction(ActionEvent event) {
            loadTabData(tab, pid);
        }

    }

}
