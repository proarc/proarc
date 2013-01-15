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
package cz.incad.pas.editor.client.widget;

import com.smartgwt.client.data.Record;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.action.Action;
import cz.incad.pas.editor.client.action.ActionEvent;
import cz.incad.pas.editor.client.action.Actions;
import cz.incad.pas.editor.client.action.RefreshAction.Refreshable;
import cz.incad.pas.editor.client.action.SaveAction;
import cz.incad.pas.editor.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.incad.pas.editor.client.ds.RelationDataSource;
import cz.incad.pas.editor.client.widget.ImportParentChooser.ImportParentHandler;

/**
 * Edits parent object of given digital object.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectParentEditor implements DatastreamEditor, Refreshable {

    private final ImportParentChooser chooser;
    private String pid;
    private String batchId;
    private boolean autosave = false;
    private SaveAction saveAction;
    private final ClientMessages i18n;

    public DigitalObjectParentEditor(ClientMessages i18n) {
        this.i18n = i18n;
        chooser = new ImportParentChooser(i18n);
        chooser.setHandler(new ImportParentHandler() {

            @Override
            public void onParentSelectionUpdated() {
                if (autosave) {
//                    save();
                }
            }
        });
        initActions();
    }

    @Override
    public void edit(String pid, String batchId, MetaModelRecord model) {
        this.pid = pid;
        this.batchId = batchId;
        if (pid != null) {
            chooser.setDigitalObject(pid);
        } else if (batchId != null) {
            chooser.setImport(batchId);
        } else {
            throw new IllegalStateException("missing pid and batchId");
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getCapability(Class<T> clazz) {
        // refresh
        T c = null;
        if (Refreshable.class.equals(clazz)) {
            c = (T) this;
        }
        return c;
    }

    @Override
    public Canvas[] getToolbarItems() {
        return new Canvas[] {
            Actions.asIconButton(saveAction, this)
        };
    }

    private Action[] initActions() {
        saveAction = new SaveAction(i18n) {

            @Override
            public void performAction(ActionEvent event) {
                save();
            }
        };
        return new Action[] {saveAction};
    }

    @Override
    public Canvas getUI() {
        return chooser.getUI();
    }

    @Override
    public void refresh() {
        edit(pid, batchId, null);
    }

    private void save() {
        if (chooser.isChanged()) {
            final Record newParent = chooser.getSelectedParent();
            Record oldParent = chooser.getOldParent();
            String parentPid = chooser.getSelectedParentPid();
            String parentId = parentPid == null ? null : newParent.getAttribute(RelationDataSource.FIELD_ID);
            String oldParentPid = chooser.getOldParentPid();
            String oldParentId = oldParent == null ? null : oldParent.getAttribute(RelationDataSource.FIELD_ID);
            RelationDataSource ds = RelationDataSource.getInstance();
//            saveDigitalObjectParent(pid, oldParentId, oldParentPid, parentId, parentPid, new BooleanCallback() {
            ds.moveChild(pid, oldParentId, oldParentPid, parentId, parentPid, new BooleanCallback() {

                @Override
                public void execute(Boolean value) {
                    if (value != null && value) {
                        chooser.onSave(newParent);
                        StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
                    }
                    // else refresh?
                }
            });
        }
    }
//
//    private void saveDigitalObjectParent(final String pid,
//            final String oldParentId, final String oldParentPid,
//            final String parentId, final String parentPid,
//            final BooleanCallback call) {
//
//        final RelationDataSource ds = RelationDataSource.getInstance();
//        BooleanCallback toAdd = new BooleanCallback() {
//
//            @Override
//            public void execute(Boolean value) {
//                if (value != null && value) {
//                    if (parentPid != null) {
//                        ds.addChild(parentId, parentPid, pid, call);
//                    } else {
//                        call.execute(value);
//                    }
//                } else {
//                    call.execute(value);
//                }
//            }
//        };
//        if (oldParentPid != null) {
//            ds.removeChild(oldParentId, oldParentPid, pid, toAdd);
//        } else {
//            toAdd.execute(Boolean.TRUE);
//        }
//    }

}
