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
package cz.incad.pas.editor.client.action;

import com.google.gwt.place.shared.PlaceController;
import com.smartgwt.client.data.Record;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.Editor;
import cz.incad.pas.editor.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.incad.pas.editor.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.incad.pas.editor.client.presenter.DigitalObjectEditing.DigitalObjectEditorPlace;
import cz.incad.pas.editor.shared.rest.DigitalObjectResourceApi.DatastreamEditorType;

/**
 * Opens data stream editor.
 *
 * It expects the event source to implement {@link Selectable}.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectEditAction extends AbstractAction {

    private DatastreamEditorType editorType;
    private final PlaceController places;
    private AcceptFilter filter;

    public DigitalObjectEditAction(
            String title,
            DatastreamEditorType editorType,
            ClientMessages i18n) {

        this(title, i18n.DigitalObjectEditAction_Hint(), null, editorType,
                Editor.getInstance().getEditorWorkFlow().getPlaceController());
    }

    /** Action accepting single selection. */
    public DigitalObjectEditAction(
            String title,
            String tooltip,
            String icon,
            DatastreamEditorType editorType,
            PlaceController places) {

        this(title, tooltip, icon, editorType, new AcceptFilter(false, false), places);
    }

    public DigitalObjectEditAction(
            String title,
            String tooltip,
            String icon,
            DatastreamEditorType editorType,
            AcceptFilter filter,
            PlaceController places) {

        super(title, icon == null ? "[SKIN]/actions/edit.png" : icon, tooltip);
        this.editorType = editorType;
        this.places = places;
        this.filter = filter;
    }

    @Override
    public void performAction(ActionEvent event) {
        Record[] selection = Actions.getSelection(event);
        if (accept(selection)) {
            DigitalObjectEditorPlace place =
                    new DigitalObjectEditorPlace(editorType, selection);
            places.goTo(place);
        }

    }

    @Override
    public boolean accept(ActionEvent event) {
        Record[] selection = Actions.getSelection(event);
        return accept(selection);
    }

    private boolean accept(Record[] selection) {
        if (selection == null || (!filter.isMultiSelection() && selection.length != 1)) {
            return false;
        }

        // check object model of each record
        for (Record record : selection) {
            DigitalObject dobj = DigitalObject.createOrNull(record);
            // is digital object?
            if (dobj == null) {
                return false;
            }
            if (filter.isAnyModel()) {
                continue;
            }
            MetaModelRecord model = dobj.getModel();
            if (model == null || !model.isSupportedDatastream(editorType.name())) {
                return false;
            }
        }
        return true;
    }

    /**
     * Helper to change acceptation rules.
     */
    public static class AcceptFilter {

        private boolean anyModel;
        private boolean multiSelection;

        public AcceptFilter(boolean anyModel, boolean multiSelection) {
            this.anyModel = anyModel;
            this.multiSelection = multiSelection;
        }

        /**
         * Any model is acceptable.
         */
        public boolean isAnyModel() {
            return anyModel;
        }

        /**
         * Multiple selection of digital objects is acceptable.
         * @return
         */
        public boolean isMultiSelection() {
            return multiSelection;
        }

    }

}
