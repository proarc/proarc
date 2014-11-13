/*
 * Copyright (C) 2013 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.client.action;

import com.google.gwt.core.client.Callback;
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceController;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.util.SC;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.SearchDataSource;
import cz.cas.lib.proarc.webapp.client.presenter.DigitalObjectEditing.DigitalObjectEditorPlace;

/**
 * Opens parent of selected digital object in the current editor.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectNavigateAction extends AbstractAction {

    private final PlaceController places;
    private final ClientMessages i18n;

    public DigitalObjectNavigateAction(ClientMessages i18n, PlaceController places) {
        this(i18n,
                i18n.DigitalObjectOpenParentAction_Title(),
                "[SKIN]/FileBrowser/upOneLevel.png",
                i18n.DigitalObjectOpenParentAction_Hint(),
                places);
    }

    public DigitalObjectNavigateAction(
            ClientMessages i18n, String title, String icon, String tooltip,
            PlaceController places) {

        super(title, icon, tooltip);
        this.places = places;
        this.i18n = i18n;
    }

    @Override
    public void performAction(ActionEvent event) {
        Record[] selectedRecords = Actions.getSelection(event);
        if (accept(selectedRecords)) {
            DigitalObject dobj = DigitalObject.create(selectedRecords[0]);
            openParent(dobj.getPid());
        }
    }

    @Override
    public boolean accept(ActionEvent event) {
        Record[] selectedRecords = Actions.getSelection(event);
        return accept(selectedRecords);
    }

    private boolean accept(Record[] selectedRecords) {
        if (selectedRecords != null && selectedRecords.length == 1) {
            return isDigitalObject(selectedRecords[0]);
        }
        return false;
    }

    private static boolean isDigitalObject(Record r) {
        DigitalObject dobj = DigitalObject.createOrNull(r);
        return dobj != null;
    }

    private void openParent(final String childPid) {
        if (childPid != null) {
            SearchDataSource.getInstance().findParent(childPid, null, new Callback<ResultSet, Void>() {

                @Override
                public void onFailure(Void reason) {
                }

                @Override
                public void onSuccess(ResultSet result) {
                    if (result.isEmpty()) {
                        SC.warn(i18n.DigitalObjectOpenParentAction_NoParent_Msg());
                    } else {
                        Record parent = result.first();
                        DigitalObject parentObj = DigitalObject.createOrNull(parent);
                        if (parentObj != null) {
                            DigitalObjectEditorPlace place = new DigitalObjectEditorPlace(
                                    getLastEditorId(),
                                    parentObj.getPid());
                            place.setSelectChildPid(childPid);
                            places.goTo(place);
                        }
                    }
                }
            });
        }
    }

    private DatastreamEditorType getLastEditorId() {
        DatastreamEditorType editorId = null;
        Place where = places.getWhere();
        if (where instanceof DigitalObjectEditorPlace) {
            DigitalObjectEditorPlace editorPlace = (DigitalObjectEditorPlace) where;
            editorId = editorPlace.getEditorId();
        }
        return editorId == null ? DatastreamEditorType.CHILDREN : editorId;
    }

}
