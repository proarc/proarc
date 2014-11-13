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
import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceController;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.types.PromptStyle;
import com.smartgwt.client.util.Page;
import com.smartgwt.client.util.SC;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.RelationDataSource;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.SearchDataSource;
import cz.cas.lib.proarc.webapp.client.presenter.DigitalObjectEditing.DigitalObjectEditorPlace;

/**
 * Opens parent, child or sibling of selected digital object in the current editor.
 * In order to navigate to given child object the action source has to implement
 * {@link ChildSelector}. Otherwise the first child is used.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectNavigateAction extends AbstractAction {

    public enum Navigation {CHILD, NEXT, PARENT, PREV}

    private final PlaceController places;
    private final ClientMessages i18n;
    private final Navigation navigation;
    private static RecordList siblings;

    public static DigitalObjectNavigateAction parent(ClientMessages i18n, PlaceController places) {
        return new DigitalObjectNavigateAction(i18n,
                i18n.DigitalObjectNavigateAction_OpenParent_Title(),
                Page.getAppDir() + "images/16/next_up.png",
                i18n.DigitalObjectNavigateAction_OpenParent_Hint(),
                Navigation.PARENT,
                places);
    }

    public static DigitalObjectNavigateAction child(ClientMessages i18n, PlaceController places) {
        return new DigitalObjectNavigateAction(i18n,
                i18n.DigitalObjectNavigateAction_OpenChild_Title(),
                Page.getAppDir() + "images/16/next_down.png",
                i18n.DigitalObjectNavigateAction_OpenChild_Hint(),
                Navigation.CHILD,
                places);
    }

    public static DigitalObjectNavigateAction next(ClientMessages i18n, PlaceController places) {
        return new DigitalObjectNavigateAction(i18n,
                i18n.DigitalObjectNavigateAction_OpenNext_Title(),
                "[SKIN]/actions/next.png",
                i18n.DigitalObjectNavigateAction_OpenNext_Hint(),
                Navigation.NEXT,
                places);
    }

    public static DigitalObjectNavigateAction previous(ClientMessages i18n, PlaceController places) {
        return new DigitalObjectNavigateAction(i18n,
                i18n.DigitalObjectNavigateAction_OpenPrevious_Title(),
                "[SKIN]/actions/prev.png",
                i18n.DigitalObjectNavigateAction_OpenPrevious_Hint(),
                Navigation.PREV,
                places);
    }

    public DigitalObjectNavigateAction(
            ClientMessages i18n, String title, String icon, String tooltip, Navigation navigation,
            PlaceController places) {

        super(title, icon, tooltip);
        this.navigation = navigation;
        this.places = places;
        this.i18n = i18n;
    }

    @Override
    public void performAction(ActionEvent event) {
        Record[] selectedRecords = Actions.getSelection(event);
        if (accept(selectedRecords)) {
            DigitalObject dobj = DigitalObject.create(selectedRecords[0]);
            String pid = dobj.getPid();
            switch (navigation) {
                case PARENT:
                    openParent(pid);
                    break;
                case NEXT:
                case PREV:
                    openSibling(pid, false);
                    break;
                case CHILD:
                    openChild(pid, getChildSelection(event));
                    break;
            }
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
                        SC.warn(i18n.DigitalObjectNavigateAction_NoParent_Msg());
                    } else {
                        Record parent = result.first();
                        DigitalObject parentObj = DigitalObject.createOrNull(parent);
                        if (parentObj != null) {
                            siblings = null;
                            DigitalObjectEditorPlace place = new DigitalObjectEditorPlace(
                                    getLastEditorId(), parentObj);
                            place.setSelectChildPid(childPid);
                            places.goTo(place);
                        }
                    }
                }
            });
        }
    }

    private void open(DigitalObject dobj) {
        if (dobj != null) {
            DigitalObjectEditorPlace place = new DigitalObjectEditorPlace(
                    getLastEditorId(), dobj);
            places.goTo(place);
        }
    }

    /**
     * Opens a child object in the editor. If child is {@code null} it fetches
     * children and opens the first one.
     */
    private void openChild(final String parentPid, DigitalObject child) {
        if (child != null) {
            siblings = null;
            open(child);
            return ;
        }
        Criteria criteria = new Criteria(RelationDataSource.FIELD_ROOT, parentPid);
        criteria.addCriteria(RelationDataSource.FIELD_PARENT, parentPid);
        RelationDataSource.getInstance().fetchData(criteria, new DSCallback() {

            @Override
            public void execute(DSResponse dsResponse, Object data, DSRequest dsRequest) {
                if (RestConfig.isStatusOk(dsResponse)) {
                    RecordList result = dsResponse.getDataAsRecordList();
                    DigitalObject dobj = null;
                    if (!result.isEmpty()) {
                        dobj = DigitalObject.createOrNull(result.get(0));
                    }
                    if (dobj != null) {
                        siblings = result;
                        open(dobj);
                    } else {
                        // No child
                        SC.warn(i18n.DigitalObjectNavigateAction_NoChild_Msg());
                    }
                }
            }
        }, createRequestWithPrompt());
    }

    private void openSibling(final String pid, boolean cached) {
        if (pid != null) {
            RecordList rs = getSiblings();
            int pidIndex = rs.findIndex(RelationDataSource.FIELD_PID, pid);
            if (pidIndex == -1) {
                // fetch
                if (cached) {
                    // not found PID
                    SC.warn("Not found " + pid);
                } else {
                    fetchSiblings(pid);
                }
            } else if (navigation == Navigation.PREV && pidIndex == 0) {
                SC.warn(i18n.DigitalObjectNavigateAction_NoPrevSibling_Msg());
            } else if (navigation == Navigation.NEXT && pidIndex + 1 >= rs.getLength()) {
                SC.warn(i18n.DigitalObjectNavigateAction_NoNextSibling_Msg());
            } else {
                // open new
                int inc = navigation == Navigation.PREV ? -1 : 1;
                DigitalObject newObj = DigitalObject.create(rs.get(pidIndex + inc));
                if (newObj != null) {
                    open(newObj);
                }
            }
        }
    }

    private RecordList getSiblings() {
        if (siblings == null) {
            // listen to RelationDataSource updates to invalidate cache?
            siblings = new RecordList();
        }
        return siblings;
    }

    private void fetchSiblings(final String pid) {
        SearchDataSource.getInstance().findParent(pid, null, new Callback<ResultSet, Void>() {

            @Override
            public void onFailure(Void reason) {
            }

            @Override
            public void onSuccess(ResultSet result) {
                if (result.isEmpty()) {
                    SC.warn(i18n.DigitalObjectNavigateAction_NoParent_Msg());
                } else {
                    Record parent = result.first();
                    DigitalObject parentObj = DigitalObject.createOrNull(parent);
                    if (parentObj != null) {
                        scheduleFetchSiblings(parentObj.getPid(), pid);
                    }
                }
            }
        });
    }

    /**
     * Postpones {@link #fetchSiblings(java.lang.String, java.lang.String) fetch}
     * to force RPCManager to notify user with the request prompt. The invocation
     * from ResultSet's DataArrivedHandler ignores prompt settings and ResultSet
     * does not provide possibility to declare the prompt.
     */
    private void scheduleFetchSiblings(final String parentPid, final String pid) {
        Scheduler.get().scheduleDeferred(new ScheduledCommand() {

            @Override
            public void execute() {
                fetchSiblings(parentPid, pid);
            }
        });
    }

    private void fetchSiblings(final String parentPid, final String pid) {
        Criteria criteria = new Criteria(RelationDataSource.FIELD_ROOT, parentPid);
        criteria.addCriteria(RelationDataSource.FIELD_PARENT, parentPid);
        RelationDataSource.getInstance().fetchData(criteria, new DSCallback() {

            @Override
            public void execute(DSResponse dsResponse, Object data, DSRequest dsRequest) {
                if (RestConfig.isStatusOk(dsResponse)) {
                    siblings = dsResponse.getDataAsRecordList();
                    openSibling(pid, true);
                }
            }
        }, createRequestWithPrompt());
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

    private static DigitalObject getChildSelection(ActionEvent event) {
        Object source = event.getSource();
        if (source instanceof ChildSelector) {
            ChildSelector selectable = (ChildSelector) source;
            Record[] children = selectable.getChildSelection();
            if (children != null && children.length > 0) {
                return DigitalObject.createOrNull(children[0]);
            }
        }
        return null;
    }

    private static DSRequest createRequestWithPrompt() {
        DSRequest dsRequest = new DSRequest();
        dsRequest.setPromptStyle(PromptStyle.CURSOR);
        dsRequest.setShowPrompt(true);
        return dsRequest;
    }

    /**
     * The action source should implement this to supply children in case of
     * the child navigation.
     */
    public interface ChildSelector {

        /**
         * Gets an array of child records.
         */
        Record[] getChildSelection();

    }
}
