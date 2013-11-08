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

import com.google.gwt.activity.shared.AbstractActivity;
import com.google.gwt.event.shared.EventBus;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceTokenizer;
import com.google.gwt.place.shared.Prefix;
import com.google.gwt.user.client.ui.AcceptsOneWidget;
import com.smartgwt.client.data.Record;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import java.util.Arrays;

/**
 * Integrates digital object editor to application workflow.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectEditing extends AbstractActivity {

    private final DigitalObjectEditorPlace place;
    private final DigitalObjectEditor editor;

    public DigitalObjectEditing(
            DigitalObjectEditorPlace place,
            DigitalObjectEditor editor
            ) {
        
        this.place = place;
        this.editor = editor;
    }

    @Override
    public void start(AcceptsOneWidget panel, EventBus eventBus) {
        panel.setWidget(editor.getUI());
        Record[] digitalObjects = place.getDigitalObjects();
        if (digitalObjects != null) {
            editor.edit(place.getEditorId(), digitalObjects);
        } else {
            editor.edit(place.getEditorId(), place.getPids());
        }
    }

    @Override
    public String mayStop() {
        return null;
    }

    public static final class DigitalObjectEditorPlace extends Place {

        private DatastreamEditorType editor;
        private String pid;
        private String[] pids;
        private Record[] digitalObjects;

        public DigitalObjectEditorPlace() {
        }

        public DigitalObjectEditorPlace(DatastreamEditorType editor, String pid) {
            this(editor, pid == null ? null : new String[] {pid});
        }

        public DigitalObjectEditorPlace(DatastreamEditorType editor, Record[] records) {
            this.editor = editor;
            this.pid = records == null || records.length == 0 ? null : records[0].getAttribute("pid");
            this.digitalObjects = records;
        }

        public DigitalObjectEditorPlace(DatastreamEditorType editor, String[] pids) {
            this.editor = editor;
            this.pid = pids == null || pids.length == 0 ? null : pids[0];
            this.pids = pids;
        }

        public DatastreamEditorType getEditorId() {
            return editor;
        }

        public void setEditorId(DatastreamEditorType editor) {
            this.editor = editor;
        }

        public String getPid() {
            return pid;
        }

        public void setPid(String pid) {
            this.pid = pid;
            this.pids = pid == null ? null : new String[] {pid};
        }

        public String[] getPids() {
            return pids;
        }

        public void setPids(String[] pids) {
            this.pids = pids;
        }

        public Record[] getDigitalObjects() {
            return digitalObjects;
        }

        public void setDigitalObjects(Record[] digitalObjects) {
            this.digitalObjects = digitalObjects;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final DigitalObjectEditorPlace other = (DigitalObjectEditorPlace) obj;
            if (this.editor != other.editor) {
                return false;
            }
            if ((this.pid == null) ? (other.pid != null) : !this.pid.equals(other.pid)) {
                return false;
            }
            if (!Arrays.equals(this.pids, other.pids)) {
                return false;
            }
            if (!Arrays.equals(this.digitalObjects, other.digitalObjects)) {
                return false;
            }
            return true;
        }


        @Prefix("doEditor")
        public static final class Tokenizer implements PlaceTokenizer<DigitalObjectEditorPlace> {

            private static final String EDITOR = "editor";
            private static final String PID = "pid";

            @Override
            public DigitalObjectEditorPlace getPlace(String token) {
                DigitalObjectEditorPlace place = new DigitalObjectEditorPlace();
                JSONObject json = JsonTokenizer.parseObject(token);
                if (json != null) {
                    try {
                        place.editor = DatastreamEditorType.valueOf(JsonTokenizer.getString(json, EDITOR));
                    } catch (Exception e) {
                        // ignore invalid editor
                    }
                    place.setPid(JsonTokenizer.getString(json, PID));
                }
                return place;
            }

            @Override
            public String getToken(DigitalObjectEditorPlace place) {
                String[] digitalObjectPids = place.getPids();
                if (digitalObjectPids != null && digitalObjectPids.length > 1) {
                    // batch edits should not be bookmarked
                    throw new UnsupportedOperationException(Arrays.toString(digitalObjectPids));
                }
                Record[] digitalObjects = place.getDigitalObjects();
                if (digitalObjects != null && digitalObjects.length > 1) {
                    // batch edits should not be bookmarked
                    String[] pids = ClientUtils.toFieldValues(digitalObjects, "pid");
                    throw new UnsupportedOperationException(Arrays.toString(pids));
                }
                JSONObject json = new JSONObject();
                JsonTokenizer.putString(json, EDITOR, place.editor == null ? null : place.editor.name());
                JsonTokenizer.putString(json, PID, place.getPid());
                String jsonString = json.toString();
                return jsonString;
            }

        }
    }

}
