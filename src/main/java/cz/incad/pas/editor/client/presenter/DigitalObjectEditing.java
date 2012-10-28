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

import com.google.gwt.activity.shared.AbstractActivity;
import com.google.gwt.event.shared.EventBus;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceController;
import com.google.gwt.place.shared.PlaceTokenizer;
import com.google.gwt.place.shared.Prefix;
import com.google.gwt.user.client.ui.AcceptsOneWidget;
import com.smartgwt.client.data.Record;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.Editor;
import cz.incad.pas.editor.client.ds.SearchDataSource;

/**
 * Integrates digital object editor to application workflow.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectEditing extends AbstractActivity {

    private final DigitalObjectEditorPlace place;
    private final PlaceController places;
    private final ClientMessages i18n;

    public DigitalObjectEditing(
            DigitalObjectEditorPlace place,
            PlaceController places,
            ClientMessages i18n
            ) {
        
        this.place = place;
        this.places = places;
        this.i18n = i18n;
    }

    @Override
    public void start(AcceptsOneWidget panel, EventBus eventBus) {
        DigitalObjectEditor editor = Editor.getInstance().getPresenterFactory().getDigitalObjectEditor();
        panel.setWidget(editor.getUI());
        editor.edit(place.getEditorId(), place.getSelection());
    }

    @Override
    public String mayStop() {
        return null;
    }

    public static final class DigitalObjectEditorPlace extends Place {

        private DigitalObjectEditor.Type editor;
        private Record selection;

        public DigitalObjectEditorPlace() {
        }

        public DigitalObjectEditorPlace(DigitalObjectEditor.Type editor, Record selection) {
            this.editor = editor;
            this.selection = selection;
        }

        public DigitalObjectEditor.Type getEditorId() {
            return editor;
        }

        public void setEditorId(DigitalObjectEditor.Type editor) {
            this.editor = editor;
        }

        public Record getSelection() {
            return selection;
        }

        public void setSelection(Record selection) {
            this.selection = selection;
        }

        @Prefix("doEditor")
        public static final class Tokenizer implements PlaceTokenizer<DigitalObjectEditorPlace> {

            private static final String EDITOR = "editor";

            @Override
            public DigitalObjectEditorPlace getPlace(String token) {
                DigitalObjectEditorPlace place = new DigitalObjectEditorPlace();
                JSONObject json = JsonTokenizer.parseObject(token);
                if (json != null) {
                    try {
                        place.editor = DigitalObjectEditor.Type.valueOf(JsonTokenizer.getString(json, EDITOR));
                    } catch (Exception e) {
                        // ignore invalid editor
                    }
                    place.selection = new Record();
                    place.selection.setAttribute(SearchDataSource.FIELD_PID,
                            JsonTokenizer.getString(json, SearchDataSource.FIELD_PID));
                    place.selection.setAttribute(SearchDataSource.FIELD_MODEL,
                            JsonTokenizer.getString(json, SearchDataSource.FIELD_MODEL));
                    place.selection.setAttribute(SearchDataSource.FIELD_LABEL,
                            JsonTokenizer.getString(json, SearchDataSource.FIELD_LABEL));
                }
                return place;
            }

            @Override
            public String getToken(DigitalObjectEditorPlace place) {
                JSONObject json = new JSONObject();
                JsonTokenizer.putString(json, EDITOR, place.editor == null ? null : place.editor.name());
                JsonTokenizer.putString(json, SearchDataSource.FIELD_PID,
                        place.selection.getAttribute(SearchDataSource.FIELD_PID));
                JsonTokenizer.putString(json, SearchDataSource.FIELD_MODEL,
                        place.selection.getAttribute(SearchDataSource.FIELD_MODEL));
                JsonTokenizer.putString(json, SearchDataSource.FIELD_LABEL,
                        place.selection.getAttribute(SearchDataSource.FIELD_LABEL));
                String jsonString = json.toString();
                return jsonString;
            }

        }
    }

}
