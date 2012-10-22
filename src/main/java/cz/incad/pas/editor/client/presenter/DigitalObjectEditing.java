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

import cz.incad.pas.editor.client.presenter.DigitalObjectEditor;
import com.google.gwt.activity.shared.AbstractActivity;
import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.core.client.JsonUtils;
import com.google.gwt.event.shared.EventBus;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONString;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceController;
import com.google.gwt.place.shared.PlaceTokenizer;
import com.google.gwt.place.shared.Prefix;
import com.google.gwt.user.client.ui.AcceptsOneWidget;
import com.smartgwt.client.data.Record;
import cz.incad.pas.editor.client.Editor;
import cz.incad.pas.editor.client.PasEditorMessages;

/**
 * Integrates digital object editor to application workflow.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectEditing extends AbstractActivity {

    private final DigitalObjectEditorPlace place;
    private final PlaceController places;
    private final PasEditorMessages i18n;

    public DigitalObjectEditing(
            DigitalObjectEditorPlace place,
            PlaceController places,
            PasEditorMessages i18n
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

        @Prefix("doeditor")
        public static final class Tokenizer implements PlaceTokenizer<DigitalObjectEditorPlace> {

            private static final String EDITOR = "editor";
            private static final String SELECTION = "selection";

            @Override
            public DigitalObjectEditorPlace getPlace(String token) {
                DigitalObjectEditorPlace place = new DigitalObjectEditorPlace();
                if (JsonUtils.safeToEval(token)) {
                    JSONValue jsonv = JSONParser.parseStrict(token);
                    JSONObject json = jsonv.isObject();
                    JSONValue editorv = json.get(EDITOR);
                    if (editorv != null) {
                        String val = editorv.isString().stringValue();
                        place.editor = DigitalObjectEditor.Type.valueOf(val);
                    }
                    JSONValue selectionv = json.get(SELECTION);
                    if (selectionv != null) {
                        JSONObject object = selectionv.isObject();
                        if (object != null) {
                            JavaScriptObject jso = object.getJavaScriptObject();
                            place.selection = Record.getOrCreateRef(jso);
                        }
                    }
                } else {
                }
                return place;
            }

            @Override
            public String getToken(DigitalObjectEditorPlace place) {
                JSONObject json = new JSONObject();
                json.put(EDITOR, new JSONString(place.editor.name()));
                json.put(SELECTION, new JSONObject(place.selection.getJsObj()));
                String jsonString = json.toString();
                return jsonString;
            }

        }
    }

}
