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
import com.google.gwt.place.shared.PlaceTokenizer;
import com.google.gwt.place.shared.Prefix;
import com.google.gwt.user.client.ui.AcceptsOneWidget;
import cz.incad.pas.editor.client.Editor.PresenterFactory;

/**
 * Integrates digital object creator to the application workflow.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectCreating extends AbstractActivity {

    private final DigitalObjectCreatorPlace place;
    private final PresenterFactory presenters;

    public DigitalObjectCreating(
            DigitalObjectCreatorPlace place,
            PresenterFactory presenters
            ) {

        this.place = place;
        this.presenters = presenters;
    }

    @Override
    public void start(AcceptsOneWidget panel, EventBus eventBus) {
        DigitalObjectCreator doc = presenters.getDigitalObjectCreator();
        panel.setWidget(doc.getUI());
        doc.newObject(place.getModel(), place.getParent());
    }

    public static final class DigitalObjectCreatorPlace extends Place {

        private String model;
        private String parent;

        public DigitalObjectCreatorPlace() {
        }

        public DigitalObjectCreatorPlace(String model, String parent) {
            this.model = model;
            this.parent = parent;
        }

        public String getModel() {
            return model;
        }

        public void setModel(String model) {
            this.model = model;
        }

        public String getParent() {
            return parent;
        }

        public void setParent(String parent) {
            this.parent = parent;
        }

        @Prefix("doCreator")
        public static final class Tokenizer implements PlaceTokenizer<DigitalObjectCreatorPlace> {

            private static final String MODEL = "model";
            private static final String PARENT = "parent";

            @Override
            public DigitalObjectCreatorPlace getPlace(String token) {
                DigitalObjectCreatorPlace p = new DigitalObjectCreatorPlace();
                JSONObject json = JsonTokenizer.parseObject(token);
                if (json != null) {
                    p.model = JsonTokenizer.getString(json, MODEL);
                    p.parent = JsonTokenizer.getString(json, PARENT);
                }

                return p;
            }

            @Override
            public String getToken(DigitalObjectCreatorPlace place) {
                JSONObject json = new JSONObject();
                JsonTokenizer.putString(json, MODEL, place.model);
                JsonTokenizer.putString(json, PARENT, place.parent);
                String jsonString = json.toString();
                return jsonString;
            }

        }
    }

}
