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
 * Integrates digital object importer to application workflow.
 *
 * @author Jan Pokorsky
 */
public final class Importing extends AbstractActivity {

    private final ImportPlace importPlace;
    private final PresenterFactory presenterFactory;

    public Importing(ImportPlace importPlace, PresenterFactory presenterFactory) {
        this.importPlace = importPlace;
        this.presenterFactory = presenterFactory;
    }

    @Override
    public void start(AcceptsOneWidget panel, EventBus eventBus) {
        ImportPresenter importPresenter = presenterFactory.getImportPresenter();
        importPresenter.bind();
        switch (importPlace.getType()) {
            case CONTENT:
                importPresenter.importFolder();
                break;
            case EDIT_ITEMS:
                importPresenter.updateImportedObjects(importPlace.getBatchId());
                break;
            case EDIT_PARENT:
                importPresenter.selectParent(importPlace.getBatchId());
                break;
            default: // HISTORY
                importPresenter.selectBatchFromHistory();
                break;
        }
        panel.setWidget(importPresenter.getUI());
    }

    public static final class ImportPlace extends Place {
        
        private String batchId;
        private Type type;

        public ImportPlace() {
            this(Type.HISTORY);
        }

        public ImportPlace(Type type) {
            this(type, null);
        }

        public ImportPlace(Type type, String batchId) {
            this.type = type;
            this.batchId = batchId;
        }

        public String getBatchId() {
            return batchId;
        }

        public void setBatchId(String batchId) {
            this.batchId = batchId;
        }

        public Type getType() {
            return type;
        }

        public void setType(Type type) {
            this.type = type;
        }

        public enum Type {

            /** import digital content */
            CONTENT,
            /** import datastream */
            DATASTREAM,
            /** edit imported objects */
            EDIT_ITEMS,
            /** edit parent of imported objects */
            EDIT_PARENT,
            /** import history */
            HISTORY;

            public static Type fromString(String s) {
                for (Type value : values()) {
                    if (value.name().equalsIgnoreCase(s)) {
                        return value;
                    }
                }
                return HISTORY;
            }
        }

        @Prefix("import")
        public static final class Tokenizer implements PlaceTokenizer<ImportPlace> {

            private static final String BATCHID = "batch";
            private static final String TYPE = "type";

            @Override
            public ImportPlace getPlace(String token) {
                ImportPlace p = new ImportPlace();
                JSONObject json = JsonTokenizer.parseObject(token);
                if (json != null) {
                    p.type = Type.fromString(JsonTokenizer.getString(json, TYPE));
                    p.batchId = JsonTokenizer.getString(json, BATCHID);
                }
                return p;
            }

            @Override
            public String getToken(ImportPlace place) {
                JSONObject json = new JSONObject();
                Type importType = place.getType();
                JsonTokenizer.putString(json, TYPE, importType == null ? null : importType.name());
                JsonTokenizer.putString(json, BATCHID, place.getBatchId());
                return json.toString();
            }

        }
    }

}
