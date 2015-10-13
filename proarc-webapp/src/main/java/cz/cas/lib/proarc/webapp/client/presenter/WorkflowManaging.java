/*
 * Copyright (C) 2015 Jan Pokorsky
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
import cz.cas.lib.proarc.webapp.client.Editor.PresenterFactory;

/**
 * Manages workflow places.
 *
 * @author Jan Pokorsky
 */
public class WorkflowManaging extends AbstractActivity {

    private final PresenterFactory presenterFactory;
    private final WorkflowPlace place;

    public WorkflowManaging(WorkflowPlace place, PresenterFactory presenterFactory) {
        this.presenterFactory = presenterFactory;
        this.place = place;
    }
    @Override
    public void start(AcceptsOneWidget panel, EventBus eventBus) {
        if (place instanceof WorkflowJobPlace) {
            WorkflowJobsEditor presenter = presenterFactory.getWorkflowJobs();
            panel.setWidget(presenter.getUI());
//        presenter.init();
        } else if (place instanceof WorkflowNewJobPlace) {
            WorkflowNewJobEditor presenter = presenterFactory.getWorkflowNewJob();
            panel.setWidget(presenter.getUI());
        } else {
            WorkflowTasksEditor presenter = presenterFactory.getWorkflowTasks();
            panel.setWidget(presenter.getUI());
//        presenter.init();
        }
    }

    public static class WorkflowJobPlace extends WorkflowPlace {

        @Override
        public String toToken() {
            JSONObject json = new JSONObject();
            JsonTokenizer.putString(json, Tokenizer.TYPE, Type.JOBS.name());
            return json.toString();
        }

        public static WorkflowJobPlace from(String type, JSONObject json) {
            if (!Type.JOBS.name().equals(type)) {
                return null;
            }
            return new WorkflowJobPlace();
        }

    }

    public static class WorkflowTaskPlace extends WorkflowPlace {

        @Override
        public String toToken() {
            JSONObject json = new JSONObject();
            JsonTokenizer.putString(json, Tokenizer.TYPE, Type.TASKS.name());
            return json.toString();
        }

        public static WorkflowTaskPlace from(String type, JSONObject json) {
            if (!Type.TASKS.name().equals(type)) {
                return null;
            }
            return new WorkflowTaskPlace();
        }

    }

    public static class WorkflowNewJobPlace extends WorkflowPlace {

        @Override
        public String toToken() {
            JSONObject json = new JSONObject();
            JsonTokenizer.putString(json, Tokenizer.TYPE, Type.NEWJOB.name());
            return json.toString();
        }

        public static WorkflowNewJobPlace from(String type, JSONObject json) {
            if (!Type.NEWJOB.name().equals(type)) {
                return null;
            }
            return new WorkflowNewJobPlace();
        }

    }

    public static abstract class WorkflowPlace extends Place {

        public abstract String toToken();

        enum Type {NEWJOB, JOBS, TASKS}

        @Prefix("workflow")
        public static final class Tokenizer implements PlaceTokenizer<WorkflowPlace> {

            private static final String TYPE = "type";

            @Override
            public WorkflowPlace getPlace(String token) {
                WorkflowPlace place = new WorkflowJobPlace();
                JSONObject json = JsonTokenizer.parseObject(token);
                if (json != null) {
                    String type = JsonTokenizer.getString(json, TYPE);
                    if ((place = WorkflowJobPlace.from(type, json)) == null) {
                        if ((place = WorkflowTaskPlace.from(type, json)) == null) {
                            if ((place = WorkflowNewJobPlace.from(type, json)) == null) {
                            }
                        }
                    }
                }
                return place;
            }

            @Override
            public String getToken(WorkflowPlace place) {
                return place.toToken();
            }

        }

    }

}
