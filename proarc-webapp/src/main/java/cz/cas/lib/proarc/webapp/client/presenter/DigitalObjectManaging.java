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
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceTokenizer;
import com.google.gwt.place.shared.Prefix;
import com.google.gwt.user.client.ui.AcceptsOneWidget;
import cz.cas.lib.proarc.webapp.client.Editor.PresenterFactory;

/**
 * Integrates digital object manager to application workflow.
 * 
 * @author Jan Pokorsky
 */
public final class DigitalObjectManaging extends AbstractActivity {

    private final DigitalObjectManagerPlace place;
    private final PresenterFactory presenterFactory;

    public DigitalObjectManaging(DigitalObjectManagerPlace place, PresenterFactory presenterFactory) {
        this.place = place;
        this.presenterFactory = presenterFactory;
    }

    @Override
    public void start(AcceptsOneWidget panel, EventBus eventBus) {
        DigitalObjectManager presenter = presenterFactory.getDigitalObjectManager();
        panel.setWidget(presenter.getUI());
        presenter.init();
    }

    public static final class DigitalObjectManagerPlace extends Place {

        @Prefix("doManager")
        public static final class Tokenizer implements PlaceTokenizer<DigitalObjectManagerPlace> {

            @Override
            public DigitalObjectManagerPlace getPlace(String token) {
                return new DigitalObjectManagerPlace();
            }

            @Override
            public String getToken(DigitalObjectManagerPlace place) {
                return "";
            }

        }
    }

}
