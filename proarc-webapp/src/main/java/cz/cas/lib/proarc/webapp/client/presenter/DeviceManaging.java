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
package cz.cas.lib.proarc.webapp.client.presenter;

import com.google.gwt.activity.shared.AbstractActivity;
import com.google.gwt.event.shared.EventBus;
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceTokenizer;
import com.google.gwt.place.shared.Prefix;
import com.google.gwt.user.client.ui.AcceptsOneWidget;
import cz.cas.lib.proarc.webapp.client.Editor.PresenterFactory;

/**
 * Integrates device manager to the application workflow.
 *
 * @author Jan Pokorsky
 */
public final class DeviceManaging extends AbstractActivity {

    private final DeviceManagerPlace place;
    private final PresenterFactory presenterFactory;

    public DeviceManaging(DeviceManagerPlace place, PresenterFactory presenterFactory) {
        this.place = place;
        this.presenterFactory = presenterFactory;
    }

    @Override
    public void start(AcceptsOneWidget panel, EventBus eventBus) {
        DeviceManager presenter = presenterFactory.getDeviceManager();
        panel.setWidget(presenter.getUI());
        presenter.init();
    }

    public static final class DeviceManagerPlace extends Place {

        @Prefix("deviceManager")
        public static final class Tokenizer implements PlaceTokenizer<DeviceManagerPlace> {

            @Override
            public DeviceManagerPlace getPlace(String token) {
                return new DeviceManagerPlace();
            }

            @Override
            public String getToken(DeviceManagerPlace place) {
                return "";
            }

        }

    }

}
