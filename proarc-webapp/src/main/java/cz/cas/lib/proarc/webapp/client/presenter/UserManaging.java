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
import cz.cas.lib.proarc.webapp.client.widget.UsersView;

/**
 * Integrates user manager to application workflow.
 * 
 * @author Jan Pokorsky
 */
public final class UserManaging extends AbstractActivity {

    private final UsersPlace place;
    private final PresenterFactory presenterFactory;

    public UserManaging(UsersPlace place, PresenterFactory presenterFactory) {
        this.place = place;
        this.presenterFactory = presenterFactory;
    }

    @Override
    public void start(AcceptsOneWidget panel, EventBus eventBus) {
        UsersView presenter = presenterFactory.getUsers();
        panel.setWidget(presenter.asWidget());
        presenter.onShow();
    }

    public static final class UsersPlace extends Place {

        @Prefix("users")
        public static final class Tokenizer implements PlaceTokenizer<UsersPlace> {

            @Override
            public UsersPlace getPlace(String token) {
                return new UsersPlace();
            }

            @Override
            public String getToken(UsersPlace place) {
                return "{}";
            }

        }

    }

}
