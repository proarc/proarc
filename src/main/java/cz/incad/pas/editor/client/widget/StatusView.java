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
package cz.incad.pas.editor.client.widget;

import com.google.gwt.user.client.Timer;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.AnimationEffect;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.Page;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Status view displays short messages in  non-modal way and
 * automatically disappears with some delay.
 * Typical usage is e.g. successful save notification.
 * For warnings and errors use dialogs.
 *
 * @author Jan Pokorsky
 */
public final class StatusView {

    private static final StatusView INSTANCE = new StatusView();

    public static StatusView getInstance() {
        return INSTANCE;
    }
    
    private final Label content;
    private final Timer timer;
    private final HLayout container;

    private StatusView() {
        container = new HLayout();
        content = new Label();
        content.setIcon("[SKIN]/Dialog/say.png");
        content.setIconSize(20);
        content.setMargin(1);
        content.setPadding(1);

        content.setHeight(38);
        content.setValign(VerticalAlignment.CENTER);
        content.setAlign(Alignment.LEFT);
        content.setWrap(Boolean.FALSE);

        container.setMembers(content);
        container.setLeft(Page.getWidth() / 2 - 50);
        container.setTop(4);
        container.setLayoutLeftMargin(4);
        container.setLayoutRightMargin(4);
        container.setBorder("1px solid red");
        container.setBackgroundColor("white");
        container.setAutoHeight();
        container.setShowShadow(Boolean.TRUE);

        timer = new Timer() {

            @Override
            public void run() {
                container.animateHide(AnimationEffect.FADE);
            }
        };
    }

    public void show(String msg) {
        content.setContents(msg);
        final int width = Math.min(300, Math.max(80, msg.length() * 4));
        if (!container.isDrawn()) {
            content.setWidth(width);
            container.setWidth(width);
        }
        container.redraw();
        timer.schedule(6000); // hide
        container.animateShow(AnimationEffect.SLIDE);
    }

}
