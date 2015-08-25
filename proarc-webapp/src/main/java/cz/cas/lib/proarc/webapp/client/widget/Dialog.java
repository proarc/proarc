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
package cz.cas.lib.proarc.webapp.client.widget;

import com.smartgwt.client.i18n.SmartGwtMessages;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.layout.HStack;
import com.smartgwt.client.widgets.layout.LayoutSpacer;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.cas.lib.proarc.webapp.client.ClientUtils;

/**
 * A helper to build a dialog with forms.
 *
 * @author Jan Pokorsky
 */
public class Dialog extends Window {

    private VLayout container;
    private HStack btnLayout;
    private String label;
    private Label question;
    private final SmartGwtMessages i18nSgwt;

    public Dialog(String title) {
        setAutoSize(true);
        setAutoCenter(true);
        setIsModal(true);
        setTitle(title);
        setShowMinimizeButton(false);
        setShowMaximizeButton(false);
        setKeepInParentRect(true);
        setShowModalMask(true);
        setCanDragResize(false);
        i18nSgwt = ClientUtils.createSmartGwtMessages();
    }

    public VLayout getDialogContentContainer() {
        if (container == null) {
            container = new VLayout();
        }
        return container;
    }

    public Label getDialogLabelContainer() {
        question = null;
        if (question == null) {
            question = new Label(label);
            question.setWidth100();
            question.setAutoHeight();
            question.setIcon("[SKIN]/Dialog/ask.png");
            question.setIconSize(32);
        }
        return question;
    }

    public HStack getDialogButtonsContainer() {
        if (btnLayout == null) {
            btnLayout = new HStack(5);
            btnLayout.setAutoHeight();
            btnLayout.setLayoutTopMargin(20);
            btnLayout.setLayoutAlign(Alignment.CENTER);
            btnLayout.setAlign(Alignment.CENTER);
        }
        return btnLayout;
    }

    public IButton addOkButton(ClickHandler handler) {
        IButton btn = new IButton(i18nSgwt.dialog_OkButtonTitle(), handler);
        getDialogButtonsContainer().addMember(btn);
        return btn;
    }

    public IButton addYesButton(ClickHandler handler) {
        IButton btn = new IButton(i18nSgwt.dialog_YesButtonTitle(), handler);
        getDialogButtonsContainer().addMember(btn);
        return btn;
    }

    /**
     * Adds a No button and registers the close handler as a click handler and
     * a dialog close handler.
     */
    public IButton addNoButton(DialogCloseHandler closeHandler) {
        return addCancelButton(i18nSgwt.dialog_NoButtonTitle(), closeHandler);
    }

    /**
     * Adds a Cancel button and registers the close handler as a click handler and
     * a dialog close handler.
     */
    public IButton addCancelButton(DialogCloseHandler closeHandler) {
        return addCancelButton(i18nSgwt.dialog_CancelButtonTitle(), closeHandler);
    }

    private IButton addCancelButton(String title, DialogCloseHandler closeHandler) {
        IButton btn = new IButton(title, closeHandler);
        addCloseClickHandler(closeHandler);
        getDialogButtonsContainer().addMember(btn);
        return btn;
    }

    public static abstract class DialogCloseHandler implements CloseClickHandler, ClickHandler {

        @Override
        public void onCloseClick(CloseClickEvent event) {
            onClose();
        }

        @Override
        public void onClick(ClickEvent event) {
            onClose();
        }

        public abstract void onClose();

    }

    @Override
    protected void onDraw() {
        super.onDraw();
        VLayout windowContainer = new VLayout();
        windowContainer.setWidth100();
        windowContainer.setMargin(10);
        if (question != null) {
            LayoutSpacer gap = new LayoutSpacer();
            gap.setHeight(30);
            windowContainer.addMember(question);
            windowContainer.addMember(gap);
        }
        windowContainer.addMember(getDialogContentContainer());
        windowContainer.addMember(getDialogButtonsContainer());
        addItem(windowContainer);
    }

}
