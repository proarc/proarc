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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.incad.pas.editor.client.widget;

import com.google.gwt.user.client.Timer;
import com.smartgwt.client.types.ImageStyle;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.util.Page;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.ColorPicker;
import com.smartgwt.client.widgets.form.events.ColorSelectedEvent;
import com.smartgwt.client.widgets.form.events.ColorSelectedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.action.AbstractAction;
import cz.incad.pas.editor.client.action.ActionEvent;
import cz.incad.pas.editor.client.action.Actions;
import cz.incad.pas.editor.client.ds.RestConfig;
import java.util.ArrayList;

/**
 * Shows preview of digital objects.
 *
 * @author Jan Pokorsky
 */
public final class DigitalObjectPreview {

    // darker variant #E6E6F5
    public static final String BACKGROUND_COLOR = "#F5F5FB";
    
    private final VLayout previewLayout;
    private Window fullImageWindow;
    private Img fullImage;
    private final Img preview;
    private String selectedPreview;
    private final ArrayList<Canvas> backgroundListeners = new ArrayList<Canvas>();
    private final ClientMessages i18n;

    public DigitalObjectPreview(ClientMessages i18n) {
        this.i18n = i18n;
        preview = new Img();
        preview.setImageType(ImageStyle.CENTER);
        preview.setOverflow(Overflow.AUTO);

        ToolStrip previewToolbar = createPreviewToolbar();
        fullImageWindow = createFullImageWindow();

        previewLayout = new VLayout();
        previewLayout.setBackgroundColor(BACKGROUND_COLOR);
        previewLayout.setMembers(previewToolbar, preview);
        preview.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                showImageAction(RestConfig.URL_DIGOBJECT_FULL, selectedPreview);
            }
        });
    }

    public Canvas asCanvas() {
        return previewLayout;
    }

    public void selectPreviewField(String previewParams) {
        this.selectedPreview = previewParams;
        if (previewParams == null) {
            preview.setSrc("blank.gif");
        } else {
            String url = buildResourceUrl(RestConfig.URL_DIGOBJECT_PREVIEW, previewParams);
            preview.setSrc(url);
        }
    }

    public void addBackgroundListeners(Canvas c) {
        backgroundListeners.add(c);
    }

    private ToolStrip createPreviewToolbar() {
        ToolStrip toolbar = Actions.createToolStrip();

        ToolStripButton btnViewFull = Actions.asToolStripButton(new AbstractAction(
                null, "[SKIN]/actions/view.png", i18n.DigitalObjectPreview_ViewFullButton_Hint()) {

            @Override
            public void performAction(ActionEvent event) {
                showImageAction(RestConfig.URL_DIGOBJECT_FULL, selectedPreview);
            }
        }, this);

        ToolStripButton btnViewRaw = Actions.asToolStripButton(new AbstractAction(
                null, "[SKIN]/actions/download.png", i18n.DigitalObjectPreview_ViewRawButton_Hint()) {

            @Override
            public void performAction(ActionEvent event) {
                showImageAction(RestConfig.URL_DIGOBJECT_RAW, selectedPreview);
            }
        }, this);

        ToolStripButton btnColorChooser = Actions.asToolStripButton(new AbstractAction(
                null, "[SKIN]/actions/color_swatch.png", i18n.DigitalObjectPreview_ColorChooserButton_Hint()) {

            @Override
            public void performAction(ActionEvent event) {
                ColorPicker picker = new ColorPicker();
                picker.addColorSelectedHandler(new ColorSelectedHandler() {

                    @Override
                    public void onColorSelected(ColorSelectedEvent event) {
                        setBackground(event.getColor());
                    }
                });
                picker.setDefaultColor(BACKGROUND_COLOR);
                picker.setKeepInParentRect(true);
                picker.show();
            }
        }, this);

        toolbar.addButton(btnViewFull);
        toolbar.addButton(btnViewRaw);
        toolbar.addButton(btnColorChooser);

        return toolbar;
    }

    private Window createFullImageWindow() {
        Window window = new Window();
        window.setWidth(Page.getWidth() - 200);
        window.setHeight(Page.getHeight() - 40);
        window.setAutoCenter(true);
        window.setCanDragResize(true);
        window.setCanDragReposition(true);
        window.setIsModal(true);
        window.setDismissOnEscape(true);
        window.setDismissOnOutsideClick(true);
        window.setKeepInParentRect(true);
        window.setShowMaximizeButton(true);
        window.setShowMinimizeButton(false);

        window.setModalMaskOpacity(10);
        window.setShowModalMask(true);

        window.setShowResizer(true);
        window.setTitle("");
        window.setBodyColor(BACKGROUND_COLOR);
        fullImage = new Img();
        fullImage.setCanFocus(true);
        fullImage.setOverflow(Overflow.AUTO);
        fullImage.setImageType(ImageStyle.CENTER);
        window.setCanFocus(true);
        window.addItem(fullImage);
        return window;
    }

    private void setBackground(String color) {
        previewLayout.setBackgroundColor(color);
        fullImageWindow.setBodyColor(color);
        for (Canvas canvas : backgroundListeners) {
            canvas.setBackgroundColor(color);
        }
    }

    private void showImageAction(String resource, String previewParams) {
        if (previewParams != null) {
            String url = buildResourceUrl(resource, previewParams);
            if (RestConfig.URL_DIGOBJECT_RAW.endsWith(resource)) {
                // open in new window
                com.google.gwt.user.client.Window.open(url, "_blank", "");
            } else {
                fullImage.setSrc(url);
                fullImageWindow.show();
                // put focus inside window to enable Window.setDismissOnEscape
                fullImage.focus();
                // XXX hack to reflect real image size. use gwt Image instead
                new Timer() {

                    @Override
                    public void run() {
                        fullImage.adjustForContent(false);
                    }
                }.schedule(1000);
            }
        }
    }

    /**
     *
     * @param resource REST resource URL
     * @param imageParams for now PID and batch ID
     * @return
     */
    private static String buildResourceUrl(String resource, String imageParams) {
        String url = ClientUtils.format("%s?%s", resource, imageParams);
        return url;
    }

}
