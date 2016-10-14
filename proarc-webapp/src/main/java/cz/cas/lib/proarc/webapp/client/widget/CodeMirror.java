/*
 * Copyright (C) 2016 Jan Pokorsky
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

import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.dom.client.TextAreaElement;
import com.google.gwt.user.client.ui.TextArea;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.WidgetCanvas;
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.events.ResizedEvent;

/**
 * The CodeMirror wrapper.
 *
 * @author Jan Pokorsky
 *
 * @see <a href='http://codemirror.net'>codemirror.net</a>
 */
public class CodeMirror {

    private final Canvas sourceForm;
    private JavaScriptObject wrapper;

    public CodeMirror() {
        final TextArea textArea = new TextArea();

        sourceForm = new WidgetCanvas(textArea);
        sourceForm.setWidth100();
        sourceForm.setHeight100();
        sourceForm.addDrawHandler((DrawEvent event) -> {
            // create wrapper on each draw as it is being discarded each time
            // the parent layout sets new members
            wrapper = initCodeMirrorJSO(textArea.getElement().<TextAreaElement>cast());
            refresh(sourceForm.getInnerContentHeight());
        });
        sourceForm.addResizedHandler((ResizedEvent event) -> {
            if (wrapper != null) {
                refresh(sourceForm.getInnerContentHeight());
            }
        });
    }

    public Canvas getUI() {
        return sourceForm;
    }

    public native void clearHistory() /*-{
        var cm = this.@cz.cas.lib.proarc.webapp.client.widget.CodeMirror::wrapper;
        if (cm) {
            cm.clearHistory();
        }
    }-*/;

    public native String getContent() /*-{
        var cm = this.@cz.cas.lib.proarc.webapp.client.widget.CodeMirror::wrapper;
        if (cm) {
            return cm.getValue();
        } else {
            return null;
        }
    }-*/;

    public native void setContent(String content) /*-{
        var cm = this.@cz.cas.lib.proarc.webapp.client.widget.CodeMirror::wrapper;
        if (cm) {
            cm.setValue(content);
        }
    }-*/;

    /**
     * Helps to stretch CodeMirrorWrapper in height as tweaking CodeMirror:height:100%
     * in codemirror.css does not work.
     */
    private native void refresh(Integer height) /*-{
        var cm = this.@cz.cas.lib.proarc.webapp.client.widget.CodeMirror::wrapper;
        if (cm) {
            cm.setSize(null, height - 8)
            cm.refresh();
        }
    }-*/;

    private native JavaScriptObject initCodeMirrorJSO(TextAreaElement tae) /*-{
        var cm = $wnd.CodeMirror.fromTextArea(tae, {
            mode: "xml",
            lineNumbers: true,
            lineWrapping: false,
            indentUnit: 4,
            autoCloseTags: true,
            foldGutter: true,
            gutters: ["CodeMirror-linenumbers", "CodeMirror-foldgutter"]
        });
        return cm;
    }-*/;

}
