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

import com.google.codemirror2_gwt.client.CodeMirrorConfig;
import com.google.codemirror2_gwt.client.CodeMirrorWrapper;
import com.google.codemirror2_gwt.client.GutterClickHandler;
import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.client.ui.TextArea;
import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.WidgetCanvas;
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.events.DrawHandler;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
import cz.incad.pas.editor.client.action.RefreshAction.Refreshable;
import cz.incad.pas.editor.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.incad.pas.editor.client.ds.ModsCustomDataSource;
import cz.incad.pas.editor.client.ds.RestConfig;
import cz.incad.pas.editor.client.ds.TextDataSource;
import cz.incad.pas.editor.client.widget.DatastreamEditor;
import java.util.logging.Logger;

/**
 * Edits MODS data in XML format.
 *
 * For now it is read only.
 *
 * @author Jan Pokorsky
 */
final class ModsXmlEditor implements DatastreamEditor, Refreshable {

    private static final Logger LOG = Logger.getLogger(ModsXmlEditor.class.getName());
    private final Canvas sourceForm;
    private String pid;
    private String batchId;
    private final CodeMirrorConfig config;
    private final TextArea textArea;
    private CodeMirrorWrapper wrapper;
    private JavaScriptObject folding;

    public ModsXmlEditor() {
        config = CodeMirrorConfig.makeBuilder();
        config.setMode("application/xml")
                .setShowLineNumbers(true)
                .setUndoDepth(0)
                .setReadOnly(true)
                .setOnGutterClick(new GutterClickHandler() {

                        @Override
                        public void onClick(int lineNumber) {
                            fold(wrapper, lineNumber);
                        }
                })
                ;
        folding = initFolding();

        textArea = new TextArea();

        sourceForm = new WidgetCanvas(textArea);
//        sourceForm.setBorder("1px solid #A7ABB4");
        sourceForm.addStyleName("defaultBorder");
        sourceForm.setWidth100();
        sourceForm.setHeight100();
        sourceForm.addDrawHandler(new DrawHandler() {

            @Override
            public void onDraw(DrawEvent event) {
                // create wrapper on each draw as it is being discarded each time
                // the parent layout sets new members
                wrapper = CodeMirrorWrapper.createEditorFromTextArea(textArea.getElement(), config);
                refreshEditorHeight();
            }
        });
        sourceForm.addResizedHandler(new ResizedHandler() {

            @Override
            public void onResized(ResizedEvent event) {
                refreshEditorHeight();
            }
        });
    }

    @Override
    public void edit(String pid, String batchId, MetaModelRecord model) {
        this.pid = pid;
        this.batchId = batchId;
        refresh();
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getCapability(Class<T> clazz) {
        T c = null;
        if (Refreshable.class.equals(clazz)) {
            c = (T) this;
        }
        return c;
    }

    @Override
    public Canvas[] getToolbarItems() {
        return new Canvas[0];
    }

    @Override
    public Canvas getUI() {
        return sourceForm;
    }

    @Override
    public void refresh() {
        if (pid != null) {
            Criteria pidCriteria = new Criteria(ModsCustomDataSource.FIELD_PID, pid);
            if (batchId != null) {
                pidCriteria.addCriteria(ModsCustomDataSource.FIELD_BATCHID, batchId);
            }
            TextDataSource.getMods().fetchData(pidCriteria, new DSCallback() {

                @Override
                public void execute(DSResponse response, Object rawData, DSRequest request) {
                    handleFetchResponse(response);
                }
            });
        }
    }

    private void handleFetchResponse(DSResponse response) {
        if (RestConfig.isStatusOk(response)) {
            Record[] data = response.getData();
            if (data != null && data.length == 1) {
                String xml = data[0].getAttribute(TextDataSource.FIELD_CONTENT);
                wrapper.setValue(xml);
                wrapper.clearHistory();
                wrapper.refresh();
            }
        }
    }

    /**
     * Helps to stretch CodeMirrorWrapper in height as tweaking CodeMirror-scroll
     * in codemirror.css does not work.
     */
    private void refreshEditorHeight() {
        if (wrapper != null) {
            Integer height = sourceForm.getInnerContentHeight() - 8;
            wrapper.getScrollerElement().getStyle().setHeight(height, Unit.PX);
            wrapper.refresh();
        }
    }

    private native JavaScriptObject initFolding() /*-{
        return $wnd.CodeMirror.newFoldFunction($wnd.CodeMirror.tagRangeFinder);
    }-*/;

    private native void fold(CodeMirrorWrapper wrapper, int line) /*-{
        var f = this.@cz.incad.pas.editor.client.presenter.ModsXmlEditor::folding;
        f(wrapper, line);
    }-*/;

}
