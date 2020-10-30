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
package cz.cas.lib.proarc.webapp.client.widget;

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.DSCallback;
import com.smartgwt.client.data.DSRequest;
import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.TextAreaWrap;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.action.Action;
import cz.cas.lib.proarc.webapp.client.action.ActionEvent;
import cz.cas.lib.proarc.webapp.client.action.Actions;
import cz.cas.lib.proarc.webapp.client.action.RefreshAction.Refreshable;
import cz.cas.lib.proarc.webapp.client.action.SaveAction;
import cz.cas.lib.proarc.webapp.client.action.Selectable;
import cz.cas.lib.proarc.webapp.client.ds.DigitalObjectDataSource.DigitalObject;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.client.ds.TextDataSource;

/**
 * Edits plain/text data stream.
 *
 * @author Jan Pokorsky
 */
public final class TextEditor implements DatastreamEditor, Refreshable, Selectable<Record> {

    public static final String OCR_TYPE = "OCR";
    public static final String NOTE_TYPE = "NOTE";
    private final ClientMessages i18n;
    private final DynamicForm editor;
    private SaveAction saveAction;
    private DigitalObject editObject;

    private TextEditor(ClientMessages i18n, TextDataSource dataSource) {
        this.i18n = i18n;
        initActions();
        this.editor = createForm(dataSource);
    }

    public static TextEditor ocr(ClientMessages i18n) {
        return new TextEditor(i18n, TextDataSource.getOcr());
    }

    public static TextEditor note(ClientMessages i18n) {
        return new TextEditor(i18n, TextDataSource.getNote());
    }

    public static TextEditor technical(ClientMessages i18n) {
        return new TextEditor(i18n, TextDataSource.getTechnicalMetadata());
    }

    @Override
    public void edit(DigitalObject digitalObject) {
        this.editObject = digitalObject;
        load(editObject);
    }

    @Override
    public void focus() {
        editor.focus();
    }

    @Override
    public Canvas[] getToolbarItems() {
        return new Canvas[] {
            Actions.asIconButton(saveAction, this)
        };
    }

    private Action[] initActions() {
        saveAction = new SaveAction(i18n) {

            @Override
            public void performAction(ActionEvent event) {
                Record r = editor.getValuesAsRecord();
                editor.saveData(new DSCallback() {

                    @Override
                    public void execute(DSResponse response, Object rawData, DSRequest request) {
                        if (RestConfig.isStatusOk(response)) {
                            StatusView.getInstance().show(i18n.SaveAction_Done_Msg());
                        }
                    }
                });
            }
        };
        return new Action[] {saveAction};
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
    public Record[] getSelection() {
        Record r = editor.getValuesAsRecord();
        return new Record[] { r };
    }

    @Override
    public Canvas getUI() {
        return editor;
    }

    @Override
    public void refresh() {
        load(editObject);
    }

    private void load(DigitalObject digitalObject) {
        if (digitalObject != null) {
            Criteria criteria = new Criteria(TextDataSource.FIELD_PID, digitalObject.getPid());
            if (digitalObject.getBatchId() != null) {
                criteria.addCriteria(TextDataSource.FIELD_BATCHID, digitalObject.getBatchId());
            }
            editor.fetchData(criteria);
        }
    }

    private DynamicForm createForm(TextDataSource dataSource) {
        DynamicForm form = new DynamicForm();
        form.setDataSource(dataSource);
        form.setWidth100();
        form.setHeight100();
        TextAreaItem textAreaItem = new TextAreaItem(TextDataSource.FIELD_CONTENT);
        textAreaItem.setColSpan("*");
        textAreaItem.setHeight("*");
        textAreaItem.setWrap(TextAreaWrap.OFF);
        textAreaItem.setShowTitle(false);
        textAreaItem.setWidth("*");
        form.setFields(textAreaItem);
        return form;
    }

}
