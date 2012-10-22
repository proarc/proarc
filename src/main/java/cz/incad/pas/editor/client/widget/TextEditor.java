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

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.TextAreaWrap;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import cz.incad.pas.editor.client.PasEditorMessages;
import cz.incad.pas.editor.client.action.Action;
import cz.incad.pas.editor.client.action.ActionEvent;
import cz.incad.pas.editor.client.action.Actions;
import cz.incad.pas.editor.client.action.RefreshAction.Refreshable;
import cz.incad.pas.editor.client.action.SaveAction;
import cz.incad.pas.editor.client.action.Selectable;
import cz.incad.pas.editor.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.incad.pas.editor.client.ds.TextDataSource;

/**
 * Edits plain/text data stream.
 *
 * @author Jan Pokorsky
 */
public final class TextEditor implements DatastreamEditor, Refreshable, Selectable<Record> {

    public static final String OCR_TYPE = "OCR";
    public static final String NOTE_TYPE = "NOTE";
    private final PasEditorMessages i18n;
    private final DynamicForm editor;
    private SaveAction saveAction;

    private TextEditor(PasEditorMessages i18n, TextDataSource dataSource) {
        this.i18n = i18n;
        initActions();
        this.editor = createForm(dataSource);
    }

    public static TextEditor ocr(PasEditorMessages i18nPas) {
        return new TextEditor(i18nPas, TextDataSource.getOcr());
    }

    public static TextEditor note(PasEditorMessages i18nPas) {
        return new TextEditor(i18nPas, TextDataSource.getNote());
    }

    @Override
    public void edit(String pid, String batchId, MetaModelRecord model) {
        load(pid);
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
                editor.saveData();
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
        load(editor.getValueAsString(TextDataSource.FIELD_PID));
    }

    private void load(String pid) {
        if (pid != null) {
            Criteria criteria = new Criteria(TextDataSource.FIELD_PID, pid);
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
