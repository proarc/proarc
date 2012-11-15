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

import com.smartgwt.client.data.Criteria;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import cz.incad.pas.editor.client.action.RefreshAction.Refreshable;
import cz.incad.pas.editor.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.incad.pas.editor.client.ds.ModsCustomDataSource;
import cz.incad.pas.editor.client.ds.TextDataSource;
import cz.incad.pas.editor.client.widget.DatastreamEditor;
import java.util.logging.Logger;

/**
 * Edits MODS data in XML format.
 *
 * For now it is read only.
 * XXX add syntax highlighting
 *
 * @author Jan Pokorsky
 */
final class ModsXmlEditor implements DatastreamEditor, Refreshable {

    private static final Logger LOG = Logger.getLogger(ModsXmlEditor.class.getName());
    private final DynamicForm sourceForm;
    private String pid;

    public ModsXmlEditor() {
        sourceForm = new DynamicForm();
        sourceForm.setCanEdit(false);
        sourceForm.setWidth100();
        sourceForm.setHeight100();
        sourceForm.setColWidths("*");
        sourceForm.setNumCols(1);
        // TextAreaItem sourceItem = new TextAreaItem(ModsTextDataSource.FIELD_CONTENT);
        // TextAreaItem.setCanEdit is unsupported http://mytechscratchpad.blogspot.com/2011/08/smartgwt-textareaitem-readonly.html
        StaticTextItem sourceItem = new StaticTextItem(TextDataSource.FIELD_CONTENT);
        sourceItem.setEscapeHTML(true);
        sourceItem.setWidth("*");
        sourceItem.setHeight("*");
        sourceItem.setShowTitle(false);
        sourceForm.setFields(sourceItem);
        sourceForm.setDataSource(TextDataSource.getMods());
    }

    @Override
    public void edit(String pid, String batchId, MetaModelRecord model) {
        this.pid = pid;
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
            sourceForm.fetchData(pidCriteria);
        }
    }

}
