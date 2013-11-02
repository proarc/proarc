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
package cz.incad.pas.editor.client.widget.mods;

import com.google.gwt.event.shared.HandlerRegistration;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.ItemChangedEvent;
import com.smartgwt.client.widgets.form.events.ItemChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.incad.pas.editor.client.widget.mods.RepeatableFormItem.CustomFormFactory;
import cz.incad.pas.editor.client.widget.mods.event.HasListChangedHandlers;
import cz.incad.pas.editor.client.widget.mods.event.ListChangedEvent;
import cz.incad.pas.editor.client.widget.mods.event.ListChangedHandler;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Widget to display {@link RepeatableFormItem}.
 *
 * @author Jan Pokorsky
 */
public final class RepeatableForm extends VLayout implements HasListChangedHandlers {

    private static final Logger LOG = Logger.getLogger(RepeatableForm.class.getName());

    /** Holds origin records with special fields. */
    private RecordList dataModel = new RecordList();
    private final CustomFormFactory formFactory;
    /** active forms */
    private ArrayList<DynamicForm> forms = new ArrayList<DynamicForm>();
    /** pool of available form widgets */
    private ArrayList<ItemForm> pool = new ArrayList<ItemForm>();
    /** active form widgets */
    private ArrayList<ItemForm> active = new ArrayList<ItemForm>();

    private static final class ItemForm {
        private DynamicForm form;
        private Canvas view;

        public ItemForm(DynamicForm form, Canvas view) {
            this.form = form;
            this.view = view;
        }
    }

    public RepeatableForm(String title, CustomFormFactory customForm) {
        setGroupTitle(title);
        setIsGroup(true);
        setLayoutTopMargin(6);
        setLayoutLeftMargin(4);
        setAutoHeight();
        setAutoWidth();
        this.formFactory = customForm;
    }

    @Override
    public HandlerRegistration addListChangedHandler(ListChangedHandler handler) {
        return doAddHandler(handler, ListChangedEvent.TYPE);
    }

    public void setData(Record... data) {
        if (data == null || data.length == 0) {
            data = new Record[] {new Record()};
        }
        RecordList recordList = new RecordList(data);
        setData(recordList);
    }

    public boolean validate(boolean showError) {
        boolean valid = true;
        for (DynamicForm df : forms) {
            valid &= showError ? df.validate() : df.valuesAreValid(false);
        }
        return valid;
    }

    public void showErrors() {
        for (DynamicForm df : forms) {
            if (!df.getErrors().isEmpty()) {
                df.showErrors();
            }
        }
    }

    public void clearErrors(boolean show) {
        for (DynamicForm df : forms) {
            if (!df.getErrors().isEmpty()) {
                df.clearErrors(show);
            }
        }
    }

    public List<Map<Object, Object>> getErrors() {
        ArrayList<Map<Object, Object>> result = new ArrayList<Map<Object, Object>>();
        for (DynamicForm df : forms) {
            @SuppressWarnings("unchecked")
            Map<Object, Object> errors = df.getErrors();
            if (!errors.isEmpty()) {
                result.add(errors);
            }
        }
        return result;
    }

    /**
     * for now it uses ResultSet as a plain static array of records
     */
    public void setData(RecordList data) {
        forms.clear();
        dataModel = data;
        if (data.isEmpty()) {
            data.add(new Record());
        }

        int itemSize = 0;
        for (; itemSize < data.getLength(); itemSize++) {
            Record record = data.get(itemSize);
            DynamicForm form;
            if (itemSize < active.size()) {
                ItemForm itemForm = active.get(itemSize);
                form = itemForm.form;
            } else {
                ItemForm itemForm = getItemForm();
                form = itemForm.form;
                addMember(itemForm.view);
                active.add(itemForm);
            }
            form.editRecord(record);
            forms.add(form);
        }
        while (itemSize < active.size()) {
            ItemForm remove = active.remove(itemSize);
            pool.add(remove);
            removeMember(remove.view);
        }
    }

    /**
     * Gets available item from the pool or creates new one.
     */
    private ItemForm getItemForm() {
        ItemForm itemForm;
        if (pool.isEmpty()) {
            DynamicForm form = createIdentifierForm();
            Canvas listItem = createListItem(form);
            itemForm = new ItemForm(form, listItem);
        } else {
            itemForm = pool.remove(0);
            itemForm.form.clearValues();
        }
        return itemForm;
    }

    public Record[] getData() {
        return getDataAsRecordList().toArray();
    }

    public RecordList getDataAsRecordList() {
        return dataModel;
    }

    private DynamicForm createIdentifierForm() {
        final DynamicForm form = formFactory.create();
        form.setShowInlineErrors(true);
        form.setShowErrorStyle(true);

        form.addItemChangedHandler(new ItemChangedHandler() {

            @Override
            public void onItemChanged(ItemChangedEvent event) {
                // get original record and update its attribute with new values
                Record record = dataModel.get(forms.indexOf(form));
                record.setAttribute(event.getItem().getName(), event.getNewValue());
                RepeatableForm.this.fireEvent(new ListChangedEvent());
            }
        });
        return form;
    }

    private Canvas createEmptyListItem() {
        return createListItem(null);
    }

    private Canvas createListItem(Canvas c) {
        HLayout hLayout = new HLayout();
        boolean emptyList = c == null;
        Canvas buttons = createItemButtons(hLayout, emptyList);
        if (!emptyList) {
//            c.setWidth100();
            hLayout.addMember(c);
        }
        hLayout.addMember(buttons);
        if (emptyList) {
            Canvas spacer = new Canvas();
            spacer.setWidth(100);
            hLayout.addMember(spacer);
        }
        return hLayout;
    }

    private Canvas createItemButtons(final Canvas item, final boolean empty) {
        HLayout hLayout = new HLayout(2);
        hLayout.setLayoutMargin(2);
        IButton btnAdd = new IButton("+", new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                Record newRecord = new Record();
                ItemForm itemForm = getItemForm();
                Canvas newListItem = itemForm.view;
                int itemIndex = -1;
                if (empty) {
                    RepeatableForm.this.removeMember(item);
                } else {
                    itemIndex = RepeatableForm.this.getMemberNumber(item);
                }
                RepeatableForm.this.addMember(newListItem, itemIndex + 1);
                forms.add(itemIndex + 1, itemForm.form);
                dataModel.addAt(newRecord, itemIndex + 1);
                active.add(itemIndex + 1, itemForm);
                RepeatableForm.this.fireEvent(new ListChangedEvent());
            }
        });
        btnAdd.setAutoFit(true);
        hLayout.addMember(btnAdd);
        if (empty) {
            return hLayout;
        }

        IButton btnRemove = new IButton("-", new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                if (RepeatableForm.this.getMembers().length == 1) {
                    // do not remove last item
                    return ;
                }
                int itemIndex = RepeatableForm.this.getMemberNumber(item);
                forms.remove(itemIndex);
                ItemForm itemForm = active.remove(itemIndex);
                pool.add(itemForm);
                dataModel.removeAt(itemIndex);
                RepeatableForm.this.removeMember(item);
                RepeatableForm.this.fireEvent(new ListChangedEvent());
            }
        });
        btnRemove.setAutoFit(true);

        hLayout.addMember(btnRemove);
        return hLayout;
    }

}
