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
package cz.cas.lib.proarc.webapp.client.widget.mods;

import com.google.gwt.event.shared.HandlerRegistration;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.events.ItemChangedEvent;
import com.smartgwt.client.widgets.form.events.ItemChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableFormItem.CustomFormFactory;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableFormItem.FormWidget;
import cz.cas.lib.proarc.webapp.client.widget.mods.RepeatableFormItem.FormWidgetFactory;
import cz.cas.lib.proarc.webapp.client.widget.mods.event.HasListChangedHandlers;
import cz.cas.lib.proarc.webapp.client.widget.mods.event.ListChangedEvent;
import cz.cas.lib.proarc.webapp.client.widget.mods.event.ListChangedHandler;
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
    /** pool of available form widgets */
    private ArrayList<Row> pool = new ArrayList<Row>();
    /** active form rows */
    private ArrayList<Row> activeRows = new ArrayList<Row>();
    private final RepeatableFormItem formItem;

    private static final class Row {
        private FormWidget formWidget;
        /** form + buttons */
        private Canvas view;
        private IButton buttonAdd;
        private IButton buttonRemove;

        public ValuesManager getForm() {
            return formWidget.getValues();
        }

        public FormWidget getFormWidget() {
            return formWidget;
        }

        public void setFormWidget(FormWidget formWidget) {
            this.formWidget = formWidget;
        }

        public Canvas getView() {
            return view;
        }

        public void setView(Canvas view) {
            this.view = view;
        }

        public IButton getButtonAdd() {
            return buttonAdd;
        }

        public void setButtonAdd(IButton buttonAdd) {
            this.buttonAdd = buttonAdd;
        }

        public IButton getButtonRemove() {
            return buttonRemove;
        }

        public void setButtonRemove(IButton buttonRemove) {
            this.buttonRemove = buttonRemove;
        }
    }

    RepeatableForm(RepeatableFormItem item) {
        this.formItem = item;
        setAutoHeight();
        this.formFactory = item.getFormFactory();
        if (formItem.getMaxOccurrences() > 1 || formItem.getTitle() != null) {
            setGroupTitle(formItem.getTitle());
            setIsGroup(true);
            setLayoutTopMargin(6);
            setLayoutLeftMargin(4);
        }
        if (formItem.isAutoWidth()) {
            setAutoWidth();
        } else if (formItem.isWidth100()) {
            setWidth100();
        } else {
            setWidth(formItem.getWidthAsString());
        }
//        ClientUtils.info(LOG, "init.RForm, name: %s, autoWidth: %s, width100: %s, width: %s",
//                formItem.getName(), formItem.isAutoWidth(), formItem.isWidth100(), formItem.getWidthAsString());
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
        for (Row row : activeRows) {
            ValuesManager vm = row.getForm();
            for (DynamicForm df : vm.getMembers()) {
                valid &= showError ? df.validate() : df.valuesAreValid(false);
            }
        }
        return valid;
    }

    public void showErrors() {
        for (Row row : activeRows) {
            ValuesManager vm = row.getForm();
            Map<?, ?> errors = vm.getErrors();
            if (errors != null && errors.isEmpty()) {
                vm.showErrors();
            }
        }
    }

    public void clearErrors(boolean show) {
        for (Row row : activeRows) {
            ValuesManager vm = row.getForm();
            // vm.clearErrors() broken in SmartGWT 3.0
            for (DynamicForm form : vm.getMembers()) {
                form.clearErrors(show);
            }
        }
    }

    public List<Map<Object, Object>> getErrors() {
        ArrayList<Map<Object, Object>> result = new ArrayList<Map<Object, Object>>();
        for (Row row : activeRows) {
            ValuesManager vm = row.getForm();
            @SuppressWarnings("unchecked")
            Map<Object, Object> errors = vm.getErrors();
            if (errors != null && !errors.isEmpty()) {
                result.add(errors);
            }
        }
        return result;
    }

    /**
     * for now it uses ResultSet as a plain static array of records
     */
    public void setData(RecordList data) {
        dataModel = data;
        if (data.isEmpty()) {
            data.add(new Record());
        }

        int rowIndex = 0;
        for (; rowIndex < data.getLength(); rowIndex++) {
            Record record = data.get(rowIndex);
            ValuesManager form;
            Row row;
            if (rowIndex < activeRows.size()) {
                // reuse active row
                row = activeRows.get(rowIndex);
                form = row.getForm();
            } else {
                // get new row
                row = getRow();
                form = row.getForm();
                addMember(row.getView());
                activeRows.add(row);
            }
            showRow(row);
            form.editRecord(record);
            row.getFormWidget().fireDataLoad();
        }
        // release unused rows
        while (rowIndex < activeRows.size()) {
            Row remove = activeRows.remove(rowIndex);
            pool.add(remove);
            removeMember(remove.view);
        }
    }

    /**
     * Gets available item from the pool or creates new one.
     */
    private Row getRow() {
        Row row;
        if (pool.isEmpty()) {
            row = new Row();
            createRowForm(row);
            Canvas listItem = createRowWidget(row);
            row.setView(listItem);
        } else {
            row = pool.remove(0);
            row.getForm().clearValues();
        }
        return row;
    }

    public Record[] getData() {
        return getDataAsRecordList().toArray();
    }

    public RecordList getDataAsRecordList() {
        return dataModel;
    }

    private void createRowForm(final Row row) {
        ValuesManager vm;
        if (formFactory instanceof FormWidgetFactory) {
            FormWidgetFactory ff = (FormWidgetFactory) formFactory;
            FormWidget formWidget = ff.createFormWidget(formItem.getProfile());
            row.setFormWidget(formWidget);
            vm = formWidget.getValues();
        } else {
            DynamicForm form = formFactory.create();
            vm = form.getValuesManager();
            if (vm == null) {
                vm = new ValuesManager();
                vm.addMember(form);
            }
            FormWidget formWidget = new FormWidget(form, vm);
            row.setFormWidget(formWidget);
        }
        for (DynamicForm form : vm.getMembers()) {
            form.setShowInlineErrors(true);
            form.setShowErrorStyle(true);

            form.addItemChangedHandler(new ItemChangedHandler() {

                @Override
                public void onItemChanged(ItemChangedEvent event) {
                    // get original record and update its attribute with new values
                    Record record = dataModel.get(activeRows.indexOf(row));
                    record.setAttribute(event.getItem().getName(), event.getNewValue());
                    RepeatableForm.this.fireEvent(new ListChangedEvent());
                }
            });
        }
    }

    private Canvas createRowWidget(Row row) {
        HLayout hLayout = new HLayout();
        Canvas buttons = createItemButtons(row);
        buttons.setLayoutAlign(VerticalAlignment.BOTTOM);
        hLayout.addMember(row.getFormWidget().getWidget());
        hLayout.addMember(buttons);
        return hLayout;
    }

    private void showRow(Row row) {
        boolean repeatable = formItem.getMaxOccurrences() != 1;
        row.getButtonAdd().setVisible(repeatable);
        row.getButtonRemove().setVisible(repeatable);
    }

    private void onAddRowClick(int eventRowIndex) {
        if (activeRows.size() >= formItem.getMaxOccurrences()) {
            return ;
        }
        Row row = getRow();
        Canvas newListItem = row.getView();
        RepeatableForm.this.addMember(newListItem, eventRowIndex + 1);
        Record newRecord = new Record();
        dataModel.addAt(newRecord, eventRowIndex + 1);
        activeRows.add(eventRowIndex + 1, row);
        // disable Add buttons
        if (activeRows.size() >= formItem.getMaxOccurrences()) {
            setAddDisabled(true);
        }
        RepeatableForm.this.fireEvent(new ListChangedEvent());
    }

    private void onRemoveRowClick(int eventRowIndex) {
        if (activeRows.size() == 1) {
            // do not remove last item
            Row row = activeRows.get(0);
            row.getForm().clearValues();
            return ;
        }
        Row row = activeRows.remove(eventRowIndex);
        pool.add(row);
        dataModel.removeAt(eventRowIndex);
        RepeatableForm.this.removeMember(row.getView());
        setAddDisabled(false);
        RepeatableForm.this.fireEvent(new ListChangedEvent());
    }

    private void setAddDisabled(boolean disable) {
        for (Row row : activeRows) {
            row.getButtonAdd().setDisabled(disable);
        }
    }

    private Canvas createItemButtons(final Row row) {
        HLayout hLayout = new HLayout(2);
        hLayout.setLayoutMargin(2);
        IButton btnAdd = new IButton("+", new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                int rowIndex = activeRows.indexOf(row);
                onAddRowClick(rowIndex);
            }
        });
        btnAdd.setAutoFit(true);
        hLayout.addMember(btnAdd);
        row.setButtonAdd(btnAdd);

        IButton btnRemove = new IButton("-", new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                int rowIndex = activeRows.indexOf(row);
                onRemoveRowClick(rowIndex);
            }
        });
        btnRemove.setAutoFit(true);

        hLayout.addMember(btnRemove);
        row.setButtonRemove(btnRemove);
        return hLayout;
    }

}
