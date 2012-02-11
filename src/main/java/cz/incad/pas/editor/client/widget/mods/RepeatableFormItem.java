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

import com.google.gwt.core.client.JavaScriptObject;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CanvasItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.events.FormItemInitHandler;
import com.smartgwt.client.widgets.form.fields.events.ShowValueEvent;
import com.smartgwt.client.widgets.form.fields.events.ShowValueHandler;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.widget.mods.event.ListChangedEvent;
import cz.incad.pas.editor.client.widget.mods.event.ListChangedHandler;
import java.util.logging.Logger;

/**
 * Repeatable dynamic form item. Set {@link #setDataSource(com.smartgwt.client.data.DataSource)
 * custom DataSource} to use all its data field. Otherwise set
 * {@link CustomFormFactory } to define own logic.
 *
 * @author Jan Pokorsky
 */
public final class RepeatableFormItem extends CanvasItem {

    private static final Logger LOG = Logger.getLogger(RepeatableFormItem.class.getName());
    
    private DataSource dataSource;
    private DynamicForm formPrototype;
    private CustomFormFactory formFactory;

    public RepeatableFormItem(String name, String title) {
        this(name, title, null);
    }
    
    public RepeatableFormItem(String name, String title, CustomFormFactory formFactory) {
        super(name, title);
        this.formFactory = formFactory;

//        setStartRow(false);
//        setEndRow(false);
        setShowTitle(false);

        setShouldSaveValue(true);
        setInitHandler(new FormItemInitHandler() {

            @Override
            public void onInit(FormItem item) {
                if (RepeatableFormItem.this.formFactory == null) {
                    RepeatableFormItem.this.formFactory = new DefaultCustomForm(formPrototype, dataSource);
                }
                RepeatableForm editor = new RepeatableForm(item.getTitle(), RepeatableFormItem.this.formFactory);
                Object value = item.getValue();
                ClientUtils.info(LOG, "## onInit: %s, dump: %s", value, ClientUtils.dump(value));
                setData(editor, value);

                editor.addListChangedHandler(new ListChangedHandler() {

                    @Override
                    public void onListChanged(ListChangedEvent event) {
                        RepeatableForm editor = (RepeatableForm) event.getSource();
//                        Record[] data = editor.getData();
//                        SC.say(SC.echo(JSOHelper.arrayConvert(data)));
                        RecordList dataAsRecordList = editor.getDataAsRecordList();
                        CanvasItem canvasItem = editor.getCanvasItem();
//                        canvasItem.storeValue(data);
//                        canvasItem.fireEvent(ItemChangedEvent.fire(data, jsObj));
//                        canvasItem.storeValue(editor.getDataAsRecordList());
//                          canvasItem.storeValue(new RecordList(dataAsRecordList.duplicate()));
                          canvasItem.storeValue(dataAsRecordList.duplicate());
                  }
                });
                
                setCanvas(editor);

                addShowValueHandler(new ShowValueHandler() {

                    @Override
                    public void onShowValue(ShowValueEvent event) {
                        RepeatableForm editor = (RepeatableForm) event.getItem().getCanvas();
                        if (editor != null) {
                            Object dataValue = event.getDataValueAsRecordList();
                            ClientUtils.info(LOG, "## onShowValue: source: %s, dump: %s", event.getSource(), ClientUtils.dump(dataValue));
                            setData(editor, dataValue);
                        }
                    }
                });
            }
        });
    }

    public void setDataSource(DataSource ds) {
        this.dataSource = ds;
    }

    public void setFormPrototype(DynamicForm formPrototype) {
        this.formPrototype = formPrototype;
    }

    public void setCustomForm(CustomFormFactory factory) {
        this.formFactory = factory;
    }

    private static void setData(RepeatableForm editor, Object value) {
//        if (value instanceof JavaScriptObject) {
//            value = Record.convertToRecordArray((JavaScriptObject) value);
//
//        }
        if (value == null || value instanceof Record[]) {
            editor.setData((Record[]) value);
        } else if (value instanceof RecordList) {
            editor.setData((RecordList) value);
        } else {
            String msg = "";
            if (value instanceof JavaScriptObject) {
                msg = ClientUtils.dump((JavaScriptObject) value);
            }
            throw new IllegalStateException("unsupported value type: " + value.getClass() + ", dump: \n" + msg);
        }
    }

    /**
     * Allows custom implementation of repeatable form.
     */
    public interface CustomFormFactory {

        DynamicForm create();

    }

    private static final class DefaultCustomForm implements CustomFormFactory {

        private final DynamicForm formPrototype;
        private final DataSource dataSource;

        public DefaultCustomForm(DynamicForm formPrototype, DataSource dataSource) {
            this.formPrototype = formPrototype != null ? formPrototype : new DynamicForm();
            this.dataSource = dataSource;
        }

        @Override
        public DynamicForm create() {
            final DynamicForm form = new DynamicForm();
            form.setNumCols(formPrototype.getNumCols());
            form.setDataSource(dataSource);
            if (formPrototype.getUseAllDataSourceFields()) {
                form.setUseAllDataSourceFields(true);
            }
            return form;
        }

    }

}
