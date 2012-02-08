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
import com.smartgwt.client.util.JSOHelper;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.ItemChangedEvent;
import com.smartgwt.client.widgets.form.fields.CanvasItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.events.FormItemInitHandler;
import com.smartgwt.client.widgets.form.fields.events.ShowValueEvent;
import com.smartgwt.client.widgets.form.fields.events.ShowValueHandler;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.widget.mods.event.ListChangedEvent;
import cz.incad.pas.editor.client.widget.mods.event.ListChangedHandler;

/**
 *
 * @author Jan Pokorsky
 */
public final class RepeatableFormItem extends CanvasItem {

    private DataSource dataSource;
    private DynamicForm formPrototype;

    public RepeatableFormItem(String name, String title) {
        super(name, title);

//        setStartRow(false);
//        setEndRow(false);
        setShowTitle(false);

        setShouldSaveValue(true);
        setInitHandler(new FormItemInitHandler() {

            @Override
            public void onInit(FormItem item) {
                RepeatableForm editor = new RepeatableForm(item.getTitle());
                editor.setDataSource(dataSource);
                editor.setFormPrototype(formPrototype);
                Object value = item.getValue();
                System.out.println("## onInit: " + value);
                setData(editor, value);

                editor.addListChangedHandler(new ListChangedHandler() {

                    @Override
                    public void onListChanged(ListChangedEvent event) {
                        RepeatableForm editor = (RepeatableForm) event.getSource();
                        Record[] data = editor.getData();
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
                            System.out.println("## onShowValue: " + event.getSource());
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

}
