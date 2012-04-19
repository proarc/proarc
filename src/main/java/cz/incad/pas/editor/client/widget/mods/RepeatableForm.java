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
import com.smartgwt.client.util.JSOHelper;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.events.ItemChangedEvent;
import com.smartgwt.client.widgets.form.events.ItemChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.incad.pas.editor.client.ClientUtils;
import cz.incad.pas.editor.client.widget.mods.RepeatableFormItem.CustomFormFactory;
import cz.incad.pas.editor.client.widget.mods.event.HasListChangedHandlers;
import cz.incad.pas.editor.client.widget.mods.event.ListChangedEvent;
import cz.incad.pas.editor.client.widget.mods.event.ListChangedHandler;
import java.util.Map;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public final class RepeatableForm extends VLayout implements HasListChangedHandlers {

    private static final Logger LOG = Logger.getLogger(RepeatableForm.class.getName());
    
    RecordList dataModel3 = new RecordList();
    private final CustomFormFactory formFactory;

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


    /**
     * for now it uses ResultSet as a plain static array of records
     */
    public void setData(RecordList data) {
//    public void setData(Record... recods) {
//        System.out.println("## setData: " + data.getLength()
//                + ", allRowsCached: " + data.allRowsCached()
//                + ", allMatchingRowsCached: " + data.allMatchingRowsCached()
//                + ", lengthIsKnown: " + data.lengthIsKnown()
//                + ", rangeIsLoaded: " + data.rangeIsLoaded(1, 2)
//                + ", findAll: " + data.findAll(Collections.emptyMap())
//                );

        Canvas[] members = getMembers();
        removeMembers(members); // XXX discard members to safe memory?

//        this.dataModel = data;
//        this.dataModel2 = new ResultSet(identifierDataSource);
//        dataModel2.setInitialData(data.toArray());
        dataModel3 = data;
//        identifierDataSource.setTestData(data.toArray());

        if (data.isEmpty()) {
            data.add(new Record());
        }
        for (Record record : data.toArray()) {
            DynamicForm form = createIdentifierForm(record);
            Canvas listItem = createListItem(form);
            addMember(listItem);
        }
    }

    public Record[] getData() {
        // XXX get modified records
//        return dataModel2.toArray();
        LOG.info("## RepeatableForm.getData: " + ClientUtils.dump(JSOHelper.convertToJavaScriptArray(dataModel3.toArray())));
        return dataModel3.toArray();
    }

    public RecordList getDataAsRecordList() {
        LOG.info("## RepeatableForm.getDataList: " + dataModel3.getLength() + ", json: " + ClientUtils.dump(JSOHelper.convertToJavaScriptArray(dataModel3.toArray())));
        return dataModel3;
    }

    private DynamicForm createIdentifierForm(final Record record) {
        final DynamicForm form = formFactory.create();

        form.addItemChangedHandler(new ItemChangedHandler() {

            @Override
            public void onItemChanged(ItemChangedEvent event) {
                LOG.info("DynamFormList.formItemChanged: " + form.getID()
                        + ", item: " + event.getItem().getName()
                        + ", newVal: " + event.getNewValue()
//                        + ", RL[0]" + form.getRecordList().get(0)
                        );
                //                GWT.log("DynamFormList.formItemChanged2: " + form.getID()
                //                        + ", item: " + event.getItem().getName()
                //                        + ", RL.valManager: " + form.getValuesManager()
                //                        + ", RL.valAsRecord: " + ClientUtils.dump(form.getValuesAsRecord().getJsObj())
                //                        + ", RL.record: " + ClientUtils.dump(record.getJsObj())
                //                        + ", RL.value: " + form.getValue("type")
                ////                        + ", RL.isEmpty: " + form.getRecordList().isEmpty()
                //                        );
                //                form.saveData();
                //                GWT.log("DynamFormList.formItemChanged3: " + form.getID()
                //                        + ", item: " + event.getItem().getName()
                //                        + ", RL.valManager: " + form.getValuesManager()
                //                        + ", RL.valAsRecord: " + ClientUtils.dump(form.getValuesAsRecord().getJsObj())
                //                        + ", RL.record: " + ClientUtils.dump(record.getJsObj())
                //                        + ", RL.value: " + form.getValue("type")
                //                        );
                Record valuesAsRecord = form.getValuesAsRecord();
                Map values = form.getValues();
                LOG.info("DynamFormList.formItemChanged2: " + ClientUtils.dump(valuesAsRecord.getJsObj()));
                LOG.info("DynamFormList.formItemChanged3: " + ClientUtils.dump(values, "", "  ", new StringBuilder()).toString());

                record.setAttribute(event.getItem().getName(), event.getNewValue());
//                Record copyRecord = identifierDataSource.copyRecord(record);
////                dataModel3.
                RepeatableForm.this.fireEvent(new ListChangedEvent());
//                Timer timer = new Timer() {
//
//                                  @Override
//                                  public void run() {
//                RepeatableForm.this.fireEvent(new ListChangedEvent());
//                                  }
//                              };
//                timer.schedule(100);
            }
        });
        if (record != null) {
            form.editRecord(record);
        } else {
//            form.editNewRecord();
        }
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
                DynamicForm form = createIdentifierForm(newRecord);
//                DynamicForm form = createIdentifierForm(new Record() {{
//                    setAttribute("type", "UUID");
//                    setAttribute("value", "aaaa");
//                }});
                Canvas newListItem = createListItem(form);
                int itemIndex = -1;
                if (empty) {
                    RepeatableForm.this.removeMember(item);
                } else {
                    itemIndex = RepeatableForm.this.getMemberNumber(item);
                }
                RepeatableForm.this.addMember(newListItem, itemIndex + 1);
                dataModel3.addAt(newRecord, itemIndex + 1);
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
                int itemIndex = RepeatableForm.this.getMemberNumber(item);
                dataModel3.removeAt(itemIndex);
                RepeatableForm.this.removeMember(item);
                if (RepeatableForm.this.getMembers().length == 0) {
                    Canvas emptyItem = createEmptyListItem();
                    RepeatableForm.this.addMember(emptyItem);
                }
                RepeatableForm.this.fireEvent(new ListChangedEvent());
            }
        });
        btnRemove.setAutoFit(true);

        hLayout.addMember(btnRemove);
        return hLayout;
    }

}
