/*
 * Copyright (C) 2011 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.client.widget;

import com.google.gwt.core.client.JavaScriptObject;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.BkgndRepeat;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.RowEndEditAction;
import com.smartgwt.client.types.Visibility;
import com.smartgwt.client.util.JSOHelper;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.FocusChangedEvent;
import com.smartgwt.client.widgets.events.FocusChangedHandler;
import com.smartgwt.client.widgets.events.KeyPressEvent;
import com.smartgwt.client.widgets.events.KeyPressHandler;
import com.smartgwt.client.widgets.form.fields.CanvasItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.HeaderItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemInitHandler;
import com.smartgwt.client.widgets.form.fields.events.ShowValueEvent;
import com.smartgwt.client.widgets.form.fields.events.ShowValueHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.events.BodyKeyPressEvent;
import com.smartgwt.client.widgets.grid.events.BodyKeyPressHandler;
import com.smartgwt.client.widgets.grid.events.CellSavedEvent;
import com.smartgwt.client.widgets.grid.events.CellSavedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

/**
 * Grid as a FormItem.
 *
 * @author Jan Pokorsky
 */
public class ListFormItem extends CanvasItem {

    private static final Logger LOG = Logger.getLogger(ListFormItem.class.getName());

    private ListGrid listGrid;

    private ListGridField[] fields;
    private String help;

    private ToolStripButton btnSwitch;
    private ToolStripButton btnAdd;

    public ListFormItem(String name, String title) {
        super(name, title);
        setup();
    }

    public ListFormItem(String name) {
        super(name);
        setup();
    }

    public ListFormItem() {
        setup();
    }

    public void setFields(ListGridField... fields) {
        this.fields = fields;
//        setAttribute("fields", fields);
    }

    public ListGridField[] getFields() {
//        JavaScriptObject fieldsJso = getAttributeAsJavaScriptObject("fields");
//        if (fieldsJso == null) {
//            return new ListGridField[0];
//        }
//        JavaScriptObject[] fieldsJsoArray = JSOHelper.toArray(fieldsJso);
//        ListGridField[] fields = new ListGridField[fieldsJsoArray.length];
//        for (int i = 0; i < fieldsJsoArray.length; i++) {
//            JavaScriptObject fieldJso = fieldsJsoArray[i];
//            fields[i] = new ListGridField(fieldJso);
//        }
        return fields;
    }

    public String getHelp() {
        return help;
    }

    public void setHelp(String help) {
        this.help = help;
    }

    public ToolStripButton getButtonSwitch() {
        return btnSwitch;
    }

    public void setButtonSwitch(ToolStripButton btnSwitch) {
        this.btnSwitch = btnSwitch;
    }

    public void changeState(boolean collapsed) {
//        LOG.info("changeState: " + getName() + ", " + collapsed);
        setCanFocus(!collapsed);
        if (btnSwitch != null) {
            String switchIcon = collapsed
                    ? "[SKIN]SectionHeader/opener_closed.png"
                    : "[SKIN]SectionHeader/opener_opened.png";
            btnSwitch.setIcon(switchIcon);
        }
        if (btnAdd != null) {
            btnAdd.setDisabled(collapsed);
        }
        ListGrid grid = getGrid();
        if (grid != null) {
            Canvas gridCanvas = grid.getParentElement();
            if (collapsed) {
                gridCanvas.hide();
            } else {
                gridCanvas.show();
            }
        }
    }

    @Override
    protected Canvas createCanvas() {
        return super.createCanvas();
    }

    @Override
    public void setCanFocus(Boolean canFocus) {
        super.setCanFocus(canFocus);
        Canvas canvas = getCanvas();
        if (canvas != null) {
            canvas.setCanFocus(canFocus);
        }
    }



    private ListGrid getGrid() {
//        Canvas canvas = getCanvas();
//        for (Canvas child : canvas.getChildren()) {
//            if (child instanceof ListGrid) {
//                return (ListGrid) child;
//            }
//        }
//        return null;
        return this.listGrid;
    }

    private void setup() {
        setWidth("*");
        setHeight("*");
        setColSpan("*");
        setEndRow(true);
        setStartRow(true);
        setShouldSaveValue(true);
        setCanFocus(false);

        setInitHandler(new FormItemInitHandler() {

            @Override
            public void onInit(FormItem item) {
                Canvas c = createCanvas(ListFormItem.this);
                item.setCanFocus(false);
                setCanvas(c);
            }
        });

        addShowValueHandler(new ShowValueHandler() {

            @Override
            public void onShowValue(ShowValueEvent event) {
                ListFormItem item = (ListFormItem) event.getSource();
                ListGrid grid = item.getGrid();
//                Object dataValue = event.getDataValue();
//                if (dataValue instanceof Record) {
//                    setData(item, (Record) dataValue);
//                } else if (dataValue instanceof Record[]) {
//                    setData(item, (Record[]) dataValue);
//                } else if (dataValue instanceof List) {
//                    setData(item, (Record[]) ((List) dataValue).toArray(new Record[0]));
//                } else if (dataValue instanceof RecordList) {
//                    setData(item, ((RecordList) dataValue).toArray());
//                } else if (dataValue instanceof JavaScriptObject) {
//                    JavaScriptObject jso = (JavaScriptObject) dataValue;
//                    Record[] convertToRecordArray = Record.convertToRecordArray(jso);
//                    setData(item, convertToRecordArray);
//                } else if (dataValue == null) {
//                    setData(item);
//                } else {
//                    LOG.info(ClientUtils.format("onShowValue.name: %s, value: %s: , class: %s",
//                            item.getName(), dataValue, ClientUtils.safeGetClass(dataValue)));
//                }
////                RecordList rl = event.getDataValueAsRecordList();
////                if (rl != null) {
////                    Record[] toArray = rl.toArray();
////                    setData(item, toArray);
////                } else {
////                    setData(item);
////                }
//                if (true) {
//                    return;
//                }
                RecordList values = event.getDataValueAsRecordList();
//                Object dataValue = event.getDataValue();
//                Class dataValueClass = (dataValue != null) ? dataValue.getClass() : null;
//                LOG.info(ClientUtils.format("onShowValue.name: %s, value: %s: , class: %s", item.getName(), dataValue, dataValueClass));
//
//                JavaScriptObject jsoValue = (JavaScriptObject) dataValue;
//                Map convertToMap = JSOHelper.convertToMap(jsObj);
//                LOG.info(
//                        convertToMap.toString()
////                        ClientUtils.dump(event.getDataValueAsRecord(), "onShowValue.name: " + item.getName())
//                );
//
//                if (true) {
//                    setData(item, null);
//                    return ;
//                }

                if (values == null || values.isEmpty()) {
                    // hide grid in case of no records
                    item.changeState(true);
                } else {
                    item.changeState(false);
                }
                setData(item, values);
            }
        });

    }

    private static void setData(ListFormItem item, Record... data) {
        LOG.fine(ClientUtils.format("[%s] data: %s, class: %s",
                item.getName(),
                data,
                ClientUtils.safeGetClass(data)
                ));
        if (data != null) {
            LOG.finest(ClientUtils.format("[%s] data.getLength: %s", item.getName(), data.length));
            LOG.fine(ClientUtils.format("[%s] data.toArray: %s", item.getName(), Arrays.toString(data)));
        }

        ListGrid canvas = item.getGrid();
        if (canvas != null) {
            canvas.setData(data);
        }
    }

    @Deprecated
    private static void setData(ListFormItem item, RecordList data) {
        LOG.fine(ClientUtils.format("[%s] data: %s, class: %s",
                item.getName(),
                data,
                data != null ? data.getClass() : null));
        if (data != null) {
            LOG.finest(ClientUtils.format("[%s] data.getLength: %s", item.getName(), data.getLength()));
            LOG.fine(ClientUtils.format("[%s] data.toArray: %s", item.getName(), Arrays.toString(data.toArray())));
        }

        ListGrid canvas = item.getGrid();
        if (canvas != null) {
            canvas.setData(data);
        }
    }

    private static Canvas createCanvas(final ListFormItem item) {
        final ListGrid grid = new ListGrid();
        item.listGrid = grid;
//        grid.setAutoWidth();
        grid.setWidth100();
//        grid.setHeight100();
        grid.setCanEdit(true);
        grid.setCanFocus(true);
        grid.setSaveLocally(true);
        grid.setShowEmptyMessage(false);

//        grid.setAutoFitData(Autofit.VERTICAL);
        grid.setAutoFitMaxRecords(10);
        grid.setBodyOverflow(Overflow.VISIBLE);
        grid.setOverflow(Overflow.VISIBLE);
//        grid.setHeight(35);

        grid.setShowAllRecords(true);
        grid.setShowAllColumns(true);
        grid.setListEndEditAction(RowEndEditAction.STOP);
        grid.setCanRemoveRecords(true);
//        grid.setRemoveIcon("minus16.png");
        grid.setRemoveIcon("[SKIN]actions/remove_Disabled.png");
//        grid.setRemoveIcon("[SKIN]DynamicForm/Remove_icon.png");
        grid.setCanResizeFields(true);
//        grid.setSelectionType(SelectionStyle.NONE);
        grid.setWrapCells(true);
        grid.setFixedRecordHeights(false);

//        grid.setAlwaysShowEditors(true);
//        grid.setCellPadding(4);

        grid.setEditEvent(ListGridEditEvent.CLICK);
        
        grid.setShowHeader(false);
        grid.setLeaveScrollbarGap(false);
        grid.setCanReorderRecords(true);

        ListGridField[] fields = item.getFields();
        grid.setFields(fields);
        item.addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                System.out.println("---Item.onChanged: " + event.getValue() + ", item.getForm().valuesHaveChanged(): " +item.getForm().valuesHaveChanged());
            }
        });
        grid.addCellSavedHandler(new CellSavedHandler() {

            int state = 0;
            int modulo = item.getFields().length;
            @Override
            public void onCellSaved(CellSavedEvent event) {
//                state++;
//                if (state % modulo != 0) {
//                    return ;
//                }
//                state = 0;

                ListGrid grid = (ListGrid) event.getSource();
//                item.storeValue(new RecordList(grid.getRecords()));
                RecordList data = grid.getDataAsRecordList();
//                System.out.println("###onCellSaved.grid: " + Arrays.toString(grid.getRecords()));
//                System.out.println("###onCellSaved.list: " + Arrays.toString(data.toArray()));
                Record[] toArray = data.toArray();
                Record[] newArray = new Record[toArray.length];
                Set<String> fieldNames = fieldNames(null, item.getFields());
                for (int i = 0; i < toArray.length; i++) {
                    Record oldrec = toArray[i];
                    Map toMap = oldrec.toMap();
                    clearMap(toMap, fieldNames);

                    Record newrec = new Record(toMap);
                    newArray[i] = newrec;
                }

//                LOG.info(null)

//                item.storeValue(newArray);
                item.storeValue(new RecordList(newArray));
//                item.storeValue(new RecordList(data.toArray()));
            }

            /** removes other than fields mapping */
            private void clearMap(Map m, Set<String> fields) {
//                Object[] keys = m.keySet().toArray();
                Set<?> keySet = m.keySet();
                keySet.retainAll(fields);
//                for (Object key : keys) {
//                    if (!fields.contains(key)) {
//                        m.remove(key);
//                    }
//                }
            }
            private Set<String> fieldNames(Map m, ListGridField[] fields) {
                Set<String> fieldNames = new HashSet<String>();
                for (ListGridField field : fields) {
                    fieldNames.add(field.getName());
                }
                return fieldNames;
            }
        });


        final VLayout layout = new VLayout(0);
        ToolStripButton btnAdd = new ToolStripButton();
        item.btnAdd = btnAdd;
        btnAdd.setIcon("[SKIN]actions/add.png");
//        btnAdd.setIcon("plus16.png");
        btnAdd.setIconSize(16);
        btnAdd.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                if (grid.getRecordList().getLength() < 10) {
                    grid.startEditingNew();
                }
            }
        });
        final ToolStripButton btnSwitch = new ToolStripButton();
//        btnSwitch.setIcon("[SKIN]actions/next.png");
        btnSwitch.setIcon("[SKIN]SectionHeader/opener_opened.png");
        btnSwitch.setIconSize(16);
        btnSwitch.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                item.changeState(grid.isVisible());
            }
        });
        item.setButtonSwitch(btnSwitch);

        final ToolStripButton btnHelp = new ToolStripButton();
        btnHelp.setIcon("[SKIN]actions/help.png");
        btnHelp.setIconSize(16);
        btnHelp.setCanFocus(false);
        btnHelp.setCursor(Cursor.HELP);
        btnHelp.setShowDown(false);
//        btnHelp.setShowFocused(false);
//        btnHelp.setShowSelectedIcon(false);
        String helpHint = item.getHelp();
        btnHelp.setTooltip(helpHint);
        if (helpHint != null && helpHint.length() > 20) {
            btnHelp.setHoverWidth(200);
        }

        ToolStrip toolStrip = new ToolStrip();
        toolStrip.setCanFocus(false);
        toolStrip.setWidth100();
        toolStrip.setAutoHeight();
        toolStrip.addButton(btnSwitch);
//            HeaderItem headerItem = new HeaderItem("header", item.getTitle());
        StaticTextItem headerItem = new StaticTextItem("header", item.getTitle());
        headerItem.setTextBoxStyle("formTitle");
        headerItem.setShowTitle(false);
        headerItem.setDefaultValue(item.getTitle());
        toolStrip.addFormItem(headerItem);
        toolStrip.addFill();
        toolStrip.addButton(btnAdd);
        toolStrip.addButton(btnHelp);
//            PickerIcon refreshPicker = new PickerIcon(PickerIcon.REFRESH);
//            item.setIcons(refreshPicker);

        HLayout gridLayout = new HLayout(0);
        gridLayout.setCanFocus(true);
        gridLayout.setAutoHeight();
        gridLayout.setWidth100();
        gridLayout.setVisibility(Visibility.VISIBLE);

        gridLayout.addMember(grid);

        layout.addMember(toolStrip);
        layout.addMember(gridLayout);
        layout.setAutoHeight();
        layout.setVisibility(Visibility.VISIBLE);
        layout.setCanFocus(item.getCanFocus());
        layout.addFocusChangedHandler(new FocusChangedHandler() {

            @Override
            public void onFocusChanged(FocusChangedEvent event) {
                boolean hasFocus = event.getHasFocus();
//                LOG.info(ClientUtils.format("LFI.layout: item: %s, hasFocus: %s, containsFocus: %s",
//                        item.getName(), hasFocus, layout.containsFocus()));
                if (!grid.getRecordList().isEmpty()) {
                    if (hasFocus) {
                        grid.selectRecord(0);
                        grid.focus();
//                LOG.info("LFI.layout.gotFocus: grid.focus");
                    } else {
//                LOG.info("LFI.layout.lostFocus");
                    }
                }
            }
        });
//        grid.addFocusChangedHandler(new FocusChangedHandler() {
//
//            @Override
//            public void onFocusChanged(FocusChangedEvent event) {
//                boolean hasFocus = event.getHasFocus();
//                if (hasFocus) {
//
//                } else {
//                    LOG.info("LFI.grid: hasFocus: " + hasFocus + ", containsFocus: " + layout.containsFocus());
//                    grid.deselectAllRecords();
//                    Integer tabIndex = item.getTabIndex();
//                    item.getForm().focusInItem(tabIndex + 1);
//                }
//            }
//        });

        // helper widget to dispatch focus properly
        final Canvas focusableBeforeGrid = new Img();
        focusableBeforeGrid.setHeight(1);
        focusableBeforeGrid.setWidth(1);
        focusableBeforeGrid.setPadding(0);
        focusableBeforeGrid.setMargin(0);
        focusableBeforeGrid.setEdgeSize(0);
        focusableBeforeGrid.setMinHeight(1);
        focusableBeforeGrid.setMinWidth(1);
        focusableBeforeGrid.setCanFocus(true);
        focusableBeforeGrid.addFocusChangedHandler(new FocusChangedHandler() {

            @Override
            public void onFocusChanged(FocusChangedEvent event) {
                boolean hasFocus = event.getHasFocus();
//                LOG.info(ClientUtils.format("LFI.focusableBeforeGrid: item: %s, hasFocus: %s, containsFocus: %s",
//                        item.getName(), hasFocus, focusableBeforeGrid.containsFocus()));
                if (hasFocus) {
                    Integer tabIndex = item.getTabIndex();
                    grid.deselectAllRecords();
                    FormItem[] items = item.getForm().getFields();
                    int itemIdx = findIndex(item, items);
                    int itemNextIdx = itemIdx - 1;
                    if (itemNextIdx < 0) {
                        // focus first item
                        itemNextIdx = items.length;
                    }
                    
                    FormItem nextFocusItem = findItemWithDataReversely(itemNextIdx, items);

//                LOG.info("LFI.labelAfterGrid: tabIndex: " + tabIndex + ", items.length: " + items.length
//                        + ", itemName: " + items[tabIndex].getName() + ", nextItemName: " + items[tabIndex + 1].getName()
//                        + "\n" + dump(items));
                    if (nextFocusItem != null) {
                        item.getForm().focusInItem(nextFocusItem);
                    }
                }
            }
        });

        gridLayout.addMember(focusableBeforeGrid, 0);


        // helper widget to dispatch focus properly
//        final Label labelAfterGrid = new Label("Focus dispather: " + item.getName());
        final Canvas focusableAfterGrid = new Img();
//        labelAfterGrid.setAutoHeight();
        focusableAfterGrid.setHeight(1);
        focusableAfterGrid.setWidth(1);
        focusableAfterGrid.setPadding(0);
        focusableAfterGrid.setMargin(0);
        focusableAfterGrid.setEdgeSize(0);
        focusableAfterGrid.setMinHeight(1);
        focusableAfterGrid.setMinWidth(1);
        focusableAfterGrid.setCanFocus(true);
        focusableAfterGrid.addFocusChangedHandler(new FocusChangedHandler() {

            @Override
            public void onFocusChanged(FocusChangedEvent event) {
                boolean hasFocus = event.getHasFocus();
//                LOG.info(ClientUtils.format("LFI.focusableAfterGrid: item: %s, hasFocus: %s, containsFocus: %s",
//                        item.getName(), hasFocus, focusableAfterGrid.containsFocus()));
                if (hasFocus) {
                    Integer tabIndex = item.getTabIndex();
                    grid.deselectAllRecords();
                    FormItem[] items = item.getForm().getFields();
                    int itemIdx = findIndex(item, items);
                    int itemNextIdx = itemIdx + 1;
                    if (itemNextIdx < 0 || itemNextIdx >= items.length) {
                        itemNextIdx = 0;
                    }

                    FormItem nextFocusItem = findItemWithData(itemNextIdx, items);

//                LOG.info("LFI.focusableAfterGrid: tabIndex: " + tabIndex + ", items.length: " + items.length
//                        + ", itemName: " + items[tabIndex].getName() + ", nextItemName: " + items[tabIndex + 1].getName()
//                        + "\n" + dump(items));
                    if (nextFocusItem != null) {
                        item.getForm().focusInItem(nextFocusItem);
                    }
                }
            }
        });
        gridLayout.addMember(focusableAfterGrid);
        return layout;
    }

    private static int findIndex(FormItem item, FormItem[] items) {
        int itemIdx = -1;
        for (int i = 0; i < items.length; i++) {
            if (item == items[i]) {
                itemIdx = i;
            }
        }
        return itemIdx;
    }

    private static FormItem findItemWithDataReversely(int startIndex, FormItem... items) {
        int mid = items.length >> 1;
        if (startIndex != mid) {
            startIndex = items.length - 1 - startIndex;
        }
        List<FormItem> itemReverseList = Arrays.asList(items);
        Collections.reverse(itemReverseList);
        return findItemWithData(startIndex, items);
    }

    private static FormItem findItemWithData(int startIndex, FormItem... items) {
        return findItemWithData(startIndex, items.length, items);
    }

    private static FormItem findItemWithData(int startIndex, int stopIndex, FormItem... items) {
        for (int i = startIndex; i < items.length && i < stopIndex; i++) {
            FormItem item = items[i];
            Boolean canFocus = item.getCanFocus();
            if (canFocus == null || canFocus) {
                return item;
            }
        }

        if (startIndex <= 0) {
            return null;
        }
        return findItemWithData(0, startIndex, items);
    }

    public static String dump(FormItem... fis) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < fis.length; i++) {
            FormItem formItem = fis[i];
            sb.append(ClientUtils.format("\n{%s: %s, tabIdx: %s, focus: %s%s}",
                    i, formItem.getName(), formItem.getTabIndex(), formItem.getCanFocus(), dumpChildren(formItem)));
        }
        return "FormItem[" + sb.toString() + ']';
    }

    private static String dumpChildren(FormItem formItem) {
            if (formItem instanceof CanvasItem) {
                CanvasItem canvasItem = (CanvasItem) formItem;
                Canvas canvas = canvasItem.getCanvas();
                return ClientUtils.format("\n  {tab: %s, focus: %s, %s, %s, %s}",
                        canvas.getTabIndex(), canvas.getCanFocus(), canvas.getID(), canvas.getClass(), dumpChildren(canvas, "    "));
            } else {
                return "";
            }
    }

    private static String dumpChildren(Canvas canvasItem, String indent) {
        StringBuilder sb = new StringBuilder();
        Canvas[] children = (canvasItem instanceof Layout)
                ? ((Layout) canvasItem).getMembers()
                : canvasItem.getChildren();
        sb.append("children.length: ").append(children.length);
        for (int i = 0; i < children.length; i++) {
            Canvas child = children[i];
            sb.append(ClientUtils.format("\n%s{%s, tab: %s, focus:%s, %s, %s, %s}",
                    indent, i, child.getTabIndex(), child.getCanFocus(), child.getID(),
                    child.getClass(), dumpChildren(child, indent + "  ")));

        }
        return sb.toString();
    }

}
