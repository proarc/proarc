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
package cz.incad.pas.editor.client.widget;

import com.google.gwt.core.client.GWT;
import com.smartgwt.client.i18n.SmartGwtMessages;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.BlurbItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.RowSpacerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.validator.IntegerRangeValidator;
import com.smartgwt.client.widgets.form.validator.RequiredIfFunction;
import com.smartgwt.client.widgets.form.validator.RequiredIfValidator;
import com.smartgwt.client.widgets.layout.HStack;

/**
 * Editor allowing user input of new values to modify multiple digital objects. It can
 * be used as a standalone {@link #getFormPanel() panel} or as a {@link #showInWindow() window}.
 *
 * @author Jan Pokorsky
 */
public class PageMetadataEditor {

    private static final PageMetadataEditor INSTANCE = new PageMetadataEditor();

    private CheckboxItem allowPageIndexes;
    private CheckboxItem allowPageNumbers;
    private CheckboxItem allowPageTypes;
    private IntegerItem indexStart;
    private IntegerItem numberStart;
    private TextItem prefix;
    private TextItem suffix;
    private StaticTextItem numberExample;
    private SelectItem pageType;
    private Window window;
    private DynamicForm form;
    private BooleanCallback windowCallback;

    public static PageMetadataEditor getInstance() {
        return INSTANCE;
    }

    private PageMetadataEditor() {
    }

    public Canvas getFormPanel() {
        if (form != null) {
            return form;
        }
        allowPageIndexes = new CheckboxItem("fillPageIndexes", "Edit page indices");
        allowPageIndexes.setStartRow(true);
        allowPageIndexes.setDefaultValue(true);
        allowPageIndexes.setColSpan("*");
        allowPageIndexes.setShowTitle(false);
//        fillPageIndexes.setShowLabel(false);

        allowPageNumbers = new CheckboxItem("fillPageNumbers", "Edit page numbers");
        allowPageNumbers.setDefaultValue(true);
//        fillPageNumbers.setShowLabel(false);
        allowPageNumbers.setStartRow(true);
        allowPageNumbers.setColSpan("*");
        allowPageNumbers.setShowTitle(false);

        allowPageTypes = new CheckboxItem("fillPageTypes", "Edit page types");
        allowPageTypes.setDefaultValue(true);
//        fillPageTypes.setShowLabel(false);
        allowPageTypes.setStartRow(true);
        allowPageTypes.setColSpan("*");
        allowPageTypes.setShowTitle(false);

        SpacerItem spacerIndex = new SpacerItem();
        spacerIndex.setStartRow(true);
        indexStart = new IntegerItem("indexStart", "Start value");
        indexStart.setValidateOnChange(true);

        numberStart = new IntegerItem("numberStart", "Start value");
        prefix = new TextItem("prefix", "Prefix");
        suffix = new TextItem("suffix", "Suffix");
        prefix.setLength(20);
        suffix.setLength(20);
        numberExample = new StaticTextItem("numberExample", "Example");
        numberExample.setEscapeHTML(true); // displays empty string as &nbsp; SmartGWT 3.0 should contain fix
        numberExample.setClipValue(true);

        pageType = new SelectItem("pageType", "Page type");
        pageType.setValueMap("ListOfIllustrations", "TableOfContents", "Index",
                "Table", "TitlePage", "ListOfMaps", "NormalPage", "Blank", "ListOfTables", "Advertisement");
        pageType.setDefaultValue("NormalPage");

//        BlurbItem blurbItem = new BlurbItem();
//        blurbItem.setDefaultValue("Page Indexes blurb");

        form = new DynamicForm();
        form.setNumCols(10);
        form.setColWidths(20);
//        formIndex.setTitleOrientation(TitleOrientation.TOP);
        form.setWrapItemTitles(false);
        form.setItems(
//                blurbItem,
                allowPageIndexes, spacerIndex, indexStart, new RowSpacerItem(),
                allowPageNumbers, spacerIndex, prefix, spacerIndex, numberStart, spacerIndex, suffix, spacerIndex, numberExample, new RowSpacerItem(),
                allowPageTypes, spacerIndex, pageType);
        form.setAutoWidth();
        form.setAutoHeight();

        allowPageIndexes.addChangedHandler(new DisableStateHandler(indexStart));
        allowPageNumbers.addChangedHandler(new DisableStateHandler(numberStart, prefix, suffix, numberExample));
        allowPageTypes.addChangedHandler(new DisableStateHandler(pageType));

        PageNumberChangeHandler pageNumberChangeHandler = new PageNumberChangeHandler();
        numberStart.addChangedHandler(pageNumberChangeHandler);
        prefix.addChangedHandler(pageNumberChangeHandler);
        suffix.addChangedHandler(pageNumberChangeHandler);

        IntegerRangeValidator integerRangeValidator = new IntegerRangeValidator();
        integerRangeValidator.setMin(0);
        integerRangeValidator.setMax(Integer.MAX_VALUE);
//        integerRangeValidator.setStopOnError(true);
        numberStart.setValidators(integerRangeValidator, new RequiredIfValidator(new RequiredIfFunction() {

            @Override
            public boolean execute(FormItem formItem, Object value) {
                String prefixValue = getPrefix();
                String suffixValue = getSuffix();
                return allowPageNumbers.getValueAsBoolean() && (prefixValue != null || suffixValue != null);
            }
        }));
        numberStart.setValidateOnChange(true);
        numberStart.setStopOnError(true);

        indexStart.setValidators(integerRangeValidator);

        return form;
    }

    private Canvas createButtons() {
        SmartGwtMessages i18n = GWT.create(SmartGwtMessages.class);
        IButton btnOk = new IButton(i18n.dialog_OkButtonTitle(), new com.smartgwt.client.widgets.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                boolean valid = form.validate();
                if (valid) {
                    window.hide();
                    if (windowCallback != null) {
                        windowCallback.execute(true);
                    }
                }
            }
        });
        IButton btnCancel = new IButton(i18n.dialog_CancelButtonTitle(), new com.smartgwt.client.widgets.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                window.hide();
                if (windowCallback != null) {
                    windowCallback.execute(false);
                }
            }
        });

        HStack btnLayout = new HStack(5);
        btnLayout.setAutoHeight();
        btnLayout.setLayoutTopMargin(20);
        btnLayout.setLayoutAlign(Alignment.CENTER);
        btnLayout.setMembers(btnOk, btnCancel);
        return btnLayout;
    }

    public void showInWindow(BooleanCallback callback) {
        this.windowCallback = callback;
        if (window != null) {
            form.clearValues();
            for (FormItem formItem : form.getFields()) {
                formItem.setDisabled(false);
            }
            window.show();
            form.focusInItem(indexStart);
            return ;
        }

        Canvas panelForm = getFormPanel();
        panelForm.setPadding(5);
        Canvas panelButtons = createButtons();
        panelButtons.setPadding(5);

        window = new Window();
        window.setAutoSize(true);
        window.setAutoCenter(true);
        window.setIsModal(true);
        window.addItem(panelForm);
        window.addItem(panelButtons);
        window.setTitle("Edit Selected Objects");
        window.setShowMinimizeButton(false);
        window.setShowModalMask(true);
        window.show();
    }

    public boolean getAllowPageIndexes() {
        return allowPageIndexes.getValueAsBoolean() && indexStart.getValueAsInteger() != null;
    }

    public boolean getAllowPageNumbers() {
        return allowPageNumbers.getValueAsBoolean() && numberStart.getValueAsInteger() != null;
    }

    public boolean getAllowPageTypes() {
        return allowPageTypes.getValueAsBoolean();
    }

    public DynamicForm getForm() {
        return form;
    }

    public Integer getIndexStart() {
        return getUnsignedInteger(indexStart);
    }

    public Integer getNumberStart() {
        return getUnsignedInteger(numberStart);
    }

    /**
     * Workaround for {@link IntegerItem#getValueAsInteger() } that fails in
     * the development mode. (SmartGWT 2.5)
     */
    private static Integer getUnsignedInteger(IntegerItem item) {
        Integer result = null;
        String value = getNormalizedString(item.getValueAsString());
        if (value != null) {
            try {
                result = Integer.parseInt(value);
            } catch (NumberFormatException ex) {
            }
        }

        return (result == null || result < 0) ? null : result;
    }

    public String getPrefix() {
        String prefixValue = prefix.getValueAsString();
        return getNormalizedString(prefixValue);
    }

    public String getSuffix() {
        String suffixValue = suffix.getValueAsString();
        return getNormalizedString(suffixValue);
    }

    public String getPageType() {
        return pageType.getValueAsString();
    }

    private static String getNormalizedString(String s) {
        if (s != null) {
            s = s.trim();
            if (s.isEmpty()) {
                s = null;
            }
        }
        return s;
    }

    private final class PageNumberChangeHandler implements ChangedHandler {

        @Override
        public void onChanged(ChangedEvent event) {
            String prefixValue = getPrefix();
            String suffixValue = getSuffix();
            Integer numberStartValue = getNumberStart();
            String number = ".";
            if (numberStartValue != null) {
                number = numberStartValue.toString();
                if (prefixValue != null) {
                    number = prefixValue + number;
                }
                if (suffixValue != null) {
                    number += suffixValue;
                }
            }
            numberExample.setValue(number);
        }

    }
    private static final class DisableStateHandler implements ChangedHandler {

        private final FormItem[] items;

        public DisableStateHandler(FormItem... items) {
            this.items = items;
        }

        @Override
        public void onChanged(ChangedEvent event) {
            boolean enabled = (Boolean) event.getValue();
            for (FormItem item : items) {
                item.setDisabled(!enabled);
            }
        }
    }

}
