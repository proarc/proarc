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

import com.google.gwt.core.client.GWT;
import com.google.gwt.safehtml.shared.SafeHtmlUtils;
import com.smartgwt.client.i18n.SmartGwtMessages;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpacerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.validator.IntegerRangeValidator;
import com.smartgwt.client.widgets.form.validator.IsIntegerValidator;
import com.smartgwt.client.widgets.form.validator.RegExpValidator;
import com.smartgwt.client.widgets.form.validator.RequiredIfFunction;
import com.smartgwt.client.widgets.form.validator.RequiredIfValidator;
import com.smartgwt.client.widgets.layout.HStack;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.ds.ModsCustomDataSource;
import cz.incad.pas.editor.shared.series.Series;
import java.util.Iterator;

/**
 * Editor allowing user input of new values to modify multiple digital objects. It can
 * be used as a standalone {@link #getFormPanel() panel} or as a {@link #showInWindow() window}.
 *
 * @author Jan Pokorsky
 */
public final class PageMetadataEditor {

    private static final String ALPHABET_LOWER_SERIES = "a - z, aa - az, ...";
    private static final String ALPHABET_UPPER_SERIES = "A - Z, AA - AZ, ...";
    private static final String ARABIC_SERIES = "1, 2, 3, 4, ...";
    private static final String ROMAN_LOWER_SERIES = "i, ii, iii, iv, ...";
    private static final String ROMAN_UPPER_SERIES = "I, II, III, IV, ...";

    private CheckboxItem allowPageIndexes;
    private CheckboxItem allowPageNumbers;
    private CheckboxItem allowPageTypes;
    private IntegerItem indexStart;
    private TextItem numberStart;
    private IntegerItem numberIncrement;
    private SelectItem seriesType;
    private TextItem prefix;
    private TextItem suffix;
    private StaticTextItem numberExample;
    private SelectItem pageType;
    private IntegerRangeValidator integerStartValidator;
    private IntegerRangeValidator integerIncrementValidator;
    private RegExpValidator romanStartValidator;
    private RegExpValidator alphabetStartValidator;
    private RequiredIfValidator numberStartRequired;
    private Window window;
    private VLayout formPanel;
    private DynamicForm formPageNumber;
    private DynamicForm formPageType;
    private DynamicForm formPageIndex;
    private BooleanCallback windowCallback;
    private final ClientMessages i18n;

    public PageMetadataEditor() {
        this.i18n = GWT.create(ClientMessages.class);
    }

    public Canvas getFormPanel() {
        if (formPanel != null) {
            return formPanel;
        }

        SpacerItem newRowSpacer = new SpacerItem();
        newRowSpacer.setStartRow(true);

        createPageIndexUi();
        createPageNumberUi();
        createPageTypeUi();

        formPanel = new VLayout(10);
        formPanel.setAutoHeight();
        formPageIndex.setItems(allowPageIndexes, indexStart);
        formPageNumber.setItems(allowPageNumbers, prefix, numberStart,
                numberIncrement, suffix, seriesType, numberExample);
        formPageType.setItems(allowPageTypes, pageType);
        formPanel.setMembers(formPageIndex, formPageNumber, formPageType);

        initAll();

        return formPanel;
    }

    public boolean validate() {
        if (formPanel != null) {
            boolean valid = true;
            boolean anyValue = false;
            if (getAllowPageIndexes()) {
                valid &= formPageIndex.validate();
                anyValue = true;
            }
            if (getAllowPageNumbers()) {
                valid &= formPageNumber.validate();
                anyValue = true;
            }
            if (getAllowPageTypes()) {
                valid &= formPageType.validate();
                anyValue = true;
            }
            return anyValue && valid;
        }
        return false;
    }

    private static DynamicForm createForm() {
        DynamicForm form = new DynamicForm();
        form.setWrapItemTitles(false);
//        form.setAutoWidth();
        form.setAutoHeight();
        form.setBrowserSpellCheck(false);
        return form;
    }

    private void createPageIndexUi() {
        formPageIndex = createForm();

        allowPageIndexes = new CheckboxItem("fillPageIndexes", i18n.PageMetadataEditor_CheckboxPageIndices_Title());
        allowPageIndexes.setStartRow(true);
        allowPageIndexes.setColSpan("*");
        allowPageIndexes.setShowTitle(false);

        IntegerRangeValidator indexValidator = new IntegerRangeValidator();
        indexValidator.setMin(0);
        indexValidator.setMax(1000000);

        indexStart = new IntegerItem("indexStart", i18n.PageMetadataEditor_IndexStartValue_Title());
        indexStart.setRequired(true);
        indexStart.setValidators(indexValidator);

        allowPageIndexes.addChangedHandler(new DisableStateHandler(indexStart));
    }

    private void createPageNumberUi() {
        formPageNumber = createForm();

        allowPageNumbers = new CheckboxItem("fillPageNumbers", i18n.PageMetadataEditor_CheckboxPageNubers_Title());
        allowPageNumbers.setStartRow(true);
        allowPageNumbers.setColSpan("*");
        allowPageNumbers.setShowTitle(false);

        PageNumberChangeHandler pageNumberChangeHandler = new PageNumberChangeHandler();

        integerStartValidator = new IntegerRangeValidator();
        integerStartValidator.setMin(0);
        integerStartValidator.setMax(1000000);
        romanStartValidator = new RegExpValidator(
                "^[1-9][0-9]{0,6}$"
                + "|^[mM]{0,6}([cC][mM]|[cC][dD]|[dD]?[cC]{0,3})([xX][cC]|[xX][lL]|[lL]?[xX]{0,3})([iI][xX]|[iI][vV]|[vV]?[iI]{0,3})$");
        alphabetStartValidator = new RegExpValidator("^[a-zA-Z]{0,6}$");
        numberStartRequired = new RequiredIfValidator(new RequiredIfFunction() {

            @Override
            public boolean execute(FormItem formItem, Object value) {
                return true;
            }
        });
        numberStart = new TextItem("numberStart", i18n.PageMetadataEditor_NumberStartValue_Title());
        numberStart.addChangedHandler(pageNumberChangeHandler);

        prefix = new TextItem("prefix", i18n.PageMetadataEditor_NumberPrefix_Title());
        prefix.setLength(20);
        prefix.addChangedHandler(pageNumberChangeHandler);

        suffix = new TextItem("suffix", i18n.PageMetadataEditor_NumberSuffix_Title());
        suffix.setLength(20);
        suffix.addChangedHandler(pageNumberChangeHandler);

        integerIncrementValidator = new IntegerRangeValidator();
        integerIncrementValidator.setMin(-1000);
        integerIncrementValidator.setMax(1000);
        numberIncrement = new IntegerItem("numberIncrement", i18n.PageMetadataEditor_NumberIncrement_Title());
        numberIncrement.addChangedHandler(pageNumberChangeHandler);
        numberIncrement.setRequired(true);
        numberIncrement.setValidators(integerIncrementValidator);

        numberExample = new StaticTextItem("numberExample", i18n.PageMetadataEditor_NumberPreview_Title());
        numberExample.setClipValue(true);
        numberExample.setWidth(120); // enforce clip value

        seriesType = new SelectItem("seriesType", i18n.PageMetadataEditor_NumberSeriesType_Title());
        seriesType.setValueMap(ARABIC_SERIES, ROMAN_UPPER_SERIES,
                ROMAN_LOWER_SERIES, ALPHABET_UPPER_SERIES, ALPHABET_LOWER_SERIES);
        seriesType.setDefaultValue(ARABIC_SERIES);
        seriesType.setValue(ARABIC_SERIES);
        seriesType.addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                setSequenceType(seriesType.getValueAsString());
            }
        });

        allowPageNumbers.addChangedHandler(new DisableStateHandler(
                prefix, numberStart, numberIncrement, suffix, seriesType, numberExample));
    }

    private void createPageTypeUi() {
        formPageType = createForm();

        allowPageTypes = new CheckboxItem("fillPageTypes", i18n.PageMetadataEditor_CheckboxPageTypes_Title());
        allowPageTypes.setStartRow(true);
        allowPageTypes.setColSpan("*");
        allowPageTypes.setShowTitle(false);

        pageType = new SelectItem(ModsCustomDataSource.FIELD_PAGE_TYPE, i18n.PageForm_PageType_Title());
        pageType.setValueMap(ModsCustomDataSource.getPageTypes());
        pageType.setDefaultValue(ModsCustomDataSource.getDefaultPageType());
        pageType.setValue(ModsCustomDataSource.getDefaultPageType());
        
        allowPageTypes.addChangedHandler(new DisableStateHandler(pageType));
    }

    private void initPageIndex() {
        formPageIndex.clearValues();
        allowPageIndexes.setValue(false);
        indexStart.setDisabled(!getAllowPageIndexes());

    }

    private void initPageNumber() {
        formPageNumber.clearValues();
        allowPageNumbers.setValue(false);
        boolean disablePageNumbers = !getAllowPageNumbers();
        numberStart.setDisabled(disablePageNumbers);
        numberIncrement.setDisabled(disablePageNumbers);
        numberIncrement.setValue(1);
        seriesType.setDisabled(disablePageNumbers);
        prefix.setDisabled(disablePageNumbers);
        suffix.setDisabled(disablePageNumbers);
    }

    private void initPageType() {
        formPageType.clearValues();
        allowPageTypes.setValue(false);
        pageType.setDisabled(!getAllowPageTypes());
    }

    /**
     * Resets form.
     */
    public void initAll() {
        if (formPanel != null) {
            initPageIndex();
            initPageNumber();
            initPageType();
            updatePageNumberValidators(seriesType.getValueAsString());
        }
    }

    private Canvas createButtons() {
        SmartGwtMessages i18nSgwt = ClientUtils.createSmartGwtMessages();
        IButton btnOk = new IButton(i18nSgwt.dialog_OkButtonTitle(), new com.smartgwt.client.widgets.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
                boolean valid = validate();
                if (valid) {
                    window.hide();
                    if (windowCallback != null) {
                        windowCallback.execute(true);
                    }
                }
            }
        });
        IButton btnCancel = new IButton(i18nSgwt.dialog_CancelButtonTitle(), new com.smartgwt.client.widgets.events.ClickHandler() {

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
        if (window == null) {
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
            window.setTitle(i18n.PageMetadataEditor_Window_Title());
            window.setShowMinimizeButton(false);
            window.setShowModalMask(true);
        }

        initAll();
        window.show();
        formPageIndex.focusInItem(allowPageIndexes);
    }

    public boolean getAllowPageIndexes() {
        return allowPageIndexes.getValueAsBoolean();
    }

    public boolean getAllowPageNumbers() {
        return allowPageNumbers.getValueAsBoolean();
    }

    public boolean getAllowPageTypes() {
        return allowPageTypes.getValueAsBoolean();
    }

    public Integer getIndexStart() {
        return getUnsignedInteger(indexStart);
    }

    public String getNumberStart() {
        return getNormalizedString(numberStart.getValueAsString());
    }

    public Iterator<String> getSequence() {
        String type = seriesType.getValueAsString();
        String start = getNumberStart();
        Integer increment = getIncrement();
        if (start == null || increment == null || !numberStart.validate() || !numberIncrement.validate()) {
            return null;
        }
        if (ARABIC_SERIES.equals(type)) {
            Integer arabicStart = getInt(start);
            return arabicStart == null ? null : Series.arabic(arabicStart, increment).iterator();
        } else if (ROMAN_UPPER_SERIES.equals(type) || ROMAN_LOWER_SERIES.equals(type)) {
            boolean upperCase = ROMAN_UPPER_SERIES.equals(type);
            if (Series.validRoman(start)) {
                return Series.roman(start, increment, upperCase).iterator();
            } else {
                Integer romanStart = getPositiveInt(start);
                return romanStart == null ? null : Series.roman(romanStart, increment, upperCase).iterator();
            }
        } else if (ALPHABET_UPPER_SERIES.equals(type) || ALPHABET_LOWER_SERIES.equals(type)) {
            boolean upperCase = ALPHABET_UPPER_SERIES.equals(type);
            if (!Series.validAlphabet(start) || Math.abs(increment) > 26) {
                return null;
            }
            return Series.alphabet(start, increment, upperCase).iterator();
        }
        throw new IllegalStateException(type);
    }

    private Integer getIncrement() {
        return getInt(getNormalizedString(numberIncrement.getValueAsString()));
    }

    private static Integer getInt(String value) {
        if (value == null) {
            return null;
        }
        try {
            return  Integer.decode(value);
        } catch (NumberFormatException ex) {
            return null;
        }
    }

    private static Integer getPositiveInt(String value) {
        Integer result = getInt(value);
        return result == null || result < 1 ? null : result;
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
        return getNormalizedPrefix(prefixValue);
    }

    public String getSuffix() {
        String suffixValue = suffix.getValueAsString();
        return getNormalizedPrefix(suffixValue);
    }

    public String getPageType() {
        return pageType.getValueAsString();
    }

    private static String getNormalizedPrefix(String s) {
        if (s != null) {
            if (s.trim().isEmpty()) {
                s = null;
            }
        }
        return s;
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

    private void setPreview() {
        String prefixValue = getPrefix();
        String suffixValue = getSuffix();
        Iterator<String> sequence = getSequence();
        StringBuilder sequenceItem = new StringBuilder();
        for (int i = 0; i < 3; i++) {
            if (prefixValue != null) {
                sequenceItem.append(prefixValue);
            }
            if (sequence != null) {
                sequenceItem.append(sequence.next());
            } else {
//                sequenceItem.append("<err>");
            }
            if (suffixValue != null) {
                sequenceItem.append(suffixValue);
            }
            if (sequenceItem.length() > 0) {
                sequenceItem.append(",&nbsp;");
            } else {
                break;
            }
        }
        if (sequenceItem.length() > 0) {
            sequenceItem.append("...");
        }
        String example = SafeHtmlUtils.htmlEscapeAllowEntities(sequenceItem.toString());
        numberExample.setValue(example);
        numberExample.setPrompt(example);
    }

    private void updatePageNumberValidators(String seriesType) {
        numberExample.setPrompt(seriesType);
        if (ARABIC_SERIES.equals(seriesType)) {
            numberStart.setValidators(new IsIntegerValidator(), integerStartValidator, numberStartRequired);
            integerIncrementValidator.setMin(-1000);
            integerIncrementValidator.setMax(1000);
        } else if (ROMAN_LOWER_SERIES.equals(seriesType) || ROMAN_UPPER_SERIES.equals(seriesType)) {
            numberStart.setValidators(romanStartValidator, numberStartRequired);
            integerIncrementValidator.setMin(-1000);
            integerIncrementValidator.setMax(1000);
        } else {
            numberStart.setValidators(alphabetStartValidator, numberStartRequired);
            integerIncrementValidator.setMin(-26);
            integerIncrementValidator.setMax(26);
        }
    }

    private void setSequenceType(String seriesType) {
        updatePageNumberValidators(seriesType);
        setPreview();
    }

    private final class PageNumberChangeHandler implements ChangedHandler {

        @Override
        public void onChanged(ChangedEvent event) {
            setPreview();
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
            if (enabled && items.length > 0) {
                items[0].focusInItem();
            }
        }
    }

}
