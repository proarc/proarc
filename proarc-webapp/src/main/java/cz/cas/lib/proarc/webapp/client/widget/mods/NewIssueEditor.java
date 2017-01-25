/*
 * Copyright (C) 2016 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.client.widget.mods;

import com.google.gwt.user.datepicker.client.CalendarUtil;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.fields.DataSourceDateField;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.i18n.SmartGwtMessages;
import com.smartgwt.client.types.MultipleAppearance;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.DateItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.validator.DateRangeValidator;
import com.smartgwt.client.widgets.form.validator.IntegerRangeValidator;
import com.smartgwt.client.widgets.layout.VLayout;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.widget.Dialog;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.logging.Logger;

/**
 * Edits properties to build a series of the periodical issues.
 *
 * @author Jan Pokorsky
 */
public class NewIssueEditor {

    private static final Logger LOG = Logger.getLogger(NewIssueEditor.class.getName());

    private DynamicForm form;
    private Dialog dialog;
    private Consumer<Record> consumer;
    private final ClientMessages i18n;
    private final SmartGwtMessages i18nSGwt;

    public NewIssueEditor(ClientMessages i18n) {
        this.i18n = i18n;
        this.i18nSGwt = ClientUtils.createSmartGwtMessages();
    }

    /**
     * Starts the editor.
     * @param consumer a callback that consumes the edited values as a record
     * @see DigitalObjectResourceApi#DIGITALOBJECT_SERIES_DATE_FROM_PARAM
     * @see DigitalObjectResourceApi#DIGITALOBJECT_SERIES_DATE_TO_PARAM
     * @see DigitalObjectResourceApi#DIGITALOBJECT_SERIES_DAYS_INCLUDED_PARAM
     * @see DigitalObjectResourceApi#DIGITALOBJECT_SERIES_PARTNUMBER_FROM_PARAM
     */
    public void showWindow(Consumer<Record> consumer) {
        Objects.nonNull(consumer);
        this.consumer = consumer;
        if (dialog == null) {
            dialog = new Dialog(i18n.DigitalObjectEditor_ChildrenEditor_AddAction_Title());
            VLayout container = dialog.getDialogContentContainer();
            container.setMembers(getWidget());
            dialog.addOkButton((event) -> {
                onSave();
            });
            dialog.addCancelButton(() -> {
                dialog.hide();
            });
        }
        dialog.show();
        form.clearValues();
        form.focus();
    }

    private void onSave() {
        if (form.validate()) {
            Record res = ClientUtils.removeNulls(form.getValuesAsRecord());
            consumer.accept(res);
            dialog.hide();
        }
    }

    private Canvas getWidget() {
        if (form == null) {
            form = createForm();
        }
        return form;
    }

    private DynamicForm createForm() {
        DataSource ds = new DataSource();
        ds.setClientOnly(true);
        DataSourceIntegerField fDays = new DataSourceIntegerField(
                DigitalObjectResourceApi.DIGITALOBJECT_SERIES_DAYS_INCLUDED_PARAM);
        fDays.setMultiple(true);
        DataSourceDateField fDateFrom = new DataSourceDateField(
                DigitalObjectResourceApi.DIGITALOBJECT_SERIES_DATE_FROM_PARAM);
        DataSourceDateField fDateTo = new DataSourceDateField(
                DigitalObjectResourceApi.DIGITALOBJECT_SERIES_DATE_TO_PARAM);
        DataSourceIntegerField fPartNumberFrom = new DataSourceIntegerField(
                DigitalObjectResourceApi.DIGITALOBJECT_SERIES_PARTNUMBER_FROM_PARAM);
        ds.setFields(fDateFrom, fDateTo, fDays, fPartNumberFrom);

        DynamicForm df = new DynamicForm();
        df.setWidth(200);
        df.setItemHoverWidth(200);
        df.setWrapItemTitles(false);
        df.setSaveOnEnter(true);

        IntegerItem partNumberStart = createPartName(fPartNumberFrom.getName());

        DateRangeValidator dateRangeValidator = new DateRangeValidator();
        DateItem dateStart = createDateFrom(fDateFrom.getName(), dateRangeValidator);

        DateItem dateEnd = new DateItem(fDateTo.getName(), i18n.NewIssueEditor_dateTo_Title());
        dateEnd.setTooltip(i18n.NewIssueEditor_dateTo_Hint());
        dateEnd.setUseTextField(true);
        dateEnd.setValidators(dateRangeValidator);
        dateEnd.setStartDate(new Date(1900 - 1900, 1, 1));

        SelectItem issueDays = createDayChooser(fDays.getName());

        df.setDataSource(ds, partNumberStart, dateStart, dateEnd, issueDays);
        return df;
    }

    private IntegerItem createPartName(String fName) {
        IntegerItem partNumberStart = new IntegerItem(fName,
                i18n.NewIssueEditor_partNumber_Title());
        partNumberStart.setTooltip(i18n.NewIssueEditor_partNumber_Hint());
        partNumberStart.setRequired(true);
        IntegerRangeValidator iv = new IntegerRangeValidator();
        iv.setMin(0);
        partNumberStart.setValidators(iv);
        return partNumberStart;
    }

    private DateItem createDateFrom(String fName, DateRangeValidator dateRangeValidator) {
        DateItem dateStart = new DateItem(fName, i18n.NewIssueEditor_dateFrom_Title());
        dateStart.setTooltip(i18n.NewIssueEditor_dateFrom_Hint());
        dateStart.setUseTextField(true);
        dateStart.setRequired(true);
        dateStart.setStartDate(new Date(1900 - 1900, 1, 1));
        dateStart.addChangedHandler((event) -> {
            Date min = dateStart.getValueAsDate();
            Date max = null;
            if (min != null) {
                max = CalendarUtil.copyDate(min);
                max.setMonth(11);
                max.setDate(31);
            }
            dateRangeValidator.setMin(min);
            dateRangeValidator.setMax(max);
        });
        return dateStart;
    }

    private SelectItem createDayChooser(String fName) {
        SelectItem issueDays = new SelectItem(fName, i18n.NewIssueEditor_daysIncluded_Title());
        issueDays.setTooltip(i18n.NewIssueEditor_daysIncluded_Hint());
        issueDays.setWidth("*");
        issueDays.setHeight(105);
//        issueDays.setMultiple(true);
        issueDays.setRequired(true);
        issueDays.setMultipleAppearance(MultipleAppearance.GRID);
        LinkedHashMap<Integer, String> weekMap = new LinkedHashMap<>();
        weekMap.put(1, i18nSGwt.date_dayNames_2());
        weekMap.put(2, i18nSGwt.date_dayNames_3());
        weekMap.put(3, i18nSGwt.date_dayNames_4());
        weekMap.put(4, i18nSGwt.date_dayNames_5());
        weekMap.put(5, i18nSGwt.date_dayNames_6());
        weekMap.put(6, i18nSGwt.date_dayNames_7());
        weekMap.put(7, i18nSGwt.date_dayNames_1());
        issueDays.setValueMap(weekMap);
        return issueDays;
    }
}
