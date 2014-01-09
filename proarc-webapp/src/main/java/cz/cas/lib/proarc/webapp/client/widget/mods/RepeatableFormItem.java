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

import com.google.gwt.core.client.JavaScriptObject;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.i18n.SmartGwtMessages;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CanvasItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.events.FormItemInitHandler;
import com.smartgwt.client.widgets.form.fields.events.ShowValueEvent;
import com.smartgwt.client.widgets.form.fields.events.ShowValueHandler;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import com.smartgwt.client.widgets.form.validator.Validator;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.widget.mods.event.ListChangedEvent;
import cz.cas.lib.proarc.webapp.client.widget.mods.event.ListChangedHandler;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Repeatable dynamic form item. Set {@link #setDataSource(com.smartgwt.client.data.DataSource)
 * custom DataSource} to use all its data field. Otherwise set
 * {@link CustomFormFactory } or {@link FormWidgetFactory} to define own logic.
 *
 * @author Jan Pokorsky
 */
public final class RepeatableFormItem extends CanvasItem {

    public static final String ATTR_PROFILE = "proarc.profile.form.field";

    private static final Logger LOG = Logger.getLogger(RepeatableFormItem.class.getName());
    
    private DataSource dataSource;
    private DynamicForm formPrototype;
    private CustomFormFactory formFactory;
    private String width = "1"; // default width is autoWidth

    public RepeatableFormItem(String name, String title) {
        this(name, title, null);
    }

    public RepeatableFormItem(String name, String title, CustomFormFactory formFactory) {
        super(name, title);
        this.formFactory = formFactory;

//        setStartRow(false);
//        setEndRow(false);
        setShowTitle(false);
        setCanFocus(true);
        setValidators();
        setAutoWidth();

        setShouldSaveValue(true);
        setInitHandler(new FormItemInitHandler() {

            @Override
            public void onInit(FormItem item) {
                RepeatableForm editor = new RepeatableForm((RepeatableFormItem) item);
                Object value = item.getValue();
                if (LOG.isLoggable(Level.FINE)) {
                    ClientUtils.fine(LOG, "## onInit: %s, dump: %s", value, ClientUtils.dump(value));
                }
                setData(editor, value);

                editor.addListChangedHandler(new ListChangedHandler() {

                    @Override
                    public void onListChanged(ListChangedEvent event) {
                        RepeatableForm editor = (RepeatableForm) event.getSource();
                        CanvasItem canvasItem = editor.getCanvasItem();
                        storeValue(editor, canvasItem);
                  }
                });
                
                setCanvas(editor);

                addShowValueHandler(new ShowValueHandler() {

                    @Override
                    public void onShowValue(ShowValueEvent event) {
                        RepeatableForm editor = (RepeatableForm) event.getItem().getCanvas();
                        if (editor != null) {
                            Object dataValue = event.getDataValueAsRecordList();
                            if (LOG.isLoggable(Level.FINE)) {
                                ClientUtils.fine(LOG, "## onShowValue: name: %s, source: %s, dump: %s",
                                        event.getItem().getName(), event.getSource(), ClientUtils.dump(dataValue));
                            }
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

    public CustomFormFactory getFormFactory() {
        if (formFactory == null) {
            formFactory = new DefaultCustomForm(formPrototype, dataSource);
        }
        return formFactory;
    }

    public void setFormFactory(CustomFormFactory factory) {
        this.formFactory = factory;
    }

    public Field getProfile() {
        return (Field) getAttributeAsObject(ATTR_PROFILE);
    }

    public void setProfile(Field profile) {
        setAttribute(ATTR_PROFILE, profile);
    }

    public int getMaxOccurrences() {
        Integer maxOccurrences = null;
        Field profile = getProfile();
        if (profile != null) {
            // prefer profile if available
            maxOccurrences = profile.getMaxOccurrences();
        }
        // read attribute?
        return maxOccurrences == null || maxOccurrences < 1 ? 5 : maxOccurrences;
    }

    @Override
    public void setWidth(String width) {
        this.width = width;
        super.setWidth(width);
    }

    @Override
    public void setWidth(int width) {
        this.width = String.valueOf(width);
        super.setWidth(width);
    }

//    @Override
//    public int getWidth() {
//        return super.getWidth();
//    }

    public String getWidthAsString() {
        String width = null;
        Field profile = getProfile();
        if (profile != null) {
            width = profile.getWidth();
        }
        if (width == null) {
            width = this.width;
        }
        // Do not try super.getWidth to initialize RepeatableForm as it will
        // throw exceptions!
        return width;
    }

//    public boolean isAutoWidth(String width) {
//        return width != null && ("*".equals(width) || "100%".equals(width));
//    }

    public boolean isAutoWidth() {
        String width = getWidthAsString();
//        return width != null && ("*".equals(width) || "100%".equals(width));
        return width != null && "1".equals(width);
    }

    public boolean isWidth100() {
        String width = getWidthAsString();
        return width != null && ("*".equals(width) || "100%".equals(width));
    }

    public void setAutoWidth() {
        setWidth(1);
    }

    /**
     * Puts default validator in front of the passed ones to validate inner forms.
     * @param validators validators
     */
    @Override
    public void setValidators(Validator... validators) {
        Validator[] wrapped;
        if (validators != null && validators.length > 0) {
            wrapped = new Validator[validators.length + 1];
            System.arraycopy(validators, 0, wrapped, 1, validators.length);
        } else {
            wrapped = new Validator[1];
        }
        wrapped[0] = new DefaultValidator();
        super.setValidators(wrapped);
    }

    /**
     * Validates fields of nested forms.
     * @return valid or not
     */
    public boolean validateInnerForms(boolean showErrors) {
        RepeatableForm editor = (RepeatableForm) getCanvas();
        boolean valid = true;
        if (editor != null) {
            valid &= editor.validate(showErrors);
            // call storeValue to propagate values changed by validators
            storeValue(editor, this);
        }
        return valid;
    }

    @Override
    public Boolean validate() {
        Boolean validate = super.validate();
        RepeatableForm editor = (RepeatableForm) getCanvas();
        storeValue(editor, this);
        return validate;
    }

    private static void storeValue(RepeatableForm editor, CanvasItem canvasItem) {
        if (editor != null) {
            RecordList dataAsRecordList = editor.getDataAsRecordList();
            canvasItem.storeValue(new RecordList(dataAsRecordList.duplicate()));
        }
    }

    public List<Map<Object, Object>> getErrors() {
        RepeatableForm editor = (RepeatableForm) getCanvas();
        return editor.getErrors();
    }

    public void showErrors() {
        RepeatableForm editor = (RepeatableForm) getCanvas();
        if (editor != null) {
            editor.showErrors();
        }
    }

    public void clearErrors(boolean show) {
        RepeatableForm editor = (RepeatableForm) getCanvas();
        if (editor != null) {
            editor.clearErrors(show);
        }
    }

    /**
     * Helper to ensure errors are displayed inside inner repeatable form items.
     * It should be called by {@link DynamicForm#showErrors() } implementation.
     * Invocation of {@link #validateInnerForms()} from {@link RepeatableFormItem}
     * does not show errors reliably.
     *
     * @param repeateableItemContainer container holding items as members
     * @param errors 
     */
    public static void showErrors(DynamicForm repeateableItemContainer, Map<?, ?> errors) {
        if (errors.isEmpty()) {
            return ;
        }
        // It should help to draw inner form errors properly.
        for (FormItem formItem : repeateableItemContainer.getFields()) {
            if (formItem instanceof RepeatableFormItem) {
                if (errors.containsKey(formItem.getName())) {
                    ((RepeatableFormItem) formItem).showErrors();
                }
            }
        }
    }

    public static void clearErrors(DynamicForm repeateableItemContainer, boolean show) {
        // It should help to draw inner form errors properly.
        for (FormItem formItem : repeateableItemContainer.getFields()) {
            if (formItem instanceof RepeatableFormItem) {
                ((RepeatableFormItem) formItem).clearErrors(show);
            }
        }
    }

    private static void setData(RepeatableForm editor, Object value) {
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
     * Allows custom implementation of a simple repeatable form.
     */
    public interface CustomFormFactory {

        DynamicForm create();

    }

    /**
     * Allows custom implementation of repeatable form.
     */
    public interface FormWidgetFactory extends CustomFormFactory {

        FormWidget createFormWidget(Field formField);

    }

    /**
     * Binds widget and the values manager.
     */
    public static final class FormWidget {

        private Canvas widget;
        private ValuesManager values;

        public FormWidget(Canvas widget, ValuesManager values) {
            this.widget = widget;
            this.values = values;
        }

        public Canvas getWidget() {
            return widget;
        }

        public ValuesManager getValues() {
            return values;
        }

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

    /**
     * Validates inner forms.
     */
    private static final class DefaultValidator extends CustomValidator {

        private final SmartGwtMessages i18SmartGwt = ClientUtils.createSmartGwtMessages();

        @Override
        protected boolean condition(Object value) {
            RepeatableFormItem rfItem = (RepeatableFormItem) formItem;
            boolean valid = rfItem.validateInnerForms(true);
            Object validatedValue = rfItem.getValue();
            setResultingValue(validatedValue);

            RecordList recordList = new RecordList((JavaScriptObject) validatedValue);
            valid &= conditionRequired(recordList);
            return valid;
        }

        private boolean conditionRequired(RecordList recordList) {
            boolean valid = true;
            Boolean required = formItem.getRequired();
            if (required != null && required) {
                boolean isEmpty = recordList == null || recordList.isEmpty();
                if (isEmpty) {
                    setErrorMessage(i18SmartGwt.validator_requiredField());
                    valid = false;
                }
            }
            return valid;
        }

    }


}
