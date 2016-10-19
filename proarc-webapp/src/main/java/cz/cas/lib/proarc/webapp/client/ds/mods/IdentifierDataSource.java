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
package cz.cas.lib.proarc.webapp.client.ds.mods;

import com.google.gwt.core.client.GWT;
import com.google.gwt.regexp.shared.RegExp;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.i18n.SmartGwtMessages;
import com.smartgwt.client.types.CharacterCasing;
import com.smartgwt.client.types.FieldType;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.ds.LocalizationDataSource;
import java.util.LinkedHashMap;
import java.util.logging.Logger;

/**
 * XXX this class could be replaced with RepeatableFormItem component.
 *
 * @author Jan Pokorsky
 */
public final class IdentifierDataSource extends DataSource {

    private static final Logger LOG = Logger.getLogger(IdentifierDataSource.class.getName());

    public static final String ID = "IdentifierDataSource";
    public static final String FIELD_TYPE = ModsConstants.FIELD_IDENTIFIER_TYPE;
    public static final String FIELD_VALUE = ModsConstants.FIELD_IDENTIFIER_VALUE;
    /** fetched object */
    public static final String FIELD_OBJECT = "IdentifierTypeClient";
    public static final String TYPE_CCNB = "ccnb";
    public static final String TYPE_DOI = "doi";
    public static final String TYPE_HANDLE = "hdl";
    public static final String TYPE_ISBN = "isbn";
    public static final String TYPE_ISSN = "issn";
    public static final String TYPE_SICI = "sici";
    public static final String TYPE_URNNBN = "urnnbn";
    public static final String TYPE_UUID = "uuid";

    private static IdentifierDataSource INSTANCE;

    /**
     * Gets localized type value map.
     * @return type map
     */
    public static LinkedHashMap<String, String> getTypeValueMap(ClientMessages i18n) {
        return LocalizationDataSource.getInstance().asValueMap(BundleName.MODS_IDENTIFIER_TYPES);
    }

    public IdentifierDataSource() {
        setID(ID);
        ClientMessages i18n = GWT.create(ClientMessages.class);
        SmartGwtMessages i18nSmartGwt = ClientUtils.createSmartGwtMessages();

        DataSourceField type = new DataSourceField(FIELD_TYPE, FieldType.TEXT,
                i18n.PageForm_IdentifierType_Title());
        type.setRequired(Boolean.TRUE);

        ComboBoxItem typeEditor = new ComboBoxItem(IdentifierDataSource.FIELD_TYPE);
        LocalizationDataSource.setOptionDataSource(typeEditor, BundleName.MODS_IDENTIFIER_TYPES);
        typeEditor.setType("comboBox");
        typeEditor.setCharacterCasing(CharacterCasing.LOWER);
        typeEditor.setBrowserSpellCheck(Boolean.FALSE);
        type.setEditorType(typeEditor);

        DataSourceField value = new DataSourceField(FIELD_VALUE, FieldType.TEXT,
                i18n.PageForm_IdentifierValue_Title());
        value.setRequired(Boolean.TRUE);
        TextItem valueEditor = new TextItem(IdentifierDataSource.FIELD_VALUE);
        valueEditor.setWidth("250");
        valueEditor.setBrowserSpellCheck(Boolean.FALSE);
        valueEditor.setRequired(Boolean.TRUE);
        valueEditor.setValidators(new IdentifierValidator(i18n, i18nSmartGwt));
        value.setEditorType(valueEditor);

        DataSourceField object = new DataSourceField(FIELD_VALUE, FieldType.ANY);
        object.setHidden(true);
        setFields(type, value);
        setClientOnly(true);
        setCacheAllData(true);
    }

    public static IdentifierDataSource getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new IdentifierDataSource();
        }
        return INSTANCE;
    }

    /**
     * Validates values according to selected identifier type.
     */
    public static final class IdentifierValidator extends CustomValidator {

        private static final RegExp RE_UUID = RegExp.compile(
                "^[A-Fa-f0-9]{8}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{4}-[A-Fa-f0-9]{12}$");
        private static final RegExp RE_ISSN = RegExp.compile(
                "^[0-9]{4}-[0-9X]{4}$");
        /** rough approximation */
        private static final RegExp RE_ISBN = RegExp.compile("^[0-9- ]{9,20}$");
        private static final RegExp RE_CCNB = RegExp.compile("^cnb[0-9]{1,20}$");

        private final ClientMessages i18n;
        private final SmartGwtMessages i18nSmartGwt;

        public IdentifierValidator(ClientMessages i18n, SmartGwtMessages i18nSmartGwt) {
            this.i18n = i18n;
            this.i18nSmartGwt = i18nSmartGwt;
        }

        @Override
        protected boolean condition(Object value) {
            boolean valid = true;
            if (value instanceof String) {
                String svalue = (String) value;
                svalue = svalue.trim();
                setResultingValue(svalue);
                FormItem fi = getFormItem();
                DynamicForm form = fi.getForm();
                String type = form.getValueAsString(FIELD_TYPE);
                if (TYPE_UUID.equals(type)) {
                    if (!RE_UUID.test(svalue)) {
                        valid = false;
                        setErrorMessage(i18n.Validation_Invalid_UUID_Msg());
                    }
                } else if (TYPE_ISSN.equals(type)) {
                    if (!RE_ISSN.test(svalue)) {
                        valid = false;
                        setErrorMessage(i18n.Validation_Invalid_ISSN_Msg());
                    }
                } else if (TYPE_ISBN.equals(type)) {
                    if (!RE_ISBN.test(svalue)) {
                        valid = false;
                        setErrorMessage(i18n.Validation_Invalid_ISBN_Msg());
                    }
                } else if (TYPE_CCNB.equals(type)) {
                    if (!RE_CCNB.test(svalue)) {
                        valid = false;
                        setErrorMessage(i18n.Validation_Invalid_CCNB_Msg());
                    }
                } else if (svalue.isEmpty()) {
                    valid = false;
                    setErrorMessage(i18nSmartGwt.validator_requiredField());
                }
            } else {
                valid = false;
                setErrorMessage(i18nSmartGwt.validator_requiredField());
            }
            return valid;
        }

    }

}
