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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cas.lib.proarc.webapp.client.widget.mods;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.JavaScriptObject;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.util.JSOHelper;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Validates repeatable form items.
 *
 * @author Jan Pokorsky
 */
abstract class RepeatableItemValidator extends CustomValidator {

    private static final Logger LOG = Logger.getLogger(RepeatableItemValidator.class.getName());
    private ClientMessages i18n;

    @Override
    protected final boolean condition(Object value) {
        FormItem formItem = getFormItem();
        if (LOG.isLoggable(Level.FINE)) {
            ClientUtils.severe(LOG, "field.name: %s, class: %s, JSO: %s",
                    formItem.getName(), ClientUtils.safeGetClass(value), ClientUtils.dump(value));
        }
        RecordList recordList = null;
        if (value instanceof RecordList || value == null) {
            recordList = (RecordList) value;
        } else if (value instanceof JavaScriptObject) {
            JavaScriptObject jso = (JavaScriptObject) value;
            if (JSOHelper.isArray(jso)) {
                recordList = new RecordList(jso);
            } else {
                recordList = new RecordList(new Record[] {new Record(jso)});
            }
        } else {
            throw new UnsupportedOperationException(ClientUtils.format("field.name: %s, class: %s, JSO: %s",
                    formItem.getName(), ClientUtils.safeGetClass(value), ClientUtils.dump(value)));
        }
        return condition(recordList);
    }

    protected abstract boolean condition(RecordList recordList);

    @Override
    public void setErrorMessage(String errorMessage) {
        super.setErrorMessage(errorMessage);
        RepeatableFormItem rfi = (RepeatableFormItem) getFormItem();
        // push error to item
        rfi.addValidationError(this, errorMessage);
    }

    public ClientMessages getI18n() {
        if (i18n == null) {
            i18n = GWT.create(ClientMessages.class);
        }
        return i18n;
    }

}
