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
package cz.incad.pas.editor.client.widget.mods;

import com.google.gwt.core.client.JavaScriptObject;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.validator.CustomValidator;
import cz.incad.pas.editor.client.ClientMessages;
import cz.incad.pas.editor.client.ClientUtils;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Validates repeatable form items.
 *
 * @author Jan Pokorsky
 */
abstract class RepeatableItemValidator extends CustomValidator {

    private static final Logger LOG = Logger.getLogger(RepeatableItemValidator.class.getName());
    protected final ClientMessages i18n;

    protected RepeatableItemValidator(ClientMessages i18n) {
        this.i18n = i18n;
    }

    @Override
    protected final boolean condition(Object value) {
        FormItem formItem = getFormItem();
        boolean isJSO = value instanceof JavaScriptObject;
        RecordList recordList = new RecordList((JavaScriptObject) value);
        if (LOG.isLoggable(Level.FINE)) {
            ClientUtils.fine(LOG, "CONDITION: value: %s,\nclass: %s\nformItem: %s"
                    + ",\nisJSO: %s,\ndump: %s",
                    value, ClientUtils.safeGetClass(value), formItem,
                    isJSO, ClientUtils.dump(value));
        }
        return condition(recordList);
    }

    protected abstract boolean condition(RecordList recordList);
    

}
