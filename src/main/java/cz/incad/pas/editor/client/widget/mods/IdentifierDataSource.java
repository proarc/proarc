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

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.FieldType;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import cz.fi.muni.xkremser.editor.client.mods.IdentifierTypeClient;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public final class IdentifierDataSource extends DataSource {

    private static final Logger LOG = Logger.getLogger(IdentifierDataSource.class.getName());

    public static final String ID = "IdentifierDataSource";
    public static final String FIELD_TYPE = "type";
    public static final String FIELD_VALUE = "value";
    /** fetched object */
    public static final String FIELD_OBJECT = "IdentifierTypeClient";

    public IdentifierDataSource() {
        DataSourceField type = new DataSourceField(FIELD_TYPE, FieldType.TEXT, "Type");

        ComboBoxItem typeEditor = new ComboBoxItem(IdentifierDataSource.FIELD_TYPE);
        typeEditor.setValueMap("ISSN", "ISBN", "čČNB");
        typeEditor.setType("comboBox");
        type.setEditorType(typeEditor);

        DataSourceField value = new DataSourceField(FIELD_VALUE, FieldType.TEXT, "Value");
        TextItem valueEditor = new TextItem(IdentifierDataSource.FIELD_VALUE);
        valueEditor.setWidth("200");
        value.setEditorType(valueEditor);

        DataSourceField object = new DataSourceField(FIELD_VALUE, FieldType.ANY, "Object");
        object.setHidden(true);
        setFields(type, value);
        setClientOnly(true);
        setCacheAllData(true);
    }

    public static Record[] convert(List<IdentifierTypeClient> identifiers) {
        if (identifiers == null) {
            return new Record[0];
        }
        Record[] records = new Record[identifiers.size()];
        int idx = 0;
        for (IdentifierTypeClient identifier : identifiers) {
            Record r = new Record();
            r.setAttribute(FIELD_TYPE, identifier.getType());
            r.setAttribute(FIELD_VALUE, identifier.getValue());
            r.setAttribute(FIELD_OBJECT, identifier);
            records[idx++] = r;
        }
        return records;
    }

    public static List<IdentifierTypeClient> convert(Record[] records) {
        LOG.info("IdentifierDataSource.convert.records: " + records);
        if (records == null || records.length == 0) {
            return null;
        }
        List<IdentifierTypeClient> identifiers = new ArrayList<IdentifierTypeClient>(records.length);
        for (Record r : records) {
            IdentifierTypeClient identifier = (IdentifierTypeClient) r.getAttributeAsObject(FIELD_OBJECT);
            if (identifier == null) {
                identifier = new IdentifierTypeClient();
            }
            String type = r.getAttribute(FIELD_TYPE);
            String value = r.getAttribute(FIELD_VALUE);
            identifier.setType(type);
            identifier.setValue(value);
            identifiers.add(identifier);
            LOG.info("IdentifierDataSource.convert.records.identifier: type: " + type + ", value: " + value);
        }
        return identifiers.isEmpty() ? null : identifiers;
    }

    public static IdentifierDataSource getInstance() {
        IdentifierDataSource ds = (IdentifierDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new IdentifierDataSource();
        return ds;
    }

}
