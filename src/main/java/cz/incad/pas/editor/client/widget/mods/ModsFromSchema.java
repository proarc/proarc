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
import com.smartgwt.client.data.SchemaSet;
import com.smartgwt.client.data.XMLTools;
import com.smartgwt.client.data.XSDLoadCallback;
import com.smartgwt.client.rpc.RPCRequest;
import com.smartgwt.client.widgets.form.DynamicForm;

/**
 *
 * @author Jan Pokorsky
 */
public class ModsFromSchema extends DynamicForm {

    public DataSource schema;

    public ModsFromSchema() {
        RPCRequest request = new RPCRequest();
        request.setBypassCache(true);
        XMLTools.loadXMLSchema("ds/mods/page.xsd", new XSDLoadCallback() {

            @Override
            public void execute(SchemaSet schemaSet) {
                schema = schemaSet.getSchema("page");
                DataSource dataSource = new DataSource();
                dataSource.setInheritsFrom(schema);
                dataSource.setUseParentFieldOrder(true);

//                DataSourceTextField itemID = new DataSourceTextField("itemId");
//                itemID.setHidden(true);
//                itemID.setPrimaryKey(true);
//
//                DataSourceTextField itemName = new DataSourceTextField("itemName", "item name");
//                DataSourceDateField nextShipment = new DataSourceDateField("nextShipment", "next shipment");
//                nextShipment.setUseTextField(true);
//
//                dataSource.setFields(itemID, itemName, nextShipment);
                ModsFromSchema.this.setDataSource(dataSource);
            }
        }, request, true);
    }



}
