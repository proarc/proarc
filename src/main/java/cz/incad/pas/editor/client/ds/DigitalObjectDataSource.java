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
package cz.incad.pas.editor.client.ds;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.OperationBinding;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.DSProtocol;
import java.util.logging.Logger;

/**
 *
 * @author Jan Pokorsky
 */
public class DigitalObjectDataSource extends DataSource {

    public static final String ID = "DigitalObjectDataSource";
    public static final String FIELD_PID = "pid";
    public static final String FIELD_MODEL = "model";

    private static final Logger LOG = Logger.getLogger(DigitalObjectDataSource.class.getName());

    public DigitalObjectDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setRecordXPath("record");
        setDataURL(RestConfig.URL_DIGOBJECT);
        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));

        DataSourceTextField pid = new DataSourceTextField(FIELD_PID);
        pid.setPrimaryKey(true);

        DataSourceTextField model = new DataSourceTextField(FIELD_MODEL);
        setFields(pid, model);

        OperationBinding addOp = new OperationBinding();
        addOp.setOperationType(DSOperationType.ADD);
        addOp.setDataProtocol(DSProtocol.POSTPARAMS);
//        OperationBinding updateOp = new OperationBinding();
//        updateOp.setOperationType(DSOperationType.UPDATE);
//        updateOp.setDataProtocol(DSProtocol.POSTXML);

        setOperationBindings(addOp);
    }

    public static DigitalObjectDataSource getInstance() {
        DigitalObjectDataSource ds = (DigitalObjectDataSource) DataSource.get(ID);
        return  ds != null ? ds : new DigitalObjectDataSource();
    }

}
