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
import com.smartgwt.client.data.DataSourceField;
import com.smartgwt.client.data.OperationBinding;
import com.smartgwt.client.types.DSDataFormat;
import com.smartgwt.client.types.DSOperationType;
import com.smartgwt.client.types.DSProtocol;
import com.smartgwt.client.types.FieldType;
import cz.incad.pas.editor.client.ds.mods.IdentifierDataSource;
import java.util.logging.Logger;

/**
 * Data source for MODS/JSON custom mapping. Later it should be fully dynamic
 * and pluggable.
 *
 * <pre>
 * {@code
 * {pid:"uuid:1",
 *  timestamp:"0",
 *  data:{
 *      identifier:[{type:"uuid", value:"1"}],
 *      pageType:"Blank"
 *  }
 * }
 * </pre>
 *
 * <p>See reasons for NOT using GWT-RPC http://forums.smartclient.com/showthread.php?t=8159#aGWTRPC,
 * http://code.google.com/p/smartgwt/issues/detail?id=303
 *
 * @author Jan Pokorsky
 */
public class ModsCustomDataSource extends DataSource {

    private static final Logger LOG = Logger.getLogger(ModsCustomDataSource.class.getName());

    public static final String ID = "ModsCustomDataSource";
    public static final String FIELD_PID = "pid";
    public static final String FIELD_EDITOR = "editor";
    public static final String FIELD_TIMESTAMP = "timestamp";
    public static final String FIELD_DATA = "customJsonData";
    
    // follows custom field names
    public static final String FIELD_STRING_VALUE = "value";
    public static final String FIELD_PAGE_TYPE = "pageType";
    public static final String FIELD_PAGE_INDEX = "pageIndex";
    public static final String FIELD_PAGE_NUMBER = "pageNumber";
    public static final String FIELD_IDENTIFIERS = "identifiers";
    public static final String FIELD_NOTE = "note";
    //periodical
    public static final String FIELD_PERIODICITIES = "periodicities";
    public static final String FIELD_SIGLA = "sigla";
    public static final String FIELD_SHELF_LOCATORS = "shelfLocators";
    public static final String FIELD_AUTHORS = "authors";
    public static final String FIELD_CONTRIBUTORS = "contributors";
    public static final String FIELD_NAME_FAMILY = "family";
    public static final String FIELD_NAME_GIVEN = "given";
    public static final String FIELD_PUBLISHERS = "publishers";
    public static final String FIELD_PRINTER_PUBLISHER_NAME = "publisherName";
    public static final String FIELD_PRINTER_PUBLISHER_DATE = "publisherDate";
    public static final String FIELD_PRINTER_PUBLISHER_PLACE = "publisherPlace";
    public static final String FIELD_PRINTERS = "printers";
    public static final String FIELD_TITLES = "titles";
    public static final String FIELD_SUBTITLES = "subtitles";
    public static final String FIELD_ALTERNATIVE_TITLES = "alternativeTitles";
    public static final String FIELD_KEY_TITLES = "keyTitles";
    public static final String FIELD_KEYWORDS = "keywords";
    public static final String FIELD_LANGUAGES = "languages";
    public static final String FIELD_LANGUAGE_CODE = "languageCode";
    public static final String FIELD_CLASSIFICATIONS = "classifications";
    public static final String FIELD_CLASSIFICATION_UDC = "classificationsUDC";
    public static final String FIELD_CLASSIFICATION_DDC = "classificationsDDC";
    public static final String FIELD_PHYSICAL_DESCRIPTIONS = "physicalDescriptions";
    public static final String FIELD_PHYSICAL_DESCRIPTIONS_EXTENT = "physicalDescriptionsExtent";
    public static final String FIELD_PHYSICAL_DESCRIPTIONS_SIZE = "physicalDescriptionsSize";
    public static final String FIELD_RECORD_ORIGIN = "recordOrigin";
    // periodical volume
    public static final String FIELD_PER_VOLUME_NUMBER = "periodicalVolumeNumber";
    public static final String FIELD_PER_VOLUME_YEAR = "periodicalVolumeYear";
    // periodical issue
    public static final String FIELD_PER_ISSUE_NUMBER = "PeriodicalItemNumber";
    public static final String FIELD_PER_ISSUE_NUMBER_SORTING = "PeriodicalItemNumberSorting";
    public static final String FIELD_PER_ISSUE_DATE = "periodicalItemDate";
    // monograph
    public static final String FIELD_PRESERVATION_TREATMENT = "preservationTreatment";
    public static final String FIELD_PRESERVATION_STATEOFART = "preservationStateOfArt";
    // monograph unit
    public static final String FIELD_MONOGRAPHUNIT_NUMBER = "monographUnitNumber";

    public ModsCustomDataSource() {
        setID(ID);
        setDataFormat(DSDataFormat.JSON);
        setDataURL(RestConfig.URL_DIGOBJECT_MODS_CUSTOM);
        setRecordXPath("/mods");

        DataSourceField fieldPid = new DataSourceField(FIELD_PID, FieldType.TEXT);
        fieldPid.setPrimaryKey(true);
        fieldPid.setRequired(true);

        DataSourceField fieldTimestamp = new DataSourceField(FIELD_TIMESTAMP, FieldType.TEXT);
        fieldTimestamp.setRequired(true);
        fieldTimestamp.setHidden(true);

        DataSourceField fieldEditor = new DataSourceField(MetaModelDataSource.FIELD_EDITOR, FieldType.TEXT);
        fieldEditor.setRequired(true);
        fieldEditor.setHidden(true);

        DataSourceField fieldData = new DataSourceField(FIELD_DATA, FieldType.ANY);
        fieldData.setTypeAsDataSource(new DataSource() {
            {
                DataSourceField identifiers = new DataSourceField(FIELD_IDENTIFIERS, FieldType.ANY);
                identifiers.setTypeAsDataSource(IdentifierDataSource.getInstance());
                setFields(identifiers);
            }
        });

        setFields(fieldPid, fieldTimestamp, fieldEditor, fieldData);

        OperationBinding updateOp = new OperationBinding();
        updateOp.setOperationType(DSOperationType.UPDATE);
        updateOp.setDataProtocol(DSProtocol.POSTPARAMS);

        setOperationBindings(updateOp);

        setRequestProperties(RestConfig.createRestRequest(getDataFormat()));
    }

    public static ModsCustomDataSource getInstance() {
        ModsCustomDataSource ds = (ModsCustomDataSource) DataSource.get(ID);
        ds = ds != null ? ds : new ModsCustomDataSource();
        return ds;
    }

}
