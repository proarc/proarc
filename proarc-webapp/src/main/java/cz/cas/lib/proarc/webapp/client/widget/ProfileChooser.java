/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.client.widget;

import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.ResultSet;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.DataArrivedEvent;
import com.smartgwt.client.widgets.form.fields.events.DataArrivedHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.HoverCustomizer;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ClientUtils;
import cz.cas.lib.proarc.webapp.client.ds.ConfigurationProfileDataSource;
import cz.cas.lib.proarc.webapp.shared.rest.ConfigurationProfileResourceApi;
import cz.cas.lib.proarc.webapp.shared.rest.ConfigurationProfileResourceApi.ProfileGroup;
import cz.cas.lib.proarc.webapp.shared.rest.ImportResourceApi;

/**
 * Widgets to handle configuration profiles.
 *
 * @author Jan Pokorsky
 */
public class ProfileChooser {

    /**
     * Creates a SelectItem with a list of profiles of given group. It selects
     * the first profile in case there is just one. It notifies misconfigured
     * profiles with red color and the error message.
     */
    public static SelectItem createProfileSelection(ProfileGroup group, ClientMessages i18n) {
        final SelectItem selectProfile = new SelectItem(ImportResourceApi.IMPORT_BATCH_PROFILE,
                i18n.ImportSourceChooser_OptionProfile_Title());
        ConfigurationProfileDataSource.setOptionDataSource(selectProfile, group);
        selectProfile.setAllowEmptyValue(true);
        selectProfile.setEmptyDisplayValue(
                ClientUtils.format("<i>&lt;%s&gt;</i>", i18n.NewDigObject_OptionModel_EmptyValue_Title()));
        selectProfile.setRequired(true);
        selectProfile.setWidth(300);

        // it is necessary to get CellFormatter working!
        selectProfile.setPickListFields(new ListGridField(ConfigurationProfileResourceApi.PROFILE_LABEL));

        ListGrid profilePickListProperties = new ListGrid();
        profilePickListProperties.setCanHover(true);
        profilePickListProperties.setShowHover(true);
        profilePickListProperties.setHoverWidth(300);
        profilePickListProperties.setHoverCustomizer(new HoverCustomizer() {

            @Override
            public String hoverHTML(Object value, ListGridRecord record, int rowNum, int colNum) {
                String error = record.getAttribute(ConfigurationProfileResourceApi.PROFILE_ERROR);
                return error != null ? error : record.getAttribute(ConfigurationProfileResourceApi.PROFILE_DESCRIPTION);
            }
        });
        profilePickListProperties.setCellFormatter(new CellFormatter() {

            @Override
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                if (value == null) {
                    return null;
                }
                String error = record.getAttribute(ConfigurationProfileResourceApi.PROFILE_ERROR);
                String result = String.valueOf(value);
                if (error != null) {
                    result = "<span style='color:red'>" + result + "</span>";
                }
                return result;
            }
        });
        selectProfile.setPickListProperties(profilePickListProperties);
        selectProfile.addDataArrivedHandler(new DataArrivedHandler() {

            @Override
            public void onDataArrived(DataArrivedEvent event) {
                if (event.getStartRow() == 0) {
                    ResultSet data = event.getData();
                    int length = data.getLength();
                    if (length == 1) {
                        // issue 190: select in case of single device
                        Record profile = data.get(0);
                        String profileId = profile.getAttribute(ConfigurationProfileResourceApi.PROFILE_ID);
                        selectProfile.setValue(profileId);
                        selectProfile.setDefaultValue(profileId);
                    }
                }
            }
        });
        return selectProfile;
    }
}
