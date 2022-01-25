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
package cz.cas.lib.proarc.webapp.client.action;

import com.smartgwt.client.data.DSResponse;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.util.SC;
import cz.cas.lib.proarc.webapp.client.ds.RestConfig;
import cz.cas.lib.proarc.webapp.shared.rest.DigitalObjectResourceApi;

/**
 * A basic implementation of {@link Action} interface.
 *
 * @author Jan Pokorsky
 */
public abstract class AbstractAction implements Action {

    private String title;
    private String icon;
    private String tooltip;

    public AbstractAction(String title, String icon, String tooltip) {
        this.title = title;
        this.icon = icon;
        this.tooltip = tooltip;
    }

    @Override
    public boolean accept(ActionEvent event) {
        return true;
    }

    @Override
    public String getIcon() {
        return icon;
    }

    @Override
    public String getTitle() {
        return title;
    }

    @Override
    public String getTooltip() {
        return tooltip;
    }

    @Override
    public void setIcon(String icon) {
        this.icon = icon;
    }

    @Override
    public void setTitle(String title) {
        this.title = title;
    }

    @Override
    public void setTooltip(String tooltip) {
        this.tooltip = tooltip;
    }

    public static boolean hasValidationError(DSResponse response) {
        return RestConfig.isStatusOk(response) && response.getTotalRows() == -1;
    }

    public static void handleValidations(DSResponse response) {
        if (response.getData() != null && response.getData().length == 1) {
            Record record = response.getData()[0];
            if (record != null) {
                SC.warn(record.getAttributeAsString(DigitalObjectResourceApi.ITEM_VALIDATION));
            }
        }
    }

}
