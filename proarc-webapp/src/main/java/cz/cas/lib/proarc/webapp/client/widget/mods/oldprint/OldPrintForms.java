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
package cz.cas.lib.proarc.webapp.client.widget.mods.oldprint;

import com.smartgwt.client.widgets.form.DynamicForm;
import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.LanguagesDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.client.widget.mods.NdkFormGenerator;
import cz.cas.lib.proarc.webapp.client.widget.mods.PageForm;
import cz.cas.lib.proarc.webapp.shared.form.Form;

/**
 * The helper for old print forms.
 *
 * @author Jan Pokorsky
 */
public class OldPrintForms {

    private final ClientMessages i18n;
    private final String activeLocale;

    public OldPrintForms(ClientMessages i18n) {
        this.i18n = i18n;
        activeLocale = LanguagesDataSource.activeLocale();
    }
    public DynamicForm getForm(MetaModelRecord model) {
        String modelId = model.getId();
        Form f;
        if ("model:oldprintvolume".equals(modelId)) {
            f = new OldPrintVolumeForm().build();
            f.setItemWidth("800");
        } else if ("model:oldprintsupplement".equals(modelId)) {
            f = new OldPrintSupplementForm().build();
        } else if ("model:oldprintpage".equals(modelId)) {
            return new PageForm(i18n, BundleName.MODS_OLDPRINT_PAGE_TYPES);
        } else if ("model:oldprintmonographtitle".equals(modelId)) {
            f = new OldPrintMonographTitleForm().build();
        } else {
            return null;
        }
        return new NdkFormGenerator(f, activeLocale).generateForm();
    }

}
