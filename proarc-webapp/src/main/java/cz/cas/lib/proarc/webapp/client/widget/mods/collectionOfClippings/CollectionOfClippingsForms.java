/*
 * Copyright (C) 2020 Lukas Sykora
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
package cz.cas.lib.proarc.webapp.client.widget.mods.collectionOfClippings;

import com.smartgwt.client.widgets.form.DynamicForm;
import cz.cas.lib.proarc.common.object.collectionOfClippings.CollectionOfClippingsPlugin;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.LanguagesDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.widget.mods.NdkFormGenerator;
import cz.cas.lib.proarc.webapp.client.widget.mods.PageForm;
import cz.cas.lib.proarc.webapp.shared.form.Form;

public class CollectionOfClippingsForms {

    private final ClientMessages i18n;
    private final String activeLocale;

    public CollectionOfClippingsForms(ClientMessages i18n) {
        this.i18n = i18n;
        activeLocale = LanguagesDataSource.activeLocale();
    }

    public DynamicForm getForm(MetaModelDataSource.MetaModelRecord model) {
        String modelId = model.getId();
        Form f;
        if (CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_VOLUME.equals(modelId)) {
            f = new CollectionOfClippingsVolumeForm().build();
            f.setItemWidth("800");
        } else if (CollectionOfClippingsPlugin.MODEL_PAGE.equals(modelId)) {
            return new PageForm(i18n);
        } else {
            return null;
        }

        return new NdkFormGenerator(f, activeLocale).generateForm();
    }
}
