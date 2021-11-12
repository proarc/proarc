/*
 * Copyright (C) 2021 Lukas Sykora
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
package cz.cas.lib.proarc.webapp.client.widget.mods.graphic;

import cz.cas.lib.proarc.common.object.graphic.GraphicPlugin;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.LanguagesDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.client.widget.mods.NdkFormGenerator;
import cz.cas.lib.proarc.webapp.client.widget.mods.PageForm;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import com.smartgwt.client.widgets.form.DynamicForm;

public class GraphicForms {

    private final ClientMessages i18n;
    private final String activeLocale;
    private final String prefix = "complex:";

    public GraphicForms(ClientMessages i18n) {
        this.i18n = i18n;
        activeLocale = LanguagesDataSource.activeLocale();
    }

    public DynamicForm getForm(MetaModelDataSource.MetaModelRecord model, String prefix) {
        String modelId = prefix + model.getId();
        Form f;
        if ((this.prefix + GraphicPlugin.MODEL_GRAPHIC).equals(modelId)) {
            f = new GraphicForm().buildComplexForm();
        } else if (GraphicPlugin.MODEL_GRAPHIC.equals(modelId)) {
            f = new GraphicForm().build();
        } else if (GraphicPlugin.MODEL_PAGE.equals(modelId)) {
            return new PageForm(i18n);
        } else {
            return null;
        }

        return new NdkFormGenerator(f, activeLocale).generateForm();
    }
}
