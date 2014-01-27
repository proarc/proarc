/*
 * Copyright (C) 2014 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.client.widget.dc;

import com.smartgwt.client.widgets.form.DynamicForm;
import cz.cas.lib.proarc.oaidublincore.DcConstants;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.client.widget.form.FormGenerator;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;

/**
 * DC forms for DES DESA model.
 *
 * @author Jan Pokorsky
 */
public class DesForms {

    public DynamicForm getForm(MetaModelRecord model, String lang) {
        String modelId = model.getId();
        Form f;
        if ("model:desFile".equals(modelId)) {
            f = fileForm();
        } else {
            return null;
        }
        return new FormGenerator(f, lang).generateForm();
    }

    private Form fileForm() {
        Form form = new Form();
        form.getFields().add(new FieldBuilder(DcConstants.TYPE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setMaxOccurrences(1)
                    .setTitle("Typ").setType(Field.SELECT).setRequired(true).setWidth("300")
                    .addMapValue("Original", "Obsah ve výstupním formátu")
                    .addMapValue("Input", "Obsah v původním digitálním formátu")
                    .addMapValue("Digitized", "Digitalizovaný obsah")
                    .addMapValue("Preview", "Obsah v náhledovém formátu")
                    .addMapValue("Migrated", "Obsah migrovaný do nového formátu")
                .createField())
            .createField()); // TYPE
        form.getFields().add(new FieldBuilder(DcConstants.FORMAT).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setMaxOccurrences(1)
                    .setTitle("MIME typ").setType(Field.TEXT).setReadOnly(true).setWidth("300")
                .createField())
            .createField()); // FORMAT
        form.getFields().add(new FieldBuilder(DcConstants.TITLE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setMaxOccurrences(1)
                    .setTitle("Název souboru").setType(Field.TEXT).setReadOnly(true).setWidth("400")
                .createField())
            .createField()); // TITLE
        return form;
    }

}
