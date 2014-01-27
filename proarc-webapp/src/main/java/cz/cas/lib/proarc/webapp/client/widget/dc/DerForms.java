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
import static cz.cas.lib.proarc.webapp.shared.form.Field.*;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;

/**
 * Helper with form definitions of DER DESA models.
 *
 * @author Jan Pokorsky
 */
public final class DerForms {

    public DynamicForm getForm(MetaModelRecord model, String lang) {
        String modelId = model.getId();
        Form f;
        if ("model:derFolder".equals(modelId)) {
            f = derFolder();
        } else if ("model:derDocument".equals(modelId)) {
            f = derDocument();
        } else if ("model:derFile".equals(modelId)) {
            f = derFile();
        } else {
            return null;
        }
        return new FormGenerator(f, lang).generateForm();
    }

    public static Form derFolder() {
        Form f = new Form();
        f.getFields().add(new FieldBuilder(DcConstants.IDENTIFIER).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setRequired(true)
                        .setTitle("Identifikátor složky")
                        .setHint("<b>Identifikátor složky</b> - pole pro zadání inventárního čísla složky")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.TITLE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setRequired(true)
                        .setTitle("Název složky")
                        .setType(TEXT).setMaxOccurrences(1).setLength(2000)
                        .createField())
                .createField());
        // ciselnik!
        f.getFields().add(new FieldBuilder(DcConstants.SUBJECT).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setRequired(true)
                        .setTitle("Klasifikace")
                        .setType(COMBO).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.TYPE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setRequired(true)
                        .setTitle("Typ složky")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.CONTRIBUTOR).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE)
                        .setTitle("Přispěvatel")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.DATE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Datum")
                        .setType(TEXT).setMaxOccurrences(1).setLength(20).setWidth("150")
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.CREATOR).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Tvůrce")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.FORMAT).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Format")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.SOURCE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Zdroj")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.LANGUAGE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Jazyk")
                        .setType(TEXT).setMaxOccurrences(1).setLength(20).setWidth("150")
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.RELATION).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Vztah")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.COVERAGE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Pokrytí")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.RIGHTS).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Práva")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.DESCRIPTION).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Popis")
                        .setType(TEXTAREA).setLength(2000).setMaxOccurrences(1).setWidth("400")
                        .createField())
                .createField());
        return f;
    }

    public static Form derDocument() {
        Form f = new Form();
        f.getFields().add(new FieldBuilder(DcConstants.IDENTIFIER).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setRequired(true)
                        .setTitle("Číslo dokumentu")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.TITLE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setRequired(true)
                        .setTitle("Název dokumentu")
                        .setType(TEXT).setMaxOccurrences(1).setLength(2000)
                        .createField())
                .createField());
        // ciselnik!
        f.getFields().add(new FieldBuilder(DcConstants.SUBJECT).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setRequired(true)
                        .setTitle("Klasifikace")
                        .setType(COMBO).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.TYPE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setRequired(true)
                        .setTitle("Typ dokumentu")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.CONTRIBUTOR).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE)
                        .setTitle("Přispěvatel")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.DATE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Datum")
                        .setType(TEXT).setMaxOccurrences(1).setLength(20).setWidth("150")
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.CREATOR).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Tvůrce")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.FORMAT).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Format")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.SOURCE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Zdroj")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.LANGUAGE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Jazyk")
                        .setType(TEXT).setMaxOccurrences(1).setLength(20).setWidth("150")
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.RELATION).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Vztah")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.COVERAGE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Pokrytí")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.RIGHTS).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Práva")
                        .setType(TEXT).setMaxOccurrences(1).setLength(1000)
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.DESCRIPTION).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Popis")
                        .setType(TEXTAREA).setLength(2000).setMaxOccurrences(1).setWidth("400")
                        .createField())
                .createField());
        return f;
    }

    public static Form derFile() {
        Form f = new Form();
        f.getFields().add(new FieldBuilder(DcConstants.TYPE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setWidth("300").setRequired(true)
                        .setMaxOccurrences(1)
                        .setType(SELECT).setTitle("Typ")
                        .addMapValue("ADM", "Technická metadata")
                        .addMapValue("PS", "Primární sken")
                        .addMapValue("MC", "Master copy")
                        .addMapValue("UC", "User copy")
                        .createField())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.FORMAT).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setMaxOccurrences(1)
                    .setTitle("MIME typ").setType(TEXT).setReadOnly(true).setWidth("300")
                .createField())
            .createField()); // FORMAT
        f.getFields().add(new FieldBuilder(DcConstants.TITLE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setMaxOccurrences(1)
                    .setTitle("Název souboru").setType(TEXT).setReadOnly(true).setWidth("400")
                .createField())
            .createField()); // TITLE
        return f;
    }

    private static Field noLang() {
        return new FieldBuilder(DcConstants.LANG).setHidden(true).createField();
    }

}
