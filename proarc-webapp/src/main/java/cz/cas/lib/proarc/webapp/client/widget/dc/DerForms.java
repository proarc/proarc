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

import cz.cas.lib.proarc.oaidublincore.DcConstants;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;

/**
 * Helper with form definitions of DER DESA models.
 *
 * @author Jan Pokorsky
 */
public final class DerForms {

    public static Form derFolder() {
        Form f = new Form();
        f.getFields().add(new FieldBuilder(DcConstants.IDENTIFIER).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setWidth("300").setRequired(true)
                        .setTitle("Identifikátor složky")
                        .setHint("<b>Identifikátor složky</b> - pole pro zadání inventárního čísla složky")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.TITLE).setMaxOccurrences(1).setWidth("*")
                .addField(new FieldBuilder(DcConstants.VALUE).setWidth("*").setRequired(true)
                        .setTitle("Název složky")
                        .createField())
                .addField(noLang())
                .createField());
        // ciselnik!
        f.getFields().add(new FieldBuilder(DcConstants.SUBJECT).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setWidth("300").setRequired(true)
                        .setTitle("Klasifikace")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.TYPE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setWidth("300").setRequired(true)
                        .setTitle("Typ složky")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.CONTRIBUTOR).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setWidth("300")
                        .setTitle("Přispěvatel")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.DATE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Datum")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.CREATOR).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Tvůrce")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.FORMAT).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Format")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.SOURCE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Zdroj")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.LANGUAGE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Jazyk")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.RELATION).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Vztah")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.COVERAGE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Pokrytí")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.RIGHTS).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Práva")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.DESCRIPTION).setMaxOccurrences(1).setWidth("*")
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Popis").setWidth("*")
                        .setType("textArea").setLength(2000)
                        .createField())
                .addField(noLang())
                .createField());
        return f;
    }

    public static Form derDocument() {
        Form f = new Form();
        f.getFields().add(new FieldBuilder(DcConstants.IDENTIFIER).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setWidth("300").setRequired(true)
                        .setTitle("Číslo dokumentu")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.TITLE).setMaxOccurrences(1).setWidth("*")
                .addField(new FieldBuilder(DcConstants.VALUE).setWidth("*").setRequired(true)
                        .setTitle("Název dokumentu")
                        .createField())
                .addField(noLang())
                .createField());
        // ciselnik!
        f.getFields().add(new FieldBuilder(DcConstants.SUBJECT).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setWidth("300").setRequired(true)
                        .setTitle("Klasifikace")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.TYPE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setWidth("300").setRequired(true)
                        .setTitle("Typ dokumentu")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.CONTRIBUTOR).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setWidth("300")
                        .setTitle("Přispěvatel")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.DATE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Datum")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.CREATOR).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Tvůrce")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.FORMAT).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Format")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.SOURCE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Zdroj")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.LANGUAGE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Jazyk")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.RELATION).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Vztah")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.COVERAGE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Pokrytí")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.RIGHTS).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Práva")
                        .createField())
                .addField(noLang())
                .createField());
        f.getFields().add(new FieldBuilder(DcConstants.DESCRIPTION).setMaxOccurrences(1).setWidth("*")
                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("Popis").setWidth("*")
                        .setType("textArea").setLength(2000)
                        .createField())
                .addField(noLang())
                .createField());
        return f;
    }

    public static Form derFile() {
        Form f = new Form();
//        f.getFields().add(new FieldBuilder(DcConstants.TITLE).setMaxOccurrences(1).setWidth("*")
//                .addField(new FieldBuilder(DcConstants.VALUE).setWidth("*").setRequired(true)
//                        .setTitle("Název souboru???")
//                        .createField())
//                .addField(noLang())
//                .createField());
        // XXX valueMap
        f.getFields().add(new FieldBuilder(DcConstants.TYPE).setMaxOccurrences(1)
                .addField(new FieldBuilder(DcConstants.VALUE).setWidth("300").setRequired(true)
                        .setType("select").setTitle("Typ")
                        .addMapValue("ADM", "Technická metadata")
                        .addMapValue("PS", "Primární sken")
                        .addMapValue("MC", "Master copy")
                        .addMapValue("UC", "User copy")
                        .createField())
                .addField(noLang())
                .createField());
//        f.getFields().add(new FieldBuilder(DcConstants.FORMAT).setMaxOccurrences(1)
//                .addField(new FieldBuilder(DcConstants.VALUE).setTitle("MIME??")
//                        .createField())
//                .addField(noLang())
//                .createField());
//        System.out.println("derFile: " + f.getFields().get(0));
        return f;
    }

    private static Field noLang() {
        return new FieldBuilder(DcConstants.LANG).setHidden(true).createField();
    }

}
