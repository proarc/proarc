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
package cz.cas.lib.proarc.webapp.client.widget.mods;

import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.List;

public class NdkNewPageForm {

    public Form build() {
        Form f = new Form();
        f.getFields().add(pageType());
        f.getFields().add(pageIndex());
        f.getFields().add(pageNumber());
        Field mods = new FieldBuilder("mods").setMaxOccurrences(1).createField();
        f.getFields().add(mods);
        List<Field> modsFields = mods.getFields();
        //modsFields.add(part());
        modsFields.add(genre(true));
        modsFields.add(titleInfo());
        modsFields.add(typeOfResource());
        modsFields.add(note());
        modsFields.add(physicalDescription());
        modsFields.add(identifier());
        return f;
    }

    private Field pageIndex() {
        return new FieldBuilder("pageIndex").setMaxOccurrences(1).setTitle("Index strany - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                        .setHint("Číslo strany, označující pořadí v reprezentaci.")
                        .createField(); // value
    }

    private Field pageNumber() {
        return new FieldBuilder("pageNumber").setMaxOccurrences(1).setTitle("Číslo strany - MA").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                .setHint("Číslo strany, označující pořadí v reprezentaci.")
                .createField(); // value
    }

    private Field part() {
        return new FieldBuilder("part").setTitle("Part - R").setMaxOccurrences(1)
                .setHint("Údaje o popisované straně.")
                /*.addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.COMBO)
                        .setDefaultValue("normalPage").setWidth("400")
                        .setHint("Vybrat jednu z hodnot.")
                        .addMapValue("advertisement", "Reklama (Advertisement)")
                        .addMapValue("appendix", "Dodatek (Appendix)")
                        .addMapValue("backCover", "Zadn\u00ed desky (BackCover)")
                        .addMapValue("backEndPaper", "Zadn\u00ed p\u0159eds\u00e1dka (BackEndPaper)")
                        .addMapValue("backEndSheet", "Zadn\u00ed p\u0159\u00edde\u0161t\u00ed (BackEndSheet)")
                        .addMapValue("bibliography", "Bibliografie (Bibliography)")
                        .addMapValue("blank", "Pr\u00e1zdn\u00e1 strana (Blank)")
                        .addMapValue("cover", "Desky vcelku (Cover)")
                        .addMapValue("dedication", "Dedikace (Dedication)")
                        .addMapValue("edge", "O\u0159\u00edzka (Edge)")
                        .addMapValue("errata", "Errata")
                        .addMapValue("flyLeaf", "Voln\u00fd list (FlyLeaf)")
                        .addMapValue("frontCover", "P\u0159edn\u00ed desky (FrontCover)")
                        .addMapValue("frontEndPaper", "P\u0159edn\u00ed p\u0159eds\u00e1dka (Front End Paper)")
                        .addMapValue("frontEndSheet", "P\u0159edn\u00ed p\u0159\u00edde\u0161t\u00ed (FrontEndSheet)")
                        .addMapValue("frontispiece", "Frontispis (Frontispiece)")
                        .addMapValue("frontJacket", "P\u0159edn\u00ed strana p\u0159ebalu (FrontJacket)")
                        .addMapValue("illustration", "Ilustrace (Illustration)")
                        .addMapValue("impressum", "Impressum (Impressum)")
                        .addMapValue("imprimatur", "Povolen\u00ed k tisku (Imprimatur)")
                        .addMapValue("index", "Rejst\u0159\u00edk (Index)")
                        .addMapValue("jacket", "P\u0159ebal (Jacket)")
                        .addMapValue("listOfIllustrations", "Seznam ilustrac\u00ed (ListOfIllustrations)")
                        .addMapValue("listOfMaps", "Seznam map (ListOfMaps)")
                        .addMapValue("listOfTables", "Seznam tabulek (ListOfTables)")
                        .addMapValue("map", "Mapa (Map)")
                        .addMapValue("normalPage", "Norm\u00e1ln\u00ed strana (NormalPage)")
                        .addMapValue("sheetmusic", "Notov\u00fd z\u00e1pis (Sheetmusic)")
                        .addMapValue("spine", "H\u0159bet (Spine)")
                        .addMapValue("table", "Tabulka (Table)")
                        .addMapValue("tableOfContents", "Obsah (Table Of Contents)")
                        .addMapValue("titlePage", "Tituln\u00ed strana (TitlePage)")
                        .createField()) // type
                .addField(new FieldBuilder("detail").setTitle("Detail - M").setMaxOccurrences(10)
                        .addField(new FieldBuilder("type").setMaxOccurrences(1).setTitle("Typ - M").setType(Field.COMBO).setDefaultValue("pageNumber")
                                .addMapValue("pageNumber", "Číslo strany")
                                .addMapValue("pageIndex", "Index strany")
                                .createField())
                        // start, type="stringPlusLanguage"
                        .addField(new FieldBuilder("number").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setTitle("Číslo - M").setMaxOccurrences(1).setType(Field.TEXT)
                                        .setHint("Číslo strany, označující pořadí v reprezentaci.")
                                        .createField()) // value
                                .createField()) // number
                        .createField())*/
                .addField(new FieldBuilder("extent").setTitle("Extent - O").setMaxOccurrences(10)
                        .addField(new FieldBuilder("unit").setMaxOccurrences(1).setTitle("Unit - M").setType(Field.COMBO)
                                .addMapValue("pages", "pages")
                                .createField())
                        // start, type="stringPlusLanguage"
                        .addField(new FieldBuilder("start").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setTitle("Start - M").setMaxOccurrences(1).setType(Field.TEXT)
                                        .setHint("Číslo strany, označující pořadí v reprezentaci.")
                                        .createField()) // value
                                .createField()) // start
                        .createField()) // extent
                .createField(); // part
    }

    protected Field physicalDescription() {
        return new FieldBuilder("physicalDescription").setTitle("Physical Description - O").setMaxOccurrences(1)
                .addField(new FieldBuilder("note").setTitle("Note - O").setMaxOccurrences(10)
                        .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                                .setHint("Poznámka o fyzickém stavu strany. Pro ka6dou dokumentu."
                                        + "<p>Pro každou poznámku je nutno vytvořit nový &lt;note> element.")
                                .createField()) // value
                        .createField()) // note
                .createField(); // physicalDescription
    }

    private Field note() {
        return new FieldBuilder("note").setTitle("Note - O").setMaxOccurrences(30)
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.COMBO)
                        .setHint("Označení pro pravou (right) či levou (left) stranu podle řazení v dokumentu; <p>" +
                                "pro označení strany spojené z vice snímků stran pak singlePage.")
                        .addMapValue("left", "Levá (left)")
                        .addMapValue("right", "Pravá (right)")
                        .addMapValue("singlePage", "Single Page")
                        .createField()) // value
                .createField(); // note
    }

    private Field titleInfo() {
        // titleInfo, titleInfoDefinition
        return new FieldBuilder("titleInfo").setTitle("Title Info - RA").setMaxOccurrences(10)
                .setHint("Název strany.<p>")
                // titleInfo@type, enum
                .addField(new FieldBuilder("title").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Title - RA").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Obsahuje nadpis či název, uvedený na dané straně. Pokud jej strana neobsahuje, je možné element ponechat nevyplněný.")
                                .createField()) // value
                        .createField()) // title
                .addField(new FieldBuilder("subTitle").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Subtitle - RA").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Podnázev strany, pakliže existuje.")
                                .createField()) // value
                        .createField()) // subTitle
                .addField(new FieldBuilder("nonSort").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Non sort - O").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Část názvu, která má být vynechána při vyhledávání (např. The)")
                                .createField()) // value
                        .createField()) // nonSort
                .createField(); // titleInfo
    }

    private Field genre(boolean required) {
        return new FieldBuilder("genre").setTitle("Genre - M").setMaxOccurrences(1)
                .setHint("Bližší údaje o typu dokumentu.<p>Pro NDK stranu hodnota “page” nebo “reprePage”.")
                //.addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                //        .createField())
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.COMBO).setRequired(required).setDefaultValue("page")
                        .addMapValue("page", "Strana")
                        .addMapValue("reprePage", "Reprezentativní strana")
                        .createField())
                .createField(); // genre
    }

    protected Field identifier() {
        return new FieldBuilder("identifier").setTitle("Identifier - M").setMaxOccurrences(10)
                .setHint("Údaje o identifikátoru.")
                .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.COMBO).setRequired(true)
                        .setHint("UUID - M - vygeneruje dodavatel")// XXX use ValueMap
                        .addMapValue("uuid", "UUID")
                        .createField())
                .addField(new FieldBuilder("value").setTitle("Identifier - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).createField())
                .createField(); // identifier
    }

    private Field typeOfResource() {
        return new FieldBuilder("typeOfResource").setMaxOccurrences(1)
                .addField(new FieldBuilder("value").setTitle("Type of Resource - R").setMaxOccurrences(1).setType(Field.SELECT)
                        .setHint("Popis charakteristiky typu nebo obsahu zdroje.<p>Pro NDK stranu hodnoty „text“, „image“, „notated music“ nebo „cartographic“.")
                        .addMapValue("text", "text")
                        .addMapValue("image", "image")
                        .addMapValue("notated music", "notated music")
                        .addMapValue("cartographic", "cartographic")
                        .createField()) // value
                .createField(); // typeOfResource
    }

    public static Field pageType() {
        return new FieldBuilder("pageType").setTitle("Typ strany - M").setMaxOccurrences(1).setType(Field.COMBO).setRequired(true)
                .setDefaultValue("normalPage").setWidth("400")
                .setHint("Vybrat jednu z hodnot.")
                .addMapValue("bibliography", "Bibliografie (Bibliography)")
                .addMapValue("dedication", "Dedikace (Dedication)")
                .addMapValue("cover", "Desky vcelku (Cover)")
                .addMapValue("appendix", "Dodatek (Appendix)")
                .addMapValue("errata", "Errata")
                .addMapValue("frontispiece", "Frontispis (Frontispiece)")
                .addMapValue("spine", "H\u0159bet (Spine)")
                .addMapValue("illustration", "Ilustrace (Illustration)")
                .addMapValue("impressum", "Impressum (Impressum)")
                .addMapValue("map", "Mapa (Map)")
                .addMapValue("normalPage", "Norm\u00e1ln\u00ed strana (NormalPage)")
                .addMapValue("sheetmusic", "Notov\u00fd z\u00e1pis (Sheetmusic)")
                .addMapValue("edge", "O\u0159\u00edzka (Edge)")
                .addMapValue("tableOfContents", "Obsah (Table Of Contents)")
                .addMapValue("jacket", "P\u0159ebal (Jacket)")
                .addMapValue("frontCover", "P\u0159edn\u00ed desky (FrontCover)")
                .addMapValue("frontEndSheet", "P\u0159edn\u00ed p\u0159\u00edde\u0161t\u00ed (FrontEndSheet)")
                .addMapValue("frontEndPaper", "P\u0159edn\u00ed p\u0159eds\u00e1dka (Front End Paper)")
                .addMapValue("frontJacket", "P\u0159edn\u00ed strana p\u0159ebalu (FrontJacket)")
                .addMapValue("imprimatur", "Povolen\u00ed k tisku (Imprimatur)")
                .addMapValue("blank", "Pr\u00e1zdn\u00e1 strana (Blank)")
                .addMapValue("index", "Rejst\u0159\u00edk (Index)")
                .addMapValue("advertisement", "Reklama (Advertisement)")
                .addMapValue("listOfIllustrations", "Seznam ilustrac\u00ed (ListOfIllustrations)")
                .addMapValue("listOfMaps", "Seznam map (ListOfMaps)")
                .addMapValue("listOfTables", "Seznam tabulek (ListOfTables)")
                .addMapValue("table", "Tabulka (Table)")
                .addMapValue("titlePage", "Tituln\u00ed strana (TitlePage)")
                .addMapValue("flyLeaf", "Voln\u00fd list (FlyLeaf)")
                .addMapValue("backCover", "Zadn\u00ed desky (BackCover)")
                .addMapValue("backEndSheet", "Zadn\u00ed p\u0159\u00edde\u0161t\u00ed (BackEndSheet)")
                .addMapValue("backEndPaper", "Zadn\u00ed p\u0159eds\u00e1dka (BackEndPaper)")
                .createField(); // type
    }
}
