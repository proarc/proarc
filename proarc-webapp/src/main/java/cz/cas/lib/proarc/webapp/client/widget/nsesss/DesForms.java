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
package cz.cas.lib.proarc.webapp.client.widget.nsesss;

import cz.cas.lib.proarc.webapp.shared.form.Field;
import static cz.cas.lib.proarc.webapp.shared.form.Field.*;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.HashMap;

/**
 * NSESSS forms for DES DESA model.
 *
 * @author Jan Pokorsky
 */
public class DesForms {

    public static Form spisForm() {
        Form f = new Form();
        f.getFields().add(new FieldBuilder("Spis").setMaxOccurrences(1)

                .addField(new FieldBuilder("EvidencniUdaje").setMaxOccurrences(1)
                    .addField(new FieldBuilder("Identifikace").setMaxOccurrences(1)
                        .addField(createIdentifikatorField("Identifikator", "Identifikace spisu v evidenci"))
                    .createField()) // Identifikace

                    .addField(new FieldBuilder("Popis").setMaxOccurrences(1).setWidth("400")
                        .addField(new FieldBuilder("Nazev").setTitle("Název spisu").setMaxOccurrences(1).setType(TEXT).setLength(100).setRequired(true).createField())
                        .addField(new FieldBuilder("Komentar").setTitle("Komentář").setMaxOccurrences(1).setType(TEXTAREA).setLength(255).createField())
                        .addField(new FieldBuilder("KlicovaSlova").setMaxOccurrences(1)
                            .addField(new FieldBuilder("KlicoveSlovo").setTitle("Klíčová slova").setMaxOccurrences(10).setType(TEXT).setLength(100).createField())
                        .createField()) // KlicovaSlova
                    .createField()) // Popis

                    .addField(new FieldBuilder("Evidence").setMaxOccurrences(1)
                        .addField(new FieldBuilder("EvidencniCislo").setTitle("Spisová značka").setMaxOccurrences(1).setType(TEXT).setLength(50).setRequired(true).createField())
                        .addField(new FieldBuilder("UrceneCasoveObdobi").setMaxOccurrences(1)
                            .addField(new FieldBuilder("Rok").setTitle("Časové období evidence").setMaxOccurrences(1).setType(G_YEAR).setRequired(true).createField())
                        .createField()) // UrceneCasoveObdobi
                        .addField(new FieldBuilder("NazevEvidenceDokumentu").setTitle("Název evidence").setMaxOccurrences(1).setType(TEXT).setLength(100).setRequired(true).createField())
                    .createField()) // Evidence

                    .addField(new FieldBuilder("Puvod").setMaxOccurrences(1)
                        .addField(new FieldBuilder("DatumVytvoreni").setMaxOccurrences(1)
                            .addField(new FieldBuilder("value").setTitle("Datum vytvoření").setMaxOccurrences(1).setType(DATE).setRequired(true).createField())
                        .createField()) // DatumVytvoreni
                    .createField()) // Puvod

                    .addField(new FieldBuilder("Trideni").setMaxOccurrences(1).setTitle("Věcná skupina").setRequired(true)
                        .addField(new FieldBuilder("JednoduchySpisovyZnak").setTitle("Kód").setMaxOccurrences(1).setType(SELECT).setLength(50).setRequired(true)
                            .setOptionDataSource(new FieldBuilder("desa-des.rec-cl").setWidth("400")
                                    .addField(new FieldBuilder("fullyQcc").setTitle("Kód").createField())
                                    .addField(new FieldBuilder("title").setTitle("Název").createField())
                                .createField(), // desa-des.rec-cl
                                "classCode", new HashMap<String, String>() {{put("classCode", "JednoduchySpisovyZnak"); put("fullyQcc", "PlneUrcenySpisovyZnak");}})
                        .createField()) // JednoduchySpisovyZnak
                        .addField(new FieldBuilder("PlneUrcenySpisovyZnak").setTitle("Plný kód").setMaxOccurrences(1).setType(TEXT).setLength(255).setRequired(true).createField())
                    .createField()) // Trideni

                    .addField(new FieldBuilder("VyrizeniUzavreni").setTitle("Uzavření").setMaxOccurrences(1).setRequired(true)
                        .addField(new FieldBuilder("Datum").setMaxOccurrences(1)
                            .addField(new FieldBuilder("value").setTitle("Datum vyřízení").setMaxOccurrences(1).setType(DATE).setRequired(true).createField())
                        .createField()) // Datum
                        .addField(new FieldBuilder("Zpusob").setTitle("Způsob vyřízení").setMaxOccurrences(1).setType(COMBO)
                            .addMapValue("VYŘÍZENÍ_DOKUMENTEM", "vyřízení dokumentem").addMapValue("POSTOUPENÍ", "postoupení").addMapValue("VZETÍ_NA_VĚDOMÍ", "vzetí na vědomí").addMapValue("JINÝ_ZPŮSOB", "jiný způsob")
                            .setRequired(true)
                        .createField()) // Zpusob
                        .addField(new FieldBuilder("Oduvodneni").setTitle("Odůvodnění vyřízení").setMaxOccurrences(1).setType(TEXT).setLength(100).createField())
                        .addField(new FieldBuilder("Zpracovatel").setMaxOccurrences(1)
                            .addField(createSubjectOsobyInterniField("Zpracovatelé")) // Subjekt
                        .createField()) // Zpracovatel
                    .createField()) // VyrizeniUzavreni

                    .addField(new FieldBuilder("Vyrazovani").setTitle("Skartační režim").setMaxOccurrences(1).setRequired(true)
                        .addField(new FieldBuilder("SkartacniRezim").setMaxOccurrences(1).setRequired(true)
                            .addField(new FieldBuilder("Nazev").setTitle("Název").setMaxOccurrences(1).setType(SELECT).setWidth("400").setLength(100).setRequired(true)
                                .setOptionDataSource(new FieldBuilder("desa-des.rd-cntrl")
                                        .addField(new FieldBuilder("acr").setTitle("Kód").createField())
                                        .addField(new FieldBuilder("title").setTitle("Název").createField())
                                    .createField(),
                                    "title",
                                    new HashMap<String, String>() {{
                                        put("acr", "Identifikator/value");
                                        put("title", "Nazev");
                                        put("reas", "Oduvodneni");
                                        put("action", "SkartacniZnak");
                                        put("period", "SkartacniLhuta");
                                    }})
                            .createField()) // Nazev
                            .addField(new FieldBuilder("Identifikator").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setTitle("Identifikátor skartačního režimu").setMaxOccurrences(1).setType(TEXT).setReadOnly(true).setWidth("400").createField())
                            .createField()) // Identifikator
                            .addField(new FieldBuilder("Oduvodneni").setTitle("Odůvodnění skartačního režimu").setMaxOccurrences(1).setType(TEXT).setReadOnly(true).setRequired(true).createField())
                            .addField(new FieldBuilder("SkartacniZnak").setTitle("Skartační znak").setMaxOccurrences(1).setType(SELECT).setReadOnly(true).setRequired(true)
                                .addMapValue("A", "A - Archiv").addMapValue("S", "S - Stoupa").addMapValue("V", "V - Výběr")
                            .createField()) // SkartacniZnak
                            .addField(new FieldBuilder("SkartacniLhuta").setTitle("Skartační lhůta").setMaxOccurrences(1).setType(TEXT).setReadOnly(true).setRequired(true).setWidth("100").createField())
                            .addField(new FieldBuilder("SpousteciUdalost").setTitle("Spouštěcí událost").setMaxOccurrences(1).setType(TEXT).setLength(2000).setRequired(true).createField())
                        .createField()) // SkartacniRezim
                        .addField(new FieldBuilder("DataceVyrazeni").setMaxOccurrences(1).setRequired(true)
                            .addField(new FieldBuilder("RokSpousteciUdalosti").setTitle("Rok spouštěcí události").setMaxOccurrences(1).setType(G_YEAR).setRequired(true).createField())
                            .addField(new FieldBuilder("RokSkartacniOperace").setTitle("Rok skartační operace").setMaxOccurrences(1).setType(G_YEAR).setRequired(true).createField())
                        .createField()) // DataceVyrazeni
                    .createField()) // Vyrazovani

                    .addField(new FieldBuilder("Manipulace").setMaxOccurrences(1).setRequired(true)
                        .addField(new FieldBuilder("NezbytnyDokument").setTitle("Nezbytný dokument").setMaxOccurrences(1).setType(RADIOGROUP).setRequired(true)
                            .addMapValue("ANO", "Ano").addMapValue("NE", "Ne")
                        .createField()) // NezbytnyDokument
                        .addField(new FieldBuilder("AnalogovyDokument").setTitle("Analogová forma spisu").setMaxOccurrences(1).setType(RADIOGROUP).setRequired(true)
                            .addMapValue("ANO", "Ano").addMapValue("NE", "Ne")
                        .createField()) // AnalogovyDokument
                        .addField(new FieldBuilder("DatumOtevreni").setMaxOccurrences(1).setRequired(true)
                            .addField(new FieldBuilder("value").setTitle("Datum otevření spisu").setMaxOccurrences(1).setType(DATE).setRequired(true).createField())
                        .createField()) // DatumOtevreni
                        .addField(new FieldBuilder("DatumUzavreni").setMaxOccurrences(1)
                            .addField(new FieldBuilder("value").setTitle("Datum uzavření spisu").setMaxOccurrences(1).setType(DATE).createField())
                        .createField()) // DatumUzavreni
                    .createField()) // Manipulace

                .createField()) // EvidencniUdaje
//            .addField(new FieldBuilder("Dokumenty").createField())
            .createField()); // Spis
        return f;
    }

    public static Form intenalDocumentForm() {
        Form f = new Form();
        f.getFields().add(new FieldBuilder("Dokument").setMaxOccurrences(1)

                .addField(new FieldBuilder("EvidencniUdaje").setMaxOccurrences(1)
                    .addField(new FieldBuilder("Identifikace").setMaxOccurrences(1)
                        .addField(createIdentifikatorField("Identifikator", "Identifikace dokumentu v evidenci"))
                    .createField()) // Identifikace

                    .addField(new FieldBuilder("Popis").setMaxOccurrences(1).setWidth("400")
                        .addField(new FieldBuilder("Nazev").setTitle("Věc").setMaxOccurrences(1).setType(TEXT).setLength(100).setRequired(true).createField())
                        .addField(new FieldBuilder("Komentar").setTitle("Komentář").setMaxOccurrences(1).setType(TEXTAREA).setLength(255).createField())
                        .addField(new FieldBuilder("KlicovaSlova").setMaxOccurrences(1)
                            .addField(new FieldBuilder("KlicoveSlovo").setTitle("Klíčová slova").setMaxOccurrences(10).setType(TEXT).setLength(100).createField())
                        .createField()) // KlicovaSlova
                    .createField()) // Popis

                    .addField(new FieldBuilder("Evidence").setMaxOccurrences(1)
                        .addField(new FieldBuilder("EvidencniCislo").setTitle("Číslo jednací").setMaxOccurrences(1).setType(TEXT).setLength(50).setRequired(true).createField())
                        .addField(new FieldBuilder("UrceneCasoveObdobi").setMaxOccurrences(1)
                            .addField(new FieldBuilder("Rok").setTitle("Časové období evidence").setMaxOccurrences(1).setType(G_YEAR).setRequired(true).createField())
                        .createField()) // UrceneCasoveObdobi
                        .addField(new FieldBuilder("NazevEvidenceDokumentu").setTitle("Název evidence").setMaxOccurrences(1).setType(TEXT).setLength(100).setRequired(true).createField())
                    .createField()) // Evidence

                    .addField(new FieldBuilder("Jazyky").setMaxOccurrences(1)
                        .addField(new FieldBuilder("Jazyk").setTitle("Jazyk").setMaxOccurrences(1).setType(TEXT).setLength(3).createField())
                    .createField()) // Jazyky

                    .addField(new FieldBuilder("Puvod").setMaxOccurrences(1)
                        .addField(new FieldBuilder("VlastniDokument").setMaxOccurrences(1)
                            .addField(new FieldBuilder("DatumVytvoreni").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setTitle("Datum vytvoření").setMaxOccurrences(1).setType(DATE).setRequired(true).createField())
                            .createField()) // DatumVytvoreni
                            .addField(new FieldBuilder("Autor").setMaxOccurrences(1)
                                .addField(createSubjectOsobyInterniField("Autoři"))
                            .createField()) // Autor
                        .createField()) // VlastniDokument
                    .createField()) // Puvod

                    .addField(new FieldBuilder("Trideni").setTitle("Věcná skupina").setMaxOccurrences(1).setRequired(true)
//                        .addField(new FieldBuilder("Titul_SpisovyZnak").setTitle("Věcná skupina").setMaxOccurrences(1).setType(SELECT).setLength(50).setWidth("400").setRequired(true)
//                            .setOptionDataSource(new FieldBuilder("desa-des.rec-cl")
//                                    .addField(new FieldBuilder("fullyQcc").setTitle("Kód").createField())
//                                    .addField(new FieldBuilder("title").setTitle("Název").createField())
//                                .createField(), // desa-des.rec-cl
//                                "title", new HashMap<String, String>() {{put("classCode", "JednoduchySpisovyZnak"); put("fullyQcc", "PlneUrcenySpisovyZnak");}})
//                        .createField()) // JednoduchySpisovyZnak
//                        .addField(new FieldBuilder("JednoduchySpisovyZnak").setTitle("Spisový znak").setMaxOccurrences(1).setHidden(true).setType(TEXT).setLength(255).setRequired(true).createField())
//                        .addField(new FieldBuilder("PlneUrcenySpisovyZnak").setTitle("Plně určený spisový znak").setMaxOccurrences(1).setHidden(true).setType(TEXT).setLength(255).setRequired(true).createField())
                        .addField(new FieldBuilder("JednoduchySpisovyZnak").setTitle("Kód").setMaxOccurrences(1).setType(SELECT).setLength(50).setRequired(true)
                            .setOptionDataSource(new FieldBuilder("desa-des.rec-cl").setWidth("400")
                                    .addField(new FieldBuilder("fullyQcc").setTitle("Kód").createField())
                                    .addField(new FieldBuilder("title").setTitle("Název").createField())
                                .createField(), // desa-des.rec-cl
//                                "fullyQcc", new HashMap<String, String>() {{put("fullyQcc", "JednoduchySpisovyZnak"); put("title", "PlneUrcenySpisovyZnak");}})
                                "classCode", new HashMap<String, String>() {{put("classCode", "JednoduchySpisovyZnak"); put("fullyQcc", "PlneUrcenySpisovyZnak");}})
                        .createField()) // JednoduchySpisovyZnak
                        .addField(new FieldBuilder("PlneUrcenySpisovyZnak").setTitle("Plný kód").setMaxOccurrences(1).setType(TEXT).setLength(255).setRequired(true).createField())
                        .addField(new FieldBuilder("TypDokumentu").setMaxOccurrences(1).setRequired(true)
                            .addField(new FieldBuilder("Identifikator").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setTitle("Typ dokumentu").setMaxOccurrences(1).setType(SELECT).setRequired(true).setLength(50).setWidth("400")
                                    .setOptionDataSource(new FieldBuilder("desa-des.rec-type")
                                        .addField(new FieldBuilder("title").createField())
                                    .createField(), "title")
                                .createField()) // value
                            .createField()) // Identifikator
                        .createField()) // TypDokumentu
                    .createField()) // Trideni

                    .addField(new FieldBuilder("Vyrizeni").setTitle("Vyřizuje").setMaxOccurrences(1).setRequired(true)
                        .addField(new FieldBuilder("Datum").setMaxOccurrences(1)
                            .addField(new FieldBuilder("value").setTitle("Datum vyřízení").setMaxOccurrences(1).setType(DATE).setRequired(true).createField())
                        .createField()) // Datum
                        .addField(new FieldBuilder("Zpusob").setTitle("Způsob vyřízení").setMaxOccurrences(1).setType(COMBO)
                            .addMapValue("VYŘÍZENÍ_DOKUMENTEM", "vyřízení dokumentem").addMapValue("POSTOUPENÍ", "postoupení").addMapValue("VZETÍ_NA_VĚDOMÍ", "vzetí na vědomí").addMapValue("JINÝ_ZPŮSOB", "jiný způsob")
                            .setRequired(true)
                        .createField()) // Zpusob
                        .addField(new FieldBuilder("Oduvodneni").setTitle("Odůvodnění vyřízení").setMaxOccurrences(1).setType(TEXT).setLength(100).createField())
                        .addField(new FieldBuilder("Zpracovatel").setMaxOccurrences(1)
                            .addField(createSubjectOsobyInterniField("Zpracovatelé")) // Subjekt
                        .createField()) // Zpracovatel

                        .addField(new FieldBuilder("Prijemce").setMaxOccurrences(1)
                            .addField(new FieldBuilder("Subjekt").setTitle("Příjemci").setMaxOccurrences(10).setType(CUSTOM_FORM).setRequired(true)
                                .addField(new FieldBuilder("subjectType").setTitle("Typ příjemce").setMaxOccurrences(1).setType(RADIOGROUP)
                                    .addMapValue("PravnickaOsoba", "Právnická osoba").addMapValue("FyzickaOsoba", "Fyzická osoba")
                                .createField())
                                .addField(createIdentifikatorField("IdentifikatorOrganizace", "IČO organizace", false))
                                .addField(new FieldBuilder("NazevOrganizace").setTitle("Název organizace").setMaxOccurrences(1).setType(TEXT).setLength(100).setRequired(true).createField())
                                .addField(createIdentifikatorField("IdentifikatorFyzickeOsoby", "Identifikace osoby odesílatele", false))
                                .addField(new FieldBuilder("NazevFyzickeOsoby").setTitle("Jméno a příjmení osoby").setMaxOccurrences(1).setType(TEXT).setLength(100).setRequired(true).createField())
                                .addField(new FieldBuilder("OrganizacniUtvar").setTitle("Organizační útvar").setMaxOccurrences(1).setType(TEXT).setLength(100).setRequired(true).createField())
                                .addField(new FieldBuilder("PracovniPozice").setTitle("Pracovní pozice").setMaxOccurrences(1).setType(TEXT).setLength(100).setRequired(true).createField())
                                .addField(new FieldBuilder("SidloOrganizace").setTitle("Adresa organizace").setMaxOccurrences(1).setType(TEXT).setLength(2000).createField())
                                .addField(new FieldBuilder("ElektronickyKontakt").setTitle("E-mail").setMaxOccurrences(1).setType(TEXT).setLength(100).setRequired(true).createField())
                                // choice 2
                                .addField(new FieldBuilder("PostovniAdresa").setTitle("Adresa").setMaxOccurrences(1).setType(TEXT).setLength(2000).setRequired(true).createField())
                            .createField()) // Subjekt
                        .createField()) // Prijemce
                    .createField()) // Vyrizeni

                    .addField(new FieldBuilder("Vyrazovani").setTitle("Skartační režim").setMaxOccurrences(1).setRequired(true)
                        .addField(new FieldBuilder("SkartacniRezim").setMaxOccurrences(1).setRequired(true)
                            .addField(new FieldBuilder("Nazev").setTitle("Název").setMaxOccurrences(1).setType(SELECT).setWidth("400").setLength(100).setRequired(true)
                                .setOptionDataSource(new FieldBuilder("desa-des.rd-cntrl")
                                        .addField(new FieldBuilder("acr").setTitle("Kód").createField())
                                        .addField(new FieldBuilder("title").setTitle("Název").createField())
                                    .createField(),
                                    "title",
                                    new HashMap<String, String>() {{
                                        put("acr", "Identifikator/value");
                                        put("title", "Nazev");
                                        put("reas", "Oduvodneni");
                                        put("action", "SkartacniZnak");
                                        put("period", "SkartacniLhuta");
                                    }})
                            .createField()) // Nazev
                            .addField(new FieldBuilder("Identifikator").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setTitle("Identifikátor skartačního režimu").setMaxOccurrences(1).setType(TEXT).setReadOnly(true).setWidth("400").createField())
                            .createField()) // Identifikator
                            .addField(new FieldBuilder("Oduvodneni").setTitle("Odůvodnění skartačního režimu").setMaxOccurrences(1).setType(TEXT).setReadOnly(true).setRequired(true).createField())
                            .addField(new FieldBuilder("SkartacniZnak").setTitle("Skartační znak").setMaxOccurrences(1).setType(SELECT).setReadOnly(true).setRequired(true)
                                .addMapValue("A", "A - Archiv").addMapValue("S", "S - Stoupa").addMapValue("V", "V - Výběr")
                            .createField()) // SkartacniZnak
                            .addField(new FieldBuilder("SkartacniLhuta").setTitle("Skartační lhůta").setMaxOccurrences(1).setType(TEXT).setReadOnly(true).setRequired(true).setWidth("100").createField())
                            .addField(new FieldBuilder("SpousteciUdalost").setTitle("Spouštěcí událost").setMaxOccurrences(1).setType(TEXT).setLength(2000).setRequired(true).createField())
                        .createField()) // SkartacniRezim
                        .addField(new FieldBuilder("DataceVyrazeni").setMaxOccurrences(1).setRequired(true)
                            .addField(new FieldBuilder("RokSpousteciUdalosti").setTitle("Rok spouštěcí události").setMaxOccurrences(1).setType(G_YEAR).setRequired(true).createField())
                            .addField(new FieldBuilder("RokSkartacniOperace").setTitle("Rok skartační operace").setMaxOccurrences(1).setType(G_YEAR).setRequired(true).createField())
                        .createField()) // DataceVyrazeni
                    .createField()) // Vyrazovani

                    .addField(new FieldBuilder("Manipulace").setMaxOccurrences(1).setWidth("200").setRequired(true)
                        .addField(new FieldBuilder("NezbytnyDokument").setTitle("Nezbytný dokument").setMaxOccurrences(1).setType(RADIOGROUP).setRequired(true)
                            .addMapValue("ANO", "Ano").addMapValue("NE", "Ne")
                        .createField()) // NezbytnyDokument
                        .addField(new FieldBuilder("AnalogovyDokument").setTitle("Analogová forma dokumentu").setMaxOccurrences(1).setType(RADIOGROUP).setRequired(true)
                            .addMapValue("ANO", "Ano").addMapValue("NE", "Ne")
                        .createField()) // AnalogovyDokument
                    .createField()) // Manipulace

                .createField()) // EvidencniUdaje

            .createField()); // Dokument
        return f;
    }

    public static Form externalDocumentForm() {
        Form f = new Form();
        f.getFields().add(new FieldBuilder("Dokument").setMaxOccurrences(1)

                .addField(new FieldBuilder("EvidencniUdaje").setMaxOccurrences(1)
                    .addField(new FieldBuilder("Identifikace").setMaxOccurrences(1)
                        .addField(createIdentifikatorField("Identifikator", "Identifikace dokumentu v evidenci"))
                    .createField()) // Identifikace

                    .addField(new FieldBuilder("Popis").setMaxOccurrences(1).setWidth("400")
                        .addField(new FieldBuilder("Nazev").setTitle("Věc").setMaxOccurrences(1).setType(TEXT).setLength(100).setRequired(true).createField())
                        .addField(new FieldBuilder("Komentar").setTitle("Komentář").setMaxOccurrences(1).setType(TEXTAREA).setLength(255).createField())
                        .addField(new FieldBuilder("KlicovaSlova").setMaxOccurrences(1)
                            .addField(new FieldBuilder("KlicoveSlovo").setTitle("Klíčová slova").setMaxOccurrences(10).setType(TEXT).setLength(100).createField())
                        .createField()) // KlicovaSlova
                    .createField()) // Popis

                    .addField(new FieldBuilder("Evidence").setMaxOccurrences(1)
                        .addField(new FieldBuilder("EvidencniCislo").setTitle("Číslo jednací").setMaxOccurrences(1).setType(TEXT).setLength(50).setRequired(true).createField())
                        .addField(new FieldBuilder("UrceneCasoveObdobi").setMaxOccurrences(1)
                            .addField(new FieldBuilder("Rok").setTitle("Časové období evidence").setMaxOccurrences(1).setType(G_YEAR).setRequired(true).createField())
                        .createField()) // UrceneCasoveObdobi
                        .addField(new FieldBuilder("NazevEvidenceDokumentu").setTitle("Název evidence").setMaxOccurrences(1).setType(TEXT).setLength(100).setRequired(true).createField())
                    .createField()) // Evidence

                    .addField(new FieldBuilder("Jazyky").setMaxOccurrences(1)
                        .addField(new FieldBuilder("Jazyk").setTitle("Jazyk").setMaxOccurrences(1).setType(TEXT).setLength(3).createField())
                    .createField()) // Jazyky

                    .addField(new FieldBuilder("Puvod").setMaxOccurrences(1)
                        .addField(new FieldBuilder("DorucenyDokument").setMaxOccurrences(1)
                            .addField(new FieldBuilder("DatumDoruceni").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setTitle("Datum doručení").setMaxOccurrences(1).setType(DATE).setRequired(true).createField())
                            .createField()) // DatumVytvoreni
                            // Odesilatel/tOsobaExterni/Subjekt:tSubjektExterni/
                            .addField(new FieldBuilder("Odesilatel").setMaxOccurrences(1)
                                .addField(new FieldBuilder("Subjekt").setTitle("Odesílatel").setMaxOccurrences(1).setType(CUSTOM_FORM).setRequired(true)
                                    .addField(new FieldBuilder("subjectType").setTitle("Typ příjemce").setMaxOccurrences(1).setType(RADIOGROUP)
                                        .addMapValue("PravnickaOsoba", "Právnická osoba").addMapValue("FyzickaOsoba", "Fyzická osoba")
                                    .createField())
                                    .addField(createIdentifikatorField("IdentifikatorOrganizace", "IČO organizace"))
                                    .addField(new FieldBuilder("NazevOrganizace").setTitle("Název organizace").setMaxOccurrences(1).setType(TEXT).setLength(100).createField())
                                    .addField(new FieldBuilder("SidloOrganizace").setTitle("Adresa organizace").setMaxOccurrences(1).setType(TEXT).setLength(2000).setRequired(true).createField())
                                    .addField(createIdentifikatorField("IdentifikatorFyzickeOsoby", "Ev. číslo osoby", false))
                                    .addField(new FieldBuilder("NazevFyzickeOsoby").setTitle("Jméno a příjmení osoby").setMaxOccurrences(1).setType(TEXT).setLength(100).createField())
                                    .addField(new FieldBuilder("OrganizacniUtvar").setTitle("Organizační útvar").setMaxOccurrences(1).setType(TEXT).setLength(100).createField())
                                    .addField(new FieldBuilder("PracovniPozice").setTitle("Pracovní pozice").setMaxOccurrences(1).setType(TEXT).setLength(100).createField())
                                    .addField(new FieldBuilder("ElektronickyKontakt").setTitle("Email").setMaxOccurrences(1).setType(TEXT).setLength(100).createField())
                                    // choice 2
                                    .addField(new FieldBuilder("PostovniAdresa").setTitle("Adresa").setMaxOccurrences(1).setType(TEXT).setLength(2000).setRequired(true).createField())
                                .createField())

                            .createField()) // Odesilatel
                        .createField()) // VlastniDokument
                    .createField()) // Puvod

                    .addField(new FieldBuilder("Trideni").setTitle("Věcná skupina").setMaxOccurrences(1).setRequired(true)
                        .addField(new FieldBuilder("JednoduchySpisovyZnak").setTitle("Kód").setMaxOccurrences(1).setType(SELECT).setLength(50).setRequired(true)
                            .setOptionDataSource(new FieldBuilder("desa-des.rec-cl").setWidth("400")
                                    .addField(new FieldBuilder("fullyQcc").setTitle("Kód").createField())
                                    .addField(new FieldBuilder("title").setTitle("Název").createField())
                                .createField(), // desa-des.rec-cl
//                                "fullyQcc", new HashMap<String, String>() {{put("fullyQcc", "JednoduchySpisovyZnak"); put("title", "PlneUrcenySpisovyZnak");}})
                                "classCode", new HashMap<String, String>() {{put("classCode", "JednoduchySpisovyZnak"); put("fullyQcc", "PlneUrcenySpisovyZnak");}})
                        .createField()) // JednoduchySpisovyZnak
                        .addField(new FieldBuilder("PlneUrcenySpisovyZnak").setTitle("Plný kód").setMaxOccurrences(1).setType(TEXT).setLength(255).setRequired(true).createField())
                        .addField(new FieldBuilder("TypDokumentu").setMaxOccurrences(1).setRequired(true)
                            .addField(new FieldBuilder("Identifikator").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setTitle("Typ dokumentu").setMaxOccurrences(1).setType(SELECT).setRequired(true).setLength(50).setWidth("400")
                                    .setOptionDataSource(new FieldBuilder("desa-des.rec-type")
                                        .addField(new FieldBuilder("title").createField())
                                    .createField(), "title")
                                .createField()) // value
                            .createField()) // Identifikator
                        .createField()) // TypDokumentu
                    .createField()) // Trideni

                    .addField(new FieldBuilder("Vyrizeni").setTitle("Vyřizuje").setMaxOccurrences(1).setRequired(true)
                        .addField(new FieldBuilder("Datum").setMaxOccurrences(1)
                            .addField(new FieldBuilder("value").setTitle("Datum vyřízení").setMaxOccurrences(1).setType(DATE).setRequired(true).createField())
                        .createField()) // Datum
                        .addField(new FieldBuilder("Zpusob").setTitle("Způsob vyřízení").setMaxOccurrences(1).setType(COMBO)
                            .addMapValue("VYŘÍZENÍ_DOKUMENTEM", "vyřízení dokumentem").addMapValue("POSTOUPENÍ", "postoupení").addMapValue("VZETÍ_NA_VĚDOMÍ", "vzetí na vědomí").addMapValue("JINÝ_ZPŮSOB", "jiný způsob")
                            .setRequired(true)
                        .createField()) // Zpusob
                        .addField(new FieldBuilder("Oduvodneni").setTitle("Odůvodnění vyřízení").setMaxOccurrences(1).setType(TEXT).setLength(100).createField())
                        .addField(new FieldBuilder("Zpracovatel").setMaxOccurrences(1)
                            .addField(createSubjectOsobyInterniField("Zpracovatelé")) // Subjekt
                        .createField()) // Zpracovatel
                    .createField()) // Vyrizeni

                    .addField(new FieldBuilder("Vyrazovani").setTitle("Skartační režim").setMaxOccurrences(1).setRequired(true)
                        .addField(new FieldBuilder("SkartacniRezim").setMaxOccurrences(1).setRequired(true)
                            .addField(new FieldBuilder("Nazev").setTitle("Název").setMaxOccurrences(1).setType(SELECT).setWidth("400").setLength(100).setRequired(true)
                                .setOptionDataSource(new FieldBuilder("desa-des.rd-cntrl")
                                        .addField(new FieldBuilder("acr").setTitle("Kód").createField())
                                        .addField(new FieldBuilder("title").setTitle("Název").createField())
                                    .createField(),
                                    "title",
                                    new HashMap<String, String>() {{
                                        put("acr", "Identifikator/value");
                                        put("title", "Nazev");
                                        put("reas", "Oduvodneni");
                                        put("action", "SkartacniZnak");
                                        put("period", "SkartacniLhuta");
                                    }})
                            .createField()) // Nazev
                            .addField(new FieldBuilder("Identifikator").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setTitle("Identifikátor skartačního režimu").setMaxOccurrences(1).setType(TEXT).setReadOnly(true).setWidth("400").createField())
                            .createField()) // Identifikator
                            .addField(new FieldBuilder("Oduvodneni").setTitle("Odůvodnění skartačního režimu").setMaxOccurrences(1).setType(TEXT).setReadOnly(true).setRequired(true).createField())
                            .addField(new FieldBuilder("SkartacniZnak").setTitle("Skartační znak").setMaxOccurrences(1).setType(SELECT).setReadOnly(true).setRequired(true)
                                .addMapValue("A", "A - Archiv").addMapValue("S", "S - Stoupa").addMapValue("V", "V - Výběr")
                            .createField()) // SkartacniZnak
                            .addField(new FieldBuilder("SkartacniLhuta").setTitle("Skartační lhůta").setMaxOccurrences(1).setType(TEXT).setReadOnly(true).setRequired(true).setWidth("100").createField())
                            .addField(new FieldBuilder("SpousteciUdalost").setTitle("Spouštěcí událost").setMaxOccurrences(1).setType(TEXT).setLength(2000).setRequired(true).createField())
                        .createField()) // SkartacniRezim
                        .addField(new FieldBuilder("DataceVyrazeni").setMaxOccurrences(1).setRequired(true)
                            .addField(new FieldBuilder("RokSpousteciUdalosti").setTitle("Rok spouštěcí události").setMaxOccurrences(1).setType(G_YEAR).setRequired(true).createField())
                            .addField(new FieldBuilder("RokSkartacniOperace").setTitle("Rok skartační operace").setMaxOccurrences(1).setType(G_YEAR).setRequired(true).createField())
                        .createField()) // DataceVyrazeni
                    .createField()) // Vyrazovani

                    .addField(new FieldBuilder("Manipulace").setMaxOccurrences(1).setWidth("200").setRequired(true)
                        .addField(new FieldBuilder("NezbytnyDokument").setTitle("Nezbytný dokument").setMaxOccurrences(1).setType(RADIOGROUP).setRequired(true)
                            .addMapValue("ANO", "Ano").addMapValue("NE", "Ne")
                        .createField()) // NezbytnyDokument
                        .addField(new FieldBuilder("AnalogovyDokument").setTitle("Analogová forma dokumentu").setMaxOccurrences(1).setType(RADIOGROUP).setRequired(true)
                            .addMapValue("ANO", "Ano").addMapValue("NE", "Ne")
                        .createField()) // AnalogovyDokument
                    .createField()) // Manipulace

                .createField()) // EvidencniUdaje

            .createField()); // Dokument
        return f;
    }

    private static Field createIdentifikatorField(String elmName, String title) {
        return createIdentifikatorField(elmName, title, true);
    }

    private static Field createIdentifikatorField(String elmName, String title, Boolean required) {
        return new FieldBuilder(elmName).setMaxOccurrences(1)
                .addField(new FieldBuilder("value").setTitle(title).setMaxOccurrences(1).setType(TEXT).setRequired(required).setLength(50).setWidth("400").createField())
            .createField();
    }

    private static Field createSubjectOsobyInterniField(String title) {
        return new FieldBuilder("Subjekt").setTitle(title).setMaxOccurrences(10).setRequired(true)
                .addField(createIdentifikatorField("IdentifikatorOrganizace", "IČO organizace"))
                .addField(new FieldBuilder("NazevOrganizace").setTitle("Název organizace").setMaxOccurrences(1).setType(TEXT).setLength(100).setRequired(true).createField())
                .addField(createIdentifikatorField("IdentifikatorFyzickeOsoby", "Ev. číslo osoby"))
                .addField(new FieldBuilder("NazevFyzickeOsoby").setTitle("Jméno a příjmení osoby").setMaxOccurrences(1).setType(TEXT).setLength(100).setRequired(true).createField())
                .addField(new FieldBuilder("OrganizacniUtvar").setTitle("Organizační útvar").setMaxOccurrences(1).setType(TEXT).setLength(100).setRequired(true).createField())
                .addField(new FieldBuilder("PracovniPozice").setTitle("Pracovní pozice").setMaxOccurrences(1).setType(TEXT).setLength(100).setRequired(true).createField())
                .addField(new FieldBuilder("SidloOrganizace").setTitle("Adresa organizace").setMaxOccurrences(1).setType(TEXT).setLength(2000).setRequired(true).createField())
            .createField();
    }

}
