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
package cz.cas.lib.proarc.webapp.client.widget.mods;

import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.List;

/**
 * The NDK Picture.
 *
 * Version 1.5
*
 * @author Jan Pokorsky
 */
public final class NdkPictureForm {

    public Form build() {
        Form f = new Form();

        f.getFields().add(new FieldBuilder("mods").setMaxOccurrences(1).createField()); // mods
        List<Field> modsFields = f.getFields().get(0).getFields();

        // titleInfo, titleInfoDefinition
        modsFields.add(new FieldBuilder("titleInfo").setTitle("Title Info - M").setMaxOccurrences(10)
                .setHint("Názvová informace vnitřní části.")
                // titleInfo@type, enum
                .addField(new FieldBuilder("type").setTitle("Type - MA").setMaxOccurrences(1).setType(Field.SELECT)
                    .setHint("Pokud jde o hlavní název, pak nechat tuto hodnotu prázdnou.<dl>Jinak použít jednu z hodnot:"
                        + "<dt>abbreviated</dt><dd>zkrácený název</dd>"
                        + "<dt>alternative</dt><dd>alternativní název</dd>"
                        + "<dt>translated</dt><dd>přeložený název</dd>"
                        + "<dt>uniform</dt><dd>stejný/jednotný název</dd>"
                        + "</dl>")
                    .addMapValue("abbreviated", "Abbreviated")
                    .addMapValue("alternative", "Alternative")
                    .addMapValue("translated", "Translated")
                    .addMapValue("uniform", "Uniform")
                .createField()) // type
                // title, type="stringPlusLanguage"
                .addField(new FieldBuilder("title").setMaxOccurrences(1)
                    .addField(new FieldBuilder("value").setTitle("Title - M").setMaxOccurrences(1).setType(Field.COMBO).setRequired(true).setWidth(f.getItemWidth())
                        .setHint("Vlastní název obrazu."
                            + "<p>Pokud není titul, nutno vyplnit hodnotu „untitled“")
                        .addMapValue("untitled", "untitled")
                    .createField()) // title/value
                    // lang, String
                    // xmlLang, lang
                    // script, String
                    // transliteration, String
                .createField()) // title
                // subTitle, type="stringPlusLanguage"
                .addField(new FieldBuilder("subTitle").setMaxOccurrences(1)
                    .addField(new FieldBuilder("value").setTitle("Subtitle - MA").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Podnázev článku. Za podnázev lze považovat i perex.")
                    .createField()) // value
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                .createField()) // subTitle
                // partNumber, type="stringPlusLanguage"
                .addField(new FieldBuilder("partNumber").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("Part Number - RA").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Číslo vnitřní části.")
                    .createField()) // value
                .createField()) // partNumber
                // partName, type="stringPlusLanguage"
                .addField(new FieldBuilder("partName").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("Part Name - RA").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Název vnitřní části.")
                    .createField()) // value
                .createField()) // partName
                // nonSort, type="stringPlusLanguage"
                // titleInfo@attributes: otherType, supplied, altRepGroup, altFormatAttributeGroup, nameTitleGroup, usage, ID, authorityAttributeGroup, xlink:simpleLink, languageAttributeGroup, displayLabel
            .createField()); // titleInfo

        // name, nameDefinition
        modsFields.add(new FieldBuilder("name").setMaxOccurrences(10).setTitle("Name - MA")
                .setHint("Údaje o odpovědnosti za vnitřní část.")
                // @ID, @authorityAttributeGroup, @xlinkSimpleLink, @languageAttributeGroup, @displayLabel, @altRepGroup, @nameTitleGroup
                // @type(personal, corporate, conference, family)
                .addField(new FieldBuilder("type").setTitle("Type - R").setMaxOccurrences(1).setType(Field.SELECT)
                    .setHint("<dl>"
                        + "<dt>personal</dt><dd>celé jméno osoby</dd>"
                        + "<dt>corporate</dt><dd>název společnosti, instituce nebo organizace</dd>"
                        + "<dt>conference</dt><dd>název konference nebo související typ setkání</dd>"
                        + "<dt>family</dt><dd>rodina/rod</dd>"
                        + "</dl>")
                    .addMapValue("personal", "personal")
                    .addMapValue("corporate", "corporate")
                    .addMapValue("conference", "conference")
                    .addMapValue("family", "family")
                .createField()) // @type
                // @usage(fixed="primary")
                .addField(new FieldBuilder("usage").setTitle("Usage - O").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("")
                    .setHint("Hodnota “primary” pro označení primární autority.")
                    .addMapValue("", "")
                    .addMapValue("primary", "primary")
                .createField()) // usage
                // namePart, namePartDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("namePart").setTitle("Name Parts - MA").setMaxOccurrences(5)
                    // @type(date, family, given, termsOfAddress)
                    .addField(new FieldBuilder("type").setTitle("Type - MA").setMaxOccurrences(1).setType(Field.SELECT)
                        .setHint("<dl>"
                            + "<dt>date</dt><dd>RA - datum</dd>"
                            + "<dt>family</dt><dd>MA -příjmení </dd>"
                            + "<dt>given</dt><dd>MA - jméno/křestní jméno</dd>"
                            + "<dt>termsOfAddress</dt><dd>RA - tituly a jiná slova nebo čísla související se jménem</dd>"
                            + "</dl>")
                        .addMapValue("date", "date")
                        .addMapValue("family", "family")
                        .addMapValue("given", "given")
                        .addMapValue("termsOfAddress", "termsOfAddress")
                    .createField()) // @type
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("Name Part - MA").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Údaje o křestním jméně, příjmení apod."
                            + "<p>Nutno vyjádřit pro křestní jméno i příjmení."
                            + "<p>Pokud nelze rozlišit křestní jméno a příjmení,"
                            + " nepoužije se type a jméno se zaznamená"
                            + " v podobě jaké je do jednoho elementu &lt;namePart>")
                    .createField()) // value
                .createField()) // namePart
                // displayForm
                .addField(new FieldBuilder("etal").setMaxOccurrences(1)
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setTitle("Etal - O").setType(Field.TEXT)
                        .setHint("Element indikující, že existuje více autorů, než pouze ti, kteří byli uvedeni v <name> elementu." +
                            "<p>V případě užití tohoto elementu je dále top element <name> neopakovatelný." +
                            "<p><etal> je nutné umístit do samostatného top elementu <name>, ve kterém se " +
                            "nesmí objevit subelementy <namePart> a <nameIdentifier>." +
                            "<p><etal> je neopakovatelný element, který se do zápisu vkládá ručně.").createField())
                    .createField()) //etal
                .addField(new FieldBuilder("affiliation").setMaxOccurrences(1)
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setTitle("Affiliation - O").setType(Field.TEXT)
                        .setHint("Umožňuje vepsat název instituce, se kterou je autor, popsaný v elementu <name> spojen.").createField())
                    .createField())// affiliation
                // role, roleDefinition
                .addField(new FieldBuilder("nameIdentifier").setTitle("Name Identifier - MA").setMaxOccurrences(5)
                        .addField(new FieldBuilder("value").setMaxOccurrences(1)
                                .setType(Field.TEXT).setRequired(false).setHint("Číslo národní autority").createField())
                        .createField()) //nameIdentifier
                .addField(new FieldBuilder("role").setTitle("Role - RA").setMaxOccurrences(5)
                    .setHint("Specifikace role osoby nebo organizace uvedené v elementu &lt;name>")
                    // roleTerm, type="roleTermDefinition" extends stringPlusLanguagePlusAuthority
                    .addField(NdkForms.roleTerm(
                            "Role Term - MA", false, "Authority - M", false, "Type - M", false
                    )) // roleTerm
                .createField()) // role
                // description
            .createField()); // name

        // genre, genreDefinition extends stringPlusLanguagePlusAuthority extends stringPlusLanguage
        modsFields.add(new FieldBuilder("genre").setTitle("Genre - M").setMaxOccurrences(1)
                // genreDefinition@attributes: type, displayLabel, altRepGroup, usage
                .addField(new FieldBuilder("type").setTitle("Type - R").setMaxOccurrences(1).setType(Field.COMBO).setWidth("200")
                    .addMapValue("table", "tabulka")
                    .addMapValue("illustration", "ilustrace")
                    .addMapValue("chart", "graf")
                    .addMapValue("photograph", "fotografie")
                    .addMapValue("graphic", "grafika")
                    .addMapValue("map", "mapa")
                    .addMapValue("advertisement", "reklama")
                    .addMapValue("plate", "obrazová příloha")
                    .addMapValue("technicalPlanScheme", "technický plán/schéma")
//                    .addMapValue("cover", "obálka")
                    .addMapValue("unspecified", "nespecifikován")
                .createField()) // @type
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                .addField(new FieldBuilder("value").setTitle("Genre - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).setDefaultValue("picture")
                    .setHint("Bližší údaj o typu vnitřní části.<p>Hodnota „picture“.")
                .createField()) // value
        .createField()); // genre

        // language, languageDefinition
        modsFields.add(new FieldBuilder("language").setTitle("Languages - MA").setMaxOccurrences(10)
                .setHint("Údaje o jazyce dokumentu; v případě vícenásobného výskytu nutno element &lt;language> opakovat")
                // @objectPart, @displayLabel, @altRepGroup, @usage
                // languageAttributeGroup: @lang, @xmlLang, @script, @transliteration
                // languageTerm, languageTermDefinition
                .addField(new FieldBuilder("languageTerm").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @authorityURI, @valueURI
                    // @authority, enum
                    .addField(new FieldBuilder("authority").setTitle("Authority - M").setMaxOccurrences(1).setType(Field.SELECT).setRequired(true)
                        .setHint("Použít hodnotu „iso639-2b“.")
                        .addMapValue("iso639-2b", "ISO 639-2B")
                        .addMapValue("rfc3066", "RFC 3066")
                        .addMapValue("iso639-3", "ISO 639-3")
                        .addMapValue("rfc4646", "RFC 4646")
                        .addMapValue("rfc5646", "RFC 5646")
                    .createField()) // authority
                    // type, codeOrText('code', 'text')
                    .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.SELECT).setRequired(true)
                        .setHint("Typ popisu.")
                        .addMapValue("code", "code")
                        .addMapValue("text", "text")
                    .createField()) // type
                    .addField(NdkForms.createLangTermValue()
                    .createField()) // value
                .createField()) // languageTerm
                // scriptTerm
        .createField()); // language

        modsFields.add(
            // physicalDescription, physicalDescriptionDefinition
            new FieldBuilder("physicalDescription").setTitle("Physical Description - R").setMaxOccurrences(10)
                    .setHint("Obsahuje údaje o fyzickém popisu vnitřní části;" +
                            "<p>určeno spíše pro články než pro obrazy.")
                    // form, formDefinition extends stringPlusLanguagePlusAuthority
                    .addField(new FieldBuilder("form").setTitle("Form - R").setMaxOccurrences(1)
                            // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                            // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                            // @type
                            // XXX autofill "marcform"
                            .addField(new FieldBuilder("authority").setTitle("Authority - R").setMaxOccurrences(1).setType(Field.COMBO)
                                    .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCFORM, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCFORM)
                                    .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCSMD, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCSMD)
                                    .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_GMD, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_GMD)
                                    .createField()) // authority
                            .addField(new FieldBuilder("value").setTitle("Form - R").setMaxOccurrences(1).setType(Field.TEXT)
                                    .setHint("Údaje o fyzické podobě vnitřní části" +
                                            "<p>např. print, electronic apod.")
                                    .createField()) // value
                            .createField()) // form
                    // reformattingQuality
                    // internetMediaType
                    // digitalOrigin
                    // extent, stringPlusLanguagePlusSupplied
                    .createField()); // physicalDescription
        // physicalDescription, physicalDescriptionDefinition

        // abstract, abstractDefinition extends stringPlusLanguage
        modsFields.add(new FieldBuilder("abstract").setTitle("Abstract - R").setMaxOccurrences(10)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @xlink:simpleLink, @shareable, @altRepGroup
                // altFormatAttributeGroup: @altFormat, @contentType
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA).setLength(2700)
                    .setHint("Shrnutí obsahu vnitřní části.")
                .createField()) // value
        .createField()); // abstract

        // note, noteDefinition extends stringPlusLanguage
        modsFields.add(new FieldBuilder("note").setTitle("Note - RA").setMaxOccurrences(30)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @typeURI, @xlink:simpleLink, @ID, @altRepGroup
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                    .setHint("Obecná poznámka k vnitřní části. Do poznámky by se"
                        + " měla dávat šifra autora vnitřní části, která se vyskytuje pod vnitřní částí.")
                .createField()) // value
        .createField()); // note

        // subject, subjectDefinition
        modsFields.add(new FieldBuilder("subject").setTitle("Subject - R").setMaxOccurrences(30)
                .setHint("Údaje o věcném třídění.")
                // @ID, @authorityAttributeGroup, @languageAttributeGroup, @xlink:simpleLink, @displayLabel, @altRepGroup, @usage
                // autofill "czenas"
                .addField(new FieldBuilder("authority").setTitle("Authority - O").setMaxOccurrences(1).setType(Field.TEXT)
                    .addMapValue("czenas", "czenas")
                    .addMapValue("eczenas", "eczenas")
                    .addMapValue("mednas", "mednas")
                    .addMapValue("czmesh", "czmesh")
                    .addMapValue("msvkth", "msvkth")
                    .addMapValue("agrovoc", "agrovoc")
                    .addMapValue("Konspekt", "Konspekt")
                .createField()) // authority


                // topic, stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("topic").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @type
                    // XXX autority.nkp.cz datasource
                    .addField(new FieldBuilder("value").setTitle("Topic - M").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Libovolný výraz specifikující nebo charakterizující obsah článku."
                            + "<p>Použít kontrolovaný slovník - např. z báze autorit AUT NK ČR (věcné téma)"
                            + " nebo obsah pole 650 záznamu MARC21 nebo obsah pole 072 $x.")
                    .createField()) // value
                .createField()) // topic

                // geographic, stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("geographic").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @type
                    // XXX autority.nkp.cz datasource
                    .addField(new FieldBuilder("value").setTitle("Geographic - MA").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Geografické věcné třídění."
                            + "<p>Použít kontrolovaný slovník - např. z báze autorit AUT NK ČR (geografický termín)"
                            + " nebo obsah pole 651 záznamu MARC21.")
                    .createField()) // value
                .createField()) // geographic
                .addField(NdkForms.geographicCode()) // geographicCode

                // temporal, temporalDefinition extends dateDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("temporal").setMaxOccurrences(1)
                    // authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @encoding, @qualifier, @point, @keyDate
                    // XXX autority.nkp.cz datasource
                    .addField(new FieldBuilder("value").setTitle("Temporal - R").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200")
                        .setHint("Chronologické věcné třídění."
                            + "<p>Použít kontrolovaný slovník - např. z báze autorit AUT NK ČR (chronologický údaj)"
                            + " nebo obsah pole 648 záznamu MARC21.")
                    .createField()) // value
                .createField()) // temporal

                // titleInfo, subjectTitleInfoDefinition
                // name, subjectNameDefinition
                .addField(nameInSubject()
                        /*new FieldBuilder("name").setMaxOccurrences(1)
                    // @type, enum: personal, corporate, ...
                    // @ID, @xlink:simpleLink, displayLabel
                    // languageAttributeGroup: @lang, @xmlLang, @script, @transliteration
                    // authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // namePart, namePartDefinition extends stringPlusLanguage
                    .addField(new FieldBuilder("namePart").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @type, enum: date, family, given, termsOfAddress
                        .addField(new FieldBuilder("value").setTitle("Name Part - R").setMaxOccurrences(1).setType(Field.TEXT)
                            .setHint("Jméno použité jako věcné záhlaví."
                                + "<p>Použít kontrolovaný slovník ‐ např. z báze autorit AUT NK ČR (jméno osobní)"
                                + " nebo obsah pole 600 záznamu MARC21."
                                + "<p>Celé jméno se zapíše do tohoto elementu.")
                        .createField()) // value
                    .createField()) // namePart
                    // displayForm
                    // affiliation
                    // role
                    // description
                .createField()*/
                        ) // name

                // geographicCode
                // hierarchicalGeographic
                // cartographics, type="cartographicsDefinition"
                .addField(new FieldBuilder("cartographics").setTitle("Cartographics - R").setMaxOccurrences(1)
                    .setHint("Kartografické údaje.")
                    // @authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // scale, 0..1, type="stringPlusLanguage"
                    .addField(new FieldBuilder("scale").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Scale - R").setMaxOccurrences(1).setType(Field.TEXT)
                            .setHint("Měřítko.")
                        .createField()) // value
                    .createField()) // scale
                    // projection, 0..1, type="stringPlusLanguage"
                    // coordinates, 0..n, type="stringPlusLanguage"
                    .addField(new FieldBuilder("coordinates").setTitle("Coordinates - R").setMaxOccurrences(10)
                        .setHint("Souřadnice.")
                        .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT)
                            .setHint("Souřadnice.")
                        .createField()) // value
                    .createField()) // coordinates
                .createField()) // cartographics
                // occupation
                // genre
        .createField()); // subject

        // classification, classificationDefinition extends stringPlusLanguagePlusAuthority
        modsFields.add(new FieldBuilder("classification").setTitle("Classification - RA").setMaxOccurrences(10)
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                // autofill "udc"
                .addField(new FieldBuilder("authority").setTitle("Authority - M").setMaxOccurrences(1).setType(Field.COMBO)
                        .addMapValue("udc", "udc")
                        .addMapValue("Konspekt", "Konspekt")
                .createField()) // authority
                .addField(new FieldBuilder("edition").setTitle("Edition - M").setMaxOccurrences(1).setType(Field.COMBO)
                        .addMapValue("", "")
                        .addMapValue("Konspekt", "Konspekt")
                .createField()) // edition
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT)
                    .setHint("Klasifikační údaje věcného třídění podle Mezinárodního"
                        + " desetinného třídění. Odpovídá poli 080 MARC21."
                        + "<p>Klasifikační údaje věcného třídění podle Konspektu."
                        + " Odpovídá poli 072 $a MARC21.")
                .createField()) // value
        .createField()); // classification

        // identifier, identifierDefinition, [0,*]
        modsFields.add(new FieldBuilder("identifier").setTitle("Identifier - M").setMaxOccurrences(10)
                .setHint("Údaje o identifikátorech.<p>Obsahuje unikátní identifikátory"
                    + " mezinárodní nebo lokální."
                    + "<p>Uvádějí se i neplatné resp. zrušené identifikátory - atribut invalid=“yes“.")
                // stringPlusLanguage@languageAttributeGroup
                //   lang, xs:string
                //   xml:lang
                //   script, xs:string
                //   transliteration, xs:string
                //   type, xs:string
                .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.COMBO).setRequired(true)
                    .setHint("UUID - M - vygeneruje dodavatel"
                            + "<br>URN:NBN - O - zápis ve tvaru urn:nbn:cz:ndk-123456 pro projekt NDK"
                            + "<br>jiný interní identifikátor - R - type = barcode, oclc, sysno, permalink apod.")
                    .addMapValue("barcode", "Čárový kód")
//                    .addMapValue("ccnb", "čČNB")
                    .addMapValue("doi", "DOI")
                    .addMapValue("hdl", "Handle")
//                    .addMapValue("isbn", "ISBN")
//                    .addMapValue("issn", "ISSN")
                    .addMapValue("permalink", "Permalink")
                    .addMapValue("sici", "SICI")
                    .addMapValue("url", "URL")
                    .addMapValue("urnnbn", "URN:NBN")
                    .addMapValue("uuid", "UUID")
                    .addMapValue("oclc", "OCLC")
                .createField())
                // stringPlusLanguage/value
                .addField(new FieldBuilder("value").setTitle("Identifier - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).createField())
                // identifierDefinition
                //   displayLabel, xs:string
                //   typeURI, xs:anyURI
                //   invalid, fixed="yes"
                .addField(new FieldBuilder("invalid").setTitle("Invalid - MA").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("")
                    .addMapValue("", "Platný")
                    .addMapValue("yes", "Neplatný")
                .createField()) // invalid
                //   altRepGroup, xs:string
        .createField()); // identifier

        // recordInfo, recordInfoDefinition
        modsFields.add(new FieldBuilder("recordInfo").setTitle("Record Info - M").setMaxOccurrences(1)
                .setHint("Údaje o metadatovém záznamu - jeho vzniku, změnách apod.")
                // languageAttributeGroup: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @altRepGroup
                // recordContentSource, stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("recordContentSource").setTitle("Record Content Source - R").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    .addField(new FieldBuilder("authority").setTitle("Authority - R").setMaxOccurrences(1).setType(Field.TEXT).setDefaultValue("marcorg").createField())
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Kód nebo jméno instituce, která záznam vytvořila nebo změnila.")
                    .createField()) // value
                .createField()) // recordContentSource
                // recordCreationDate, dateDefinition
                .addField(new FieldBuilder("recordCreationDate").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // @encoding, @qualifier, @point, @keyDate
                    .addField(new FieldBuilder("encoding").setMaxOccurrences(1).setHidden(true).setType(Field.TEXT).createField())
                    .addField(new FieldBuilder("value").setTitle("Record Creation Date - M").setMaxOccurrences(1).setReadOnly(true).setType(Field.TEXT).createField())
                .createField()) // recordCreationDate
                // recordChangeDate, dateDefinition
                .addField(new FieldBuilder("recordChangeDate").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // @encoding, @qualifier, @point, @keyDate
                    .addField(new FieldBuilder("encoding").setMaxOccurrences(1).setHidden(true).setType(Field.TEXT).createField())
                    .addField(new FieldBuilder("value").setTitle("Record Change Date - R").setMaxOccurrences(1).setReadOnly(true).setType(Field.TEXT).createField())
                .createField()) // recordChangeDate
                // recordIdentifier
                // languageOfCataloging
                // recordOrigin, extends stringPlusLanguage
                .addField(new FieldBuilder("recordOrigin").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("Record Origin - R").setMaxOccurrences(1).setType(Field.COMBO).setWidth("200")
                        .setHint("Údaje o vzniku záznamu.")
                        .addMapValue("machine generated", "machine generated")
                        .addMapValue("human prepared", "human prepared")
                    .createField()) // value
                .createField()) // recordOrigin
                .addField(new FieldBuilder("recordInfoNote").setMaxOccurrences(1)
                    .addField(new FieldBuilder("value").setTitle("Record Info Note - O").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200")
                        .setHint("Poznámka k záznamu").createField())
                    .createField()) //recordInfoNote
                // descriptionStandard
        .createField()); // recordInfo

        return f;
    }

    private Field nameInSubject() {
        // name, nameDefinition
        return new FieldBuilder("name").setMaxOccurrences(10).setTitle("Name - R")
                .setHint("Údaje o odpovědnosti za svazek."
                        + "<p>Pokud má monografie autora a ilustrátora, element &lt;name> se opakuje s různými rolemi.")
                // @ID, @authorityAttributeGroup, @xlinkSimpleLink, @languageAttributeGroup, @displayLabel, @altRepGroup, @nameTitleGroup
                // @type(personal, corporate, conference, family)
                .addField(new FieldBuilder("type").setTitle("Type - R").setMaxOccurrences(1).setType(Field.SELECT)
                        // issue: 612 not required
                        .setRequired(false)
                        .setHint("<dl>"
                                + "<dt>personal</dt><dd>celé jméno osoby</dd>"
                                + "<dt>corporate</dt><dd>název společnosti, instituce nebo organizace</dd>"
                                + "<dt>conference</dt><dd>název konference nebo související typ setkání</dd>"
                                + "<dt>family</dt><dd>rodina/rod</dd>"
                                + "</dl>")
                        .addMapValue("personal", "personal")
                        .addMapValue("corporate", "corporate")
                        .addMapValue("conference", "conference")
                        .addMapValue("family", "family")
                        .createField()) // @type
                // @usage(fixed="primary")
                .addField(new FieldBuilder("usage").setTitle("Usage - O").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("")
                        .setHint("Hodnota “primary” pro označení primární autority.")
                        .addMapValue("", "")
                        .addMapValue("primary", "primary")
                        .createField()) // usage
                // namePart, namePartDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("namePart").setTitle("Name Parts - R").setMaxOccurrences(5)
                        // @type(date, family, given, termsOfAddress)
                        .addField(new FieldBuilder("type").setTitle("Type - R").setMaxOccurrences(1).setType(Field.SELECT)
                                .setHint("<dl>"
                                        + "<dt>date</dt><dd>RA - datum</dd>"
                                        + "<dt>family</dt><dd>MA -příjmení </dd>"
                                        + "<dt>given</dt><dd>MA - jméno/křestní jméno</dd>"
                                        + "<dt>termsOfAddress</dt><dd>RA - tituly a jiná slova nebo čísla související se jménem</dd>"
                                        + "</dl>")
                                .addMapValue("date", "date")
                                .addMapValue("family", "family")
                                .addMapValue("given", "given")
                                .addMapValue("termsOfAddress", "termsOfAddress")
                                .createField()) // @type
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Name Part - R").setMaxOccurrences(1)
                                .setType(Field.TEXT)
                                // issue: 612 not required
                                .setRequired(false)
                                .setHint("Údaje o křestním jméně, příjmení apod."
                                        + "<p>Nutno vyjádřit pro křestní jméno i příjmení."
                                        + "<p>Pokud nelze rozlišit křestní jméno a příjmení,"
                                        + " nepoužije se type a jméno se zaznamená"
                                        + " v podobě jaké je do jednoho elementu &lt;namePart>"
                                        + "<p>Pokud známe datum narození a úmrtí autora, vyplnit"
                                        + " ve tvaru RRRR-RRRR s atributem type=”date”.")
                                .createField()) // value
                        .createField()) // namePart
                // displayForm
                .addField(new FieldBuilder("etal").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setMaxOccurrences(1).setTitle("Etal - O").setType(Field.TEXT)
                                .setHint("Element indikující, že existuje více autorů, než pouze ti, kteří byli uvedeni v <name> elementu." +
                                        "<p>V případě užití tohoto elementu je dále top element <name> neopakovatelný." +
                                        "<p><etal> je nutné umístit do samostatného top elementu <name>, ve kterém se " +
                                        "nesmí objevit subelementy <namePart> a <nameIdentifier>." +
                                        "<p><etal> je neopakovatelný element, který se do zápisu vkládá ručně.").createField())
                        .createField()) //etal
                // affiliation
                // role, roleDefinition
                .addField(new FieldBuilder("nameIdentifier").setTitle("Name Identifier - MA").setMaxOccurrences(5)
                        .addField(new FieldBuilder("value").setMaxOccurrences(1)
                                .setType(Field.TEXT).setRequired(false).setHint("Číslo národní autority").createField())
                        .createField()) //nameIdentifier
                .addField(new FieldBuilder("role").setTitle("Role - R").setMaxOccurrences(5)
                        .setHint("Specifikace role osoby nebo organizace uvedené v elementu &lt;name>")
                        // roleTerm, type="roleTermDefinition" extends stringPlusLanguagePlusAuthority
                        // issue: 612 not required
                        .addField(NdkForms.roleTerm(
                                "Role Term - R", false, "Authority - R", false, "Type - M", false
                        )) // roleTerm
                        .createField()) // role
                // description
                .createField(); // name
    }
}
