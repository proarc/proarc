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

import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.List;

/**
 * The NDK Monograph Chapter.
 *
 * Version 1.1_2
 *
 * @author Jan Pokorsky
 */
public final class NdkChapterForm {

    public Form build() {
        Form f = new Form();

        f.getFields().add(new FieldBuilder("mods").setMaxOccurrences(1).createField()); // mods
        List<Field> modsFields = f.getFields().get(0).getFields();

        // titleInfo, titleInfoDefinition
        modsFields.add(new FieldBuilder("titleInfo").setTitle("Title Info - M").setMaxOccurrences(10)
                .setHint("Názvová informace vnitřní části.")
                // titleInfo@type, enum
                .addField(new FieldBuilder("type").setTitle("Type - MA").setMaxOccurrences(1).setType(Field.SELECT)
                    .setHint("Hlavní název bez type.<dl>Hodnoty:"
                        + "<dt>abbreviated</dt><dd>zkrácený název</dd>"
                        + "<dt>alternative</dt><dd>alternativní název</dd>"
                        + "<dt>translated</dt><dd>přeložený název</dd>"
                        + "<dt>uniform</dt><dd>stejný/jednotný název</dd>"
                        + "</dl>")
                    .addMapValue("abbreviated", "Abbreviated")
                    .addMapValue("alternative", "Alternative")
                    .addMapValue("translated", "Translated")
                    .addMapValue("uniform", "Uniform")
                .createField())
                // title, type="stringPlusLanguage"
                .addField(new FieldBuilder("title").setMaxOccurrences(1)
                    .addField(new FieldBuilder("value").setTitle("Title - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                        .setHint("Název kapitoly."
                            + "<p>Pokud není titul, nutno vyplnit hodnotu „untitled“")
                        .addMapValue("untitled", "Bez názvu")
                    .createField()) // title/value
                    // lang, String
                    // xmlLang, lang
                    // script, String
                    // transliteration, String
                .createField()) // title
                // subTitle, type="stringPlusLanguage"
                .addField(new FieldBuilder("subTitle").setMaxOccurrences(1)
                    .addField(new FieldBuilder("value").setTitle("Subtitle - MA").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Podnázev kapitoly.")
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
                .setHint("Údaje o odpovědnosti za kapitolu.")
                // @ID, @authorityAttributeGroup, @xlinkSimpleLink, @languageAttributeGroup, @displayLabel, @altRepGroup, @nameTitleGroup
                // @usage(fixed="primary")
                // @type(personal, corporate, conference, family)
                .addField(new FieldBuilder("type").setTitle("Type - MA").setMaxOccurrences(1).setType(Field.SELECT)
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
                    .addField(new FieldBuilder("value").setTitle("Name Part - MA").setMaxOccurrences(1)
                        .setType(Field.TEXT).setRequired(true)
                        .setHint("Údaje o křestním jméně, příjmení apod."
                            + "<p>Nutno vyjádřit pro křestní jméno i příjmení."
                            + "<p>Pokud nelze rozlišit křestní jméno a příjmení,"
                            + " nepoužije se type a jméno se zaznamená"
                            + " v podobě jaké je do jednoho elementu &lt;namePart>.")
                    .createField()) // value
                .createField()) // namePart
                // displayForm
                // etal
                // affiliation
                // role, roleDefinition
                .addField(new FieldBuilder("role").setTitle("Role - MA").setMaxOccurrences(5)
                    .setHint("Specifikace role osoby nebo organizace uvedené v elementu &lt;name>")
                    // roleTerm, type="roleTermDefinition" extends stringPlusLanguagePlusAuthority
                    .addField(NdkForms.roleTerm(
                            "Role Term - MA", true, "Authority - M", true, "Type - M", true
                    )) // roleTerm
                .createField()) // role
                // description
            .createField()); // name

        // genre, genreDefinition extends stringPlusLanguagePlusAuthority extends stringPlusLanguage
        modsFields.add(new FieldBuilder("genre").setTitle("Genre - M").setMaxOccurrences(1)
                // genreDefinition@attributes: type, displayLabel, altRepGroup, usage
                .addField(new FieldBuilder("type").setTitle("Type - R").setMaxOccurrences(1).setType(Field.COMBO).setWidth("200")
                    .addMapValue("table of content", "obsah")
                    .addMapValue("advertisement", "reklama")
                    .addMapValue("abstract", "abstrakt")
                    .addMapValue("introduction", "úvod")
                    .addMapValue("review", "recenze")
                    .addMapValue("dedication", "věnování")
                    .addMapValue("bibliography", "bibliografie")
                    .addMapValue("editorsNote", "úvodník")
                    .addMapValue("preface", "předmluva")
                    .addMapValue("chapter", "kapitola")
                    .addMapValue("article", "článek")
                    .addMapValue("index", "rejstřík")
                    .addMapValue("unspecified", "nespecifikován")
                .createField()) // @type
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                .addField(new FieldBuilder("value").setTitle("Genre - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).setDefaultValue("chapter")
                    .setHint("Bližší údaj o typu vnitřní části.<p>Hodnota „chapter“.")
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
                    .addField(new FieldBuilder("authority").setTitle("Authority - M").setMaxOccurrences(1)
                        .setType(Field.SELECT).setRequired(true)
                        .setHint("Použít hodnotu „iso639-2b“.")
                        .addMapValue("iso639-2b", "ISO 639-2B")
                        .addMapValue("rfc3066", "RFC 3066")
                        .addMapValue("iso639-3", "ISO 639-3")
                        .addMapValue("rfc4646", "RFC 4646")
                        .addMapValue("rfc5646", "RFC 5646")
                    .createField()) // authority
                    // type, codeOrText('code', 'text')
                    .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1)
                        .setType(Field.SELECT).setRequired(true)
                        .setHint("Typ popisu.")
                        .addMapValue("code", "code")
                        .addMapValue("text", "text")
                    .createField()) // type
                    .addField(NdkForms.createLangTermValue()
                    .createField()) // value
                .createField()) // languageTerm
                // scriptTerm
        .createField()); // language

        // physicalDescription, physicalDescriptionDefinition
        modsFields.add(new FieldBuilder("physicalDescription").setTitle("Physical Description - R").setMaxOccurrences(10)
                .setHint("Obsahuje údaje o fyzickém popisu článku.")
                // form, formDefinition extends stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("form").setTitle("Form - R").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @type, string
                    // XXX autofill "marcform"
                    .addField(new FieldBuilder("authority").setTitle("Authority - R").setMaxOccurrences(1).setType(Field.COMBO).setDefaultValue("marcform")
                        .addMapValue("marcform", "marcform")
                        .addMapValue("gmd", "gmd")
                    .createField()) // authority
                    .addField(new FieldBuilder("value").setTitle("Form - R").setMaxOccurrences(1).setType(Field.COMBO)
                        .setHint("Údaje o fyzické podobě dokumentu, např. print, electronic, microfilm apod."
                            + "<p>Odpovídá hodnotě v poli 008/23")
                        .addMapValue("braille", "braille")
                        .addMapValue("electronic", "electronic")
                        .addMapValue("large print", "large print")
                        .addMapValue("microfilm", "microfilm")
                        .addMapValue("microfiche", "microfiche")
                        .addMapValue("print", "print")
                    .createField()) // value
                .createField()) // form
                // reformattingQuality
                // internetMediaType
                // digitalOrigin
                // extent, stringPlusLanguagePlusSupplied
                // note, physicalDescriptionNote extends stringPlusLanguage
        .createField()); // physicalDescription

        // abstract, abstractDefinition extends stringPlusLanguage
        modsFields.add(new FieldBuilder("abstract").setTitle("Abstract - R").setMaxOccurrences(10)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @xlink:simpleLink, @shareable, @altRepGroup
                // altFormatAttributeGroup: @altFormat, @contentType
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                    .setHint("Shrnutí obsahu kapitoly.")
                .createField()) // value
        .createField()); // abstract

        // note, noteDefinition extends stringPlusLanguage
        modsFields.add(new FieldBuilder("note").setTitle("Note - RA").setMaxOccurrences(10)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @typeURI, @xlink:simpleLink, @ID, @altRepGroup
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                    .setHint("Obecná poznámka k vnitřní části. Do poznámky by se"
                        + " měla dávat šifra autora vnitřní části, která se vyskytuje pod vnitřní částí.")
                .createField()) // value
        .createField()); // note

        // subject, subjectDefinition
        modsFields.add(new FieldBuilder("subject").setTitle("Subject - R").setMaxOccurrences(10)
                .setHint("Údaje o věcném třídění.")
                // @ID, @authorityAttributeGroup, @languageAttributeGroup, @xlink:simpleLink, @displayLabel, @altRepGroup, @usage
                .addField(new FieldBuilder("authority").setTitle("Authority - O").setMaxOccurrences(1).setType(Field.COMBO)
                    .addMapValue("czenas", "czenas")
                    .addMapValue("eczenas", "eczenas")
                    .addMapValue("Konspekt", "Konspekt")
                .createField()) // authority

                // topic, stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("topic").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @type
                    // XXX autority.nkp.cz datasource
                    .addField(new FieldBuilder("value").setTitle("Topic - M").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Libovolný výraz specifikující nebo charakterizující obsah kapitoly."
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
                    .addField(new FieldBuilder("value").setTitle("Geographic - R").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Geografické věcné třídění."
                            + "<p>Použít kontrolovaný slovník - např. z báze autorit AUT NK ČR (geografický termín)"
                            + " nebo obsah pole 651 záznamu MARC21.")
                    .createField()) // value
                .createField()) // geographic

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
                .addField(new FieldBuilder("name").setMaxOccurrences(1)
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
                .createField()) // name

                // geographicCode
                // hierarchicalGeographic
                // cartographics
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
                .createField())
                // stringPlusLanguage/value
                .addField(new FieldBuilder("value").setTitle("Identifier - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).createField())
                // identifierDefinition
                //   displayLabel, xs:string
                //   typeURI, xs:anyURI
                //   invalid, fixed="yes"
//                .addField(new FieldBuilder("invalid").setTitle("Neplatný").setMaxOccurrences(1).setType(Field.SELECT)
//                    .addMapValue("", "Platný")
//                    .addMapValue("yes", "Neplatný")
//                .createField())
                //   altRepGroup, xs:string
        .createField()); // identifier

        // part, type="partDefinition"
        modsFields.add(new FieldBuilder("part").setTitle("Part - RA").setMaxOccurrences(1)
                .setHint("Popis rozsahu.")
                // @ID, @type, @order, @displayLabel, @altRepGroup
                // @languageAttributeGroup(lang, XmlLang, script, transliteration)
                // detail, type="detailDefinition"
                // extent, type="extentDefinition"
                .addField(new FieldBuilder("extent").setTitle("Extent - MA").setMaxOccurrences(10)
                    // start, type="stringPlusLanguage"
                    .addField(new FieldBuilder("start").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Start - MA").setMaxOccurrences(1).setType(Field.TEXT)
                            .setHint("První stránka, na které kapitola začíná.")
                        .createField()) // value
                    .createField()) // start
                    // end, type="stringPlusLanguage"
                    .addField(new FieldBuilder("end").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("End - MA").setMaxOccurrences(1).setType(Field.TEXT)
                            .setHint("Poslední stránka, na které kapitola končí.")
                        .createField()) // value
                    .createField()) // end
                    // total, type="xs:positiveInteger"
                    // list, type="stringPlusLanguage"
                .createField()) // extent
                // date
                // text
            .createField()); // part

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
                // descriptionStandard
        .createField()); // recordInfo

        return f;
    }

}
