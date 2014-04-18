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
 * The NDK Article.
 *
 * Version 1.4
 *
 * @author Jan Pokorsky
 */
public final class NdkArticleForm {

    public Form build() {
        Form f = new Form();

        f.getFields().add(new FieldBuilder("mods").setMaxOccurrences(1).createField()); // mods
        List<Field> modsFields = f.getFields().get(0).getFields();

        // titleInfo, titleInfoDefinition
        modsFields.add(new FieldBuilder("titleInfo").setTitle("Název").setMaxOccurrences(10)
                // titleInfo@type, enum
                .addField(new FieldBuilder("type").setTitle("Typ").setMaxOccurrences(1).setType(Field.SELECT)
                    .addMapValue("abbreviated", "Abbreviated")
                    .addMapValue("alternative", "Alternative")
                    .addMapValue("translated", "Translated")
                    .addMapValue("uniform", "Uniform")
                .createField())
                // title, type="stringPlusLanguage"
                .addField(new FieldBuilder("title").setMaxOccurrences(1)
                    .addField(new FieldBuilder("value").setTitle("Název článku").setMaxOccurrences(1).setType(Field.COMBO).setRequired(true)
                        .addMapValue("untitled", "Bez názvu")
                    .createField()) // title/value
                    // lang, String
                    // xmlLang, lang
                    // script, String
                    // transliteration, String
                .createField()) // title
                // subTitle, type="stringPlusLanguage"
                .addField(new FieldBuilder("subTitle").setMaxOccurrences(1)
                    .addField(new FieldBuilder("value").setTitle("Podnázev článku").setMaxOccurrences(1).setType(Field.TEXT).createField())
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                .createField()) // subTitle
                // partNumber, type="stringPlusLanguage"
                .addField(new FieldBuilder("partNumber").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("Číslo článku").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .createField()) // partNumber
                // partName, type="stringPlusLanguage"
                .addField(new FieldBuilder("partName").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("Název dílu").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .createField()) // partName
                // nonSort, type="stringPlusLanguage"
                // titleInfo@attributes: otherType, supplied, altRepGroup, altFormatAttributeGroup, nameTitleGroup, usage, ID, authorityAttributeGroup, xlink:simpleLink, languageAttributeGroup, displayLabel
            .createField()); // titleInfo

        // name, nameDefinition
        modsFields.add(new FieldBuilder("name").setMaxOccurrences(10).setTitle("Name")
                // @ID, @authorityAttributeGroup, @xlinkSimpleLink, @languageAttributeGroup, @displayLabel, @altRepGroup, @nameTitleGroup
                // @usage(fixed="primary")
                // @type(personal, corporate, conference, family)
                .addField(new FieldBuilder("type").setTitle("Typ").setMaxOccurrences(1).setType(Field.SELECT)
                    .addMapValue("personal", "personal")
                    .addMapValue("corporate", "corporate")
                    .addMapValue("conference", "conference")
                    .addMapValue("family", "family")
                .createField()) // @type
                // namePart, namePartDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("namePart").setTitle("NamePart").setMaxOccurrences(5)
                    // @type(date, family, given, termsOfAddress)
                    .addField(new FieldBuilder("type").setTitle("Typ").setMaxOccurrences(1).setType(Field.SELECT)
                        .addMapValue("date", "date")
                        .addMapValue("family", "family")
                        .addMapValue("given", "given")
                        .addMapValue("termsOfAddress", "termsOfAddress")
                    .createField()) // @type
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .createField()) // namePart
                // displayForm
                // etal
                // affiliation
                // role, roleDefinition
                .addField(new FieldBuilder("role").setTitle("Role").setMaxOccurrences(5)
                    // roleTerm, type="roleTermDefinition" extends stringPlusLanguagePlusAuthority
                    .addField(new FieldBuilder("roleTerm").setMaxOccurrences(1)
                        // @type, codeOrText(code, text)
                        .addField(new FieldBuilder("type").setTitle("Type").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("CODE")
                            .addMapValue("CODE", "code")
                            .addMapValue("TEXT", "text")
                        .createField()) // @type
                        // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("authority").setTitle("Authority").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200").setDefaultValue("marcrelator").createField())
                        .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.COMBO).setWidth("200")
                            // XXX use http://www.loc.gov/marc/relators/relacode.html
                            .addMapValue("cre", "Creator")
                            .addMapValue("crp", "Correspondent")
                        .createField()) // value
                    .createField()) // roleTerm
                .createField()) // role
                // description
            .createField()); // name

        // genre, genreDefinition extends stringPlusLanguagePlusAuthority extends stringPlusLanguage
        modsFields.add(new FieldBuilder("genre").setTitle("Genre").setMaxOccurrences(1)
                // genreDefinition@attributes: type, displayLabel, altRepGroup, usage
                .addField(new FieldBuilder("type").setTitle("Typ").setMaxOccurrences(1).setType(Field.COMBO).setWidth("100").setRequired(true)
                    .addMapValue("news", "zpráva")
                    .addMapValue("table of content", "obsah")
                    .addMapValue("advertisement", "reklama")
                    .addMapValue("abstract", "abstrakt")
                    .addMapValue("introduction", "úvod")
                    .addMapValue("review", "recenze")
                    .addMapValue("dedication", "věnování")
                    .addMapValue("bibliography", "bibliografie")
                    .addMapValue("editorsNote", "úvodník")
                    .addMapValue("preface", "přemluva")
                    .addMapValue("main article", "hlavní článek")
                    .addMapValue("index", "rejstřík")
                    .addMapValue("unspecified", "nespecifikován")
                .createField()) // @type
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).setDefaultValue("article").createField())
        .createField()); // genre

        // language, languageDefinition
        modsFields.add(new FieldBuilder("language").setTitle("Jazyky").setMaxOccurrences(10)
                // @objectPart, @displayLabel, @altRepGroup, @usage
                // languageAttributeGroup: @lang, @xmlLang, @script, @transliteration
                // languageTerm, languageTermDefinition
                .addField(new FieldBuilder("languageTerm").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @authorityURI, @valueURI
                    // @authority, enum
                    // XXX fill with "iso639-2b"
                    .addField(new FieldBuilder("authority").setMaxOccurrences(1).setType(Field.SELECT).setHint("languageTerm@authority").setDefaultValue("iso639-2b")
                        .addMapValue("iso639-2b", "ISO 639-2B")
                        .addMapValue("rfc3066", "RFC 3066")
                        .addMapValue("iso639-3", "ISO 639-3")
                        .addMapValue("rfc4646", "RFC 4646")
                        .addMapValue("rfc5646", "RFC 5646")
                    .createField())
                    // type, codeOrText('code', 'text')
                    // XXX autofill "code" value
                    .addField(new FieldBuilder("type").setMaxOccurrences(1).setType(Field.SELECT).setHint("languageTerm@type").setDefaultValue("CODE")
                        .addMapValue("CODE", "code")
                        .addMapValue("TEXT", "text")
                    .createField())
                    // XXX replace with http://www.loc.gov/standards/iso639‐2/php/code_list.php
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).createField())
                .createField()) // languageTerm
                // scriptTerm
        .createField()); // language

        // physicalDescription, physicalDescriptionDefinition
        modsFields.add(new FieldBuilder("physicalDescription").setTitle("Fyzický popis").setMaxOccurrences(10)
                // form, formDefinition extends stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("form").setTitle("Form").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @type, string
                    // XXX autofill "marcform"
                    .addField(new FieldBuilder("type").setTitle("Typ").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200").setDefaultValue("code").createField())
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.COMBO).setRequired(true).setHint("form")
                        .addMapValue("electronic", "electronic")
                        .addMapValue("print", "print")
                    .createField())
                .createField()) // form
                // reformattingQuality
                // internetMediaType
                // digitalOrigin
                // extent, stringPlusLanguagePlusSupplied
                // note, physicalDescriptionNote extends stringPlusLanguage
        .createField()); // physicalDescription

        // abstract, abstractDefinition extends stringPlusLanguage
        modsFields.add(new FieldBuilder("abstract").setMaxOccurrences(1)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @xlink:simpleLink, @shareable, @altRepGroup
                // altFormatAttributeGroup: @altFormat, @contentType
                .addField(new FieldBuilder("value").setTitle("Abstract").setMaxOccurrences(1).setType(Field.TEXTAREA).createField())
        .createField()); // abstract

        // note, noteDefinition extends stringPlusLanguage
        modsFields.add(new FieldBuilder("note").setTitle("Poznámka").setMaxOccurrences(10)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @typeURI, @xlink:simpleLink, @ID, @altRepGroup
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA).createField())
        .createField()); // note

        // subject, subjectDefinition
        modsFields.add(new FieldBuilder("subject").setTitle("Subject").setMaxOccurrences(10)
                // @ID, @authorityAttributeGroup, @languageAttributeGroup, @xlink:simpleLink, @displayLabel, @altRepGroup, @usage
                // autofill "czenas"
                .addField(new FieldBuilder("authority").setTitle("Authority").setMaxOccurrences(1).setType(Field.TEXT).setDefaultValue("czenas").createField())

                // topic, stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("topic").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @type
                    // XXX autority.nkp.cz datasource
                    .addField(new FieldBuilder("value").setTitle("Topic").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .createField()) // topic

                // geographic, stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("geographic").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @type
                    // XXX autority.nkp.cz datasource
                    .addField(new FieldBuilder("value").setTitle("Geographic").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .createField()) // geographic

                // temporal, temporalDefinition extends dateDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("temporal").setMaxOccurrences(1)
                    // authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @encoding, @qualifier, @point, @keyDate
                    // XXX autority.nkp.cz datasource
                    .addField(new FieldBuilder("value").setTitle("temporal").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200").createField())
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
                        .addField(new FieldBuilder("value").setTitle("namePart").setMaxOccurrences(1).setType(Field.TEXT).createField())
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
        modsFields.add(new FieldBuilder("classification").setTitle("Classification").setMaxOccurrences(10)
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                // autofill "udc"
                .addField(new FieldBuilder("authority").setTitle("Authority").setMaxOccurrences(1).setType(Field.TEXT).setDefaultValue("udc").setRequired(true).createField())
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).createField())
        .createField()); // classification

        // identifier, identifierDefinition, [0,*]
        modsFields.add(new FieldBuilder("identifier").setTitle("Identifikátory").setMaxOccurrences(10)
                // stringPlusLanguage@languageAttributeGroup
                //   lang, xs:string
                //   xml:lang
                //   script, xs:string
                //   transliteration, xs:string
                //   type, xs:string
                .addField(new FieldBuilder("type").setTitle("Typ").setMaxOccurrences(1).setType(Field.COMBO).setRequired(true)
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
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).createField())
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
        modsFields.add(new FieldBuilder("part").setTitle("Popis částí dokumentu").setMaxOccurrences(1)
                // @ID, @type, @order, @displayLabel, @altRepGroup
                // @languageAttributeGroup(lang, XmlLang, script, transliteration)
                // detail, type="detailDefinition"
                // extent, type="extentDefinition"
                .addField(new FieldBuilder("extent").setTitle("Rozsah").setMaxOccurrences(10)
                    // start, type="stringPlusLanguage"
                    .addField(new FieldBuilder("start").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Od").setMaxOccurrences(1).setType(Field.TEXT).createField())
                    .createField()) // start
                    // end, type="stringPlusLanguage"
                    .addField(new FieldBuilder("end").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Do").setMaxOccurrences(1).setType(Field.TEXT).createField())
                    .createField()) // end
                    // total, type="xs:positiveInteger"
                    // list, type="stringPlusLanguage"
                .createField()) // extent
                // date
                // text
            .createField()); // part

        // recordInfo, recordInfoDefinition
        modsFields.add(new FieldBuilder("recordInfo").setTitle("RecordInfo").setMaxOccurrences(1)
                // languageAttributeGroup: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @altRepGroup
                // recordContentSource, stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("recordContentSource").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    .addField(new FieldBuilder("value").setTitle("recordContentSource").setMaxOccurrences(1).setReadOnly(true).setType(Field.TEXT).createField())
                .createField()) // recordContentSource
                // recordCreationDate, dateDefinition
                .addField(new FieldBuilder("recordCreationDate").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // @encoding, @qualifier, @point, @keyDate
                    .addField(new FieldBuilder("encoding").setMaxOccurrences(1).setHidden(true).setType(Field.TEXT).createField())
                    .addField(new FieldBuilder("value").setTitle("recordCreationDate").setMaxOccurrences(1).setReadOnly(true).setType(Field.TEXT).createField())
                .createField()) // recordCreationDate
                // recordChangeDate, dateDefinition
                .addField(new FieldBuilder("recordChangeDate").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // @encoding, @qualifier, @point, @keyDate
                    .addField(new FieldBuilder("encoding").setMaxOccurrences(1).setHidden(true).setType(Field.TEXT).createField())
                    .addField(new FieldBuilder("value").setTitle("recordChangeDate").setMaxOccurrences(1).setReadOnly(true).setType(Field.TEXT).createField())
                .createField()) // recordChangeDate
                // recordIdentifier
                // languageOfCataloging
                // recordOrigin, extends stringPlusLanguage
                .addField(new FieldBuilder("recordOrigin").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("recordOrigin").setMaxOccurrences(1).setType(Field.COMBO)
                        .addMapValue("machine generated", "machine generated")
                        .addMapValue("human prepared", "human prepared")
                    .createField())
                .createField()) // recordChangeDate
                // descriptionStandard
        .createField()); // recordInfo

        return f;
    }

}
