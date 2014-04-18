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
 * The NDK Periodical Title.
 *
 * Version 1.4
 *
 * @author Jan Pokorsky
 */
public final class NdkPeriodicalForm {

    public Form build() {
        Form f = new Form();

        f.getFields().add(new FieldBuilder("mods").setMaxOccurrences(1).createField()); // mods
        List<Field> modsFields = f.getFields().get(0).getFields();

//        modsFields.add(new FieldBuilder("ID").setTitle("ID").setMaxOccurrences(1).setType(Field.TEXT).createField());
//        modsFields.add(new FieldBuilder("version").setTitle("Verze").setMaxOccurrences(1).setType(Field.TEXT).setReadOnly(true).createField());

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
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).createField())
                    // lang, String
                    // xmlLang, lang
                    // script, String
                    // transliteration, String
                .createField()) // title
                // subTitle, type="stringPlusLanguage"
                .addField(new FieldBuilder("subTitle").setMaxOccurrences(1)
                    .addField(new FieldBuilder("value").setTitle("Podnázev").setMaxOccurrences(1).setType(Field.TEXT).createField())
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                .createField()) // subTitle
                // partNumber, type="stringPlusLanguage"
                .addField(new FieldBuilder("partNumber").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("partNumber").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .createField()) // partNumber
                // partName, type="stringPlusLanguage"
                .addField(new FieldBuilder("partName").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("partName").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .createField()) // partName
                // nonSort, type="stringPlusLanguage"
                // titleInfo@attributes: otherType, supplied, altRepGroup, altFormatAttributeGroup, nameTitleGroup, usage, ID, authorityAttributeGroup, xlink:simpleLink, languageAttributeGroup, displayLabel
        .createField()); // titleInfo

        // typeOfResource, typeOfResourceDefinition extends resourceTypeDefinition
        modsFields.add(new FieldBuilder("typeOfResource").setMaxOccurrences(1)
                // typeOfResourceDefinition
                //   collection
                //   manuscript
                //   displayLabel
                //   altRepGroup
                //   usage
                // resourceTypeDefinition
                .addField(new FieldBuilder("value").setTitle("Obsah").setMaxOccurrences(1).setType(Field.SELECT).setRequired(true).setHint("&lt;typeOfResource>")
                    .addMapValue("text", "text")
                .createField())
        .createField()); // typeOfResource

        // genre, genreDefinition extends stringPlusLanguagePlusAuthority extends stringPlusLanguage
        modsFields.add(new FieldBuilder("genre").setTitle("Genre").setMaxOccurrences(1)
                // genreDefinition@attributes: type, displayLabel, altRepGroup, usage
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // XXX auto fill with issue
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).createField())
        .createField()); // genre

        // originInfo, originInfoDefinition
        modsFields.add(new FieldBuilder("originInfo").setTitle("OriginInfo").setMaxOccurrences(10)
                // @languageAttributeGroup(lang, XmlLang, script, transliteration)
                .addField(new FieldBuilder("transliteration").setTitle("Transliteration").setMaxOccurrences(1).setType(Field.COMBO)
                    .addMapValue("printer", "Tiskař")
                .createField())
                // @displayLabel
                // @altRepGroup
                // @eventType
                // place, placeDefinition
                .addField(new FieldBuilder("place").setMaxOccurrences(1)
                    // @supplied
                    // placeTerm, placeTermDefinition extends stringPlusLanguage
                    .addField(new FieldBuilder("placeTerm").setMaxOccurrences(1)
                        // type, codeOrText('code', 'text')
                        // XXX autofill "text" value
                        .addField(new FieldBuilder("type").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("TEXT").setReadOnly(true).setHint("&lt;originInfo>&lt;place>&lt;placeTerm>@type")
                            .addMapValue("CODE", "code")
                            .addMapValue("TEXT", "text")
                        .createField()) // type
                        .addField(new FieldBuilder("value").setTitle("Určení místa").setMaxOccurrences(1).setType(Field.TEXT).setHint("&lt;originInfo>&lt;place>&lt;placeTerm>").createField())
                        // @authorityURI, @valueURI,@authority
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .createField()) // placeTerm
                .createField()) // place
                // publisher, stringPlusLanguagePlusSupplied
                .addField(new FieldBuilder("publisher").setMaxOccurrences(1)
                    // stringPlusLanguagePlusSupplied: @supplied
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("Publisher").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .createField()) // publisher
                // dateIssued, dateDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("dateIssued").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @encoding, @qualifier, @point, @keyDate
                    .addField(new FieldBuilder("value").setTitle("Datum vydání").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).setWidth("200").createField())
                .createField()) // dateIssued
                // dateCreated, dateDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("dateCreated").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @encoding, @qualifier, @point, @keyDate
                    .addField(new FieldBuilder("value").setTitle("Datum vytvoření").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200").createField())
                .createField()) // dateCreated
                // dateCaptured
                // dateValid
                // dateModified
                // copyrightDate
                // dateOther
                // edition
                // issuance, issuanceDefinition, enum
                // XXX autofill "continuing"
                .addField(new FieldBuilder("issuance").setTitle("Issuance").setMaxOccurrences(1).setType(Field.SELECT).setRequired(true)
                    .addMapValue("CONTINUING", "continuing")
                    .addMapValue("MONOGRAPHIC", "monographic")
                    .addMapValue("SINGLE_UNIT", "single unit")
                    .addMapValue("MULTIPART_MONOGRAPH", "multipart monograph")
                    .addMapValue("SERIAL", "serial")
                    .addMapValue("INTEGRATING_RESOURCE", "integrating resource")
                .createField()) // issuance
                // frequency, stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("frequency").setMaxOccurrences(1)
                        // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Frequency").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .createField()) // frequency
        .createField()); // originInfo

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
                    .addField(new FieldBuilder("type").setMaxOccurrences(1).setType(Field.SELECT).setReadOnly(true).setHint("languageTerm@type")
                        .addMapValue("CODE", "code")
                        .addMapValue("TEXT", "text")
                    .createField())
                    // XXX replace with http://www.loc.gov/standards/iso639‐2/php/code_list.php
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setHint("languageTerm").createField())
                .createField()) // languageTerm
                // scriptTerm
        .createField()); // language

        // physicalDescription, physicalDescriptionDefinition
        modsFields.add(new FieldBuilder("physicalDescription").setTitle("Fyzický popis").setMaxOccurrences(10)
                // form, formDefinition extends stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("form").setTitle("Form").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @type
                    // XXX autofill "marcform"
                    .addField(new FieldBuilder("authority").setTitle("Authority").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200").setDefaultValue("marcform").createField())
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).setHint("form").setDefaultValue("print").createField())
                .createField()) // form
                // reformattingQuality
                // internetMediaType
                // digitalOrigin
                // extent, stringPlusLanguagePlusSupplied
                .addField(new FieldBuilder("extent").setTitle("Extent").setMaxOccurrences(5)
                    // stringPlusLanguagePlusSupplied: @supplied
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @unit
                    .addField(new FieldBuilder("unit").setTitle("Unit").setMaxOccurrences(1).setType(Field.TEXT).createField())
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setHint("extent").createField())
                .createField()) // extent
                // note, physicalDescriptionNote extends stringPlusLanguage
                .addField(new FieldBuilder("note").setTitle("Poznámka o fyzickém stavu dokumentu").setMaxOccurrences(5)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @displayLabel, @type, @typeURI, @xlinkSimpleLink, @ID
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA).createField())
                .createField()) // note
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
//                    // XXX autofill "marcform"
//                    .addField(new FieldBuilder("authority").setTitle("Authority").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200").createField())
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

        // XXX unsupported yet
        // relatedItem

        // XXX create type Identifier??
        // identifier, identifierDefinition, [0,*]
        modsFields.add(new FieldBuilder("identifier").setTitle("Identifikátory").setMaxOccurrences(10)
                // stringPlusLanguage@languageAttributeGroup
                //   lang, xs:string
                //   xml:lang
                //   script, xs:string
                //   transliteration, xs:string
                //   type, xs:string
                .addField(new FieldBuilder("type").setTitle("Typ").setMaxOccurrences(1).setType(Field.COMBO).setRequired(true)
                    // XXX use ValueMap
                    .addMapValue("barcode", "Čárový kód")
                    .addMapValue("ccnb", "čČNB")
                    .addMapValue("doi", "DOI")
                    .addMapValue("hdl", "Handle")
//                    .addMapValue("isbn", "ISBN")
                    .addMapValue("issn", "ISSN")
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

        // location, locationDefinition
        modsFields.add(new FieldBuilder("location").setTitle("Uložení dokumentu").setHint("&lt;location>").setMaxOccurrences(10)
                // languageAttributeGroup: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @altRepGroup
                // physicalLocation, physicalLocationDefinition extends stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("physicalLocation").setTitle("Sigla").setHint("&lt;physicalLocation>").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // autofill "siglaADR"
                    .addField(new FieldBuilder("authority").setTitle("Authority").setMaxOccurrences(1).setType(Field.TEXT).setDefaultValue("siglaADR").createField())
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @xlink:simpleLink, @displayLabel, @type
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).createField())
                .createField()) // physicalLocation
                // shelfLocator, stringPlusLanguage
                .addField(new FieldBuilder("shelfLocator").setTitle("Signatura").setMaxOccurrences(10)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).setHint("&lt;shelfLocator>").createField())
                .createField()) // shelfLocator
                // url, urlDefinition extends xs:anyURI
                .addField(new FieldBuilder("url").setTitle("URL").setMaxOccurrences(10)
                    // @dateLastAccessed, @displayLabel, @access(preview, raw object, object in context), @usage(primary display, primary)
                    // @note
                    .addField(new FieldBuilder("note").setTitle("Popis").setMaxOccurrences(1).setType(Field.TEXT).createField())
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .createField()) // url
                // holdingSimple
                // holdingExternal
        .createField()); // location

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
