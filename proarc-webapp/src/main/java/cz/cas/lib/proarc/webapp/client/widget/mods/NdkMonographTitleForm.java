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
 * The NDK Multipart Monograph.
 *
 * Version 1.1_2
 *
 * @author Jan Pokorsky
 */
public final class NdkMonographTitleForm {

    public Form build() {
        Form f = new Form();

        f.getFields().add(new FieldBuilder("mods").setMaxOccurrences(1).createField()); // mods
        List<Field> modsFields = f.getFields().get(0).getFields();

        // titleInfo, titleInfoDefinition
        modsFields.add(new FieldBuilder("titleInfo").setTitle("Název").setMaxOccurrences(10)
                // titleInfo@type, enum
    //            .addField(new FieldBuilder("type").setTitle("Typ").setMaxOccurrences(1).setType(Field.SELECT)
    //                .addMapValue("abbreviated", "Abbreviated")
    //                .addMapValue("alternative", "Alternative")
    //                .addMapValue("translated", "Translated")
    //                .addMapValue("uniform", "Uniform")
    //            .createField())
                // title, type="stringPlusLanguage"
                .addField(new FieldBuilder("title").setMaxOccurrences(1)
                    .addField(new FieldBuilder("value").setTitle("Titul").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).createField())
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
    //            .addField(new FieldBuilder("partNumber").setMaxOccurrences(1)
    //                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
    //                .addField(new FieldBuilder("value").setTitle("partNumber").setMaxOccurrences(1).setType(Field.TEXT).createField())
    //            .createField()) // partNumber
                // partName, type="stringPlusLanguage"
    //            .addField(new FieldBuilder("partName").setMaxOccurrences(1)
    //                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
    //                .addField(new FieldBuilder("value").setTitle("partName").setMaxOccurrences(1).setType(Field.TEXT).createField())
    //            .createField()) // partName
                // nonSort, type="stringPlusLanguage"
                // titleInfo@attributes: otherType, supplied, altRepGroup, altFormatAttributeGroup, nameTitleGroup, usage, ID, authorityAttributeGroup, xlink:simpleLink, languageAttributeGroup, displayLabel
        .createField()); // titleInfo

        // originInfo, originInfoDefinition
        modsFields.add(new FieldBuilder("originInfo").setTitle("OriginInfo").setMaxOccurrences(10)
                // @languageAttributeGroup(lang, XmlLang, script, transliteration)
                // @displayLabel
                // @altRepGroup
                // @eventType
                // place, placeDefinition
                // publisher, stringPlusLanguagePlusSupplied
                .addField(new FieldBuilder("publisher").setMaxOccurrences(1)
                    // stringPlusLanguagePlusSupplied: @supplied
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("Publisher").setMaxOccurrences(1).setType(Field.TEXT).createField())
                .createField()) // publisher
                // dateIssued, dateDefinition extends stringPlusLanguage
                // dateCreated, dateDefinition extends stringPlusLanguage
                // dateCaptured
                // dateValid
                // dateModified
                // copyrightDate
                // dateOther
                // edition, type="stringPlusLanguagePlusSupplied"
                .addField(new FieldBuilder("edition").setMaxOccurrences(1)
                    // stringPlusLanguagePlusSupplied: @supplied
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("Pořadí vydání").setMaxOccurrences(1).setType(Field.TEXT).setHint("&lt;edition>").createField())
                .createField()) // edition
                // issuance, issuanceDefinition, enum
                // frequency, stringPlusLanguagePlusAuthority
        .createField()); // originInfo

        // genre, genreDefinition extends stringPlusLanguagePlusAuthority extends stringPlusLanguage
        modsFields.add(new FieldBuilder("genre").setTitle("Genre").setMaxOccurrences(1)
                // genreDefinition@attributes: type, displayLabel, altRepGroup, usage
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setDefaultValue("title").setRequired(true).createField())
        .createField()); // genre

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
                    .addMapValue("isbn", "ISBN")
                    .addMapValue("ismn", "ISMN")
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

        return f;
    }

}
