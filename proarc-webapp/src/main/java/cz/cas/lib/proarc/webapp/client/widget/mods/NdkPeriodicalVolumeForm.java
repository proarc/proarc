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
 * The NDK Periodical Volume.
 *
 * Version 1.5
 *
 * @author Jan Pokorsky
 */
public class NdkPeriodicalVolumeForm {

    public Form build() {
        Form f = new Form();

        f.getFields().add(new FieldBuilder("mods").setMaxOccurrences(1).createField()); // mods
        List<Field> modsFields = f.getFields().get(0).getFields();

        modsFields.add(titleInfo());
        modsFields.add(genre());
        modsFields.add(originInfo());
        modsFields.add(physicalDescription());
        modsFields.add(identifier());

        return f;
    }

    private Field titleInfo() {
        // titleInfo, titleInfoDefinition
        return new FieldBuilder("titleInfo").setMaxOccurrences(1)
                // titleInfo@type, enum
                // title, type="stringPlusLanguage"
                // subTitle, type="stringPlusLanguage"
                // partNumber, type="stringPlusLanguage"
                .addField(new FieldBuilder("partNumber").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("Part Number - MA").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                        .setHint("Pořadové číslo vydání ročníku, např. 40")
                    .createField()) // value
                .createField()) // partNumber
                // partName, type="stringPlusLanguage"
                // nonSort, type="stringPlusLanguage"
                // titleInfo@attributes: otherType, supplied, altRepGroup, altFormatAttributeGroup, nameTitleGroup, usage, ID, authorityAttributeGroup, xlink:simpleLink, languageAttributeGroup, displayLabel
        .createField(); // titleInfo
    }

    private Field genre() {
        // genre, genreDefinition extends stringPlusLanguagePlusAuthority extends stringPlusLanguage
        return new FieldBuilder("genre").setMaxOccurrences(1)
                // genreDefinition@attributes: type, displayLabel, altRepGroup, usage
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // XXX auto fill with "volume"
                .addField(new FieldBuilder("value").setTitle("Genre - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                    .setHint("bližší údaje o typu dokumentu\n hodnota “volume”")
                .createField()) // value
        .createField(); // genre
    }

    private Field originInfo() {
        // originInfo, originInfoDefinition
        return new FieldBuilder("originInfo").setMaxOccurrences(1)
                // @languageAttributeGroup(lang, XmlLang, script, transliteration)
                // @displayLabel
                // @altRepGroup
                // @eventType
                // place, placeDefinition
                // publisher, stringPlusLanguagePlusSupplied
                // dateIssued, dateDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("dateIssued").setTitle("Date Issued - M").setMaxOccurrences(1)
                    .setHint("Datum vydání předlohy, rok nebo rozsah let, kdy ročník vycházel.")
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @encoding, @qualifier, @point, @keyDate
                    .addField(new FieldBuilder("point").setTitle("Point - O").setMaxOccurrences(1).setType(Field.SELECT)
                        .setHint("Hodnoty „start“ resp. „end“ jen u údaje z pole 008, pro rozmezí dat.")
                        .addMapValue("start", "Začátek")
                        .addMapValue("end", "Konec")
                    .createField()) // @point
                    .addField(new FieldBuilder("qualifier").setTitle("Qualifier - O").setMaxOccurrences(1).setType(Field.SELECT)
                        .setHint("Možnost dalšího upřesnění, hodnota „approximate“ pro data, kde nevíme přesný údaj.")
                        .addMapValue("approximate", "Approximate")
                        .addMapValue("inferred", "Inferred")
                        .addMapValue("questionable", "Questionable")
                    .createField()) // qualifier
                    .addField(new FieldBuilder("value").setTitle("Date - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).setWidth("200")
                        .setHint("Datum vydání předlohy, rok nebo rozsah let, kdy ročník vycházel.")
                    .createField()) // value
                .createField()) // dateIssued
                // dateCreated, dateDefinition extends stringPlusLanguage
                // dateCaptured
                // dateValid
                // dateModified
                // copyrightDate
                // dateOther
                // edition
                // issuance, issuanceDefinition, enum
                // frequency, stringPlusLanguagePlusAuthority
        .createField(); // originInfo
    }

    private Field physicalDescription() {
        // physicalDescription, physicalDescriptionDefinition
        return new FieldBuilder("physicalDescription").setMaxOccurrences(1)
                // form, formDefinition extends stringPlusLanguagePlusAuthority
                // reformattingQuality
                // internetMediaType
                // digitalOrigin
                // extent, stringPlusLanguagePlusSupplied
                // note, physicalDescriptionNote extends stringPlusLanguage
                .addField(new FieldBuilder("note").setTitle("Note - O").setMaxOccurrences(5)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @displayLabel, @type, @typeURI, @xlinkSimpleLink, @ID
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA).setWidth("400")
                        .setHint("Poznámka o fyzickém stavu dokumentu."
                            + "<p>Pro každou poznámku je nutno vytvořit nový &lt;note> element."
                            + "<p>Zde se zapíší defekty zjištěné při digitalizaci pro"
                            + " úroveň ročníku (např. chybějící čísla apod.)")
                    .createField()) // value
                .createField()) // note
        .createField(); // physicalDescription
    }

    private Field identifier() {
        // identifier, identifierDefinition, [0,*]
        return new FieldBuilder("identifier").setTitle("Identifier - M").setMaxOccurrences(10)
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
                    .addMapValue("uuid", "UUID")
                    .addMapValue("urnnbn", "URN:NBN")
                    .addMapValue("barcode", "Čárový kód")
                    .addMapValue("oclc", "OCLC")
                    .addMapValue("permalink", "Permalink")
                    .addMapValue("sysno", "Sysno")
                .createField())
                // stringPlusLanguage/value
                .addField(new FieldBuilder("value").setTitle("Identifier - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).setHint("Hodnota identifikátoru.").createField())
                // identifierDefinition
                //   displayLabel, xs:string
                //   typeURI, xs:anyURI
                //   invalid, fixed="yes"
                .addField(new FieldBuilder("invalid").setTitle("Invalid - MA").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("")
                    .addMapValue("", "Platný")
                    .addMapValue("yes", "Neplatný")
                .createField()) // invalid
                //   altRepGroup, xs:string
        .createField(); // identifier
    }

}
