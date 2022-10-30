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
 * The NDK Multipart Monograph.
 * <p>
 * Version 1.1_2
 *
 * @author Jan Pokorsky
 */
public final class NdkMonographTitleForm {

    public Form build() {
        Form f = new Form();

        f.getFields().add(NdkForms.descriptionRadioButton());

        Field mods = new FieldBuilder("mods").setMaxOccurrences(1).createField();
        f.getFields().add(mods);
        List<Field> modsFields = mods.getFields();

        modsFields.add(titleInfo());
        modsFields.add(originInfo());
        modsFields.add(genre());
        modsFields.add(language());
        modsFields.add(identifier());
        modsFields.add(NdkForms.recordInfo());

        return f;
    }

    private Field titleInfo() {
        // titleInfo, titleInfoDefinition
        return new FieldBuilder("titleInfo").setTitle("Title Info - M").setMaxOccurrences(10)
                .setHint("Název titulu, souborný název.<p>Pro plnění použít katalogizační záznam.")
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
                        .createField()) // type
                // title, type="stringPlusLanguage"
                .addField(new FieldBuilder("title").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Title - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                                .setHint("Názvová informace - název monografického dokumentu.")
                                .createField()) // value
                        // lang, String
                        // xmlLang, lang
                        // script, String
                        // transliteration, String
                        .createField()) // title
                // subTitle, type="stringPlusLanguage"
                .addField(new FieldBuilder("subTitle").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Subtitle - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Podnázev svazku monografie.")
                                .createField()) // value
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .createField()) // subTitle
                // partNumber, type="stringPlusLanguage"
                .addField(new FieldBuilder("partNumber").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Part Number - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Číslo svazku souborného záznamu.")
                                .createField()) // value
                        .createField()) // partNumber
                // partName, type="stringPlusLanguage"
                .addField(new FieldBuilder("partName").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Part Name - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Název svazku souborného záznamu.")
                                .createField()) // value
                        .createField()) // partName
                // nonSort, type="stringPlusLanguage"
                // titleInfo@attributes: otherType, supplied, altRepGroup, altFormatAttributeGroup, nameTitleGroup, usage, ID, authorityAttributeGroup, xlink:simpleLink, languageAttributeGroup, displayLabel
                .createField(); // titleInfo
    }

    private Field originInfo() {
        // originInfo, originInfoDefinition
        return new FieldBuilder("originInfo").setTitle("Origin Info - M").setMaxOccurrences(10)
                .setHint("Informace o původu předlohy.")
                // @eventType
                .addField(new FieldBuilder("eventType").setTitle("Event Type - M").setMaxOccurrences(1).setType(Field.COMBO)
                        .setHint("Hodnoty dle druhého indikátoru pole 264:"
                                + "<p>264_0 production (R) se uvádí, jestliže pole obsahuje údaje o vytvoření zdroje v nezveřejněné podobě."
                                + "<p>264_1 publication (R) se uvádí, jestliže pole obsahuje údaje o nakladateli zdroje."
                                + "<p>264_2 distribution (R) se uvádí, jestliže pole obsahuje údaje o distribuci zdroje."
                                + "<p>264_3 manufacture (R) se uvádí, jestliže pole obsahuje údaje o tisku, výrobě zdroje ve zveřejněné podobě."
                                + "<p>264_4 copyright (R) se uvádí, jestliže pole obsahuje údaje o ochraně podle autorského práva (copyright).")
                        .addMapValue("", "")
                        .addMapValue(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PRODUCTION, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PRODUCTION)
                        .addMapValue(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PUBLICATION, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PUBLICATION)
                        .addMapValue(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_DISTRIBUTION, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_DISTRIBUTION)
                        .addMapValue(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_MANUFACTURE, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_MANUFACTURE)
                        .addMapValue(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_COPYRIGHT, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_COPYRIGHT)
                        .createField()) // eventType
                // @languageAttributeGroup(lang, XmlLang, script, transliteration)
                // @displayLabel
                // @altRepGroup
                // place, placeDefinition
                .addField(new FieldBuilder("place").setTitle("Place - O").setMaxOccurrences(10)
                        .setHint("Údaje o místě spojeném s vydáním, výrobou nebo původem popisovaného dokumentu.")
                        // @supplied
                        // placeTerm, placeTermDefinition extends stringPlusLanguage
                        .addField(new FieldBuilder("placeTerm").setMaxOccurrences(1)
                                // type, codeOrText('code', 'text')
                                .addField(new FieldBuilder("type").setTitle("Type - O").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("text")
                                        .setHint("Typ popisu místa. Kódem nebo textově."
                                                + "<p>Pokud má dokument více míst vydání v poli 260, podpole „a“, přebírají se ze záznamu všechna místa"
                                                + "<li>“code” pro údaj z pole 008</li><li>“text” pro údaj z pole 260</li>")
                                        .addMapValue("code", "code")
                                        .addMapValue("text", "text")
                                        .createField()) // type
                                // @authorityURI, @valueURI,@authority
                                .addField(new FieldBuilder("authority").setTitle("Authority - O").setMaxOccurrences(1).setType(Field.COMBO)
                                        .setHint("Hodnota “marccountry” jen u údaje z pole 008")
                                        .addMapValue("marccountry", "marccountry")
                                        .createField()) // @authority
                                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                                .addField(new FieldBuilder("value").setTitle("Place Term - O").setMaxOccurrences(1).setType(Field.TEXT)
                                        .setHint("Konkrétní určení místa a země vydání, např. Praha resp. xr pro ČR."
                                                + "<p>Odpovídá hodnotám z katalogizačního záznamu, pole 260, podpole „a“ resp. pole 008/15-17.")
                                        .createField()) // value
                                .createField()) // placeTerm
                        .createField()) // place
                // publisher, stringPlusLanguagePlusSupplied
                .addField(new FieldBuilder("publisher").setMaxOccurrences(1)
                        // stringPlusLanguagePlusSupplied: @supplied
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Publisher - MA").setMaxOccurrences(1).setType(Field.TEXT).createField())
                        .setHint("Jméno entity, která dokument vydala, vytiskla nebo jinak vyprodukovala."
                                + "<p>Pokud existuje více vydavatelů, přebírají se ze záznamu všechny.")
                        .createField()) // publisher
                // dateIssued, dateDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("dateIssued").setTitle("Date Issued - O").setMaxOccurrences(10)
                        .setHint("Datum vydání předlohy."
                                + "<p>Odpovídá hodnotě z katalogizačního záznamu, pole 260, podpole „c“ a pole 008/07-10.")
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @encoding(w3cdtf, iso8601, marc, temper, edtf), @qualifier, @point(start, end), @keyDate
                        .addField(new FieldBuilder("encoding").setTitle("Encoding - O").setMaxOccurrences(1).setType(Field.SELECT)
                                .setHint("Kódování - hodnota „marc“ jen u údaje z pole 008.")
                                .addMapValue("iso8601", "ISO 8601")
                                .addMapValue("edtf", "EDTF")
                                .addMapValue("marc", "MARC")
                                .addMapValue("temper", "temper")
                                .addMapValue("w3cdtf", "W3CDTF")
                                .createField()) // @encoding
                        .addField(new FieldBuilder("point").setTitle("Point - O").setMaxOccurrences(1).setType(Field.SELECT).setRequired(false)
                                .setHint("Hodnoty „start“ resp. „end“ jen u údaje z pole 008, pro rozmezí dat.")
                                .addMapValue("start", "start")
                                .addMapValue("end", "end")
                                .createField()) // @point
                        .addField(new FieldBuilder("qualifier").setTitle("Qualifier - O").setMaxOccurrences(1).setType(Field.SELECT)
                                .setHint("Možnost dalšího upřesnění, hodnota „approximate“ pro data, kde nevíme přesný údaj. Hodnota  „inferred“ pro odvozený nebo dopočítaný údaj")
                                .addMapValue("approximate", "Approximate")
                                .addMapValue("inferred", "Inferred")
                                .createField()) // @qualifier
                        .addField(new FieldBuilder("value").setTitle("Date - O").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200")
                                .setHint("Datum vydání předlohy."
                                        + "<p>Odpovídá hodnotě z katalogizačního záznamu, pole 260, podpole „c“ a pole 008/07-10.")
                                .createField()) // value
                        .createField()) // dateIssued
                // dateCreated, dateDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("dateCreated").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @encoding, @qualifier, @point, @keyDate
                        .addField(new FieldBuilder("value").setTitle("Date Created - O").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200")
                                .setHint("Datum vytvoření předlohy.")
                                .createField()) // value
                        .createField()) // dateCreated
                // dateCaptured
                // dateValid
                // dateModified
                // copyrightDate
                .addField(new FieldBuilder("copyrightDate").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Copyright Date - O").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Využije se pouze v případě výskuytu pole 264 s druhým indikátorem \"4\" a podpolem $c.")
                                .createField()) // value
                        .createField()) // copyrightDate
                // dateOther
                .addField(new FieldBuilder("dateOther").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Date Other - O").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Datum vytvoření, distribuce, výroby předlohy."
                                        + "<p>Tento element se využije v případě výskytu $c v:"
                                        + "<p>264_0 je production"
                                        + "<p>264_2 je distribution"
                                        + "<p>264_3 je manufacture")
                                .createField())// value
                        .createField()) // dateOther
                // edition, type="stringPlusLanguagePlusSupplied"
                .addField(new FieldBuilder("edition").setMaxOccurrences(1)
                        // stringPlusLanguagePlusSupplied: @supplied
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Edition - MA").setMaxOccurrences(1).setType(Field.TEXT).setHint("Pořadí vydání").createField())
                        .createField()) // edition
                // issuance, issuanceDefinition, enum
                // issuance
                // frequency, stringPlusLanguagePlusAuthority
                .createField(); // originInfo
    }

    private Field genre() {
        // genre, genreDefinition extends stringPlusLanguagePlusAuthority extends stringPlusLanguage
        return new FieldBuilder("genre").setTitle("Genre - M").setMaxOccurrences(1)
                .setHint("Bližší údaje o typu dokumentu.<p>Pro vícesvazkovou monografii “title”.")
                // genreDefinition@attributes: type, displayLabel, altRepGroup, usage
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).createField())
                .createField(); // genre
    }

    private Field language() {
        // language, languageDefinition
        return new FieldBuilder("language").setTitle("Languages - O").setMaxOccurrences(10)
                .setHint("Údaje o jazyce dokumentu; v případě vícenásobného výskytu nutno element <language> opakovat")
                // @objectPart, @displayLabel, @altRepGroup, @usage
                .addField(new FieldBuilder("objectPart").setTitle("Object Part - O").setMaxOccurrences(1).setType(Field.COMBO).setWidth("300")
                        .setHint("Možnost vyjádřit jazyk konkrétní části svazku.")
                        // .addMapValue("summary", "summary")
                        // .addMapValue("table of contents", "table of contents")
                        // .addMapValue("accompanying material", "accompanying material")
                        .addMapValue("translation", "translation")
                        .createField()) // @objectPart
                // languageAttributeGroup: @lang, @xmlLang, @script, @transliteration
                // languageTerm, languageTermDefinition
                .addField(new FieldBuilder("languageTerm").setMaxOccurrences(1)
                        .setHint("Přesné určení jazyka – kódem nutno použít kontrolovaný slovník ISO 639-2,"
                                + "http://www.loc.gov/standards/iso639-2/php/code_list.php"
                                + "<p>Odpovídá poli 008/35-37, resp. 041")
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
                .createField(); // language
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
                                + "<br>čČNB - MA - převzít z katalogizačního záznamu z pole 015, podpole „a“, „z“"
                                + "<br>ISBN - MA - převzít z katalogizačního záznamu z pole 020, podpole „a“, „z“"
                                + "<br>ISMN - MA - převzít z katalogizačního záznamu z pole 024 (1. ind.=“2“), podpole „a“, „z“"
                                + "<br>URN:NBN - O - zápis ve tvaru urn:nbn:cz:ndk-123456 pro projekt NDK"
                                + "<br>jiný interní identifikátor - R - type = barcode, oclc, sysno, permalink apod.")
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
                .createField(); // identifier
    }

}
