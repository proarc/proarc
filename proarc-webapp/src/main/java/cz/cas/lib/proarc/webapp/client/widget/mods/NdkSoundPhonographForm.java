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

import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.List;

/**
 * The NDK Sound Phonograph cylinder
 *
 * Version NDK Standard 0.1"
 *
 * @author Lukas Sykora
 */
public final class NdkSoundPhonographForm {

    public Form build() {
        Form f = new Form();

        f.getFields().add(NdkForms.descriptionRadioButton());

        Field mods = new FieldBuilder("mods").setMaxOccurrences(1).createField();
        f.getFields().add(mods);
        List<Field> modsFields = mods.getFields();

//        modsFields.add(new FieldBuilder("ID").setTitle("ID").setMaxOccurrences(1).setType(Field.TEXT).createField());
//        modsFields.add(new FieldBuilder("version").setTitle("Verze").setMaxOccurrences(1).setType(Field.TEXT).setReadOnly(true).createField());

        modsFields.add(titleInfo(true));
        modsFields.add(name());
        modsFields.add(typeOfResource());
        modsFields.add(genre(true));
        modsFields.add(originInfo());
        modsFields.add(physicalDescription(true));
        modsFields.add(note());
        modsFields.add(tableOfContents());
        modsFields.add(identifier(true));
        modsFields.add(location(true));
        modsFields.add(NdkForms.recordInfo());
        modsFields.add(relatedItem());

        return f;
    }

    private Field titleInfo(boolean required) {
        // titleInfo, titleInfoDefinition
        return new FieldBuilder("titleInfo").setTitle("Title Info - M").setMaxOccurrences(10)
                .setHint("Název zvukového dokumentu, souborný název (samozřejmě lze využít"
                        + "<p> všech prvků a elementů MODS, které názvové informace popisují).")
                // titleInfo@type, enum
                .addField(new FieldBuilder("type").setTitle("Type - MA").setMaxOccurrences(1).setType(Field.SELECT)
                        .setHint("Hlavní název bez type.<dl>Hodnoty:"
                                + "<dt>alternative</dt><dd>alternativní název</dd>"
                                + "<dt>translated</dt><dd>přeložený název</dd>"
                                + "<dt>uniform</dt><dd>stejný/jednotný název</dd>"
                                + "<dt>abbreviated</dt><dd>zkrácený název</dd>"
                                + "</dl>")
                        .addMapValue("alternative", "Alternative")
                        .addMapValue("translated", "Translated")
                        .addMapValue("uniform", "Uniform")
                        .addMapValue("abbreviated", "Abreviated")
                        .createField()) // type
                // title, type="stringPlusLanguage"
                .addField(new FieldBuilder("title").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Title - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(required)
                                .setHint("Názvová informace - název titulu zvukový dokument.")
                                .createField()) // value
                        // lang, String
                        // xmlLang, lang
                        // script, String
                        // transliteration, String
                        .createField()) // title
                // subTitle, type="stringPlusLanguage"
                .addField(new FieldBuilder("subTitle").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Subtitle - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Podnázev titulu zvukový dokument.")
                                .createField()) // value
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .createField()) // subTitle
                // partNumber, type="stringPlusLanguage"
                .addField(new FieldBuilder("partNumber").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Part Number - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Údaje o názvu - číslo části/sekce.")
                                .createField()) // value
                        .createField()) // partNumber
                // partName, type="stringPlusLanguage"
                .addField(new FieldBuilder("partName").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Part Name - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Unifikovaný název - číslo části/sekce díla.")
                                .createField()) // value
                        .createField()) // partName
                .addField(new FieldBuilder("nonSort").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Non sort - O").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Část názvu, která má být vynechána při vyhledávání (např. The)")
                                .createField()) // value
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .createField()) // nonSort
                // nonSort, type="stringPlusLanguage"
                // titleInfo@attributes: otherType, supplied, altRepGroup, altFormatAttributeGroup, nameTitleGroup, usage, ID, authorityAttributeGroup, xlink:simpleLink, languageAttributeGroup, displayLabel
                .createField(); // titleInfo
    }

    private Field name() {
        // name, nameDefinition
        return new FieldBuilder("name").setMaxOccurrences(10).setTitle("Name - MA")
                .setHint("Údaje o odpovědnosti za zvukový dokument.")
                // @ID, @authorityAttributeGroup, @xlinkSimpleLink, @languageAttributeGroup, @displayLabel, @altRepGroup, @nameTitleGroup
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
                // @usage
                // namePart, namePartDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("namePart").setTitle("Name Parts - MA").setMaxOccurrences(5)
                        // @type(date, family, given, termsOfAddress)
                        .addField(new FieldBuilder("type").setTitle("Type").setMaxOccurrences(1).setType(Field.SELECT)
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
                                .setType(Field.TEXT)
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
                .addField(new FieldBuilder("nameIdentifier").setTitle("Name Identifier - RA").setMaxOccurrences(5)
                        .addField(new FieldBuilder("value").setMaxOccurrences(1)
                                .setType(Field.TEXT).setHint("Číslo národní autority").createField())
                        .createField()) //nameIdentifier

                // etal
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
                .addField(new FieldBuilder("role").setTitle("Role - MA").setMaxOccurrences(5)
                        .setHint("Specifikace role osoby nebo organizace uvedené v elementu &lt;name>")
                        // roleTerm, type="roleTermDefinition" extends stringPlusLanguagePlusAuthority
                        .addField(NdkForms.roleTerm(
                                "Role Term - M", false, "Authority - M", false, "Type - M", false
                        )) // roleTerm
                        .createField()) // role
                // description
                .createField(); // name
    }

    private Field typeOfResource() {
        // typeOfResource, typeOfResourceDefinition extends resourceTypeDefinition
        return new FieldBuilder("typeOfResource").setMaxOccurrences(1)
                // typeOfResourceDefinition
                //   collection
                //   manuscript
                //   displayLabel
                //   altRepGroup
                //   usage
                // resourceTypeDefinition
                .addField(new FieldBuilder("value").setTitle("Type of Resource - R").setMaxOccurrences(1).setType(Field.SELECT)
                        .setHint("Druh dokumentu.")
                        .addMapValue("sound recording", "sound recording")
                        .addMapValue("sound recording-musical", "sound recording-musical")
                        .addMapValue("sound recording-nonmusical", "sound recording-nonmusical")
                        .createField()) // value
                .createField(); // typeOfResource
    }

    private Field genre(boolean required) {
        // genre, genreDefinition extends stringPlusLanguagePlusAuthority extends stringPlusLanguage
        return new FieldBuilder("genre").setTitle("Genre - M").setMaxOccurrences(10)
                .setHint("Bližší údaje o typu dokumentu (dle mapování LoC by zde měla být"
                        + "<p>převedená z pole 655 a 336 v MARC21."
                        + "<p>Je možné použít jen jednu stanovenou hodnotu)")
                // genreDefinition@attributes: type, displayLabel, altRepGroup, usage
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // XXX auto fill with issue
                .addField(new FieldBuilder("value").setTitle("Genre - M").setMaxOccurrences(1).setType(Field.TEXT)
                        .setRequired(required).setDefaultValue("sound recording").createField())
                .createField(); // genre
    }

    private Field originInfo() {
        // originInfo, originInfoDefinition
        return new FieldBuilder("originInfo").setTitle("Origin Info - M").setMaxOccurrences(10)
                .setHint("Informace o původu předlohy.")
                // eventType
                .addField(new FieldBuilder("eventType").setTitle("Event Type - M").setMaxOccurrences(1). setType(Field.COMBO)
                        .setHint("Hodnoty dle druhého indikátoru pole 264:"
                                +"<p>264_1 publication se uvádí, jestliže pole obsahuje údaje o nakladateli zdroje."
                                +"<p>264_2 distribution se uvádí, jestliže pole obsahuje údaje o distribuci zdroje.")
                        .addMapValue("", "")
                        .addMapValue("publication", "publication")
                        .addMapValue("distribution", "distribution")
                        .createField()) //eventType
                // @languageAttributeGroup(lang, XmlLang, script, transliteration)
                // @transliteration
                // @displayLabel
                // @altRepGroup
                // place, placeDefinition
                .addField(new FieldBuilder("place").setTitle("Place - MA").setMaxOccurrences(10)
                        .setHint("Údaje o místě spojeném s vydáním, výrobou nebo původem popisovaného dokumentu.")
                        // @supplied
                        // placeTerm, placeTermDefinition extends stringPlusLanguage
                        .addField(new FieldBuilder("placeTerm").setMaxOccurrences(1)
                                // type, codeOrText('code', 'text')
                                .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("text")
                                        .setHint("Typ popisu místa. Kódem nebo textově."
                                                + "<p>Pokud má dokument více míst vydání v poli 260, podpole „a“, přebírají se ze záznamu všechna místa"
                                                + "<li>“code” pro údaj z pole 008</li><li>“text” pro údaj z pole 260</li>")
                                        .addMapValue("code", "code")
                                        .addMapValue("text", "text")
                                        .createField()) // type
                                .addField(new FieldBuilder("authority").setTitle("Authority - MA").setMaxOccurrences(1).setType(Field.COMBO)
                                        .setHint("Hodnota “marccountry” jen u údaje z pole 008")
                                        .addMapValue("marccountry", "marccountry")
                                        .createField()) // @authority
                                .addField(new FieldBuilder("value").setTitle("Place Term - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                        .setHint("Konkrétní určení místa a země vydání, např. Praha resp. xr pro ČR."
                                                + "<p>Odpovídá hodnotám z katalogizačního záznamu, pole 260, podpole „a“ resp. pole 008/15-17.")
                                        .createField()) // value
                                // @authorityURI, @valueURI,@authority
                                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                                .createField()) // placeTerm
                        .createField()) // place
                // publisher, stringPlusLanguagePlusSupplied
                .addField(new FieldBuilder("publisher").setTitle("Publisher - MA").setMaxOccurrences(10)
                        // stringPlusLanguagePlusSupplied: @supplied
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Jméno entity, která dokument vydala, vytiskla nebo jinak vyprodukovala."
                                        + "<p>Odpovídá poli 260 podpoli „b“ katalogizačního záznamu v MARC21;"
                                        + "<p>Pokud má titul více vydavatelů, přebírají se ze záznamu všichni (jsou v jednom poli 260).")
                                .createField()) // value
                        .createField()) // publisher
                // dateIssued, dateDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("dateIssued").setTitle("Date Issued - M").setMaxOccurrences(10)
                        .setHint("Datum vydání předlohy, nutno zaznamenat rok v nichž časopis vycházel - formu zápisu přebírat z katalogu (např. 1900-1939)"
                                + "<p>Odpovídá hodnotě z katalogizačního záznamu, pole 260, podpole „c“ a pole 008/07-10.")
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @encoding(w3cdtf, iso8601, marc, temper, edtf), @keyDate
                        .addField(new FieldBuilder("encoding").setTitle("Encoding - R").setMaxOccurrences(1).setType(Field.COMBO)
                                .setHint("Kódování - hodnota „marc“ jen u údaje z pole 008.")
                                .addMapValue("marc", "MARC")
                                .createField()) // @encoding
                        // @point(start, end)
                        .addField(new FieldBuilder("point").setTitle("Point - MA").setMaxOccurrences(1).setType(Field.SELECT)
                                .setHint("Hodnoty „start“ resp. „end“ jen u údaje z pole 008, pro rozmezí dat.")
                                .addMapValue("start", "start")
                                .addMapValue("end", "end")
                                .createField()) // @point
                        // @qualifier
                        .addField(new FieldBuilder("qualifier").setTitle("Qualifier - R").setMaxOccurrences(1).setType(Field.SELECT)
                                .setHint("Možnost dalšího upřesnění, hodnota „approximate“ pro data, kde nevíme přesný údaj. Hodnota  „inferred“ pro odvozený nebo dopočítaný údaj")
                                .addMapValue("approximate", "Approximate")
                                .addMapValue("inferred", "Inferred")
                        .createField()) // @qualifier
                        .addField(new FieldBuilder("value").setTitle("Date - M").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200")
                                .setHint("Datum vydání předlohy, nutno zaznamenat rok v nichž časopis vycházel - formu zápisu přebírat z katalogu (např. 1900-1939)"
                                        + "<p>Odpovídá hodnotě z katalogizačního záznamu, pole 260, podpole „c“ a pole 008/07-10.")
                                .createField()) // value
                        .createField()) // dateIssued
                // dateOther, dateOtherDefinition extends dateDefinition
                .addField(new FieldBuilder("dateOther").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Date Other - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Datum vytvoření, distribuce, výroby předlohy."
                                        + "<p>Tento elemet se využije v případě výskytu $c v:"
                                        + "<p>264_2 je distribution")
                                .createField()) // value
                        .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.SELECT)
                                .setHint("Hodnota distribution v případě pole 264_2.").createField())
                        .createField()) // dateOther

                // copyrightDate, dateDefinition extends stringPlusLanguage
                // dateCreated, dateDefinition extends stringPlusLanguage
                // dateCaptured
                // dateValid
                // dateModified
                // edition

                .addField(new FieldBuilder("edition").setMaxOccurrences(5)
                        // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Edition - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Údaje o pořadí vydávání.<p>Odpovídá údaji MARC21 v poli 250 $a.")
                                .createField())
                        .createField())
                // issuance
                .addField(new FieldBuilder("issuance").setTitle("Issuance - M").setMaxOccurrences(1).setType(Field.SELECT)
                        .setHint("Údaje o vydávání.<p>Odpovídá hodnotě uvedené v návěští MARC21 na pozici 07.")
                        .addMapValue("monographic", "monographic")
                        .createField()) // issuance
                // frequency, stringPlusLanguagePlusAuthority
                .createField(); // originInfo
    }

    private Field physicalDescription(boolean required) {
        // physicalDescription, physicalDescriptionDefinition
        return new FieldBuilder("physicalDescription").setTitle("Physical Description - M").setMaxOccurrences(10)
                .setHint("Obsahuje údaje o fyzickém popisu zdroje/předlohy.")
                // form, formDefinition extends stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("form").setTitle("Form - M").setMaxOccurrences(1)
                        // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @type
                        // XXX autofill "marcform"
                        .addField(new FieldBuilder("authority").setTitle("Authority - MA").setMaxOccurrences(1).setType(Field.COMBO)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCCATEGORY, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCCATEGORY)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCSMD, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCSMD)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDAMEDIA, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDAMEDIA)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDACARRIER, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDACARRIER)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MEDIA, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MEDIA)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_CARRIER, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_CARRIER)
                                .createField()) // authority
                        .addField(new FieldBuilder("value").setTitle("Form - M").setMaxOccurrences(1)
                                .setType(Field.COMBO).setRequired(required).setHint("form").setDefaultValue("cylinder")
                                .setHint("Údaje o fyzické podobě dokumentu, např. sound recording, cylinder"
                                        + "<p>Odpovídá hodnotě v poli 008/23")
                                .addMapValue("unspecified", "nespecifikováno")
                                .addMapValue("sound recording", "sound recording")
                                .addMapValue("cylinder", "cylinder")
                                .createField()) // value
                        .createField()) // form
                // reformattingQuality
                // internetMediaType
                // digitalOrigin
                // extent, stringPlusLanguagePlusSupplied
                .addField(new FieldBuilder("extent").setMaxOccurrences(5)
                        .addField(new FieldBuilder("value").setTitle("Extent - M").setMaxOccurrences(1).setType(Field.TEXT)
                                .setType(Field.TEXT).setRequired(required)
                                .setHint("Údaje o rozsahu.")
                                .createField())
                        .createField())// extent
                // note, physicalDescriptionNote extends stringPlusLanguage
                .createField(); // physicalDescription
    }

    private Field note() {
        // note, noteDefinition extends stringPlusLanguage
        return new FieldBuilder("note").setTitle("Note - RA").setMaxOccurrences(30)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @typeURI, @xlink:simpleLink, @ID, @altRepGroup
                .addField(new FieldBuilder("type").setTitle("Type - MA").setMaxOccurrences(1).setType(Field.COMBO)
                        .setHint("Upřesnění obsahu poznámky.")
                        .addMapValue("statement of responsibility", "statement of responsibility")
                        .addMapValue("creation/production details", "creation/production details")
                        .addMapValue("performers", "performers")
                        .addMapValue("venue", "venue")
                        .addMapValue("language", "language")
                        .addMapValue("ownership", "ownership")
                        .addMapValue("action", "action")
                        .addMapValue("version identification", "version identification")
                        .createField()) // type
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                        .setHint("Obecná poznámka k titulu periodika jako celku."
                                + "<p>Odpovídá hodnotám v poli 245, podpole „c“ (statement of responsibility)"
                                + " a v polích 5XX (poznámky) katalogizačního záznamu")
                        .createField()) // value
                .createField(); // note
    }

    private Field tableOfContents() {
        // tableOfContents, TableOfContentsDefinition extends StringPlusLanguage
        return new FieldBuilder("tableOfContents").setTitle("Table Of Contents").setMaxOccurrences(10)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @xlink:simpleLink, @shareable, @altRepGroup
                // altFormatAttributeGroup: @altFormat, @contentType
                .addField(new FieldBuilder("displayLabel").setTitle("Display Label - RA").setMaxOccurrences(1).setType(Field.COMBO)
                        .setHint("Formalizovaná poznámka k obsahu - text formalizované poznámky")
                        .addMapValue("contents", "Contents")
                        .addMapValue("incomplete contents", "Incomplete contents")
                        .addMapValue("partial contents", "Partial contents")
                        .createField()) // displeyLabel
                .addField(new FieldBuilder("value").setTitle("Table of Contents - MA").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Slouží k vepsání názvů částí skladby, pokud je skladba obsahuje; obsah pole 505 $a")
                        .createField()) // value
                .createField(); // tableOfContent
    }

    private Field identifier(boolean required) {
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
                .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.COMBO).setRequired(required)
                        .setHint("UUID - M - vygeneruje dodavatel"
                                + "<br>URN:NBN - O - zápis ve tvaru urn:nbn:cz:ndk-123456 pro projekt NDK"
                                + "<br>jiný interní identifikátor - R - type = barcode, oclc, sysno, permalink apod.")
                        .addMapValue("issue number", "issue number")
                        .addMapValue("local", "local")
                        .addMapValue("music-publisher", "music-publisher")
                        .addMapValue("upc", "upc")
                        .addMapValue("urnnbn", "URN:NBN")
                        .addMapValue("uuid", "UUID")
                        .createField())
                // stringPlusLanguage/value
                .addField(new FieldBuilder("value").setTitle("Identifier - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(required).createField())
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

    private Field location(boolean required) {
        // location, locationDefinition
        return new FieldBuilder("location").setTitle("Location - MA").setMaxOccurrences(10)
                .setHint("Údaje o uložení popisovaného dokumentu, např. signatura, místo uložení apod.")
                // languageAttributeGroup: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @altRepGroup
                // physicalLocation, physicalLocationDefinition extends stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("physicalLocation").setTitle("Physical Location - MA").setMaxOccurrences(1)
                        // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                        // autofill "siglaADR"
                        .addField(new FieldBuilder("authority").setTitle("Authority - M").setMaxOccurrences(1).setType(Field.TEXT).setDefaultValue("siglaADR").createField())
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @xlink:simpleLink, @displayLabel, @type
                        .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(required)
                                .setHint("Údaje o instituci, kde je fyzicky uložen popisovaný dokument. Např. NK ČR."
                                        + "<p>Nutno použít kontrolovaný slovník - sigly knihovnen (ABA001 atd.)"
                                        + "<p>Odpovídá poli 910 $a v MARC21."
                                        + "<p>Pozn. u dokumentů v digitální podobě není možné vyplnit.")
                                .createField()) // value
                        .createField()) // physicalLocation
                // shelfLocator, stringPlusLanguage
                .addField(new FieldBuilder("shelfLocator").setTitle("Shelf Locator - M").setMaxOccurrences(10)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(required)
                                .setHint("Signatura nebo lokační údaje o daném konkrétním dokumentu, který slouží jako předloha.")
                                .createField()) // value
                        .createField()) // shelfLocator
                // url, urlDefinition extends xs:anyURI
                // holdingSimple
                // holdingExternal
                .createField(); // location
    }

    private Field relatedItem() {
        return new FieldBuilder("relatedItem").setTitle("Related Item - RA").setMaxOccurrences(10)
                .addField(new FieldBuilder("type").setTitle("Type - R").setMaxOccurrences(1)
                        .setHint("Type spolu s otherType popisují vztah položky, popsané " +
                                "v <relatedItem> a dokumentu, který je předmětem MODS záznamu").setType(Field.SELECT)
                        .addMapValue("preceding", "preceding")
                        .addMapValue("succeeding", "succeeding")
                        .addMapValue("original", "original")
                        .addMapValue("host", "host")
                        .addMapValue("constituent", "constituent")
                        .addMapValue("series", "series")
                        .addMapValue("otherVersion", "otherVersion")
                        .addMapValue("otherFormat", "otherFormat")
                        .addMapValue("isReferencedBy", "isReferencedBy")
                        .addMapValue("references", "references")
                        .addMapValue("reviewOf", "reviewOf")
                        .createField())
                .addField(titleInfo(false))
                .addField(name())
                .addField(typeOfResource())
                .addField(genre(false))
                .addField(originInfo())
                .addField(physicalDescription(false))
                .addField(note())
                .addField(identifier(false))
                .addField(location(false))
                //.addField(NdkForms.recordInfo())
                .createField();
    }
}
