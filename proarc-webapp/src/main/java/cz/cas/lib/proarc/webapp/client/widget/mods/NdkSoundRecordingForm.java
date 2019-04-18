/*
 * Copyright (C) 2017 Lukas Sykora
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
 * The NDK Sound Recording
 *
 * Version NDK Standard 0.3
 *
 * @author Lukas Sykora
 */
public final class NdkSoundRecordingForm {

    public Form build() {
        Form f = new Form();

        f.getFields().add(new FieldBuilder("rdaRules").setTitle("Zvolte pravidla popisu (Description Standard) - MA").setMaxOccurrences(1)
                .setType(Field.RADIOGROUP).setRequired(true).setDefaultValue("true")
                .addMapValue("true", ModsConstants.VALUE_DESCRIPTIONSTANDARD_RDA)
                .addMapValue("false", ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR)
                .createField());

        Field mods = new FieldBuilder("mods").setMaxOccurrences(1).createField();
        f.getFields().add(mods);

        List<Field> modsFields = mods.getFields();

//        modsFields.add(new FieldBuilder("ID").setTitle("ID").setMaxOccurrences(1).setType(Field.TEXT).createField());
//        modsFields.add(new FieldBuilder("version").setTitle("Verze").setMaxOccurrences(1).setType(Field.TEXT).setReadOnly(true).createField());

        modsFields.add(titleInfo());
        modsFields.add(name());
        modsFields.add(genre());
        modsFields.add(originInfo());
        modsFields.add(physicalDescription());
        modsFields.add(language());
        modsFields.add(typeOfResource());
        // modsFields.add(abstracts());
        modsFields.add(tableOfContents());
        modsFields.add(note());
        modsFields.add(subject());
        // modsFields.add(classification());
        // XXX unsupported yet
        // relatedItem
        modsFields.add(identifier());
        // modsFields.add(location());
        modsFields.add(recordInfo());

        return f;
    }

    private Field titleInfo() {
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
                                + "</dl>")
                        .addMapValue("alternative", "Alternative")
                        //.addMapValue("translated", "Translated")
                        .addMapValue("uniform", "Uniform")
                        .createField()) // type
                // title, type="stringPlusLanguage"
                .addField(new FieldBuilder("title").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Title - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
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
                        .addField(new FieldBuilder("value").setTitle("Part Number - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Údaje o názvu - číslo části/sekce.")
                                .createField()) // value
                        .createField()) // partNumber
                // partName, type="stringPlusLanguage"
                // nonSort, type="stringPlusLanguage"
                // titleInfo@attributes: otherType, supplied, altRepGroup, altFormatAttributeGroup, nameTitleGroup, usage, ID, authorityAttributeGroup, xlink:simpleLink, languageAttributeGroup, displayLabel
                .createField(); // titleInfo
    }

    private Field name() {
        // name, nameDefinition
        return new FieldBuilder("name").setMaxOccurrences(10).setTitle("Name - MA")
                .setHint("Údaje o odpovědnosti za skladby.")
                // @ID, @authorityAttributeGroup, @xlinkSimpleLink, @languageAttributeGroup, @displayLabel, @altRepGroup, @nameTitleGroup
                // @type(personal, corporate, conference, family)
                .addField(new FieldBuilder("type").setTitle("Type - R").setMaxOccurrences(1).setType(Field.SELECT).setRequired(true)
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
                                .setType(Field.TEXT).setRequired(true)
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
                // etal
                // affiliation
                // role, roleDefinition
                .addField(new FieldBuilder("role").setTitle("Role - MA").setMaxOccurrences(5)
                        .setHint("Specifikace role osoby nebo organizace uvedené v elementu &lt;name>")
                        // roleTerm, type="roleTermDefinition" extends stringPlusLanguagePlusAuthority
                        .addField(NdkForms.roleTerm(
                                "Role Term - MA", true, "Authority - MA", true, "Type - M", true
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
                        //.addMapValue("sound recording", "sound recording")
                        .addMapValue("sound recording-musical", "sound recording-musical")
                        .addMapValue("sound recording-nonmusical", "sound recording-nonmusical")
                        .createField()) // value
                .createField(); // typeOfResource
    }

    private Field genre() {
        // genre, genreDefinition extends stringPlusLanguagePlusAuthority extends stringPlusLanguage
        return new FieldBuilder("genre").setTitle("Genre - M").setMaxOccurrences(1)
                .setHint("Bližší údaje o typu dokumentu (dle mapování LoC by zde měla být"
                        + "<p>převedená z pole 655 a 336 v MARC21."
                        + "<p>Je možné použít jen jednu stanovenou hodnotu)")
                // genreDefinition@attributes: type, displayLabel, altRepGroup, usage
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // XXX auto fill with issue
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).setDefaultValue("soundrecording").createField())
                .createField(); // genre
    }

    private Field originInfo() {
        // originInfo, originInfoDefinition
        return new FieldBuilder("originInfo").setTitle("Origin Info - M").setMaxOccurrences(10)
                .setHint("Informace o původu předlohy.")
                .addField(new FieldBuilder("eventType").setTitle("Event Type - M").setMaxOccurrences(1). setType(Field.COMBO)
                        .setHint("Hodnoty dle druhého indikátoru pole 264:"
                                +"<p>264_0 production se uvádí, jestliže pole obsahuje údaje o vytvoření zdroje v nezveřejněné podobě."
                                +"<p>264_1 publication se uvádí, jestliže pole obsahuje údaje o nakladateli zdroje."
                                +"<p>264_2 distribution se uvádí, jestliže pole obsahuje údaje o distribuci zdroje."
                                +"<p>264_3 manufacture se uvádí, jestliže pole obsahuje údaje o tisku, výrobě zdroje ve zveřejněné podobě."
                                +"<p>264_4 copyright (R) se uvádí, jestliže pole obsahuje údaje o ochraně podle autorského práva (copyright).")
                        .addMapValue("", "")
                        .addMapValue(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PRODUCTION, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PRODUCTION)
                        .addMapValue(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PUBLICATION, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PUBLICATION)
                        .addMapValue(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_DISTRIBUTION, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_DISTRIBUTION)
                        .addMapValue(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_MANUFACTURE, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_MANUFACTURE)
                        .addMapValue(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_COPYRIGHT, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_COPYRIGHT)
                        .createField()) // eventType
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
                // @publisher
                // @dateIssued
                // @dateOther
                // @copyrightDate
                // @dateCreated
                // @edition
                // issuance, issuanceDefinition, enum
                .addField(new FieldBuilder("issuance").setTitle("Issuance - M").setMaxOccurrences(1).setType(Field.COMBO).setRequired(true)
                        .setHint("Údaje o vydávání.<p>Odpovídá hodnotě uvedené v návěští MARC21 na pozici 07.")
                        .addMapValue("monographic", "monographic")
                        .createField()) // issuance
                // @frequency
                .createField(); // originInfo
    }

    private Field language() {
        // language, languageDefinition
        return new FieldBuilder("language").setTitle("Languages - M").setMaxOccurrences(10)
                .setHint("Údaje o jazyce dokumentu; v případě vícenásobného výskytu nutno element &lt;language> opakovat")
                // @objectPart, @displayLabel, @altRepGroup, @usage
                // languageAttributeGroup: @lang, @xmlLang, @script, @transliteration
                // languageTerm, languageTermDefinition
                .addField(new FieldBuilder("objectPart").setTitle("Object Part - MA").setMaxOccurrences(1)
                        .setType(Field.COMBO)
                        .setHint("Kód jazyka zpívaného nebo mluveného slova.")
                        .addMapValue("sung or spoken text", "sung or spoken text")
                        .addMapValue("translation", "translation")
                        .addMapValue("libretto", "libretto")
                        .createField()) // objectPart
                .addField(new FieldBuilder("languageTerm").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @authorityURI, @valueURI
                        // @authority, enum
                        .addField(new FieldBuilder("authority").setTitle("Authority - M").setMaxOccurrences(1)
                                .setType(Field.SELECT).setRequired(true).setDefaultValue("ISO 639-2B")
                                .setHint("Použít hodnotu „iso639-2b“.")
                                .addMapValue("iso639-2b", "ISO 639-2B")
                                .createField()) // authority
                        // type, codeOrText('code', 'text')
                        .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1)
                                .setType(Field.SELECT).setRequired(true)
                                .setHint("Typ popisu.")
                                .addMapValue("code", "code")
                                .addMapValue("text", "text")
                                .createField()) // type
                        //objectPart
                        .createField()) // languageTerm
                .addField(NdkForms.createLangTermValue()
                        .createField()) // value
                // scriptTerm
                .createField(); // language
    }

    private Field physicalDescription() {
        // physicalDescription, physicalDescriptionDefinition
        return new FieldBuilder("physicalDescription").setTitle("Physical Description - M").setMaxOccurrences(10)
                .setHint("Obsahuje údaje o fyzickém popisu zdroje/předlohy.")
                // form, formDefinition extends stringPlusLanguagePlusAuthority
                // reformattingQuality
                // internetMediaType
                // digitalOrigin
                // extent, stringPlusLanguagePlusSupplied
                .addField(new FieldBuilder("extent").setTitle("Extent - M").setMaxOccurrences(5).setType(Field.TEXT)
                        .setHint("Údaje o rozsahu.")
                        .createField()) // extent
                // note, physicalDescriptionNote extends stringPlusLanguage
                .createField(); // physicalDescription
    }

    private Field abstracts() {
        // abstract, abstractDefinition extends stringPlusLanguage
        return new FieldBuilder("abstract").setTitle("Abstract - R").setMaxOccurrences(10)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @xlink:simpleLink, @shareable, @altRepGroup
                // altFormatAttributeGroup: @altFormat, @contentType
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                        .setHint("Shrnutí obsahu jako celku. Odpovídá poli 520 MARC21")
                        .createField()) // value
                .createField(); // abstract
    }

    private Field tableOfContents() {
        // tableOfContents, TableOfContentsDefinition extends StringPlusLanguage
        return new FieldBuilder("tableOfContents").setTitle("Table Of Contents - MA").setMaxOccurrences(10)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @xlink:simpleLink, @shareable, @altRepGroup
                // altFormatAttributeGroup: @altFormat, @contentType
                .addField(new FieldBuilder("displayLabel").setTitle("Display Label - M").setMaxOccurrences(1).setType(Field.COMBO)
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

    private Field note() {
        // note, noteDefinition extends stringPlusLanguage
        return new FieldBuilder("note").setTitle("Note - RA").setMaxOccurrences(10)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @typeURI, @xlink:simpleLink, @ID, @altRepGroup
                .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.COMBO)
                        .setHint("Upřesnění obsahu poznámky.")
                        .addMapValue("statement of responsibility", "statement of responsibility")
                        .addMapValue("creation/production credits", "creation/production credits")
                        .addMapValue("performers", "performers")
                        .addMapValue("venue", "venue")
                        .addMapValue("language", "language")
                        //.addMapValue("version identification", "version identification")
                        .createField()) // type
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                        .setHint("Obecná poznámka k titulu periodika jako celku."
                                + "<p>Odpovídá hodnotám v poli 245, podpole „c“ (statement of responsibility)"
                                + " a v polích 5XX (poznámky) katalogizačního záznamu")
                        .createField()) // value
                .createField(); // note
    }

    private Field subject() {
        // subject, subjectDefinition
        return new FieldBuilder("subject").setTitle("Subject - R").setMaxOccurrences(10)
                .setHint("Údaje o věcném třídění.")
                // @ID, @authorityAttributeGroup, @languageAttributeGroup, @xlink:simpleLink, @displayLabel, @altRepGroup, @usage
                // topic, stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("topic").setMaxOccurrences(1)
                        // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
//                    // XXX autofill "marcform"
//                    .addField(new FieldBuilder("authority").setTitle("Authority").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200").createField())
                        // @type
                        // XXX autority.nkp.cz datasource
                        .addField(new FieldBuilder("value").setTitle("Topic - M").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Libovolný výraz specifikující nebo charakterizující obsah periodika."
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

                // @geographicCode
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
                            .addField(new FieldBuilder("value").setTitle("Name Part - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Jméno použité jako věcné záhlaví."
                                    + "<p>Použít kontrolovaný slovník ‐ např. z báze autorit AUT NK ČR (jméno osobní)"
                                    + " nebo obsah pole 600 záznamu MARC21."
                                    + "<p>Celé jméno se zapíše do tohoto elementu.")
                            .createField()) // value
                        // displayForm
                        // affiliation
                        // role
                        // description
                        .createField()) // name
                // hierarchicalGeographic
                // cartographics
                // occupation
                // genre
                .createField(); // subject
    }

    private Field classification() {
        // classification, classificationDefinition extends stringPlusLanguagePlusAuthority
        return new FieldBuilder("classification").setTitle("Classification - R").setMaxOccurrences(10)
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                // autofill "udc"
                .addField(new FieldBuilder("authority").setTitle("Authority - M").setMaxOccurrences(1).setType(Field.COMBO)
                        .addMapValue("udc", "udc")
                        .addMapValue("Konspekt", "Konspekt")
                        .createField())
                .addField(new FieldBuilder("edition").setTitle("Edition - M").setMaxOccurrences(1).setType(Field.COMBO)
                        .addMapValue("", "")
                        .addMapValue("Konspekt", "Konspekt")
                        .createField())
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Klasifikační údaje věcného třídění podle Mezinárodního"
                                + " desetinného třídění. Odpovídá poli 080 MARC21."
                                + "<p>Klasifikační údaje věcného třídění podle Konspektu."
                                + " Odpovídá poli 072 $a MARC21.")
                        .createField())
                .createField(); // classification
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
                              //  + "<br>čČNB - MA - převzít z katalogizačního záznamu z pole 015, podpole „a“, „z“"
                              //  + "<br>ISSN - MA - převzít z katalogizačního záznamu"
                              //  + "<br>URN:NBN - O - zápis ve tvaru urn:nbn:cz:ndk-123456 pro projekt NDK"
                                + "<br>jiný interní identifikátor - R - type = barcode, oclc, sysno, permalink apod.")
                        .addMapValue("uuid", "UUID")
                        .addMapValue("matrix number", "matrix number")
                        .addMapValue("issue number", "issue number")
                        .addMapValue("local", "local")
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

    private Field location() {
        // location, locationDefinition
        return new FieldBuilder("location").setTitle("Location - MA").setMaxOccurrences(10)
                .setHint("Údaje o uložení popisovaného dokumentu, např. signatura, místo uložení apod.")
                // languageAttributeGroup: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @altRepGroup
                // physicalLocation, physicalLocationDefinition extends stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("physicalLocation").setTitle("Physical Location - M").setMaxOccurrences(1)
                        // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                        // autofill "siglaADR"
                        .addField(new FieldBuilder("authority").setTitle("Authority - O").setMaxOccurrences(1).setType(Field.TEXT).setDefaultValue("siglaADR").createField())
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @xlink:simpleLink, @displayLabel, @type
                        .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                                .setHint("Údaje o instituci, kde je fyzicky uložen popisovaný dokument. Např. NK ČR."
                                        + "<p>Nutno použít kontrolovaný slovník - sigly knihovnen (ABA001 atd.)"
                                        + "<p>Odpovídá poli 910 $a v MARC21."
                                        + "<p>Pozn. u dokumentů v digitální podobě není možné vyplnit.")
                                .createField()) // value
                        .createField()) // physicalLocation
                // shelfLocator, stringPlusLanguage
                .addField(new FieldBuilder("shelfLocator").setTitle("Shelf Locator - M").setMaxOccurrences(10)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                                .setHint("Signatura nebo lokační údaje o daném konkrétním dokumentu, který slouží jako předloha.")
                                .createField()) // value
                        .createField()) // shelfLocator
                // url, urlDefinition extends xs:anyURI
                // holdingSimple
                // holdingExternal
                .createField(); // location
    }

    private Field recordInfo() {
        // recordInfo, recordInfoDefinition
        return new FieldBuilder("recordInfo").setTitle("Record Info - M").setMaxOccurrences(1)
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
                        .addField(new FieldBuilder("value").setTitle("Record Change Date - MA").setMaxOccurrences(1).setReadOnly(true).setType(Field.TEXT).createField())
                        .createField()) // recordChangeDate
                // recordIdentifier, type="recordIdentifierDefinition" extends stringPlusLanguage
                .addField(new FieldBuilder("recordIdentifier").setTitle("Record Identifier - R").setMaxOccurrences(1)
                        // lang, String
                        // xmlLang, lang
                        // script, String
                        // transliteration, String
                        // @source, string
                        .addField(new FieldBuilder("source").setTitle("Source - R").setMaxOccurrences(1).setType(Field.TEXT).createField())
                        .addField(new FieldBuilder("value").setTitle("Identifier - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Identifikátor záznamu v katalogu, přebírá se z pole 001.")
                                .createField())
                        .createField()) // recordIdentifier
                // recordOrigin, extends stringPlusLanguage
                .addField(new FieldBuilder("recordOrigin").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Record Origin - R").setMaxOccurrences(1).setType(Field.COMBO).setWidth("200")
                                .setHint("Údaje o vzniku záznamu.")
                                .addMapValue("machine generated", "machine generated")
                                .addMapValue("human prepared", "human prepared")
                                .createField()) // value
                        .createField()) // recordOrigin
                // languageOfCataloging, languageDefinition
                .addField(new FieldBuilder("languageOfCataloging").setTitle("Language of Cataloging - R").setMaxOccurrences(10)
                        // @objectPart, @displayLabel, @altRepGroup, @usage
                        // languageAttributeGroup: @lang, @xmlLang, @script, @transliteration
                        // languageTerm, languageTermDefinition
                        .addField(new FieldBuilder("languageTerm").setMaxOccurrences(1)
                                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                                // @authorityURI, @valueURI
                                // @authority, enum
                                .addField(new FieldBuilder("authority").setTitle("Authority - R").setMaxOccurrences(1).setType(Field.SELECT)
                                        .addMapValue("iso639-2b", "ISO 639-2B")
                                        .createField()) // authority
                                // type, codeOrText('code', 'text')
                                .addField(new FieldBuilder("type").setTitle("Type - R").setMaxOccurrences(1).setType(Field.SELECT)
                                        .addMapValue("code", "code")
                                        .addMapValue("text", "text")
                                        .createField()) // type
                                .addField(NdkForms.createLangTermValue()
                                        .setTitle("Language - R").setRequired(Boolean.FALSE)
                                        .createField()) // value
                                .createField()) // languageTerm
                        // scriptTerm
                        .createField()) // languageOfCataloging
                .addField(new FieldBuilder("descriptionStandard").setMaxOccurrences(1).setHidden(true).setType(Field.TEXT).createField()) //descriptionStandard
                .createField(); // recordInfo
    }
}
