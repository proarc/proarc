/*
 * Copyright (C) 2018 Martin Rumanek
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

package cz.cas.lib.proarc.webapp.client.widget.mods.eborn;

import cz.cas.lib.proarc.webapp.client.widget.mods.NdkArticleForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.NdkForms;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.List;

/**
 * {@link <a href="https://www.ndk.cz/standardy-digitalizace/dmf_eborn_perio#page=29">3.4.4. Pole MODS pro článek</a>}
 */
public class NdkEArticleForm extends NdkArticleForm {
    @Override
    public Form build() {
        Form f = new Form();

        f.getFields().add(new FieldBuilder("mods").setMaxOccurrences(1).createField()); // mods
        List<Field> modsFields = f.getFields().get(0).getFields();

        modsFields.add(titleInfo(f.getItemWidth()));
        modsFields.add(name());
        modsFields.add(genre());
        modsFields.add(language());
        modsFields.add(physicalDescription());
        modsFields.add(subject());
        modsFields.add(classification());
        modsFields.add(identifier());
        modsFields.add(location());
        modsFields.add(part());
        modsFields.add(relatedItem(f.getItemWidth()));

        return f;
    }

    private Field relatedItem(String width) {
        return new FieldBuilder("relatedItem").setTitle("Related Item - MA").setMaxOccurrences(10)
                .addField(relatedTitleInfo(width))
                .addField(relatedName())
                .addField(relatedOriginInfo())
                .addField(identifier())
                .createField();
    }

    private Field relatedTitleInfo(String width) {
        // titleInfo, titleInfoDefinition
        return new FieldBuilder("titleInfo").setMaxOccurrences(1)
                // title, type="stringPlusLanguage"
                .addField(new FieldBuilder("title").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setMaxOccurrences(1)
                                .setTitle("Title Info - M")
                                .setHint("Název recenzovaného díla. Odpovídá poli 787$t")
                                .setType(Field.TEXTAREA)
                                .setHeight("50")
                                .setWidth(width)
                                .createField()) // title/value
                        .createField()) // title
                .addField(new FieldBuilder("partNumber").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Part Number - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Pořadové číslo.")
                                .createField()) // value
                        .createField()) // partNumber
                // partName, type="stringPlusLanguage"
                .createField(); // titleInfo
    }

    private Field relatedName() {
        // name, nameDefinition
        return new FieldBuilder("name").setMaxOccurrences(10).setTitle("Name - M")
                .setHint("Autor recenzovaného díla ve tvaru: \"Příjmení, Jméno\". Odpovídá poli 787$a")
                // @ID, @authorityAttributeGroup, @xlinkSimpleLink, @languageAttributeGroup, @displayLabel, @altRepGroup, @nameTitleGroup
                // @type(personal, corporate, conference, family)
                .addField(new FieldBuilder("type").setTitle("Type - R").setMaxOccurrences(1).setType(Field.TEXT)
                        .setDefaultValue("personal")
                        .setHidden(true)
                        .createField()) // @type
                // @usage(fixed="primary")
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
                // etal
                // affiliation
                // role, roleDefinition
                // description
                .createField(); // name
    }

    private Field relatedOriginInfo() {
        return new FieldBuilder("originInfo").setMaxOccurrences(1)
                .addField(new FieldBuilder("publisher").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT)
                                .setTitle("Publisher - M")
                                .setHint("Vydavatel recenzovaného díla ve tvaru: \"město : nakladatelství, rok vydání\". Odpovídá poli 787$d.")
                                .createField()) // value
                        .createField()) // publisher
                .createField(); // originInfo
    }

    @Override
    protected Field titleInfo(String width) {
        // titleInfo, titleInfoDefinition
        return new FieldBuilder("titleInfo").setTitle("Title Info - M").setMaxOccurrences(10)
                .setHint("Názvová informace.")
                // titleInfo@type, enum
                .addField(new FieldBuilder("type").setTitle("Type - R").setMaxOccurrences(1).setType(Field.SELECT)
                        .setHint("Type.<dl>Hodnoty:"
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
                        .addField(new FieldBuilder("value").setTitle("Title - M").setMaxOccurrences(1).setType(Field.COMBO).setRequired(true).setWidth(width)
                                .setHint("Název článku.")
                                .createField()) // title/value
                        // lang, String
                        // xmlLang, lang
                        // script, String
                        // transliteration, String
                        .createField()) // title
                // subTitle, type="stringPlusLanguage"
                .addField(new FieldBuilder("subTitle").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Subtitle - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Podnázev článku.")
                                .createField()) // value
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .createField()) // subTitle
                // partNumber, type="stringPlusLanguage"
                .addField(new FieldBuilder("partNumber").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Part Number - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Pořadové číslo.")
                                .createField()) // value
                        .createField()) // partNumber
                // partName, type="stringPlusLanguage"
                .addField(new FieldBuilder("partName").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Part Name - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Název části článku.")
                                .createField()) // value
                        .createField()) // partName
                .addField(new FieldBuilder("nonSort").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Non sort - O").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Část názvu, která má být vynechána při vyhledávání (např. The)")
                                .createField()) // value
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .createField()) // nonSort
                // titleInfo@attributes: otherType, supplied, altRepGroup, altFormatAttributeGroup, nameTitleGroup, usage, ID, authorityAttributeGroup, xlink:simpleLink, languageAttributeGroup, displayLabel
                .createField(); // titleInfo
    }

    @Override
    protected Field name() {
        // name, nameDefinition
        return new FieldBuilder("name").setMaxOccurrences(10).setTitle("Name - MA")
                .setHint("Údaje o odpovědnosti za článek.")
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
                                .setHint("Údaje o křestním jméně a příjmení autora či názvu korporace."
                                        + "<p>Není-li možno rozlišit křestní jméno a příjmení,"
                                        + " nepoužije se type a jméno se zaznamená"
                                        + " v podobě jaké je do jednoho elementu &lt;namePart>")
                                .createField()) // value
                        .createField()) // namePart
                // displayForm
                // etal
                // affiliation
                .addField(new FieldBuilder("affiliation").setTitle("Affiliation - O")
                        .setHint("Slouží k uvedení instituce, ve které autor pracuje (např. Akademie věd)")
                        .setMaxOccurrences(5)
                        .addField(new FieldBuilder("value").setTitle("Affilition - O")
                                .setType(Field.TEXT).setMaxOccurrences(0).createField())
                        .createField())
                // TODO-MR add note? (violate mods 3.5): https://github.com/NLCR/Standard_NDK/issues/63#issuecomment-403295182
                // role, roleDefinition
                .addField(new FieldBuilder("role").setTitle("Role - MA").setMaxOccurrences(5)
                        .setHint("Specifikace role osoby nebo organizace uvedené v elementu &lt;name>")
                        // roleTerm, type="roleTermDefinition" extends stringPlusLanguagePlusAuthority
                        .addField(NdkForms.roleTerm(
                                "Role Term - MA", false, "Authority - M", false, "Type - M", false
                        )) // roleTerm
                        .createField()) // role
                // description
                .createField(); // name
    }

    @Override
    protected Field genre() {
        // genre, genreDefinition extends stringPlusLanguagePlusAuthority extends stringPlusLanguage
        //TODO-MR SWAP value and type?: https://github.com/NLCR/Standard_NDK/issues/63#issuecomment-403296963
        return new FieldBuilder("genre").setTitle("Genre - M")
                .setHint("Bližší údaje o typu dokumentu.")
                .setMaxOccurrences(1)
                // genreDefinition@attributes: type, displayLabel, altRepGroup, usage
                .addField(new FieldBuilder("type").setTitle("Type - R").setMaxOccurrences(1).setType(Field.COMBO).setWidth("200")
                        .addMapValue("news", "zpráva")
                        .addMapValue("table of content", "obsah")
                        .addMapValue("advertisement", "reklama")
                        .addMapValue("abstract", "abstrakt")
                        .addMapValue("introduction", "úvod")
                        .addMapValue("review", "recenze")
                        .addMapValue("dedication", "věnování")
                        .addMapValue("bibliography", "bibliografie")
                        .addMapValue("editorsNote", "úvodník")
                        .addMapValue("preface", "předmluva")
                        .addMapValue("main article", "hlavní článek")
                        .addMapValue("index", "rejstřík")
                        .addMapValue("unspecified", "nespecifikován")
                        .createField()) // @type
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                .addField(new FieldBuilder("value").setTitle("Genre - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).setDefaultValue("article")
                        .setHint("Bližší údaj o typu vnitřní části.<p>Hodnota „article“.")
                        .createField()) // value
                .createField(); // genre
    }

    @Override
    protected Field language() {
        // language, languageDefinition
        return new FieldBuilder("language").setTitle("Languages - R").setMaxOccurrences(10)
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
                                .setHint("Typ popisu. Použít hodnotu code.")
                                .addMapValue("code", "code")
                                .addMapValue("text", "text")
                                .createField()) // type
                        .addField(NdkForms.createLangTermValue()
                                .createField()) // value
                        .createField()) // languageTerm
                // scriptTerm
                .createField(); // language
    }

    @Override
    protected Field physicalDescription() {
        // physicalDescription, physicalDescriptionDefinition
        return new FieldBuilder("physicalDescription").setTitle("Physical Description - R").setMaxOccurrences(10)
                .setHint("Obsahuje údaje o fyzickém popisu zdroje.")
                // form, formDefinition extends stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("form").setTitle("Form - RA").setMaxOccurrences(1)
                        // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @type, string
                        .addField(new FieldBuilder("value").setTitle("Form - RA").setMaxOccurrences(1).setType(Field.COMBO)
                                .setHint("Údaje o fyzické podobě dokumentu, např. electronic.")
                                .addMapValue("braille", "braille")
                                .addMapValue("electronic", "electronic")
                                .addMapValue("large print", "large print")
                                .addMapValue("microfilm", "microfilm")
                                .addMapValue("microfiche", "microfiche")
                                .addMapValue("print", "print")
                                .createField()) // value
                        .createField()) // form
                .addField(new FieldBuilder("note").setTitle("Note - O").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                                .setHint("Obecná poznámka k dokumentu.")
                                .createField()).createField())
                // reformattingQuality
                // internetMediaType
                // digitalOrigin
                .addField(new FieldBuilder("digitalOrigin").setTitle("Digital origin - M").setMaxOccurrences(1).setType(Field.TEXT).setDefaultValue("born digital")
                        .setHint("Indikátor zdroje digitálního dokumentu, hodnota born digital").createField())
                // digitalOrigin
                // extent, stringPlusLanguagePlusSupplied
                // note, physicalDescriptionNote extends stringPlusLanguage
                .createField(); // physicalDescription
    }

    @Override
    protected Field subject() {
        // subject, subjectDefinition
        return new FieldBuilder("subject").setTitle("Subject - RA").setMaxOccurrences(10)
                .setHint("Údaje o věcném třídění." +
                        "<p>Předpokládá se přebírání z katalogizačního záznamu.</p>")
                // @ID, @authorityAttributeGroup, @languageAttributeGroup, @xlink:simpleLink, @displayLabel, @altRepGroup, @usage
                .addField(new FieldBuilder("authority").setTitle("Authority - MA").setMaxOccurrences(1).setType(Field.COMBO)
                        .setHint("Odpovídá hodnotě v $2, Konspekt" +
                                "<p>Při použití volných klíčových slov atribut authority nepoužívat.</p>")
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
                        .addField(new FieldBuilder("value").setTitle("Topic - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Libovolný výraz specifikující nebo charakterizující obsah vnitřní Části."
                                        + "<p>Lze (není ovšem není nutno) použít kontrolovaný slovník - např. z báze autorit AUT NK ČR (věcné téma)"
                                        + " nebo obsah pole 650 záznamu MARC21 nebo obsah pole 072 $x.")
                                .createField()) // value
                        .createField()) // topic

                // geographic, stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("geographic").setMaxOccurrences(1)
                        // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @type
                        // XXX autority.nkp.cz datasource
                        .addField(new FieldBuilder("value").setTitle("Geographic - O").setMaxOccurrences(1).setType(Field.TEXT)
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
                // cartographics
                // occupation
                // genre
                .createField(); // subject
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
                .addField(new FieldBuilder("nameIdentifier").setTitle("Name Identifier - R").setMaxOccurrences(5)
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


    @Override
    protected Field classification() {
        // classification, classificationDefinition extends stringPlusLanguagePlusAuthority
        return new FieldBuilder("classification").setTitle("Classification - RA").setMaxOccurrences(10)
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                // autofill "udc"
                .addField(new FieldBuilder("authority").setTitle("Authority - M").setMaxOccurrences(1).setType(Field.COMBO)
                        .setHint("Vyplnit hodnotu \"udc\" (v případě 072 $a) " +
                                "<p>Vyplnit hodnotu \"Konskept\" (v případě 072 $9)</p")
                        .addMapValue("udc", "udc")
                        .addMapValue("Konspekt", "Konspekt")
                        .createField()) // authority
                .addField(new FieldBuilder("edition").setTitle("Edition - M").setMaxOccurrences(1).setType(Field.COMBO)
                        .setHint("Vyplnit hodnotu \"Konspekt\" (v případě 072 $a)")
                        .addMapValue("", "")
                        .addMapValue("Konspekt", "Konspekt")
                        .createField()) // edition
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Klasifikační údaje věcného třídění podle Konspektu."
                                + " Odpovídá poli 072 $a MARC21.")
                        .createField()) // value
                .createField(); // classification
    }

    private Field location() {
        // location, locationDefinition
        return new FieldBuilder("location").setTitle("Location - MA").setMaxOccurrences(10)
                .setHint("Informace o uložení dokumentu.")
                // languageAttributeGroup: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @altRepGroup
                // physicalLocation, physicalLocationDefinition extends stringPlusLanguagePlusAuthority
                // shelfLocator, stringPlusLanguage
                // url, urlDefinition extends xs:anyURI
                .addField(new FieldBuilder("url").setTitle("URL - MA").setMaxOccurrences(1)
                        // @dateLastAccessed, @displayLabel, @access(preview, raw object, object in context), @usage(primary display, primary)
                        // @note
                        .addField(new FieldBuilder("note").setTitle("Note - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Informace o vyžadovaném softwaru pro zobrazení dokumentu" +
                                        "<p>např. \"Adobe Acrobat Reader required\"</p>")
                                .createField()) // note
                        .addField(new FieldBuilder("usage").setTitle("Usage - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Hodnota \"primary\" v případě, že link vede k přímému zobrazení dokumentu.")
                                .createField()) // note
                        .addField(new FieldBuilder("value").setTitle("URL - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Odkaz na adresu dokumentu.")
                                .createField()) // value
                        .createField()) // url
                // holdingSimple
                // holdingExternal
                .createField(); // location
    }

    @Override
    protected Field part() {
        // part, type="partDefinition"
        return new FieldBuilder("part").setTitle("Part - RA").setMaxOccurrences(1)
                .setHint("Popis rozsahu.")
                // @ID, @type, @order, @displayLabel, @altRepGroup
                // @languageAttributeGroup(lang, XmlLang, script, transliteration)
                // detail, type="detailDefinition"
                // extent, type="extentDefinition"
                .addField(new FieldBuilder("extent").setTitle("Extent - MA").setMaxOccurrences(10)
                        .setHint("Upřesnění popisu části - rozsah na stránkách.")
                        // start, type="stringPlusLanguage"
                        .addField(new FieldBuilder("start").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setTitle("Start - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                        .setHint("První stránka, na které vnitřní část začíná.")
                                        .createField()) // value
                                .createField()) // start
                        // end, type="stringPlusLanguage"
                        .addField(new FieldBuilder("end").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setTitle("End - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                        .setHint("Poslední stránka, na které vnitřní část končí.")
                                        .createField()) // value
                                .createField()) // end
                        // total, type="xs:positiveInteger"
                        // list, type="stringPlusLanguage"
                        .createField()) // extent
                // date
                // text
                .createField(); // part
    }
}
