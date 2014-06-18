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
 * The NDK Periodical Issue.
 *
 * Version 1.5
 *
 * @author Jan Pokorsky
 */
public final class NdkPeriodicalIssueForm {

    public Form build() {
        Form f = new Form();

        f.getFields().add(new FieldBuilder("mods").setMaxOccurrences(1).createField()); // mods
        List<Field> modsFields = f.getFields().get(0).getFields();

        // titleInfo, titleInfoDefinition
        modsFields.add(new FieldBuilder("titleInfo").setTitle("Title Info - M").setMaxOccurrences(10)
                .setHint("Název titulu periodika, kterého je číslo součástí."
                    + "<p>Pro plnění použít katalogizační záznam nebo názvové autority.")
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
                        .setHint("Názvová informace - název titulu periodika.")
                    .createField()) // value
                    // lang, String
                    // xmlLang, lang
                    // script, String
                    // transliteration, String
                .createField()) // title
                // subTitle, type="stringPlusLanguage"
                .addField(new FieldBuilder("subTitle").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("Subtitle - RA").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Podnázev titulu periodika.")
                    .createField()) // value
                .createField()) // subTitle
                // partNumber, type="stringPlusLanguage"
                .addField(new FieldBuilder("partNumber").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("Part Number - MA").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                        .setHint("Pořadové číslo vydání (čísla), např. 40.<p>U ročenek číslo řady/edice.")
                    .createField()) // value
                .createField()) // partNumber
                // partName, type="stringPlusLanguage"
                .addField(new FieldBuilder("partName").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setTitle("Part Name - R").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Název edice nebo speciální řady, lze uvést i název tématického čísla nebo zvláštního vydání."
                            + "<p>Použití u ročenek, specializovaných periodik, tematických čísel nebo zvláštních vydání.")
                    .createField()) // value
                .createField()) // partName
                // nonSort, type="stringPlusLanguage"
                // titleInfo@attributes: otherType, supplied, altRepGroup, altFormatAttributeGroup, nameTitleGroup, usage, ID, authorityAttributeGroup, xlink:simpleLink, languageAttributeGroup, displayLabel
            .createField()); // titleInfo

        // part, type="partDefinition"
        modsFields.add(new FieldBuilder("part").setTitle("Part - O").setMaxOccurrences(1)
                .setHint("Popis části, pokud je svazek částí souboru.")
                // @ID, @type, @order, @displayLabel, @altRepGroup
                // @languageAttributeGroup(lang, XmlLang, script, transliteration)
                .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.TEXT).setDefaultValue("issue")
                    .setHint("Hodnota bude vždy „issue“.")
                .createField()) // type
                // detail, type="detailDefinition"
                .addField(new FieldBuilder("detail").setMaxOccurrences(1)
                    // @type, level
                    // number
                    // caption, type="stringPlusLanguage"
                    .addField(new FieldBuilder("caption").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Detail Caption - RA").setMaxOccurrences(1).setType(Field.COMBO)
                            .setHint("Text před označením čísla.")
                            .addMapValue("č.", "č.")
                            .addMapValue("část", "část")
                            .addMapValue("číslo", "číslo")
                            .addMapValue("No.", "No.")
                        .createField()) // value
                    .createField()) // caption
                    // title
                .createField()) // detail
                // extent, type="extentDefinition"
                // date
                // text
            .createField()); // part

        // name, nameDefinition
        modsFields.add(new FieldBuilder("name").setMaxOccurrences(10).setTitle("Name - MA")
                .setHint("Údaje o odpovědnosti za číslo periodika.<p>Použití u ročenek,"
                    + " specializovaných periodik, tematických čísel nebo zvláštních vydání.")
                // @ID, @authorityAttributeGroup, @xlinkSimpleLink, @languageAttributeGroup, @displayLabel, @altRepGroup, @nameTitleGroup
                // @type(personal, corporate, conference, family)
                .addField(new FieldBuilder("type").setTitle("Type - MA").setMaxOccurrences(1).setType(Field.SELECT).setRequired(true)
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
                // etal
                // affiliation
                // role, roleDefinition
                .addField(new FieldBuilder("role").setTitle("Role - MA").setMaxOccurrences(5)
                    .setHint("Specifikace role osoby nebo organizace uvedené v elementu &lt;name>")
                    // roleTerm, type="roleTermDefinition" extends stringPlusLanguagePlusAuthority
                    .addField(new FieldBuilder("roleTerm").setMaxOccurrences(1)
                        // @type, codeOrText(code, text)
                        .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("CODE")
                            .addMapValue("CODE", "code")
                            .addMapValue("TEXT", "text")
                        .createField()) // @type
                        // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("authority").setTitle("Authority - MA").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200").setDefaultValue("marcrelator")
                            .setHint("Údaje o kontrolovaném slovníku využitém k popisu role."
                                + "<p>K popisu výše uvedeného MARC seznamu nutno uvést authority=“marcrelator“.")
                        .createField()) // authority
                        .addField(new FieldBuilder("value").setTitle("Role Term - MA").setMaxOccurrences(1).setType(Field.COMBO).setWidth("200")
                            .setHint("Kód role z kontrolovaného slovníku.")
                            // XXX use http://www.loc.gov/marc/relators/relacode.html
                            .addMapValue("cre", "Creator")
                            .addMapValue("crp", "Correspondent")
                        .createField()) // value
                    .createField()) // roleTerm
                .createField()) // role
                // description
            .createField()); // name

        // genre, genreDefinition extends stringPlusLanguagePlusAuthority extends stringPlusLanguage
        modsFields.add(new FieldBuilder("genre").setTitle("Genre - M").setMaxOccurrences(1)
                // genreDefinition@attributes: type, displayLabel, altRepGroup, usage
                .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.COMBO).setRequired(true)
                    .setHint("Upřesnění typu čísla a jednotlivých vydání."
                        + "<dl>normal<dt></dt><dd>běžné vydání</dd>"
                        + "<dt>morning</dt><dd>ranní vydání</dd>"
                        + "<dt>afternoon</dt><dd>odpolední vydání</dd>"
                        + "<dt>evening</dt><dd>večerní vydání</dd>"
                        + "<dt>corrected</dt><dd>opravené vydání</dd>"
                        + "<dt>special</dt><dd>zvláštní vydání</dd>"
                        + "<dt>supplement</dt><dd>příloha - v případě, že se příloha periodického typu popisuje jako číslo</dd>"
                        + "<dt>sequence_X</dt><dd>pořadní vydání (sequence_1 = první vydání toho dne, sequence_2 = druhé vydání atd.)</dd>"
                        + "</dl>")
                    .addMapValue("normal", "běžné vydání")
                    .addMapValue("morning", "ranní vydání")
                    .addMapValue("afternoon", "odpolední vydání")
                    .addMapValue("evening", "večerní vydání")
                    .addMapValue("corrected", "opravené vydání")
                    .addMapValue("special", "zvláštní vydání")
                    .addMapValue("supplement", "příloha")
                    .addMapValue("sequence_", "sequence_")
                .createField()) // @type
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                .addField(new FieldBuilder("value").setTitle("Genre - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                    .setHint("Bližší údaje o typu dokumentu.<p>Hodnota “issue”.")
                .createField()) // value
        .createField()); // genre

        // originInfo, originInfoDefinition
        modsFields.add(new FieldBuilder("originInfo").setTitle("Origin Info - R").setMaxOccurrences(10)
                .setHint("informace o původu předlohy."
                    + "<p>Doporučené kde lze vyplnit (např. u ročenek, kde se vydavatel měnil)."
                    + "<p><b>Nepovinné pro deníky a běžná čísla periodik</b>"
                    + "<p>Poznámka: Jeden nebo více výskytů elementů se"
                    + " předpokládá pro vydavatele, další výskyt"
                    + " v případě nutnosti popsat tiskaře. Pokud je nutno"
                    + " vyjádřit tiskaře (pole 260 podpole „f“ a „e“ a „g“"
                    + " v MARC21), je nutno element <originInfo>"
                    + " opakovat s atributem transliteration=“printer“ a"
                    + " elementy <place>, <publisher>, <dateCreated>,"
                    + " které budou obsahovat údaje o tiskaři.")
                // @languageAttributeGroup(lang, XmlLang, script, transliteration)
                .addField(new FieldBuilder("transliteration").setTitle("Transliteration - O").setMaxOccurrences(1).setType(Field.COMBO)
                    .setHint("Atribut pro vyjádření tiskaře.")
                    .addMapValue("printer", "printer")
                .createField())
                // @displayLabel
                // @altRepGroup
                // @eventType
                // place, placeDefinition
                .addField(new FieldBuilder("place").setTitle("Place - MA").setMaxOccurrences(10)
                    .setHint("Údaje o místě spojeném s vydáním, výrobou nebo původem popisovaného dokumentu.")
                    // @supplied
                    // placeTerm, placeTermDefinition extends stringPlusLanguage
                    .addField(new FieldBuilder("placeTerm").setMaxOccurrences(1)
                        // type, codeOrText('code', 'text')
                        .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("TEXT")
                            .setHint("Typ popisu místa. Kódem nebo textově."
                                + "<p>Pokud má dokument více míst vydání v poli 260, podpole „a“, přebírají se ze záznamu všechna místa"
                                + "<li>“code” pro údaj z pole 008</li><li>“text” pro údaj z pole 260</li>")
                            .addMapValue("CODE", "code")
                            .addMapValue("TEXT", "text")
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
                .addField(new FieldBuilder("dateIssued").setTitle("Date Issued - MA").setMaxOccurrences(10)
                    .setHint("Datum vydání předlohy. V případě čísla datum"
                        + " dne, kdy vyšlo. Musí vyjádřit den, měsíc a rok, dle"
                        + " toho jaké údaje jsou k dispozici."
                        + "<p>Nutno zapsat v následujících podobách:"
                        + "<dl><dt>DD.MM.RRRR</dt><dd>pokud víme den, měsíc i rok vydání</dd>"
                        + "<dt>MM.RRRR</dt><dd>pokud víme jen měsíc a rok vydání</dd>"
                        + "<dt>RRRR</dt><dd>pokud víme pouze rok</dd>"
                        + "<dt>DD.‐DD.MM.RRRR</dt><dd>vydání pro více dní</dd>"
                        + "<dt>MM.‐MM.RRRR</dt><dd>vydání pro více měsíců</dd>"
                        + "</dl>")
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @encoding(w3cdtf, iso8601, marc, temper, edtf), @keyDate
                    .addField(new FieldBuilder("encoding").setTitle("Encoding - O").setMaxOccurrences(1).setType(Field.SELECT)
                        .setHint("Kódování - hodnota „marc“ jen u údaje z pole 008.")
                        .addMapValue("iso8601", "ISO 8601")
                        .addMapValue("edtf", "EDTF")
                        .addMapValue("marc", "MARC")
                        .addMapValue("temper", "temper")
                        .addMapValue("w3cdtf", "W3CDTF")
                    .createField()) // @encoding
                    // @point(start, end)
                    .addField(new FieldBuilder("point").setTitle("Point - O").setMaxOccurrences(1).setType(Field.SELECT)
                        .setHint("Hodnoty „start“ resp. „end“ jen u údaje z pole 008, pro rozmezí dat.")
                        .addMapValue("start", "start")
                        .addMapValue("end", "end")
                    .createField()) // @point
                    // @qualifier
                    .addField(new FieldBuilder("qualifier").setTitle("Qualifier - R").setMaxOccurrences(1).setType(Field.SELECT)
                        .setHint("Možnost dalšího upřesnění, hodnota „approximate“ pro data, kde nevíme přesný údaj.")
                        .addMapValue("approximate", "Approximate")
                        .addMapValue("inferred", "Inferred")
                        .addMapValue("questionable", "Questionable")
                    .createField()) // @qualifier
                    .addField(new FieldBuilder("value").setTitle("Date - MA").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).setWidth("200")
                        .setHint("Datum vydání předlohy. V případě čísla datum"
                            + " dne, kdy vyšlo. Musí vyjádřit den, měsíc a rok, dle"
                            + " toho jaké údaje jsou k dispozici."
                            + "<p>Nutno zapsat v následujících podobách:"
                            + "<dl><dt>DD.MM.RRRR</dt><dd>pokud víme den, měsíc i rok vydání</dd>"
                            + "<dt>MM.RRRR</dt><dd>pokud víme jen měsíc a rok vydání</dd>"
                            + "<dt>RRRR</dt><dd>pokud víme pouze rok</dd>"
                            + "<dt>DD.‐DD.MM.RRRR</dt><dd>vydání pro více dní</dd>"
                            + "<dt>MM.‐MM.RRRR</dt><dd>vydání pro více měsíců</dd>"
                            + "</dl>")
                    .createField()) // value
                .createField()) // dateIssued
                // dateCreated, dateDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("dateCreated").setTitle("Date Created - R").setMaxOccurrences(1)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @encoding, @qualifier, @point, @keyDate
                    .addField(new FieldBuilder("qualifier").setTitle("Qualifier - R").setMaxOccurrences(1).setType(Field.SELECT)
                        .setHint("Možnost dalšího upřesnění, hodnota „approximate“ pro data, kde nevíme přesný údaj.")
                        .addMapValue("approximate", "Approximate")
                        .addMapValue("inferred", "Inferred")
                        .addMapValue("questionable", "Questionable")
                    .createField())
                    .addField(new FieldBuilder("value").setTitle("Date - R").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200")
                        .setHint("Datum vytvoření předlohy."
                            + "<p>Bude použito pouze při popisu tiskaře, viz poznámka u"
                            + " elementu &lt;originInfo>."
                            + "<p>Odpovídá hodnotě z katalogizačního záznamu pole 260, podpole „g“")
                    .createField()) // value
                .createField()) // dateCreated
                // dateCaptured
                // dateValid
                // dateModified
                // copyrightDate
                // dateOther
                // edition
                // issuance
                // frequency
        .createField()); // originInfo

        // language, languageDefinition
        modsFields.add(new FieldBuilder("language").setTitle("Languages - M").setMaxOccurrences(10)
                .setHint("Údaje o jazyce dokumentu; v případě vícenásobného výskytu nutno element &lt;language> opakovat")
                // @objectPart, @displayLabel, @altRepGroup, @usage
                .addField(new FieldBuilder("objectPart").setTitle("Object Part - MA").setMaxOccurrences(1).setType(Field.COMBO).setWidth("300")
                    .setHint("Možnost vyjádřit jazyk konkrétní části svazku.")
                    .addMapValue("summary", "summary")
                    .addMapValue("table of contents", "table of contents")
                    .addMapValue("accompanying material", "accompanying material")
                    .addMapValue("translation", "translation")
                .createField()) // @objectPart
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
                        .addMapValue("CODE", "code")
                        .addMapValue("TEXT", "text")
                    .createField()) // type
                    .addField(NdkForms.createLangTermValue()
                    .createField()) // value
                .createField()) // languageTerm
                // scriptTerm
        .createField()); // language

        // physicalDescription, physicalDescriptionDefinition
        modsFields.add(new FieldBuilder("physicalDescription").setTitle("Physical Description - R").setMaxOccurrences(10)
                .setHint("Obsahuje údaje o fyzickém popisu zdroje/předlohy.")
                // form, formDefinition extends stringPlusLanguagePlusAuthority
                // reformattingQuality
                // internetMediaType
                // digitalOrigin
                // extent, stringPlusLanguagePlusSupplied
                .addField(new FieldBuilder("extent").setTitle("Extent - RA").setMaxOccurrences(5)
                    // stringPlusLanguagePlusSupplied: @supplied
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @unit
                    .addField(new FieldBuilder("unit").setTitle("Unit - O").setMaxOccurrences(1).setType(Field.TEXT).createField())
                    .addField(new FieldBuilder("value").setTitle("Extent - RA").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Údaje o rozsahu (stran, svazků nebo rozměrů)"
                                + "<p>Odpovídá hodnotě v poli 300, podpole „a“, „b“ a „c“"
                                + "<p>Počet stránek bude vyjádřen ve fyzické strukturální mapě")
                    .createField()) // value
                .createField()) // extent
                // note, physicalDescriptionNote extends stringPlusLanguage
                .addField(new FieldBuilder("note").setTitle("Note - RA").setMaxOccurrences(5)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @displayLabel, @type, @typeURI, @xlinkSimpleLink, @ID
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                        .setHint("Poznámka o fyzickém stavu dokumentu."
                            + "<p>Pro každou poznámku je nutno vytvořit nový &lt;note> element.")
                    .createField()) // value
                .createField()) // note
        .createField()); // physicalDescription

        // abstract, abstractDefinition extends stringPlusLanguage
        modsFields.add(new FieldBuilder("abstract").setTitle("Abstract - R").setMaxOccurrences(10)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @xlink:simpleLink, @shareable, @altRepGroup
                // altFormatAttributeGroup: @altFormat, @contentType
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                    .setHint("Shrnutí obsahu dokumentu. Zvláště pro ročenky, zvláštní vydání a tematická čísla."
                        + "<p>Plnit pouze v případech, že se liší od abstraktu na úrovni titulu.")
                .createField()) // value
        .createField()); // abstract

        // note, noteDefinition extends stringPlusLanguage
        modsFields.add(new FieldBuilder("note").setTitle("Note - RA").setMaxOccurrences(10)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @typeURI, @xlink:simpleLink, @ID, @altRepGroup
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                    .setHint("Obecná poznámka k dokumentu.")
                .createField()) // value
        .createField()); // note

        // subject, subjectDefinition
        modsFields.add(new FieldBuilder("subject").setTitle("Subject - RA").setMaxOccurrences(10)
                .setHint("Údaje o věcném třídění.<p>Použití u ročenek, specializovaných periodik,"
                    + " tematických čísel nebo zvláštních vydání.")
                // @ID, @authorityAttributeGroup, @languageAttributeGroup, @xlink:simpleLink, @displayLabel, @altRepGroup, @usage
                // autofill "czenas"
                .addField(new FieldBuilder("authority").setTitle("Authority - RA").setMaxOccurrences(1).setType(Field.TEXT).setDefaultValue("czenas").createField())

                // topic, stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("topic").setMaxOccurrences(1)
                    // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    // @type
                    // XXX autority.nkp.cz datasource
                    .addField(new FieldBuilder("value").setTitle("Topic - R").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Libovolný výraz specifikující nebo charakterizující obsah čísla periodika."
                            + "<p>Použít kontrolovaný slovník - např. z báze autorit AUT NK ČR (věcné téma)"
                            + " nebo obsah pole 650 záznamu MARC21.")
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
                            + "<br>URN:NBN - M - zápis ve tvaru urn:nbn:cz:ndk-123456 pro projekt NDK"
                            + "<br>jiný interní identifikátor - R - type = barcode, oclc, sysno, permalink apod.")
                    // XXX use ValueMap
                    .addMapValue("barcode", "Čárový kód")
                    .addMapValue("ccnb", "čČNB")
                    .addMapValue("doi", "DOI")
                    .addMapValue("hdl", "Handle")
                    .addMapValue("isbn", "ISBN")
                    .addMapValue("issn", "ISSN")
                    .addMapValue("oclc", "OCLC")
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
        .createField()); // identifier

        // location, locationDefinition
        modsFields.add(new FieldBuilder("location").setTitle("Location - R").setMaxOccurrences(10)
                .setHint("Údaje o uložení popisovaného dokumentu, např. signatura, místo uložení apod.")
                // @languageAttributeGroup: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @altRepGroup
                // physicalLocation, physicalLocationDefinition extends stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("physicalLocation").setTitle("Physical Location - MA").setMaxOccurrences(1)
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
                .addField(new FieldBuilder("shelfLocator").setTitle("Shelf Locator - MA").setMaxOccurrences(10)
                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                    .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                        .setHint("Signatura nebo lokační údaje o daném konkrétním dokumentu, který slouží jako předloha.")
                    .createField()) // value
                .createField()) // shelfLocator
                // url, urlDefinition extends xs:anyURI
                .addField(new FieldBuilder("url").setTitle("URL - O").setMaxOccurrences(10)
                    // @dateLastAccessed, @displayLabel, @access(preview, raw object, object in context), @usage(primary display, primary)
                    // @note
                    .addField(new FieldBuilder("note").setTitle("Note - O").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Pro poznámku o typu URL (na plný text, abstrakt apod.)")
                    .createField()) // note
                    .addField(new FieldBuilder("value").setTitle("URL - O").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Pro uvedení lokace elektronického dokumentu.")
                    .createField())
                .createField()) // url
                // holdingSimple
                // holdingExternal
        .createField()); // location

        return f;
    }

}
