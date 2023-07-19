/*
 * Copyright (C) 2023 Lukas Sykora
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

package cz.cas.lib.proarc.webapp.client.widget.mods.eborn;

import cz.cas.lib.proarc.webapp.client.widget.mods.NdkForms;
import cz.cas.lib.proarc.webapp.client.widget.mods.NdkPeriodicalIssueForm;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.List;

/**
 * {@link <a href="https://www.ndk.cz/standardy-digitalizace/dmf_eborn_perio#page=24">3.4.3. Pole MODS pro přílohu periodika</a>}
 */
public class NdkEPeriodicalSupplementForm extends NdkPeriodicalIssueForm {
    @Override
    public Form build() {
        Form f = new Form();

        f.getFields().add(NdkForms.descriptionRadioButton());

        Field mods = new FieldBuilder("mods").setMaxOccurrences(1).createField();
        f.getFields().add(mods);
        List<Field> modsFields = mods.getFields();

        // TODO-MR the field order specified by issue 225
        modsFields.add(titleInfo());
        modsFields.add(genre());
        modsFields.add(name());
        modsFields.add(originInfo());
        modsFields.add(language());
        modsFields.add(physicalDescription());
        modsFields.add(subject());
        modsFields.add(classification());
        modsFields.add(identifier());
        modsFields.add(location());
        modsFields.add(NdkForms.recordInfo());

        return f;
    }

    @Override
    protected Field titleInfo() {
        // titleInfo, titleInfoDefinition
        return new FieldBuilder("titleInfo").setTitle("Title Info - M").setMaxOccurrences(10)
                .setHint("Název titulu periodika, kterého je číslo součástí")
                // titleInfo@type, enum
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
                .addField(new FieldBuilder("nonSort").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Non sort - O").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Část názvu, která má být vynechána při vyhledávání (např. The)")
                                .createField()) // value
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .createField()) // nonSort
                // subTitle, type="stringPlusLanguage"
                .addField(new FieldBuilder("subTitle").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Subtitle - O").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Podnázev čísla periodika periodika, použije se v případě ročenky.")
                                .createField()) // value
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .createField()) // subTitle
                // partNumber, type="stringPlusLanguage"
                .addField(new FieldBuilder("partNumber").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Part Number - M").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Pořadové číslo vydání, např. 40; u ročenek číslo řady/edice.")
                                .createField()) // value
                        .createField()) // partNumber
                // partName, type="stringPlusLanguage"
                .addField(new FieldBuilder("partName").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Part Name - O").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Jméno edice nebo speciální řady, lze uvést i název tematického Čísla nebo zvláštního vydání." +
                                        "<p>Použití u ročenek, specializovaných periodik, tematických čísel nebo zvláštních vydání.</p>")
                                .createField()) // value
                        .createField()) // partName
                // nonSort, type="stringPlusLanguage"
                // titleInfo@attributes: otherType, supplied, altRepGroup, altFormatAttributeGroup, nameTitleGroup, usage, ID, authorityAttributeGroup, xlink:simpleLink, languageAttributeGroup, displayLabel
                .createField(); // titleInfo
    }

    @Override
    protected Field name() {
        // name, nameDefinition
        return new FieldBuilder("name").setMaxOccurrences(10).setTitle("Name - MA")
                .setHint("Údaje o odpovědnosti za číslo periodika.<p>Použití u ročenek,"
                        + " specializovaných periodik, tematických čísel nebo zvláštních vydání.")
                // @ID, @authorityAttributeGroup, @xlinkSimpleLink, @languageAttributeGroup, @displayLabel, @altRepGroup, @nameTitleGroup
                // @type(personal, corporate, conference, family)
                .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.SELECT)
                        // issue 219: not required
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
                .addField(new FieldBuilder("namePart").setTitle("Name Part - MA").setMaxOccurrences(5)
                        // @type(date, family, given, termsOfAddress)
                        .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.SELECT)
                                // issue: 612 not required
                                .setRequired(false)
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
                                .setHint("Údaje o křestním jméně, příjmení či názvu korporace."
                                        + "<p>Není-li možno rozlišit křestní jméno a příjmení,"
                                        + " nepoužije se atribut type a jméno se zaznamená"
                                        + " v podobě, jaké je, do jednoho &lt;namePart> elementu.")
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
                                "Role Term - MA", false, "Authority - M", false, "Type - M", false
                        )) // roleTerm
                        .createField()) // role
                // description
                .createField(); // name
    }

    @Override
    protected Field genre() {
        // genre, genreDefinition extends stringPlusLanguagePlusAuthority extends stringPlusLanguage
        return new FieldBuilder("genre").setTitle("Genre - M").setMaxOccurrences(1)
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
                        .setHint("Bližší údaje o typu dokumentu.<p>Hodnota “electronic_issue”.")
                        .createField()) // value
                .createField(); // genre
    }

    @Override
    protected Field originInfo() {
        // originInfo, originInfoDefinition
        return new FieldBuilder("originInfo").setTitle("Origin Info - MA").setMaxOccurrences(10)
                .setHint("informace o původu dokumentu."
                        + "<p>Doporučené tam, kde lze vyplnit).")
                // @languageAttributeGroup(lang, XmlLang, script, transliteration)
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
                                .addField(new FieldBuilder("type").setTitle("Type - O").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("text")
                                        .setHint("\"Text\" pro textový údaj místa/země vydání.")
                                        .addMapValue("code", "code")
                                        .addMapValue("text", "text")
                                        .createField()) // type
                                .addField(new FieldBuilder("authority").setTitle("Authority - O").setMaxOccurrences(1).setType(Field.COMBO)
                                        .setHint("Hodnota “marccountry” pro kódované údaje")
                                        .createField()) // @authority
                                .addField(new FieldBuilder("value").setTitle("Place Term - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                        .setHint("Konkrétní určení místa a země vydání, např. Brno")
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
                                .setHint("Jméno entity, která dokument vydala, vytiskla nebo jinak vyprodukovala.")
                                .createField()) // value
                        .createField()) // publisher
                // dateIssued, dateDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("dateIssued").setTitle("Date Issued - MA").setMaxOccurrences(10)
                        .setHint("Datum vydání dokumentu. Vyplňuje se ručně podle předlohy."
                                + "<p>Nutno zapsat v následujících podobách:"
                                + "<dl><dt>DD.MM.RRRR</dt><dd>pokud víme den, měsíc i rok vydání</dd>"
                                + "<dt>MM.RRRR</dt><dd>pokud víme jen měsíc a rok vydání</dd>"
                                + "<dt>RRRR</dt><dd>pokud víme pouze rok</dd>"
                                + "<dt>DD.‐DD.MM.RRRR</dt><dd>vydání pro více dní</dd>"
                                + "<dt>MM.‐MM.RRRR</dt><dd>vydání pro více měsíců</dd>"
                                + "</dl>")
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @encoding(w3cdtf, iso8601, marc, temper, edtf), @keyDate, @encoding
                        // @point(start, end)
                        .addField(new FieldBuilder("point").setTitle("Point - O").setMaxOccurrences(1).setType(Field.SELECT)
                                .setHint("Hodnoty „start“, resp. „end“ pro rozmezí dat.")
                                .addMapValue("start", "start")
                                .addMapValue("end", "end")
                                .createField()) // @point
                        // @qualifier
                        .addField(new FieldBuilder("qualifier").setTitle("Qualifier - O").setMaxOccurrences(1).setType(Field.SELECT)
                                .setHint("Možnost dalšího upřesnění, hodnota „approximate“ pro data, kde nevíme přesný údaj.")
                                .addMapValue("approximate", "Approximate")
                                .addMapValue("inferred", "Inferred")
                                .createField()) // @qualifier
                        .addField(new FieldBuilder("value").setTitle("Date - MA").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).setWidth("200")
                                .setHint("Datum vydání dokumentu. Vyplňuje se ručně podle předlohy."
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
                                .setHint("Možnost dalšího upřesnění, hodnota „approximate“ pro data, kde nevíme přesný údaj. Hodnota  „inferred“ pro odvozený nebo dopočítaný údaj")
                                .addMapValue("approximate", "Approximate")
                                .addMapValue("inferred", "Inferred")
                                .createField()) // @qualifier
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
                .createField(); // originInfo
    }

    @Override
    protected Field physicalDescription() {
        // physicalDescription, physicalDescriptionDefinition
        return new FieldBuilder("physicalDescription").setTitle("Physical Description - R").setMaxOccurrences(10)
                .setHint("Obsahuje údaje o fyzickém popisu zdroje/předlohy.")
                // form, formDefinition extends stringPlusLanguagePlusAuthority
                // reformattingQuality
                // internetMediaType
                // digitalOrigin
                // extent, stringPlusLanguagePlusSupplied
                .addField(new FieldBuilder("extent").setTitle("Extent - RA").setMaxOccurrences(5)
                        // stringPlusLanguagePlusSupplied: @supplied
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
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
                        .addField(new FieldBuilder("digitalOrigin").setTitle("Digital origin - M").setMaxOccurrences(1).setType(Field.TEXT).setDefaultValue("born digital")
                                .setHint("Indikátor zdroje digitálního dokumentu, hodnota born digital.").createField())
                        .createField()) // note
                .createField(); // physicalDescription
    }

    @Override
    protected Field subject() {
        // subject, subjectDefinition
        return new FieldBuilder("subject").setTitle("Subject - RA").setMaxOccurrences(10)
                .setHint("Údaje o věcném třídění.")
                // @ID, @authorityAttributeGroup, @languageAttributeGroup, @xlink:simpleLink, @displayLabel, @altRepGroup, @usage
                .addField(new FieldBuilder("authority").setTitle("Authority - MA")
                        .setHint("Odpovídá hodnotě v $2, Konspekt." +
                                "<p>Při použití volných klíčových slov atribut " +
                                "authority nepoužívat.</p>")
                        .setMaxOccurrences(1).setType(Field.COMBO)
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
                                .setHint("Libovolný výraz specifikující nebo charakterizující obsah vnitřní části."
                                        + "<p>Lze (není ovšem nutno) použít kontrolovaný slovník - např. z báze autorit AUT NK ČR (věcné téma)"
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
                .addField(NdkForms.geographicCode()) // geographicCode

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

    private Field classification() {
        // classification, classificationDefinition extends stringPlusLanguagePlusAuthority
        return new FieldBuilder("classification").setTitle("Classification - R").setMaxOccurrences(10)
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                // autofill "udc"
                .addField(new FieldBuilder("authority").setTitle("Authority - M")
                        .setHint("Vyplnit hodnotu \"udc\" (v případě 072 $a)." +
                                "<p>Vyplnit hodnotu \"Konspekt\" (v případě 072 $9).</p>")
                        .setMaxOccurrences(1).setType(Field.COMBO)
                        .addMapValue("udc", "udc")
                        .addMapValue("Konspekt", "Konspekt")
                        .createField()) // authority
                .addField(new FieldBuilder("edition").setTitle("Edition - M")
                        .setHint("Vyplnit hodnotu \"Konspekt\" (v případě 072 $a).")
                        .setMaxOccurrences(1).setType(Field.COMBO)
                        .addMapValue("", "")
                        .addMapValue("Konspekt", "Konspekt")
                        .createField()) // edition
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Klasifikační údaje věcného třídění podle Konspektu."
                                + " Odpovídá poli 072 $a MARC21.")
                        .createField()) // value
                .createField(); // classification
    }

    @Override
    protected Field location() {
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

    protected Field language() {
        // language, languageDefinition
        return new FieldBuilder("language").setTitle("Languages - R").setMaxOccurrences(10)
                .setHint("Údaje o jazyce dokumentu; v případě vícenásobného výskytu nutno element &lt;language> opakovat")
                // @objectPart, @displayLabel, @altRepGroup, @usage
                .addField(new FieldBuilder("objectPart").setTitle("Object Part - R").setMaxOccurrences(1).setType(Field.COMBO).setWidth("300")
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
                                .setType(Field.SELECT).setRequired(false)
                                .setHint("Použít hodnotu „iso639-2b“.")
                                .addMapValue("iso639-2b", "ISO 639-2B")
                                .addMapValue("rfc3066", "RFC 3066")
                                .addMapValue("iso639-3", "ISO 639-3")
                                .addMapValue("rfc4646", "RFC 4646")
                                .addMapValue("rfc5646", "RFC 5646")
                                .createField()) // authority
                        // type, codeOrText('code', 'text')
                        .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1)
                                .setType(Field.SELECT).setRequired(false).setDefaultValue("code")
                                .setHint("Použít hodnotu \"code\".")
                                .addMapValue("code", "code")
                                .addMapValue("text", "text")
                                .createField()) // type
                        .addField(new FieldBuilder("value").setTitle("Language - R").setMaxOccurrences(1)
                                .setType(Field.COMBO).setRequired(false)
                                .setHint("Přesné určení jazyka kódem.<p>Nutno použít kontrolovaný slovník ISO 639-2.")
                                .setOptionDataSource(new FieldBuilder("ndk.mods.languageTerms").setWidth("300")
                                                .addField(new FieldBuilder("title").setTitle("Name").createField())
                                                .addField(new FieldBuilder("value").setTitle("Language").createField())
                                                .addField(new FieldBuilder("type").setTitle("Type").createField())
                                                .addField(new FieldBuilder("authority").setTitle("Authority").createField())
                                                .createField(),
                                        "value", "type", "authority").createField()) // value) // value
                        .createField()) // languageTerm
                // scriptTerm
                .createField(); // language
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
}
