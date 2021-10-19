/*
 * Copyright (C) 2021 Lukas Sykora
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
package cz.cas.lib.proarc.webapp.client.widget.mods.graphic;

import cz.cas.lib.proarc.webapp.client.widget.mods.NdkForms;
import cz.cas.lib.proarc.webapp.client.widget.mods.NdkMonographVolumeForm;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.List;

/**
 * The Graphic.
 *
 * @author Lukas Sykora
 */
public class GraphicForm {

    public Form buildComplexForm() {
        return new NdkMonographVolumeForm().build();
    }

    public Form build() {
        Form f = new Form();

        Field mods = new FieldBuilder("mods").setMaxOccurrences(1).createField();
        f.getFields().add(mods);
        List<Field> modsFields = mods.getFields();

        modsFields.add(genre(false));
        modsFields.add(titleInfo(true));
        modsFields.add(name());
        modsFields.add(originInfo(false));
        modsFields.add(subject());
        modsFields.add(location(false));
        modsFields.add(note());
        modsFields.add(identifier(true));
        modsFields.add(NdkForms.recordInfo());

        return f;
    }

    private Field titleInfo(boolean required) {
        // titleInfo, titleInfoDefinition
        return new FieldBuilder("titleInfo").setTitle("Title Info - M").setMaxOccurrences(10)
                .setHint("Název titulu.<p>Pro plnění použít katalogizační záznam.")
                // titleInfo@type, enum
                // title, type="stringPlusLanguage"
                .addField(new FieldBuilder("title").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Title - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(required)
                                .setHint("Název svazku monografie.")
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
                // partName, type="stringPlusLanguage"
                // nonSort, type="stringPlusLanguage"
                // titleInfo@attributes: otherType, supplied, altRepGroup, altFormatAttributeGroup, nameTitleGroup, usage, ID, authorityAttributeGroup, xlink:simpleLink, languageAttributeGroup, displayLabel
                .createField(); // titleInfo
    }

    private Field name() {
        // name, nameDefinition
        return new FieldBuilder("name").setMaxOccurrences(10).setTitle("Name - MA")
                .setHint("Údaje o odpovědnosti za svazek."
                        + "<p>Pokud má monografie autora a ilustrátora, element &lt;name> se opakuje s různými rolemi.")
                // @ID, @authorityAttributeGroup, @xlinkSimpleLink, @languageAttributeGroup, @displayLabel, @altRepGroup, @nameTitleGroup
                // @type(personal, corporate, conference, family)
                .addField(new FieldBuilder("type").setTitle("Type - MA").setMaxOccurrences(1).setType(Field.SELECT)
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
                // namePart, namePartDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("namePart").setTitle("Name Parts - M").setMaxOccurrences(5)
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
                        .addField(new FieldBuilder("value").setTitle("Name Part - M").setMaxOccurrences(1)
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
                // affiliation
                // role, roleDefinition
                // description
                .createField(); // name
    }

    private Field genre(boolean required) {
        // genre, genreDefinition extends stringPlusLanguagePlusAuthority extends stringPlusLanguage
        return new FieldBuilder("genre").setTitle("Genre - M").setMaxOccurrences(10)
                .setHint("Bližší údaje o typu dokumentu.<p>Pro grafiky hodnota “graphic”.")
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.COMBO).setRequired(required).setDefaultValue("graphic").addMapValue("graphic", "Grafika").createField())
                .addField(new FieldBuilder("type").setMaxOccurrences(1).setTitle("Specific genre").setType(Field.COMBO).setRequired(required).setDefaultValue("graphic")
                        .addMapValue("graphic", "Grafika")
                        .addMapValue("photo", "Fotografie")
                        .createField())
                .createField(); // genre
    }

    private Field originInfo(boolean required) {
        // originInfo, originInfoDefinition
        return new FieldBuilder("originInfo").setTitle("Origin Info - M").setMaxOccurrences(10)
                .setHint("Informace o původu předlohy.")
                // @languageAttributeGroup(lang, XmlLang, script, transliteration)
                // @displayLabel
                // @altRepGroup
                // eventType
                // place, placeDefinition
                .addField(new FieldBuilder("place").setTitle("Place - MA").setMaxOccurrences(10)
                        .setHint("Údaje o místě spojeném s vydáním, výrobou nebo původem popisovaného dokumentu.")
                        // @supplied
                        // placeTerm, placeTermDefinition extends stringPlusLanguage
                        .addField(new FieldBuilder("placeTerm").setMaxOccurrences(1)
                                // type, codeOrText('code', 'text')
                                // @authorityURI, @valueURI,@authority
                                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                                .addField(new FieldBuilder("value").setTitle("Place Term - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                        .setHint("Konkrétní určení místa a země vydání, např. Praha resp. xr pro ČR."
                                                + "<p>Odpovídá hodnotám z katalogizačního záznamu, pole 260, podpole „a“ resp. pole 008/15-17.")
                                        .createField()) // value
                                .createField()) // placeTerm
                        .createField()) // place
                // publisher, stringPlusLanguagePlusSupplied
                // dateIssued, dateDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("dateIssued").setTitle("Date Issued - M").setMaxOccurrences(10)
                        .setHint("Datum vydání předlohy."
                                + "<p>Odpovídá hodnotě z katalogizačního záznamu, pole 260, podpole „c“ a pole 008/07-10.")
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @encoding(w3cdtf, iso8601, marc, temper, edtf), @qualifier, @point(start, end), @keyDate
                        .addField(new FieldBuilder("value").setTitle("Date - M").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200")
                                .setHint("Datum vydání předlohy."
                                        + "<p>Odpovídá hodnotě z katalogizačního záznamu, pole 260, podpole „c“ a pole 008/07-10.")
                                .createField()) // value
                        .createField()) // dateIssued
                // dateOther, dateOtherDefinition extends dateDefinition
                // copyrightDate, dateDefinition extends stringPlusLanguage
                // dateCreated, dateDefinition extends stringPlusLanguage
                // dateCaptured
                // dateValid
                // dateModified
                // edition, type="stringPlusLanguagePlusSupplied"
                // issuance, issuanceDefinition, enum
                // frequency, stringPlusLanguagePlusAuthority
                .createField(); // originInfo
    }

    private Field note() {
        // note, noteDefinition extends stringPlusLanguage
        return new FieldBuilder("note").setTitle("Note - RA").setMaxOccurrences(30)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @typeURI, @xlink:simpleLink, @ID, @altRepGroup
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                        .setHint("Obecná poznámka k titulu jako celku."
                                + "<p>Odpovídá hodnotám v poli 245, podpole „c“ (statement of responsibility)"
                                + " a v polích 5XX (poznámky) katalogizačního záznamu")
                        .createField()) // value
                .createField(); // note
    }

    private Field subject() {
        // subject, subjectDefinition
        return new FieldBuilder("subject").setTitle("Subject - R").setMaxOccurrences(30)
                .setHint("Údaje o věcném třídění.")
                // @ID, @authorityAttributeGroup, @languageAttributeGroup, @xlink:simpleLink, @displayLabel, @altRepGroup, @usage
                // autofill "czenas"
                // topic, stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("topic").setMaxOccurrences(1)
                        // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
//                    // XXX autofill "marcform"
//                    .addField(new FieldBuilder("authority").setTitle("Authority").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200").createField())
                        // @type
                        // XXX autority.nkp.cz datasource
                        .addField(new FieldBuilder("value").setTitle("Topic - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Libovolný výraz specifikující nebo charakterizující obsah monografie."
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
                // temporal, temporalDefinition extends dateDefinition extends stringPlusLanguage
                // titleInfo, subjectTitleInfoDefinition
                // name, subjectNameDefinition
                // name

                // geographicCode
                // hierarchicalGeographic
                // cartographics
                // occupation
                // genre
                .createField(); // subject
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
                                + "<br>čČNB - MA - převzít z katalogizačního záznamu z pole 015, podpole „a“, „z“"
                                + "<br>ISBN - MA - převzít z katalogizačního záznamu z pole 020, podpole „a“, „z“"
                                + "<br>ISMN - MA - převzít z katalogizačního záznamu z pole 024 (1. ind.=“2“), podpole „a“, „z“"
                                + "<br>URN:NBN - M - zápis ve tvaru urn:nbn:cz:ndk-123456 pro projekt NDK"
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
                        .addMapValue("oclc", "OCLC")
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
                .addField(new FieldBuilder("physicalLocation").setTitle("Physical Location - M").setMaxOccurrences(1)
                        // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                        // autofill "siglaADR"
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
                // url, urlDefinition extends xs:anyURI
                // holdingSimple
                // holdingExternal
                .createField(); // location
    }
}
