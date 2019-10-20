package cz.cas.lib.proarc.webapp.client.widget.mods.chronicle;

import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.webapp.client.widget.mods.NdkForms;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;

import java.util.List;

/**
 * Chronicle supplement of chronicle volume.
 *
 * @author Lukas Sykora
 */
public final class ChronicleSupplementForm {

    public Form build() {
        Form f = new Form();

        f.getFields().add(new FieldBuilder("rdaRules").setTitle("Zvolte pravidla popisu (Description Standard) - MA").setMaxOccurrences(1)
                .setType(Field.RADIOGROUP).setRequired(true)
                .addMapValue("true", ModsConstants.VALUE_DESCRIPTIONSTANDARD_RDA)
                .addMapValue("false", ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR)
                .createField());

        Field mods = new FieldBuilder("mods").setMaxOccurrences(1).createField();
        f.getFields().add(mods);
        List<Field> modsFields = mods.getFields();

        modsFields.add(titleInfo());
        modsFields.add(name());
        //modsFields.add(typeOfResource());
        modsFields.add(genre());
        modsFields.add(originInfo());
        modsFields.add(language());
        modsFields.add(physicalDescription());
        //modsFields.add(abstracts());
        modsFields.add(note());
        //modsFields.add(subject());
        //modsFields.add(classification());
        modsFields.add(identifier());
        modsFields.add(recordInfo());

        return f;
    }

    private Field titleInfo() {
        // titleInfo, titleInfoDefinition
        return new FieldBuilder("titleInfo").setTitle("Title Info - M").setMaxOccurrences(10)
                .setHint("Názvová informace přílohy.")
                // titleInfo@type, enum
//                .addField(new FieldBuilder("type").setTitle("Typ").setMaxOccurrences(1).setType(Field.SELECT)
//                    .addMapValue("abbreviated", "Abbreviated")
//                    .addMapValue("alternative", "Alternative")
//                    .addMapValue("translated", "Translated")
//                    .addMapValue("uniform", "Uniform")
//                .createField())
                // title, type="stringPlusLanguage"
                .addField(new FieldBuilder("title").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Title - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                                .setHint("Název svazku kroniky, jehož je příloha součástí.")
                                .createField()) // value
                        // lang, String
                        // xmlLang, lang
                        // script, String
                        // transliteration, String
                        .createField()) // title
                // subTitle, type="stringPlusLanguage"
//                .addField(new FieldBuilder("subTitle").setMaxOccurrences(1)
//                    .addField(new FieldBuilder("value").setTitle("Podnázev").setMaxOccurrences(1).setType(Field.TEXT).createField())
//                    // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
//                .createField()) // subTitle
                // partNumber, type="stringPlusLanguage"
                .addField(new FieldBuilder("partNumber").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Part Number - MA").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                                .setHint("Číslo přílohy.")
                                .createField()) // value
                        .createField()) // partNumber
                // partName, type="stringPlusLanguage"
                .addField(new FieldBuilder("partName").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Part Name - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Název přílohy.")
                                .createField()) // value
                        .createField()) // partName
                // nonSort, type="stringPlusLanguage"
                // titleInfo@attributes: otherType, supplied, altRepGroup, altFormatAttributeGroup, nameTitleGroup, usage, ID, authorityAttributeGroup, xlink:simpleLink, languageAttributeGroup, displayLabel
                .createField(); // titleInfo
    }

    private Field name() {
        // name, nameDefinition
        return new FieldBuilder("name").setMaxOccurrences(10).setTitle("Name - MA")
                .setHint("Údaje o odpovědnosti za přílohu.")
                // @ID, @authorityAttributeGroup, @xlinkSimpleLink, @languageAttributeGroup, @displayLabel, @altRepGroup, @nameTitleGroup
                // @type(personal, corporate, conference, family)
                .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.SELECT).setRequired(true)
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
                .addField(new FieldBuilder("namePart").setTitle("Name Part - M").setMaxOccurrences(5).setRequired(true)
                        // @type(date, family, given, termsOfAddress)
                        .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.SELECT)
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
                        .addField(new FieldBuilder("value").setTitle("Name Part - M").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Údaje o křestním jméně, příjmení apod."
                                        + "<p>Nutno vyjádřit pro křestní jméno i příjmení."
                                        + "<p>Pokud nelze rozlišit křestní jméno a příjmení,"
                                        + " nepoužije se type a jméno se zaznamená"
                                        + " v podobě jaké je do jednoho elementu &lt;namePart>")
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
                .addField(new FieldBuilder("nameIdentifier").setTitle("Name Identifier - RA").setMaxOccurrences(5)
                        .addField(new FieldBuilder("value").setMaxOccurrences(1)
                                .setType(Field.TEXT).setRequired(false).setHint("Číslo národní autority").createField())
                        .createField()) //nameIdentifier
                .addField(new FieldBuilder("role").setTitle("Role - MA").setMaxOccurrences(5)
                        .setHint("Specifikace role osoby nebo organizace uvedené v elementu &lt;name>")
                        // roleTerm, type="roleTermDefinition" extends stringPlusLanguagePlusAuthority
                        .addField(NdkForms.roleTerm(
                                "Role Term - MA", false, "Authority - R", false, "Type - M", false
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
                        .setType(Field.SELECT).setWidth("200")
                        .setHint("Popis charakteristiky typu nebo obsahu zdroje.")
                        .addMapValue("text", "text: časopis, kniha, brožura apod.")
                        .addMapValue("cartographic", "mapa")
                        .addMapValue("notated music", "notated music")
                        .addMapValue("sound recording‐musical", "hudební CD/DVD")
                        .addMapValue("sound recording‐nonmusical", "sound recording‐nonmusical")
                        .addMapValue("sound recording", "sound recording")
                        .addMapValue("still image", "fotografie, plakáty apod.")
                        .addMapValue("moving image", "filmová DVD")
                        .addMapValue("three dimensional object", "3D objekt")
                        .addMapValue("software, multimedia", "CD/DVD se software")
                        .addMapValue("mixed material", "mixed material")
                        .createField())
                .createField(); // typeOfResource
    }

    private Field genre() {
        // genre, genreDefinition extends stringPlusLanguagePlusAuthority extends stringPlusLanguage
        return new FieldBuilder("genre").setMaxOccurrences(1)
                // genreDefinition@attributes: type, displayLabel, altRepGroup, usage
                // stringPlusLanguagePlusAuthority: authorityAttributeGroup
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                .addField(new FieldBuilder("value").setTitle("Genre - M").setMaxOccurrences(1)
                        .setType(Field.COMBO).setRequired(true)
                        .setHint("Bližší údaje o typu dokumentu.<p>Hodnota „supplement“.")
                        .addMapValue("supplement", "supplement")
                        .createField()) // value
                .createField(); // genre
    }

    private Field originInfo() {
        // originInfo, originInfoDefinition
        return new FieldBuilder("originInfo").setTitle("Origin Info - MA").setMaxOccurrences(10)
                .setHint("Informace o původu přílohy."
                        + "<p>Plnit pokud se liší od údajů v popisu svazku kroniky."
                        + "<p>Poznámka: Jeden nebo více výskytů elementů se"
                        + " předpokládá pro vydavatele, další výskyt"
                        + " v případě nutnosti popsat tiskaře. Pokud je nutno"
                        + " vyjádřit tiskaře (pole 260 podpole „f“ a „e“ a „g“"
                        + " v MARC21), je nutno element <originInfo>"
                        + " opakovat s atributem transliteration=“printer“ a"
                        + " elementy <place>, <publisher>, <dateCreated>,"
                        + " které budou obsahovat údaje o tiskaři.")
                // eventType
                .addField(new FieldBuilder("eventType").setTitle("Event Type - M").setMaxOccurrences(1). setType(Field.COMBO)
                        .setHint("Hodnoty dle druhého indikátoru pole 264:"
                                +"<p>264_0 production (R) se uvádí, jestliže pole obsahuje údaje o vytvoření zdroje v nezveřejněné podobě."
                                +"<p>264_1 publication (R) se uvádí, jestliže pole obsahuje údaje o nakladateli zdroje."
                                +"<p>264_2 distribution (R) se uvádí, jestliže pole obsahuje údaje o distribuci zdroje."
                                +"<p>264_3 manufacture (R) se uvádí, jestliže pole obsahuje údaje o tisku, výrobě zdroje ve zveřejněné podobě."
                                +"<p>264_4 copyright (R) se uvádí, jestliže pole obsahuje údaje o ochraně podle autorského práva (copyright).")
                        .addMapValue("", "")
                        .addMapValue(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PRODUCTION, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PRODUCTION)
                        .addMapValue(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PUBLICATION, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_PUBLICATION)
                        .addMapValue(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_DISTRIBUTION, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_DISTRIBUTION)
                        .addMapValue(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_MANUFACTURE, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_MANUFACTURE)
                        .addMapValue(ModsConstants.VALUE_ORIGININFO_EVENTTYPE_COPYRIGHT, ModsConstants.VALUE_ORIGININFO_EVENTTYPE_COPYRIGHT)
                        .createField()) // eventType
                // @languageAttributeGroup(lang, XmlLang, script, transliteration)
                .addField(new FieldBuilder("transliteration").setTitle("Transliteration - O").setMaxOccurrences(1).setType(Field.COMBO)
                        .setHint("Atribut pro vyjádření tiskaře.")
                        .addMapValue("printer", "printer")
                        .createField()) // transliteration
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
                                .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("text")
                                        .setHint("Typ popisu místa. Kódem nebo textově."
                                                + "<p>Pokud má dokument více míst vydání v poli 260, podpole „a“, přebírají se ze záznamu všechna místa"
                                                + "<li>“code” pro údaj z pole 008</li><li>“text” pro údaj z pole 260</li>")
                                        .addMapValue("code", "code")
                                        .addMapValue("text", "text")
                                        .createField()) // type
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
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @encoding, @qualifier, @point, @keyDate
                        .addField(new FieldBuilder("qualifier").setTitle("Qualifier - O").setMaxOccurrences(1).setType(Field.SELECT)
                                .setHint("Možnost dalšího upřesnění, hodnota „approximate“ pro data, kde nevíme přesný údaj. Hodnota  „inferred“ pro odvozený nebo dopočítaný údaj")
                                .addMapValue("approximate", "Approximate")
                                .addMapValue("inferred", "Inferred")
                                .createField()) // @qualifier
                        .addField(new FieldBuilder("value").setTitle("Date - MA").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200")
                                .setHint("Datum vydání přílohy.")
                                .createField()) // value
                        .createField()) // dateIssued
                // dateOther, dateOtherDefinition extends dateDefinition
                .addField(new FieldBuilder("dateOther").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Date Other - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Datum vytvoření, distribuce, výroby předlohy."
                                        + "<p>Tento elemet se využije v případě výskytu $c v:"
                                        + "<p>264_0 je production"
                                        + "<p>264_2 je distribution"
                                        + "<p>264_3 je manufacture")
                                .createField()) // value
                        .createField()) // dateOther
                // copyrightDate, dateDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("copyrightDate").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Copyright Date - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Využije se pouze v případě výskuytu pole 264 s druhým indikátorem \"4\" a podpolem $c.")
                                .createField()) // value
                        .createField()) // copyrightDate
                // dateCreated, dateDefinition extends stringPlusLanguage
                .addField(new FieldBuilder("dateCreated").setTitle("Date Created - R").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @encoding, @qualifier, @point, @keyDate
                        .addField(new FieldBuilder("qualifier").setTitle("Qualifier - O").setMaxOccurrences(1).setType(Field.SELECT)
                                .setHint("Možnost dalšího upřesnění, hodnota „approximate“ pro data, kde nevíme přesný údaj. Hodnota  „inferred“ pro odvozený nebo dopočítaný údaj")
                                .addMapValue("approximate", "Approximate")
                                .addMapValue("inferred", "Inferred")
                                .createField()) // @qualifier
                        .addField(new FieldBuilder("value").setTitle("Date - R").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).setWidth("200")
                                .setHint("Datum vytvoření přílohy."
                                        + "<p>Bude použito pouze při popisu tiskaře, viz poznámka"
                                        + " u elementu &lt;originInfo> nebo např. u popisu CD/DVD apod."
                                        + "<p>Odpovídá hodnotě z katalogizačního záznamu, pole 260, podpole „g“.")
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
                .addField(new FieldBuilder("frequency").setTitle("Frequencies - RA").setMaxOccurrences(5)
                        // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("authority").setTitle("Authority - R").setMaxOccurrences(1).setType(Field.COMBO)
                                .setHint("Hodnota “marcfrequency” u údajů z pole 008 pro kontrolovaný slovník")
                                .addMapValue("marcfrequency", "marcfrequency")
                                .createField()) // authority
                        .addField(new FieldBuilder("value").setTitle("Frequency - RA").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Údaje o pravidelnosti vydávání.<p>Odpovídá údaji MARC21 v poli 310 nebo pozici 18 v poli 008.")
                                .createField())
                        .createField()) // frequency
                .createField(); // originInfo
    }

    private Field language() {
        // language, languageDefinition
        return new FieldBuilder("language").setTitle("Languages - M").setMaxOccurrences(10)
                .setHint("Údaje o jazyce dokumentu; v případě vícenásobného výskytu nutno element &lt;language> opakovat")
                // @objectPart, @displayLabel, @altRepGroup, @usage
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
                                .createField())
                        // type, codeOrText('code', 'text')
                        .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1)
                                .setType(Field.SELECT).setRequired(true)
                                .setHint("Typ popisu.")
                                .addMapValue("code", "code")
                                .addMapValue("text", "text")
                                .createField())
                        .addField(NdkForms.createLangTermValue()
                                .createField()) // value
                        .createField()) // languageTerm
                // scriptTerm
                .createField(); // language
    }

    private Field physicalDescription() {
        // physicalDescription, physicalDescriptionDefinition
        return new FieldBuilder("physicalDescription").setTitle("Physical Description - M").setMaxOccurrences(10)
                .setHint("Obsahuje údaje o fyzickém popisu.")
                // form, formDefinition extends stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("form").setTitle("Form - M").setMaxOccurrences(1)
                        // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @type
                        .addField(new FieldBuilder("authority").setTitle("Authority - MA").setMaxOccurrences(1).setType(Field.COMBO)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCFORM, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCFORM)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCCATEGORY, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCCATEGORY)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCSMD, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCSMD)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_GMD, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_GMD)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDAMEDIA, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDAMEDIA)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDACARRIER, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDACARRIER)
                                .createField()) // authority
                        .addField(new FieldBuilder("value").setTitle("Form - M").setMaxOccurrences(1)
                                .setType(Field.COMBO).setRequired(true).setHint("form")
                                .setHint("Údaje o fyzické podobě dokumentu, např. print, electronic, microfilm apod."
                                        + "<p>Odpovídá hodnotě v poli 008/23 a 008/29")
                                .addMapValue("braille", "braille")
                                .addMapValue("electronic", "electronic")
                                .addMapValue("large print", "large print")
                                .addMapValue("microfilm", "microfilm")
                                .addMapValue("microfiche", "microfiche")
                                .addMapValue("print", "print")
                                .addMapValue("jiný", "jiný")
                                .addMapValue("audio", "rdamedai - audio")
                                .addMapValue("počítač", "rdamedia - počítač")
                                .addMapValue("mikroforma", "rdamedia - mikroforma")
                                .addMapValue("mikroskop", "rdamedia - mikroskop")
                                .addMapValue("projekce", "rdamedia - projekce")
                                .addMapValue("stereograf", "rdamedia - stereograf")
                                .addMapValue("bez media", "rdamedia - bez media")
                                .addMapValue("video", "rdamedia - video")
                                .addMapValue("svazek", "rdacarrier - svazek")
                                .addMapValue("online zdroj", "rdacarrier - online zdroj")
                                .addMapValue("audiodisk", "rdacarrier - audiodisk")
                                .addMapValue("počítačový disk", "rdacarrier - počítačový disk")
                                .createField()) // value
                        .createField()) // form
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
                        .createField()) // note
                .createField(); // physicalDescription
    }

    private Field abstracts() {
        // abstract, abstractDefinition extends stringPlusLanguage
        return new FieldBuilder("abstract").setTitle("Abstract - RA").setMaxOccurrences(10)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @xlink:simpleLink, @shareable, @altRepGroup
                // altFormatAttributeGroup: @altFormat, @contentType
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA).setLength(2700)
                        .setHint("Shrnutí obsahu jako celku. Odpovídá poli 520 MARC21")
                        .createField()) // value
                .createField(); // abstract
    }

    private Field note() {
        // note, noteDefinition extends stringPlusLanguage
        return new FieldBuilder("note").setTitle("Note - RA").setMaxOccurrences(30)
                // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                // @displayLabel, @type, @typeURI, @xlink:simpleLink, @ID, @altRepGroup
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                        .setHint("Obecná poznámka k dokumentu.")
                        .createField()) // value
                .createField(); // note
    }

    private Field subject() {
        // subject, subjectDefinition
        return new FieldBuilder("subject").setTitle("Subject - R").setMaxOccurrences(30)
                .setHint("Údaje o věcném třídění.")
                // @ID, @authorityAttributeGroup, @languageAttributeGroup, @xlink:simpleLink, @displayLabel, @altRepGroup, @usage
                .addField(new FieldBuilder("authority").setTitle("Authority - R").setMaxOccurrences(1).setType(Field.COMBO)
                        .addMapValue("czenas", "czenas")
                        .addMapValue("eczenas", "eczenas")
                        .addMapValue("mednas", "mednas")
                        .addMapValue("czmesh", "czmesh")
                        .addMapValue("msvkth", "msvkth")
                        .addMapValue("agrovoc", "agrovoc")
                        .addMapValue("Konspekt", "Konspekt")
                        .createField()) // authority

                // topic, stringPlusLanguagePlusAuthority
                .addField(new FieldBuilder("topic").setMaxOccurrences(1)
                        // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        // @type
                        // XXX autority.nkp.cz datasource
                        .addField(new FieldBuilder("value").setTitle("Topic - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Libovolný výraz specifikující nebo charakterizující obsah přílohy."
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
                .addField(new FieldBuilder("authority").setTitle("Authority - M").setMaxOccurrences(1).setType(Field.COMBO)
                        .addMapValue("udc", "udc")
                        .addMapValue("Konspekt", "Konspekt")
                        .createField()) // authority
                .addField(new FieldBuilder("edition").setTitle("Edition - M").setMaxOccurrences(1).setType(Field.COMBO)
                        .addMapValue("", "")
                        .addMapValue("Konspekt", "Konspekt")
                        .createField()) // edition
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Klasifikační údaje věcného třídění podle Mezinárodního"
                                + " desetinného třídění. Odpovídá poli 080 MARC21."
                                + "<p>Klasifikační údaje věcného třídění podle Konspektu."
                                + " Odpovídá poli 072 $a MARC21.")
                        .createField()) // value
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
                                + "<br>čČNB - MA - převzít z katalogizačního záznamu z pole 015, podpole „a“, „z“"
                                + "<br>ISBN - MA - převzít z katalogizačního záznamu z pole 020 (1. ind.=“2“), podpole „a“, „z“"
                                + "<br>ISNM - MA - převzít z katalogizačního záznamu z pole 024 (1. ind.=“2“), podpole „a“, „z“"
                                + "<br>ISSN - MA - převzít z katalogizačního záznamu"
                                + "<br>jiný interní identifikátor - R - type = barcode, oclc, sysno, permalink apod.")
                        .addMapValue("barcode", "Čárový kód")
                        .addMapValue("ccnb", "čČNB")
                        .addMapValue("doi", "DOI")
                        .addMapValue("hdl", "Handle")
                        .addMapValue("isbn", "ISBN")
                        .addMapValue("isnm", "ISNM")
                        .addMapValue("issn", "ISSN")
                        .addMapValue("permalink", "Permalink")
                        .addMapValue("sici", "SICI")
                        .addMapValue("url", "URL")
                        .addMapValue("uuid", "UUID")
                        .addMapValue("oclc", "OCLC")
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

    private Field nameInSubject() {
        // name, nameDefinition
        return new FieldBuilder("name").setMaxOccurrences(10).setTitle("Name - R")
                .setHint("Údaje o odpovědnosti za svazek."
                        + "<p>Pokud má kronika autora a ilustrátora, element &lt;name> se opakuje s různými rolemi.")
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
                                        .addMapValue("rfc3066", "RFC 3066")
                                        .addMapValue("iso639-3", "ISO 639-3")
                                        .addMapValue("rfc4646", "RFC 4646")
                                        .addMapValue("rfc5646", "RFC 5646")
                                        .createField()) // authority
                                // type, codeOrText('code', 'text')
                                .addField(new FieldBuilder("type").setTitle("Type - R").setMaxOccurrences(1).setType(Field.SELECT)
                                        .addMapValue("code", "code")
                                        .addMapValue("text", "text")
                                        .createField())
                                .addField(NdkForms.createLangTermValue()
                                        .setTitle("Language - R").setRequired(Boolean.FALSE)
                                        .createField()) // value
                                .createField()) // languageTerm
                        // scriptTerm
                        .createField()) // languageOfCataloging

                // recordOrigin, extends stringPlusLanguage
                .addField(new FieldBuilder("recordOrigin").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Record Origin - R").setMaxOccurrences(1).setType(Field.COMBO).setWidth("200")
                                .setHint("Údaje o vzniku záznamu.")
                                .addMapValue("machine generated", "machine generated")
                                .addMapValue("human prepared", "human prepared")
                                .createField()) // value
                        .createField()) // recordChangeDate
                .addField(new FieldBuilder("recordInfoNote").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Record Info Note - O").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200")
                                .setHint("Poznámka k záznamu").createField())
                        .createField()) //recordInfoNote
                .addField(new FieldBuilder("descriptionStandard").setMaxOccurrences(1).setHidden(true).setType(Field.TEXT).createField()) //descriptionStandard
                .createField(); // recordInfo
    }

}
