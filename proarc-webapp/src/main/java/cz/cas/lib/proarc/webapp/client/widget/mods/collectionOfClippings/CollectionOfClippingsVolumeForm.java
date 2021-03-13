package cz.cas.lib.proarc.webapp.client.widget.mods.collectionOfClippings;

import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.webapp.client.widget.mods.NdkForms;
import cz.cas.lib.proarc.webapp.client.widget.mods.NdkMonographVolumeForm;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.List;

public class CollectionOfClippingsVolumeForm {

    public Form buildComplexForm() {
        return new NdkMonographVolumeForm().build();
    }

    public Form build() {
        Form f = new Form();
        Field mods = new FieldBuilder("mods").setMaxOccurrences(1).createField();
        f.getFields().add(mods);
        List<Field> modsFields = mods.getFields();

        modsFields.add(titleInfo(true));
        modsFields.add(language(true));
        modsFields.add(physicalDescription(true));
        modsFields.add(note());
        modsFields.add(originInfo(true));
        modsFields.add(typeOfResource());
        modsFields.add(genre(true));
        modsFields.add(location(true));
        modsFields.add(identifier(true));
        return f;
    }

    private Field titleInfo(boolean required) {
        return new FieldBuilder("titleInfo").setTitle("Title Info - M").setMaxOccurrences(10)
                .setHint("Název titulu.<p>Pro plnění použít katalogizační záznam.")
                .addField(new FieldBuilder("title").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Title - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(required)
                                .setHint("Název svazku monografie.")
                                .createField()) // value
                        .createField()) // title
                .addField(new FieldBuilder("subTitle").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Subtitle - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Podnázev svazku monografie.")
                                .createField()) // value
                        .createField()) // subTitle
                .createField(); // titleInfo
    }

    private Field typeOfResource() {
        return new FieldBuilder("typeOfResource").setMaxOccurrences(1)
                .addField(new FieldBuilder("value").setTitle("Type of Resource - R").setMaxOccurrences(1).setType(Field.SELECT)
                        .setHint("Popis charakteristiky typu nebo obsahu zdroje.<p>Pro monografie hodnota „text“.")
                        .addMapValue("text", "text")
                        .createField()) // value
                .createField(); // typeOfResource
    }

    private Field genre(boolean required) {
        return new FieldBuilder("genre").setTitle("Genre - M").setMaxOccurrences(10)
                .setHint("Bližší údaje o typu dokumentu.<p>Pro monografie hodnota “volume”.")
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(required).setDefaultValue("volume").createField())
                .createField(); // genre
    }

    private Field originInfo(boolean required) {
        return new FieldBuilder("originInfo").setTitle("Origin Info - M").setMaxOccurrences(10)
                .setHint("Informace o původu předlohy.")
                .addField(new FieldBuilder("place").setTitle("Place - MA").setMaxOccurrences(10)
                        .setHint("Údaje o místě spojeném s vydáním, výrobou nebo původem popisovaného dokumentu.")
                        .addField(new FieldBuilder("placeTerm").setMaxOccurrences(1)
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
                                .createField()) // placeTerm
                        .createField()) // place
                .addField(new FieldBuilder("dateIssued").setTitle("Date Issued - M").setMaxOccurrences(10)
                        .setHint("Datum vydání předlohy."
                                + "<p>Odpovídá hodnotě z katalogizačního záznamu, pole 260, podpole „c“ a pole 008/07-10.")
                        .addField(new FieldBuilder("encoding").setTitle("Encoding - R").setMaxOccurrences(1).setType(Field.SELECT)
                                .setHint("Kódování - hodnota „marc“ jen u údaje z pole 008.")
                                .addMapValue("iso8601", "ISO 8601")
                                .addMapValue("edtf", "EDTF")
                                .addMapValue("marc", "MARC")
                                .addMapValue("temper", "temper")
                                .addMapValue("w3cdtf", "W3CDTF")
                                .createField()) // @encoding
                        .addField(new FieldBuilder("point").setTitle("Point - MA").setMaxOccurrences(1).setType(Field.SELECT).setRequired(false)
                                .setHint("Hodnoty „start“ resp. „end“ jen u údaje z pole 008, pro rozmezí dat.")
                                .addMapValue("start", "start")
                                .addMapValue("end", "end")
                                .createField()) // @point
                        .addField(new FieldBuilder("qualifier").setTitle("Qualifier - R").setMaxOccurrences(1).setType(Field.SELECT)
                                .setHint("Možnost dalšího upřesnění, hodnota „approximate“ pro data, kde nevíme přesný údaj. Hodnota  „inferred“ pro odvozený nebo dopočítaný údaj")
                                .addMapValue("approximate", "Approximate")
                                .addMapValue("inferred", "Inferred")
                                .createField()) // @qualifier
                        .addField(new FieldBuilder("value").setTitle("Date - M").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200")
                                .setHint("Datum vydání předlohy."
                                        + "<p>Odpovídá hodnotě z katalogizačního záznamu, pole 260, podpole „c“ a pole 008/07-10.")
                                .createField()) // value
                        .createField()) // dateIssued
                .addField(new FieldBuilder("issuance").setTitle("Issuance - M").setMaxOccurrences(1).setType(Field.SELECT).setRequired(required)
                        .setHint("Údaje o vydávání.<p>Odpovídá hodnotě uvedené v návěští MARC21 na pozici 07.").setDefaultValue("monographic")
                        .addMapValue("monographic", "monographic")
                        .addMapValue("single unit", "single unit")
                        .addMapValue("multipart monograph", "multipart monograph")
                        .createField()) // issuance
                .createField(); // originInfo
    }

    private Field language(boolean required) {
        return new FieldBuilder("language").setTitle("Languages - M").setMaxOccurrences(10)
                .setHint("Údaje o jazyce dokumentu; v případě vícenásobného výskytu nutno element &lt;language> opakovat")
                .addField(new FieldBuilder("languageTerm").setMaxOccurrences(1)
                        .addField(new FieldBuilder("authority").setTitle("Authority - M").setMaxOccurrences(1)
                                .setType(Field.SELECT).setRequired(required).setDefaultValue("iso639-2b")
                                .setHint("Použít hodnotu „iso639-2b“.")
                                .addMapValue("iso639-2b", "ISO 639-2B")
                                .addMapValue("rfc3066", "RFC 3066")
                                .addMapValue("iso639-3", "ISO 639-3")
                                .addMapValue("rfc4646", "RFC 4646")
                                .addMapValue("rfc5646", "RFC 5646")
                                .createField()) // authority
                        .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1)
                                .setType(Field.SELECT).setRequired(required)
                                .setHint("Typ popisu.").setDefaultValue("code")
                                .addMapValue("code", "code")
                                .addMapValue("text", "text")
                                .createField()) // type
                        .addField(NdkForms.createLangTermValue(required)
                                .createField()) // value
                        .createField()) // languageTerm
                .createField(); // language
    }

    private Field physicalDescription(boolean required) {
        return new FieldBuilder("physicalDescription").setTitle("Physical Description - M").setMaxOccurrences(10)
                .setHint("Obsahuje údaje o fyzickém popisu zdroje/předlohy.")
                .addField(new FieldBuilder("form").setTitle("Form - M").setMaxOccurrences(1)
                        .addField(new FieldBuilder("authority").setTitle("Authority - M").setMaxOccurrences(1).setType(Field.COMBO)
                                .setDefaultValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCFORM)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCFORM, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCFORM)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCCATEGORY, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCCATEGORY)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCSMD, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_MARCSMD)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_GMD, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_GMD)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDAMEDIA, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDAMEDIA)
                                .addMapValue(ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDACARRIER, ModsConstants.VALUE_PHYSICALDESCRIPTION_FORM_RDACARRIER)
                                .createField()) // authority
                        .addField(new FieldBuilder("value").setTitle("Form - M").setMaxOccurrences(1)
                                .setType(Field.COMBO).setRequired(required).setHint("form").setDefaultValue("print")
                                .setHint("Údaje o fyzické podobě dokumentu, např. print, electronic, microfilm apod."
                                        + "<p>Odpovídá hodnotě v poli 008/23")
                                .addMapValue("braille", "braille")
                                .addMapValue("electronic", "electronic")
                                .addMapValue("large print", "large print")
                                .addMapValue("microfilm", "microfilm")
                                .addMapValue("microfiche", "microfiche")
                                .addMapValue("print", "print")
                                .addMapValue("jiný", "jiný")
                                .addMapValue("audio", "rdamedia - audio")
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
                .addField(new FieldBuilder("note").setTitle("Note - RA").setMaxOccurrences(5)
                        .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                                .setHint("Poznámka o fyzickém stavu dokumentu."
                                        + "<p>Pro každou poznámku je nutno vytvořit nový &lt;note> element.")
                                .createField()) // value
                        .createField()) // note
                .createField(); // physicalDescription
    }

    private Field note() {
        return new FieldBuilder("note").setTitle("Note - RA").setMaxOccurrences(30)
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                        .setHint("Obecná poznámka k titulu jako celku."
                                + "<p>Odpovídá hodnotám v poli 245, podpole „c“ (statement of responsibility)"
                                + " a v polích 5XX (poznámky) katalogizačního záznamu")
                        .createField()) // value
                .createField(); // note
    }

    private Field identifier(boolean required) {
        return new FieldBuilder("identifier").setTitle("Identifier - M").setMaxOccurrences(10)
                .setHint("Údaje o identifikátorech.<p>Obsahuje unikátní identifikátory"
                        + " mezinárodní nebo lokální."
                        + "<p>Uvádějí se i neplatné resp. zrušené identifikátory - atribut invalid=“yes“.")
                .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.COMBO).setRequired(required)
                        .setHint("UUID - M - vygeneruje dodavatel"
                                + "<br>čČNB - MA - převzít z katalogizačního záznamu z pole 015, podpole „a“, „z“"
                                + "<br>ISBN - MA - převzít z katalogizačního záznamu z pole 020, podpole „a“, „z“"
                                + "<br>ISMN - MA - převzít z katalogizačního záznamu z pole 024 (1. ind.=“2“), podpole „a“, „z“"
                                + "<br>URN:NBN - M - zápis ve tvaru urn:nbn:cz:ndk-123456 pro projekt NDK"
                                + "<br>jiný interní identifikátor - R - type = barcode, oclc, sysno, permalink apod.")
                        .addMapValue("barcode", "Čárový kód")
                        .addMapValue("ccnb", "čČNB")
                        .addMapValue("doi", "DOI")
                        .addMapValue("hdl", "Handle")
                        .addMapValue("isbn", "ISBN")
                        .addMapValue("ismn", "ISMN")
                        .addMapValue("permalink", "Permalink")
                        .addMapValue("sici", "SICI")
                        .addMapValue("url", "URL")
                        .addMapValue("urnnbn", "URN:NBN")
                        .addMapValue("uuid", "UUID")
                        .addMapValue("oclc", "OCLC")
                        .createField())
                .addField(new FieldBuilder("value").setTitle("Identifier - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(required).createField())
                .addField(new FieldBuilder("invalid").setTitle("Invalid - MA").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("")
                        .addMapValue("", "Platný")
                        .addMapValue("yes", "Neplatný")
                        .createField()) // invalid
                .createField(); // identifier
    }

    private Field location(boolean required) {
        return new FieldBuilder("location").setTitle("Location - MA").setMaxOccurrences(10)
                .setHint("Údaje o uložení popisovaného dokumentu, např. signatura, místo uložení apod.")
                .addField(new FieldBuilder("physicalLocation").setTitle("Physical Location - M").setMaxOccurrences(1)
                        .addField(new FieldBuilder("authority").setTitle("Authority - O").setMaxOccurrences(1).setType(Field.TEXT).setDefaultValue("siglaADR").createField())
                        .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(required).setDefaultValue("ABE301")
                                .setHint("Údaje o instituci, kde je fyzicky uložen popisovaný dokument. Např. NK ČR."
                                        + "<p>Nutno použít kontrolovaný slovník - sigly knihovnen (ABA001 atd.)"
                                        + "<p>Odpovídá poli 910 $a v MARC21."
                                        + "<p>Pozn. u dokumentů v digitální podobě není možné vyplnit.")
                                .createField()) // value
                        .createField()) // physicalLocation
                .createField(); // location
    }
}
