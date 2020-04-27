package cz.cas.lib.proarc.webapp.client.widget.mods.collectionOfClippings;

import cz.cas.lib.proarc.webapp.client.widget.mods.NdkForms;
import cz.cas.lib.proarc.webapp.client.widget.mods.NdkMonographTitleForm;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.List;

public class CollectionOfClippingsTitleForm {

    public Form buildComplexForm() {
        return new NdkMonographTitleForm().build();
    }

    public Form build() {
        Form f = new Form();

        Field mods = new FieldBuilder("mods").setMaxOccurrences(1).createField();
        f.getFields().add(mods);
        List<Field> modsFields = mods.getFields();

        modsFields.add(titleInfo());
        modsFields.add(genre());
        modsFields.add(language());
        modsFields.add(identifier());
        return f;
    }

    private Field titleInfo() {
        return new FieldBuilder("titleInfo").setTitle("Title Info - M").setMaxOccurrences(10)
                .setHint("Název titulu, souborný název.<p>Pro plnění použít katalogizační záznam.")
                .addField(new FieldBuilder("title").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Title - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                                .setHint("Názvová informace - název monografického dokumentu.")
                                .createField()) // value
                        .createField()) // title
                .addField(new FieldBuilder("subTitle").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Subtitle - MA").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Podnázev svazku monografie.")
                                .createField()) // value
                        .createField()) // subTitle
                .createField(); // titleInfo
    }

    private Field genre() {
        return new FieldBuilder("genre").setTitle("Genre - M").setMaxOccurrences(1)
                .setHint("Bližší údaje o typu dokumentu.<p>Pro vícesvazkovou monografii “title”.")
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).setDefaultValue("title").createField())
                .createField(); // genre
    }

    private Field language() {
        return new FieldBuilder("language").setTitle("Languages - O").setMaxOccurrences(10)
                .setHint("Údaje o jazyce dokumentu; v případě vícenásobného výskytu nutno element <language> opakovat")
                .addField(new FieldBuilder("objectPart").setTitle("Object Part - O").setMaxOccurrences(1).setType(Field.COMBO).setWidth("300")
                        .setHint("Možnost vyjádřit jazyk konkrétní části svazku.")
                        .addMapValue("translation", "translation")
                        .createField()) // @objectPart
                .addField(new FieldBuilder("languageTerm").setMaxOccurrences(1)
                        .setHint("Přesné určení jazyka – kódem nutno použít kontrolovaný slovník ISO 639-2,"
                                + "http://www.loc.gov/standards/iso639-2/php/code_list.php"
                                + "<p>Odpovídá poli 008/35-37, resp. 041")
                        .addField(new FieldBuilder("authority").setTitle("Authority - M").setMaxOccurrences(1)
                                .setType(Field.SELECT).setRequired(true)
                                .setHint("Použít hodnotu „iso639-2b“.")
                                .addMapValue("iso639-2b", "ISO 639-2B")
                                .addMapValue("rfc3066", "RFC 3066")
                                .addMapValue("iso639-3", "ISO 639-3")
                                .addMapValue("rfc4646", "RFC 4646")
                                .addMapValue("rfc5646", "RFC 5646")
                                .createField()) // authority
                        .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1)
                                .setType(Field.SELECT).setRequired(true)
                                .setHint("Typ popisu.")
                                .addMapValue("code", "code")
                                .addMapValue("text", "text")
                                .createField()) // type
                        .addField(NdkForms.createLangTermValue()
                                .createField()) // value
                        .createField()) // languageTerm
                .createField(); // language
    }

    private Field identifier() {
        // identifier, identifierDefinition, [0,*]
        return new FieldBuilder("identifier").setTitle("Identifier - M").setMaxOccurrences(10)
                .setHint("Údaje o identifikátorech.<p>Obsahuje unikátní identifikátory"
                        + " mezinárodní nebo lokální."
                        + "<p>Uvádějí se i neplatné resp. zrušené identifikátory - atribut invalid=“yes“.")
                .addField(new FieldBuilder("type").setTitle("Type - M").setMaxOccurrences(1).setType(Field.COMBO).setRequired(true)
                        .setHint("UUID - M - vygeneruje dodavatel"
                                + "<br>čČNB - MA - převzít z katalogizačního záznamu z pole 015, podpole „a“, „z“"
                                + "<br>ISBN - MA - převzít z katalogizačního záznamu z pole 020, podpole „a“, „z“"
                                + "<br>ISMN - MA - převzít z katalogizačního záznamu z pole 024 (1. ind.=“2“), podpole „a“, „z“"
                                + "<br>URN:NBN - O - zápis ve tvaru urn:nbn:cz:ndk-123456 pro projekt NDK"
                                + "<br>jiný interní identifikátor - R - type = barcode, oclc, sysno, permalink apod.")
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
                .addField(new FieldBuilder("value").setTitle("Identifier - M").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).createField())
                .addField(new FieldBuilder("invalid").setTitle("Invalid - MA").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("")
                        .addMapValue("", "Platný")
                        .addMapValue("yes", "Neplatný")
                        .createField()) // invalid
                .createField(); // identifier
    }
}

