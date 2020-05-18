package cz.cas.lib.proarc.webapp.client.widget.mods.chronicle;

import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;

import java.util.List;

/**
 * The Chronicle  Volume.
 *
 * @author Lukas Sykora
 */
public class SimpleChronicleVolumeForm {

    public Form build() {
        Form f = new Form();

        Field mods = new FieldBuilder("mods").setMaxOccurrences(1).createField();
        f.getFields().add(mods);
        List<Field> modsFields = mods.getFields();
        modsFields.add(location());
        modsFields.add(relatedItem(f.getItemWidth()));
        //modsFields.add(identifier());
        modsFields.add(genre());
        modsFields.add(titleInfo());
        modsFields.add(abstracts());
        modsFields.add(language());
        modsFields.add(originInfo());
        modsFields.add(name());
        modsFields.add(note());
        //modsFields.add(ChronicleForms.physicalDescription());
        //modsFields.add(ChronicleForms.part());

        return f;
    }


    private Field relatedItem(String width) {
        return new FieldBuilder("relatedItem").setTitle("Identifikátory").setMaxOccurrences(1)
                .addField(identifier())
                .createField();
    }

    private Field titleInfo() {
        return new FieldBuilder("titleInfo").setTitle("Informace o názvu").setMaxOccurrences(10)
                .setHint("Název titulu.<p>Pro plnění použít katalogizační záznam.")
                .addField(new FieldBuilder("title").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Název").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                                .setHint("Název svazku kroniky.").createField()) // value
                        .createField()) // title
                .addField(new FieldBuilder("subTitle").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Podnázev").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Podnázev svazku kroniky.")
                                .createField()) // value
                        .createField()) // subTitle
                .addField(new FieldBuilder("partNumber").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Díl").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Číslo části<p>V případě, že se jedná o vícesvazkovou kroniku je zde uvedeno číslo svazku.")
                                .createField()) // value
                        .createField()) // partNumber
                .addField(new FieldBuilder("partName").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Část").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Název části<p>V případě, že se jedná o vícesvazkovou kroniku je zde uveden název svazku.")
                                .createField()) // value
                        .createField()) // partName
                .createField(); // titleInfo
    }

    private Field name() {
        return new FieldBuilder("name").setMaxOccurrences(10).setTitle("Osoby, které se podílely na vzniku")
                .setHint("Údaje o odpovědnosti za svazek."
                        + "<p>Pokud má kronika autora a ilustrátora, element &lt;name> se opakuje s různými rolemi.")
                .addField(new FieldBuilder("role").setMaxOccurrences(1)
                        .setHint("Specifikace role osoby nebo organizace uvedené v elementu &lt;name>")
                        .addField(new FieldBuilder("roleTerm").setMaxOccurrences(1)
                                .addField(new FieldBuilder("value").setMaxOccurrences(1).setTitle("Role").setType(Field.SELECT)
                                        .addMapValue("ann","Kronikář")
                                        .addMapValue("aut", "Autor")
                                        .addMapValue("dub", "Domnělý (pochybný) autor")
                                        .addMapValue("edt", "Editor")
                                        .addMapValue("ill", "Ilustrátor")
                                        .addMapValue("scr", "Písař")
                                        .addMapValue("trl", "Překladatel")
                                        .addMapValue("egr", "Rytec")
                                        .addMapValue("prt", "Tiskař")
                                        .addMapValue("oth", "Jiná role")
                                        .createField()) // value
                                .createField())
                        .createField()) // role
                .addField(new FieldBuilder("namePart").setTitle("Jméno osoby").setMaxOccurrences(5)
                        .addField(new FieldBuilder("type").setTitle("Typ").setMaxOccurrences(1).setType(Field.SELECT)
                                .setHint("<dl>"
                                        + "<dt>date</dt><dd>RA - datum činnosti</dd>"
                                        + "<dt>family</dt><dd>MA -příjmení </dd>"
                                        + "<dt>given</dt><dd>MA - jméno/křestní jméno</dd>"
                                        //+ "<dt>termsOfAddress</dt><dd>RA - tituly a jiná slova nebo čísla související se jménem</dd>"
                                        + "</dl>")
                                .addMapValue("date", "datum činnosti")
                                .addMapValue("family", "příjmení")
                                .addMapValue("given", "křestní jméno")
                                //.addMapValue("termsOfAddress", "tituly a jiné označení")
                                .createField()) // @type
                        .addField(new FieldBuilder("value").setTitle("Hodnota").setMaxOccurrences(1)
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
                .createField(); // name
    }

    private Field genre() {
        return new FieldBuilder("genre").setTitle("Charakter kroniky").setMaxOccurrences(10)
                .setHint("Bližší údaje o typu dokumentu.<p>Pro svazek kroniky hodnota “kronika”.")
                .addField(new FieldBuilder("value").setTitle("Žánr").setMaxOccurrences(1).setType(Field.COMBO).setRequired(true).setDefaultValue("rkp")
                        .addMapValue("ukn", "Úřední kniha")
                        .addMapValue("rkp", "Rukopis")
                        .createField())
                .addField(new FieldBuilder("lang").setTitle("Číslo").setMaxOccurrences(1).setType(Field.TEXT).setRequired(false).createField())
                .addField(new FieldBuilder("type").setTitle("Typ obsahu - R").setMaxOccurrences(1).setType(Field.COMBO).setWidth("200")
                        .addMapValue("skolniKronika","Školní kronika")
                        .addMapValue("obecniKronika","Obecní kronika")
                        .addMapValue("spolecenskaKronika","Společenská kronika (spolková)")
                        .addMapValue("obcanske", "Občanská")
                        .addMapValue("osadni", "Osadní (kronika místních částí)")
                        .addMapValue("podnikova", "Podnikové (firmy)")
                        .addMapValue("vojenske", "Vojenské a jiné (ZV, odborové, ...")
                        .addMapValue("cirkevni", "Církevní")
                        .addMapValue("unspecified","Nespecifikováno")
                        .createField())
                .createField(); // genre
    }

    private Field originInfo() {
        return new FieldBuilder("originInfo").setTitle("Informace o místě a data vzniku").setMaxOccurrences(10)
                .setHint("Informace o kronice.")
                .addField(new FieldBuilder("place").setTitle("Místo vzniku").setMaxOccurrences(10)
                        .setHint("Údaje o místě spojeném s vydáním, výrobou nebo původem popisovaného dokumentu.")
                        .addField(new FieldBuilder("placeTerm").setMaxOccurrences(1)
                               /* .addField(new FieldBuilder("type").setTitle("Typ").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("text")
                                        .setHint("Typ popisu místa. Kódem nebo textově.")
                                        .addMapValue("code", "code")
                                        .addMapValue("text", "text").setHidden(true)
                                        .createField()) // type
                                .addField(new FieldBuilder("authority").setTitle("Authority - MA").setMaxOccurrences(1).setType(Field.COMBO)
                                        .setHint("Hodnota “marccountry” jen u údaje z pole 008")
                                        .addMapValue("marccountry", "marccountry").setHidden(true)
                                        .createField()) // @authority
                                */.addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXT)
                                        .setHint("Konkrétní určení místa a země vydání, např. Praha resp. xr pro ČR."
                                                + "<p>Odpovídá hodnotám z katalogizačního záznamu, pole 260, podpole „a“ resp. pole 008/15-17.")
                                        .createField()) // value
                                .createField()) // placeTerm
                        .createField()) // place
                .addField(new FieldBuilder("dateIssued").setTitle("Datum vzniku").setMaxOccurrences(10)
                        .setHint("Datum vydání kroniky.")
                        /*.addField(new FieldBuilder("encoding").setTitle("Encoding - R").setMaxOccurrences(1).setType(Field.SELECT)
                                .setHint("Kódování - hodnota „marc“ jen u údaje z pole 008.")
                                .addMapValue("iso8601", "ISO 8601").setHidden(true)
                                .createField()) // @encoding
                        .addField(new FieldBuilder("point").setTitle("Rozmezí").setMaxOccurrences(1).setType(Field.SELECT).setRequired(false)
                                .setHint("Hodnoty „Od“ resp. „Do“ jen u údaje pro rozmezí dat.")
                                .addMapValue("start", "Od")
                                .addMapValue("end", "Do")
                                .createField()) // @point
                         */
                        .addField(new FieldBuilder("qualifier").setTitle("Odhad").setMaxOccurrences(1).setType(Field.SELECT)
                                .setHint("Možnost dalšího upřesnění, hodnota „přibližne“ pro data, kde nevíme přesný údaj.")
                                .addMapValue("approximate", "přibližně")
                                .createField()) // @qualifier
                        .addField(new FieldBuilder("value").setTitle("Hodnota").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200")
                                .setHint("Datum vydání předlohy.")
                                .createField()) // value
                        .createField()) // dateIssued
                .createField(); // originInfo
    }

    private Field language() {
        return new FieldBuilder("language").setMaxOccurrences(10).setTitle("Jazyk")
                .setHint("Údaje o jazyce dokumentu; v případě vícenásobného výskytu nutno element &lt;language> opakovat")
                .addField(new FieldBuilder("languageTerm").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setMaxOccurrences(1)
                                .setType(Field.COMBO)
                                .setHint("Přesné určení jazyka kódem.<p>Nutno použít kontrolovaný slovník ISO 639-2.")
                                .setOptionDataSource(new FieldBuilder("ndk.mods.languageTerms").setWidth("600")
                                                .addField(new FieldBuilder("title").setTitle("Name").createField())
                                                .addField(new FieldBuilder("value").setTitle("Language").createField())
                                                .addField(new FieldBuilder("type").setTitle("Type").createField())
                                                .addField(new FieldBuilder("authority").setTitle("Authority").createField())
                                                .createField(),
                                        "value", "type", "authority")
                                .createField()) // value
                        .createField()) // languageTerm
                .createField(); // language
    }

    private Field abstracts() {
        return new FieldBuilder("abstract").setTitle("Obsah, regest").setMaxOccurrences(10)
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA).setLength(2700)
                        .setHint("Shrnutí obsahu jako celku.")
                        .createField()) // value
                .createField(); // abstract
    }

    private Field note() {
        return new FieldBuilder("note").setTitle("Poznámka").setMaxOccurrences(30)
                .addField(new FieldBuilder("type").setMaxOccurrences(1).setType(Field.COMBO).setDefaultValue("public")
                    .addMapValue("public", "veřejná")
                    .addMapValue("private", "nepublikovatelná").createField())
                .addField(new FieldBuilder("value").setMaxOccurrences(1).setType(Field.TEXTAREA)
                        .setHint("Obecná poznámka k titulu jako celku.").createField()) // value
                .createField(); // note
    }

    private Field identifier() {
        return new FieldBuilder("identifier").setTitle("Identifikátor").setMaxOccurrences(10)
                .setHint("Údaje o identifikátorech.<p>Obsahuje unikátní identifikátory"
                        + " mezinárodní nebo lokální."
                        + "<p>Uvádějí se i neplatné resp. zrušené identifikátory - atribut invalid=“yes“.")
                .addField(new FieldBuilder("type").setTitle("Typ").setMaxOccurrences(1).setType(Field.COMBO).setRequired(true)
                        //.addMapValue("id", "Id")
                        //.addMapValue("localId", "LocalId")
                        .addMapValue("signature1", "Signatura přidělená původcem")
                        .addMapValue("signature2", "Signatura přidělená při zpracování archiválie")
                        .addMapValue("officialNumber", "Číslo vložky úřední desky")
                        .addMapValue("inventaryNumber", "Inventární číslo")
                        .addMapValue("OtherNumber", "Přírůstkové číslo")
                        //.addMapValue("uuid", "UUID")
                        .createField())
                .addField(new FieldBuilder("value").setTitle("Hodnota").setMaxOccurrences(1).setType(Field.TEXT).setRequired(true).createField())
                .addField(new FieldBuilder("invalid").setTitle("Platnost").setMaxOccurrences(1).setType(Field.SELECT).setDefaultValue("yes")
                        .addMapValue("yes", "Platný")
                        .addMapValue("no", "Neplatný")
                        .createField()) // invalid
                .createField(); // identifier
    }

    private Field location() {
        return new FieldBuilder("location").setTitle("Umístění").setMaxOccurrences(10)
                .setHint("Údaje o uložení popisovaného dokumentu, např. signatura, místo uložení apod.")
                .addField(new FieldBuilder("physicalLocation").setMaxOccurrences(1)
                        .addField(new FieldBuilder("type").setTitle("Název archivu").setMaxOccurrences(1).setType(Field.COMBO)
                                .addMapValue("226102010","SOkA Jihlava")
                                .addMapValue("226103010","SOkA Pelhřimov")
                                .addMapValue("226101010","SOkA Havlíčkův Brod")
                                .addMapValue("226104010","SOkA Třebíč")
                                .addMapValue("226105010","SOkA Žďár nad Sázavou")
                                .createField())
                        //.addField(new FieldBuilder("authorityURI").setTitle("Číslo fondu uložené").setMaxOccurrences(1).setType(Field.TEXT).createField())
                        .addField(new FieldBuilder("displayLabel").setMaxOccurrences(1).setTitle("Název fondu").setType(Field.TEXT).createField()) // value
                        .createField()) // physicalLocation
                .createField(); // location
    }
}

