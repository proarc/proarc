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
package cz.cas.lib.proarc.webapp.client.widget.technicalMetadata;

import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.List;

public class AesForm {


    public Form build() {
        Form f = new Form();
        Field audioObject = new FieldBuilder("aes").setMaxOccurrences(1).createField();
        f.getFields().add(audioObject);

        List<Field> audioObjectFields = audioObject.getFields();
        audioObjectFields.add(audioDataEncoding());
        audioObjectFields.add(appSpecificData());
        audioObjectFields.add(format());
        audioObjectFields.add(byteOrder());
        audioObjectFields.add(firstSampleOffset());
        audioObjectFields.add(audioDataBlockSize());
        audioObjectFields.add(use());
        audioObjectFields.add(primaryIdentifier());
        audioObjectFields.add(fileChecksum());
        audioObjectFields.add(soundDataChecksum());
        audioObjectFields.add(face());
        audioObjectFields.add(formatList());
        return f;
    }

    private Field audioDataEncoding() {
        return new FieldBuilder("audioDataEncoding").setTitle("Audio Data Encoding - M").setMaxOccurrences(1).setRequired(true)
                .setType(Field.TEXT).setHint("Použitá modulační metoda převodu analogového signálu na signál digitální. Pro WAV soubory.")
                .createField();
    }

    private Field format() {
        return new FieldBuilder("format").setTitle("Format - M").setMaxOccurrences(1)
                .addField(new FieldBuilder("value")
                        .setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                        .setHint("Oficiální název formátu, tj. \"Waveform_Audio\" (nastavit fixně pro soubory WAV)")
                        .setDefaultValue("Waveform_Audio")
                        .createField())
                .addField(new FieldBuilder("specificationVersion").setTitle("Specification Version - MA")
                        .setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("verze formátu, např. \"1 PCM encoding\"")
                        .createField())
                .createField();
    }

    private Field appSpecificData() {
        return new FieldBuilder("appSpecificData").setTitle("App Specific Data - R").setMaxOccurrences(10)
                .addField(new FieldBuilder("value")
                        .setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Aplikace, která se podílí na tvorbě audia a která do souboru vložila nějaká svá metadata")
                        .createField())
                .addField(new FieldBuilder("appVersion").setTitle("App Version - R")
                        .setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Verze aplikace")
                        .createField())
                .createField();
    }

    private Field byteOrder() {
        return new FieldBuilder("byteOrder").setTitle("Byte Order - M").setMaxOccurrences(1).setRequired(true)
                .setType(Field.SELECT)
                .addMapValue("LITTLE_ENDIAN", "Little endian")
                .addMapValue("BIG_ENDIAN", "Big endian")
                .setHint("Endianita, pořadí bajtů")
                .createField();
    }

    private Field firstSampleOffset() {
        return new FieldBuilder("firstSampleOffset").setTitle("First Sample Offset - R")
                .setMaxOccurrences(1).setType(Field.TEXT)
                .setHint("Počet bajtů, které se v souboru vyskytují ještě před popisovaným audio objektem (před prvním bajtem audio dat)." +
                        " Hodnotu vypíší charakterizační nástroje, např. FITS a JHOVE")
                .createField();
    }

    private Field audioDataBlockSize() {
        return new FieldBuilder("audioDataBlockSize").setTitle("Audio Data Block Size - R")
                .setMaxOccurrences(1).setType(Field.TEXT)
                .setHint("Velikost datových bloků popisovaného audio dokumentu, velikost se udává v bajtech.")
                .createField();
    }

    private Field use() {
        return new FieldBuilder("use").setTitle("Use - M").setMaxOccurrences(10)
                .addField(new FieldBuilder("useType")
                        .setMaxOccurrences(1).setType(Field.SELECT).setRequired(true)
                        .addMapValue("PRESERVATION_MASTER", "Preservation master")
                        .setDefaultValue("PRESERVATION_MASTER")
                        .setHint("Účel popisovaného souboru. Povolené hodnoty: \"PRESERVATION_MASTER\"")
                        .createField())
                .createField();
    }

    private Field primaryIdentifier() {
        return new FieldBuilder("primaryIdentifier").setTitle("Primary Identifier - M").setMaxOccurrences(10)
                .addField(new FieldBuilder("value")
                        .setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                        .setHint("Údaj o identifikátoru popisovaného objektu.")
                        .createField())
                .addField(new FieldBuilder("identifierType").setTitle("Identifier Type - MA")
                        .setMaxOccurrences(1).setType(Field.SELECT)
                        .setHint("Typ identifikátu.")
                        .addMapValue("UMID", "umid")
                        .addMapValue("FILE_NAME", "file name")
                        .addMapValue("SHELF_NUMBER", "shelf number")
                        .addMapValue("OTHER", "other")
                        .createField())
                .addField(new FieldBuilder("odOtherType").setTitle("Od Other Type - MA")
                        .setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Vyplníme, pokud u atribitutu identifierType zapíšeme \"OTHER\", zde může být cokoliv")
                        .createField())
                .createField();
    }

    private Field fileChecksum() {
        return new FieldBuilder("fileChecksum").setTitle("File Checksum - M").setMaxOccurrences(1)
                .addField(new FieldBuilder("checksumValue").setTitle("Checksum Value - M")
                        .setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                        .setHint("Hodnota kontrolního součtu.")
                        .createField())
                .addField(new FieldBuilder("checksumKind").setTitle("Checksum Kind - M")
                        .setMaxOccurrences(1).setType(Field.SELECT).setRequired(true)
                        .addMapValue("CRC", "CRC")
                        .addMapValue("MD5", "MD5")
                        .addMapValue("SHA-1", "SHA-1")
                        .setHint("CRC nebo MD5 nebo SHA-1 nebo novější.")
                        .createField())
                .addField(new FieldBuilder("checksumCreateDate").setTitle("Checksum Create Date - M")
                        .setMaxOccurrences(1).setType(Field.DATE).setRequired(true)
                        .setHint("Datum vytvoření kontrolního součtu.")
                        .createField())
                .createField();
    }

    private Field soundDataChecksum() {
        return new FieldBuilder("soundDataChecksum").setTitle("Sound Data Checksum - O").setMaxOccurrences(1)
                .addField(new FieldBuilder("checksumValue").setTitle("Checksum Value - O")
                        .setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Hodnota kontrolního součtu.")
                        .createField())
                .addField(new FieldBuilder("checksumKind").setTitle("Checksum Kind - O")
                        .setMaxOccurrences(1).setType(Field.SELECT)
                        .addMapValue("CRC", "CRC")
                        .addMapValue("MD5", "MD5")
                        .addMapValue("SHA-1", "SHA-1")
                        .setHint("CRC nebo MD5 nebo SHA-1 nebo novější.")
                        .createField())
                .addField(new FieldBuilder("checksumCreateDate").setTitle("Checksum Create Date - O")
                        .setMaxOccurrences(1).setType(Field.DATE)
                        .setHint("Datum vytvoření kontrolního součtu.")
                        .createField())
                .createField();
    }

    private Field face() {
        return new FieldBuilder("face").setTitle("Face - M").setMaxOccurrences(10)
                /*.addField(new FieldBuilder("ID").setTitle("ID - M")
                        .setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                        .setHint("Identifikátor této části dat, který se bude používat při odkazování z jiných částí záznamů (z region).")
                        .createField())
                 */
                .addField(new FieldBuilder("direction").setTitle("Direction - M")
                        .setMaxOccurrences(1).setType(Field.SELECT).setRequired(true)
                        .setHint("u digitálního souboru vždy hodnota “NONE”")
                        .addMapValue("NONE", "NONE")
                        .createField())
                /*.addField(new FieldBuilder("audioObjectRef").setTitle("Audio Object Ref - M")
                        .setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                        .setHint("Odkaz na ID audioObject ")
                        .createField())
                 */
                .addField(new FieldBuilder("label").setTitle("Label - O")
                        .setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Název logického audio celku, např. název strany desky, pokud nějaký je (Rusalka, Půlnoční….) nebo Side_A")
                        .createField())

                .addField(new FieldBuilder("timeline").setTitle("Timeline - M").setMaxOccurrences(1)
                        .addField(timeExtension("startTime", "Start time - M"))
                        .addField(timeExtension("duration", "Duration - M"))
                        .createField())
                .addField(new FieldBuilder("region").setTitle("Region - M").setMaxOccurrences(10)
                        .addField(new FieldBuilder("label").setTitle("Label - O")
                                .setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Název této úrovně audio objektu, např. Region_1 nebo název písně, proslovu apod.")
                                .createField())
                        .addField(new FieldBuilder("numChannels").setTitle("Number of Channels")
                                .setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                                .setHint("Počet kanálů (audio streams) regionu. Zapisovanou hodnotou je číslo, př. \"1\"")
                                .createField())
                        .addField(new FieldBuilder("conditionNote").setTitle("Condition note - O").setMaxOccurrences(10)
                                .addField(new FieldBuilder("note").setTitle("Note - M")
                                        .setMaxOccurrences(1).setType(Field.TEXT)
                                        .setHint("Text poznámky.")
                                        .createField())
                                .addField(new FieldBuilder("creationDate").setTitle("Creation Date - M")
                                        .setMaxOccurrences(1).setType(Field.DATE)
                                        .setHint("Čas vytvoření poznámky, zápis dle normy ISO 8601.")
                                        .createField())
                                .createField())
                        .addField(new FieldBuilder("securityNote").setTitle("Security Note - M")
                                .setMaxOccurrences(10).setType(Field.TEXT)
                                .setHint("Obsahem elementu je informace (volný text) o obsahu citlivých údajů (telefonních čísel, jmén…)")
                                .createField())
                        .addField(new FieldBuilder("stream").setTitle("Stream - M").setMaxOccurrences(10)
                                .addField(new FieldBuilder("label").setTitle("Label - O")
                                        .setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                                        .setHint("Pojmenování kanálu.")
                                        .createField())
                                .addField(new FieldBuilder("channelAssignment").setTitle("Channel Assignment - M").setMaxOccurrences(1)
                                        .addField(new FieldBuilder("channelNum").setTitle("Channel Number - M")
                                                .setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                                                .setHint("Číselné označení popisovaného kanálu")
                                                .createField())
                                        .addField(new FieldBuilder("leftRightPosition").setTitle("Left/Right Position - M")
                                                .setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                                                .setHint("Číselné označení umístění kanálu na ose levá-pravá. Hodnoty např. „0.0“ (střed) nebo „100.0“ (vpravo) nebo „-100.0“ (vlevo)")
                                                .createField())
                                        .addField(new FieldBuilder("frontRearPosition").setTitle("Front/Rear Position - M")
                                                .setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                                                .setHint("Číselné označení umístění kanálu na ose přední-zadní. Hodnoty např. „0.0“ (střed) nebo „100.0“ (vpředu) nebo „-100.0“ (vzadu)")
                                                .createField())
                                        .createField())
                                .addField(new FieldBuilder("conditionNote").setTitle("Condition note - O").setMaxOccurrences(10)
                                        .addField(new FieldBuilder("note").setTitle("Note - M")
                                                .setMaxOccurrences(1).setType(Field.TEXT)
                                                .setHint("Text poznámky.")
                                                .createField())
                                        .addField(new FieldBuilder("creationDate").setTitle("Creation Date - M")
                                                .setMaxOccurrences(1).setType(Field.DATE)
                                                .setHint("Čas vytvoření poznámky, zápis dle normy ISO 8601.")
                                                .createField())
                                        .createField())
                                .createField())
                        .addField(new FieldBuilder("timeRange").setTitle("Timerange - M").setMaxOccurrences(1)
                                .addField(timeExtension("startTime", "Start time - M"))
                                .addField(timeExtension("duration", "Duration - M"))
                                .createField())
                        .createField())
                .createField();
    }

    private Field timeExtension(String name, String title) {
        return new FieldBuilder(name).setTitle(title).setMaxOccurrences(1)
                .addField(new FieldBuilder("hours").setTitle("Hours")
                        .setMaxOccurrences(1).setType(Field.TEXT)
                        .createField())
                .addField(new FieldBuilder("minutes").setTitle("Minutes")
                        .setMaxOccurrences(1).setType(Field.TEXT)
                        .createField())
                .addField(new FieldBuilder("seconds").setTitle("Seconds")
                        .setMaxOccurrences(1).setType(Field.TEXT)
                        .createField())
                .addField(new FieldBuilder("frames").setTitle("Frames")
                        .setMaxOccurrences(1).setType(Field.TEXT)
                        .setHint("Délka/Začátek nahrávky, udíává se ve frames")
                        .createField())
                .addField(new FieldBuilder("samples").setTitle("Samples")
                        .addField(new FieldBuilder("numberOfSamples").setTitle("Number of Samples")
                                .setMaxOccurrences(1).setType(Field.TEXT)
                                .createField())
                        .createField())
                .createField();
    }

    private Field formatList() {
        return new FieldBuilder("formatList").setTitle("Format list - M").setMaxOccurrences(1)
                .addField(new FieldBuilder("formatRegion").setTitle("Format Region - M")
                        .addField(new FieldBuilder("label").setTitle("Label - O")
                                .setMaxOccurrences(1).setType(Field.TEXT)
                                .setDefaultValue("audio/xwav")
                                .setHint("Použije se pro zápis Mimetype, tj. \"audio/xwav\"")
                                .createField())
                        .addField(new FieldBuilder("bitDepth").setTitle("Bit depth - M")
                                .setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                                .setHint("Bitová hloubka digitálního zvukového dokumentu. Pro gramofonové desky je aktuálně předepsaná hodnota nejméně „24“")
                                .createField())
                        .addField(new FieldBuilder("sampleRate").setTitle("Sample rate - M")
                                .setMaxOccurrences(1).setType(Field.TEXT).setRequired(true)
                                .setHint("Vzorkovací frekvence, aktuální předepsaná hodnota pro gramofonové desky je „96000“")
                                .createField())
                        .addField(new FieldBuilder("wordSize").setTitle("Word Size - R")
                                .setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Velikost přenášeného vzorku v bajtech. Zapisovanou hodnotou je číslo, např. „3“ (odpovídá 24 bitové hloubce)")
                                .createField())
                        .addField(new FieldBuilder("soundField").setTitle("Sound Field - M")
                                .setMaxOccurrences(1).setType(Field.SELECT).setRequired(true)
                                .addMapValue("MONO", "MONO")
                                .addMapValue("STEREO", "STEREO")
                                .addMapValue("SURROUND", "SURROUND")
                                .setHint("Obsahem elementu je popis zvukového pole, možné hodnoty jsou: „MONO“ nebo „STEREO“ nebo „SURROUND“")
                                .createField())
                        .createField())
                .createField();
    }


}
