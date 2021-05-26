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

import com.smartgwt.client.widgets.form.DynamicForm;
import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkEbornPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.LanguagesDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.client.widget.mods.eborn.NdkEArticleForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.eborn.NdkEPeriodicalForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.eborn.NdkEPeriodicalIssueForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.eborn.NdkEPeriodicalVolumeForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.eborn.NdkEmonographChapterForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.eborn.NdkEmonographTitleForm;
import cz.cas.lib.proarc.webapp.client.widget.mods.eborn.NdkEmonographVolumeForm;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

/**
 * The helper for NDK forms.
 *
 * @author Jan Pokorsky
 */
public final class NdkForms {

    private final ClientMessages i18n;
    private final String activeLocale;
    private final static Map<String, Supplier<Form>> mappers = new HashMap<>();

    static {
        mappers.put("model:ndkperiodical", () -> {
                    Form form = new NdkPeriodicalForm().build();
                    form.setItemWidth("800");
                    return form;
                }
        );
        mappers.put("model:ndkperiodicalvolume", new NdkPeriodicalVolumeForm()::build);
        mappers.put("model:ndkperiodicalissue", new NdkPeriodicalIssueForm()::build);
        mappers.put("model:ndkperiodicalsupplement", new NdkPeriodicalSupplementForm()::build);
        mappers.put("model:ndkarticle", new NdkArticleForm()::build);
        mappers.put("model:ndkchapter", new NdkChapterForm()::build);
        mappers.put("model:ndkpicture", new NdkPictureForm()::build);
        mappers.put("model:ndkmonographvolume", new NdkMonographVolumeForm()::build);
        mappers.put("model:ndkmonographtitle", new NdkMonographTitleForm()::build);
        mappers.put("model:ndkmonographsupplement", new NdkMonographSupplementForm()::build);
        mappers.put("model:ndkmap", new NdkCartographicForm()::build);
        mappers.put("model:ndksheetmusic", new NdkSheetMusicForm()::build);
        mappers.put(NdkAudioPlugin.MODEL_MUSICDOCUMENT, new NdkSoundCollectionForm()::build);
        mappers.put(NdkAudioPlugin.MODEL_PHONOGRAPH, new NdkSoundPhonographForm()::build);
        mappers.put(NdkAudioPlugin.MODEL_SONG, new NdkSoundRecordingForm()::build);
        mappers.put(NdkAudioPlugin.MODEL_TRACK, new NdkSoundPartForm()::build);
        mappers.put(NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME, new NdkEmonographVolumeForm()::build);
        mappers.put(NdkEbornPlugin.MODEL_ECHAPTER, new NdkEmonographChapterForm()::build);
        mappers.put(NdkEbornPlugin.MODEL_EMONOGRAPHTITLE, new NdkEmonographTitleForm()::build);
        mappers.put(NdkEbornPlugin.MODEL_EPERIODICAL, new NdkEPeriodicalForm()::build);
        mappers.put(NdkEbornPlugin.MODEL_EPERIODICALISSUE, new NdkEPeriodicalIssueForm()::build);
        mappers.put(NdkEbornPlugin.MODEL_EPERIODICALVOLUME, new NdkEPeriodicalVolumeForm()::build);
        mappers.put(NdkEbornPlugin.MODEL_EARTICLE, new NdkEArticleForm()::build);
        mappers.put(NdkPlugin.MODEL_NDK_PAGE, new NdkNewPageForm()::build);
    }

    public NdkForms(ClientMessages i18n) {
        this.i18n = i18n;
        activeLocale = LanguagesDataSource.activeLocale();
    }

    public DynamicForm getForm(MetaModelRecord model) {
        String modelId = model.getId();
        if (NdkPlugin.MODEL_PAGE.equals(modelId)) {
            return new PageForm(i18n);
        } else if ("model:ndkaudiopage".equals(modelId)) {
            return new NdkAudioPageForm(i18n, BundleName.MODS_AUDIO_PAGE_TYPES);
        //} else if (NdkPlugin.MODEL_NDK_PAGE.equals(modelId)) {
        //    return new NdkPageForm(i18n);
        }

        return mappers.get(modelId) == null ? null : new NdkFormGenerator(mappers.get(modelId).get(), activeLocale).generateForm();
    }

    public static FieldBuilder createLangTermValue() {
        return createLangTermValue(true);
    }

    public static FieldBuilder createLangTermValue(boolean required) {
        return new FieldBuilder("value").setTitle("Language - M").setMaxOccurrences(1)
                .setType(Field.COMBO).setRequired(required)
                .setHint("Přesné určení jazyka kódem.<p>Nutno použít kontrolovaný slovník ISO 639-2.")
                .setOptionDataSource(new FieldBuilder("ndk.mods.languageTerms").setWidth("300")
                        .addField(new FieldBuilder("title").setTitle("Name").createField())
                        .addField(new FieldBuilder("value").setTitle("Language").createField())
                        .addField(new FieldBuilder("type").setTitle("Type").createField())
                        .addField(new FieldBuilder("authority").setTitle("Authority").createField())
                    .createField(),
                    "value", "type", "authority");
    }

    public static Field roleTerm(String valueTitle, Boolean isValueRequired,
            String authorityTitle, Boolean isAuthorityRequired,
            String typeTitle, Boolean isTypeRequired) {

        // roleTerm, type="roleTermDefinition" extends stringPlusLanguagePlusAuthority
        return new FieldBuilder("roleTerm").setMaxOccurrences(1)
            .addField(new FieldBuilder("value").setTitle(valueTitle).setMaxOccurrences(1)
                .setType(Field.COMBO)
                .setWidth("200")
                .setRequired(isValueRequired)
                .setHint("Kód role z kontrolovaného slovníku.")
                .setOptionDataSource(new FieldBuilder(BundleName.MODS_ROLES.getValueMapId()).setWidth("300")
                    .addField(new FieldBuilder("value").setTitle("Kód").createField())
                    .addField(new FieldBuilder("label").setTitle("Popis").createField())
                .createField(), "value", "type", "authority")
            .createField()) // value
            // @type, codeOrText(code, text)
            .addField(new FieldBuilder("type").setTitle(typeTitle).setMaxOccurrences(1)
                .setType(Field.SELECT)
                .setRequired(isTypeRequired)
                .addMapValue("code", "code")
                .addMapValue("text", "text")
            .createField()) // @type
            // stringPlusLanguagePlusAuthority: authorityAttributeGroup: @authority, @authorityURI, @valueURI
            // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
            .addField(new FieldBuilder("authority").setTitle(authorityTitle).setMaxOccurrences(1)
                .setType(Field.COMBO)
                .setWidth("200")
                .setRequired(isAuthorityRequired)
                .addMapValue("marcrelator", "marcrelator")
                .setHint("Údaje o kontrolovaném slovníku využitém k popisu role."
                    + "<p>K popisu výše uvedeného MARC seznamu nutno uvést authority=“marcrelator“.")
            .createField()) // authority
        .createField(); // roleTerm
    }

    public static Field recordInfo() {
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
                        .addField(new FieldBuilder("source").setTitle("Source - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Katalogová hodnota pole 003")
                                .createField())
                        .addField(new FieldBuilder("value").setTitle("Identifier - R").setMaxOccurrences(1).setType(Field.TEXT)
                                .setHint("Identifikátor záznamu v katalogu, přebírá se z pole 001.")
                                .createField())
                        .createField()) // recordIdentifier
                // recordOrigin, extends stringPlusLanguage
                .addField(new FieldBuilder("recordOrigin").setMaxOccurrences(1)
                        // stringPlusLanguage: @lang, @xmlLang, @script, @transliteration
                        .addField(new FieldBuilder("value").setTitle("Record Origin - R").setMaxOccurrences(1).setType(Field.COMBO).setWidth("200")
                                .setHint("Údaje o vzniku záznamu.").setDefaultValue("human prepared")
                                .addMapValue("machine generated", "machine generated")
                                .addMapValue("human prepared", "human prepared")
                                .createField()) // value
                        .createField()) // recordOrigin
                .addField(new FieldBuilder("recordInfoNote").setMaxOccurrences(1)
                        .addField(new FieldBuilder("value").setTitle("Record Info Note - O").setMaxOccurrences(1).setType(Field.TEXT).setWidth("200")
                                .setHint("Poznámka k záznamu").createField())
                        .createField()) //recordInfoNote
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

    public static Field geographicCode() {
        return new FieldBuilder("geographicCode").setTitle("Geographic Code - R").setMaxOccurrences(10)
                .addField(new FieldBuilder("authority").setTitle("Authority - R").setMaxOccurrences(1).setType(Field.COMBO)
                        .addMapValue("marcgac", "marcgac")
                        .createField()) //authority
                .addField(new FieldBuilder("value").setTitle("Value - R").setMaxOccurrences(1).setType(Field.COMBO)
                        .setHint("Geografické věcné třídění."
                                + "<p>použít kontrolovaný slovník - např. z báze autorit AUT NK ČR (geografický termín).")
                        .addMapValue("e-xr---", "Česko")
                        .addMapValue("e-xr-cc", "Čechy")
                        .addMapValue("e-xr-pg", "Praha (Česko : kraj)")
                        .addMapValue("e-xr-st", "Středočeský kraj")
                        .addMapValue("e-xr-kr", "Královéhradecký kraj")
                        .addMapValue("e-xr-pa", "Pardubický kraj")
                        .addMapValue("e-xr-us", "Ústecký kraj")
                        .addMapValue("e-xr-li", "Liberecký kraj")
                        .addMapValue("e-xr-pl", "Plzeňský kraj")
                        .addMapValue("e-xr-ka", "Karlovarský kraj")
                        .addMapValue("e-xr-jc", "Jihočeský kraj")
                        .addMapValue("e-xr-jm", "Jihomoravský kraj")
                        .addMapValue("e-xr-zl", "Zlínský kraj")
                        .addMapValue("e-xr-vy", "Vysočina")
                        .addMapValue("e-xr-mo", "Moravskoslezský kraj")
                        .addMapValue("e-xr-ol", "Olomoucký kraj")
                        .addMapValue("e-xr-mr", "Morava")
                        .addMapValue("e-xr-sl", "Slezsko (Česko)")
                        .createField()) // value
                .createField(); // geographicCode
    }

    public static Field descriptionRadioButton() {
        return new FieldBuilder("rdaRules").setTitle("Zvolte pravidla popisu (Description Standard) - MA").setMaxOccurrences(1)
                .setType(Field.RADIOGROUP).setRequired(true)
                .addMapValue("true", ModsConstants.VALUE_DESCRIPTIONSTANDARD_RDA)
                .addMapValue("false", ModsConstants.VALUE_DESCRIPTIONSTANDARD_AACR)
                .createField();
    }

    public static Field reviewRadioButton(String title) {
        return new FieldBuilder("reviewed").setTitle(title).setMaxOccurrences(1)
                .setType(Field.RADIOGROUP).setRequired(true)
                .addMapValue("true", "recenzovaný článek")
                .addMapValue("false", "nerecenzovaný článek")
                .createField();
    }
}
