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
        mappers.put("model:ndkmusicdocument", new NdkSoundCollectionForm()::build);
        mappers.put("model:ndksong", new NdkSoundRecordingForm()::build);
        mappers.put("model:ndktrack", new NdkSoundPartForm()::build);
        mappers.put(NdkEbornPlugin.MODEL_EMONOGRAPHVOLUME, new NdkEmonographVolumeForm()::build);
        mappers.put(NdkEbornPlugin.MODEL_ECHAPTER, new NdkEmonographChapterForm()::build);
        mappers.put(NdkEbornPlugin.MODEL_EMONOGRAPHTITLE, new NdkEmonographTitleForm()::build);
        mappers.put(NdkEbornPlugin.MODEL_EPERIODICAL, new NdkEPeriodicalForm()::build);
        mappers.put(NdkEbornPlugin.MODEL_EPERIODICALISSUE, new NdkEPeriodicalIssueForm()::build);
        mappers.put(NdkEbornPlugin.MODEL_EPERIODICALVOLUME, new NdkEPeriodicalVolumeForm()::build);
        mappers.put(NdkEbornPlugin.MODEL_EARTICLE, new NdkEArticleForm()::build);
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
        } else if (NdkPlugin.MODEL_NDK_PAGE.equals(modelId)) {
            return new NdkPageForm(i18n);
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
}
