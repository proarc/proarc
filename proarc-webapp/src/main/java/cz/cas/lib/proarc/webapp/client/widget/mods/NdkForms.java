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
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.LanguagesDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;

/**
 * The helper for NDK forms.
 *
 * @author Jan Pokorsky
 */
public final class NdkForms {

    private final ClientMessages i18n;
    private final String activeLocale;

    public NdkForms(ClientMessages i18n) {
        this.i18n = i18n;
        activeLocale = LanguagesDataSource.activeLocale();
    }

    public DynamicForm getForm(MetaModelRecord model) {
        String modelId = model.getId();
        Form f;
        if ("model:ndkperiodical".equals(modelId)) {
            f = new NdkPeriodicalForm().build();
            f.setItemWidth("800");
        } else if ("model:ndkperiodicalvolume".equals(modelId)) {
            f = new NdkPeriodicalVolumeForm().build();
        } else if ("model:ndkperiodicalissue".equals(modelId)) {
            f = new NdkPeriodicalIssueForm().build();
        } else if ("model:ndkperiodicalsupplement".equals(modelId)) {
            f = new NdkPeriodicalSupplementForm().build();
        } else if ("model:ndkarticle".equals(modelId)) {
            f = new NdkArticleForm().build();
        } else if ("model:ndkchapter".equals(modelId)) {
            f = new NdkChapterForm().build();
        } else if ("model:ndkpicture".equals(modelId)) {
            f = new NdkPictureForm().build();
        } else if ("model:ndkmonographvolume".equals(modelId)) {
            f = new NdkMonographVolumeForm().build();
        } else if ("model:ndkmonographtitle".equals(modelId)) {
            f = new NdkMonographTitleForm().build();
        } else if ("model:ndkmonographsupplement".equals(modelId)) {
            f = new NdkMonographSupplementForm().build();
        } else if ("model:ndkmap".equals(modelId)) {
            f = new NdkCartographicForm().build();
        } else if ("model:ndksheetmusic".equals(modelId)) {
            f = new NdkSheetMusicForm().build();
        } else if ("model:page".equals(modelId)) {
            return new PageForm(i18n);
        } else if ("model:ndkmusicdocument".equals(modelId)) {
            f = new NdkMusicDocumentForm().build();
        } else if ("model:ndksong".equals(modelId)) {
            f = new NdkSongForm().build();
        } else if ("model:ndktrack".equals(modelId)) {
            f = new NdkTrackForm().build();
        } else {
            return null;
        }
        return new NdkFormGenerator(f, activeLocale).generateForm();
    }

    public static FieldBuilder createLangTermValue() {
        return new FieldBuilder("value").setTitle("Language - M").setMaxOccurrences(1)
                .setType(Field.COMBO).setRequired(true)
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
