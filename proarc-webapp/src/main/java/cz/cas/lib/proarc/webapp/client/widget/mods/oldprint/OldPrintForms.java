/*
 * Copyright (C) 2015 Jan Pokorsky
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
package cz.cas.lib.proarc.webapp.client.widget.mods.oldprint;

import com.smartgwt.client.widgets.form.DynamicForm;
import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.LanguagesDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource.MetaModelRecord;
import cz.cas.lib.proarc.webapp.client.widget.mods.NdkFormGenerator;
import cz.cas.lib.proarc.webapp.client.widget.mods.NdkNewPageForm;
import cz.cas.lib.proarc.webapp.shared.form.Field;
import cz.cas.lib.proarc.webapp.shared.form.FieldBuilder;
import cz.cas.lib.proarc.webapp.shared.form.Form;

/**
 * The helper for old print forms.
 *
 * @author Jan Pokorsky
 */
public class OldPrintForms {

    private final ClientMessages i18n;
    private final String activeLocale;

    public OldPrintForms(ClientMessages i18n) {
        this.i18n = i18n;
        activeLocale = LanguagesDataSource.activeLocale();
    }
    public DynamicForm getForm(MetaModelRecord model) {
        String modelId = model.getId();
        Form f;
        if (OldPrintPlugin.MODEL_MONOGRAPHVOLUME.equals(modelId)) {
            f = new OldPrintVolumeForm().build();
            f.setItemWidth("800");
        } else if (OldPrintPlugin.MODEL_MONOGRAPHUNIT.equals(modelId)) {
            f = new OldPrintMonographUnitForm().build();
        } else if (OldPrintPlugin.MODEL_SUPPLEMENT.equals(modelId)) {
            f = new OldPrintSupplementForm().build();
        } else if (OldPrintPlugin.MODEL_PAGE.equals(modelId)) {
            //return new PageForm(i18n, BundleName.MODS_OLDPRINT_PAGE_TYPES);
            f = new NdkNewPageForm().build();
        } else if (OldPrintPlugin.MODEL_MONOGRAPHTITLE.equals(modelId)) {
            f = new OldPrintMonographTitleForm().build();
        } else if (OldPrintPlugin.MODEL_CHAPTER.equals(modelId)) {
            f = new OldPrintChapterForms().build();
        } else if (OldPrintPlugin.MODEL_CARTOGRAPHIC.equals(modelId)) {
            f = new OldPrintCartographicForm().build();
        } else if (OldPrintPlugin.MODEL_GRAPHICS.equals(modelId)) {
            f = new OldPrintGraphicsForm().build();
        } else if (OldPrintPlugin.MODEL_CONVOLUTTE.equals(modelId)) {
            f = new OldPrintOmnibusVolumeForm().build();
        } else if (OldPrintPlugin.MODEL_SHEETMUSIC.equals(modelId)) {
            f = new OldPrintSheetMusicForm().build();
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
