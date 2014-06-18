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
        } else {
            return null;
        }
        return new NdkFormGenerator(f, activeLocale).generateForm();
    }

    static FieldBuilder createLangTermValue() {
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
}
