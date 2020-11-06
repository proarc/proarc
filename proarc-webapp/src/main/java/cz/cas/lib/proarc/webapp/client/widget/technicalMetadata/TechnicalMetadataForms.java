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

import com.smartgwt.client.widgets.form.DynamicForm;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
import cz.cas.lib.proarc.common.object.oldprint.OldPrintPlugin;
import cz.cas.lib.proarc.webapp.client.ClientMessages;
import cz.cas.lib.proarc.webapp.client.ds.LanguagesDataSource;
import cz.cas.lib.proarc.webapp.client.ds.MetaModelDataSource;
import cz.cas.lib.proarc.webapp.shared.form.Form;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

public class TechnicalMetadataForms {

    private final ClientMessages i18n;
    private final String activeLocale;
    private final static Map<String, Supplier<Form>> mappers = new HashMap<>();

    static {
        mappers.put(NdkPlugin.MODEL_PAGE, new MixForm() :: build);
        mappers.put(NdkPlugin.MODEL_NDK_PAGE, new MixForm() :: build);
        mappers.put(OldPrintPlugin.MODEL_PAGE, new MixForm() :: build);
        mappers.put(NdkAudioPlugin.MODEL_PAGE, new AesForm() :: build);
    }

    public TechnicalMetadataForms(ClientMessages i18n) {
        this.i18n = i18n;
        activeLocale = LanguagesDataSource.activeLocale();
    }

    public DynamicForm getForm(MetaModelDataSource.MetaModelRecord model) {
        String modelId = model.getId();
        return  mappers.get(modelId) == null ? null : new TechnicalMetadataFormGenerator(mappers.get(modelId).get(), activeLocale).generateForm();
    }
}
