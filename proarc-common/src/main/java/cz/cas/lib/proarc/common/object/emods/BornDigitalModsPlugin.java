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
package cz.cas.lib.proarc.common.object.emods;

import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.i18n.JsonValueMap;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.DefaultDisseminationHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectPlugin;
import cz.cas.lib.proarc.common.object.DisseminationHandler;
import cz.cas.lib.proarc.common.object.HasDataHandler;
import cz.cas.lib.proarc.common.object.HasDisseminationHandler;
import cz.cas.lib.proarc.common.object.HasMetadataHandler;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.ValueMap;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;

/**
 * The plugin to handle materials that originate in a digital form and can be
 * described by MODS.
 *
 * @author Jan Pokorsky
 */
public class BornDigitalModsPlugin implements DigitalObjectPlugin, HasMetadataHandler<ModsDefinition>,
        HasDisseminationHandler {

    /**
     * The plugin ID. Born-digital-MODS.
     */
    public static final String ID = "bdm";
    /**
     * The born digital article described with MODS (CEJSH, ...).
     */
    public static final String MODEL_ARTICLE = "model:bdmarticle";

    @Override
    public String getId() {
        return ID;
    }

    @Override
    public Collection<MetaModel> getModel() {
        // for now it is a read only repository
        List<MetaModel> models = new ArrayList<MetaModel>();
        models.add(new MetaModel(
                MODEL_ARTICLE, null, null,
                Arrays.asList(new ElementType("eArticle", "en"), new ElementType("eČlánek", "cs")),
                ModsConstants.NS,
                MODEL_ARTICLE,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.OCR, DatastreamEditorType.MEDIA,
                        DatastreamEditorType.ATM)
                ));
        return models;
    }

    @Override
    public <T extends HasDataHandler> T getHandlerProvider(Class<T> type) {
        return type.isInstance(this) ? type.cast(this): null;
    }

    @Override
    public List<ValueMap> getValueMaps(ValueMap.Context context) {
        final ArrayList<ValueMap> maps = new ArrayList<ValueMap>();
        maps.add(JsonValueMap.fromBundle(BundleName.CEJSH_ROLES, context.getLocale()));
        return maps;
    }

    @Override
    public MetadataHandler<ModsDefinition> createMetadataHandler(DigitalObjectHandler handler) {
        return new NdkMetadataHandler(handler, new BornDigitalModsMapperFactory());
    }

    @Override
    public DisseminationHandler createDisseminationHandler(final String dsId, final DigitalObjectHandler handler) {
        final DefaultDisseminationHandler ddh = new DefaultDisseminationHandler(dsId, handler);
        if (BinaryEditor.RAW_ID.equals(dsId)) {
            return new BornDigitalDisseminationHandler(ddh);
        } else {
            return ddh;
        }
    }

}
