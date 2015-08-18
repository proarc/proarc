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
package cz.cas.lib.proarc.common.object.oldprint;

import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectPlugin;
import cz.cas.lib.proarc.common.object.HasDataHandler;
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
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;

/**
 * The plug-in to support old print digital objects.
 *
 * @author Jan Pokorsky
 */
public class OldPrintPlugin implements DigitalObjectPlugin, HasMetadataHandler<ModsDefinition> {

    /**
     * The plugin ID. Old prints.
     */
    public static final String ID = "oldprint";

    /**
     * The volume of old prints.
     */
    public static final String MODEL_VOLUME = "model:oldprintvolume";

    /**
     * The supplement of old prints.
     */
    public static final String MODEL_SUPPLEMENT = "model:oldprintsupplement";

    @Override
    public String getId() {
        return ID;
    }

    @Override
    public Collection<MetaModel> getModel() {
        // for now it is read only repository
        List<MetaModel> models = new ArrayList<MetaModel>();
        models.add(new MetaModel(
                MODEL_VOLUME, true, null,
                Arrays.asList(new ElementType("Old Print Volume", "en"), new ElementType("Starý tisk - Svazek", "cs")),
                ModsConstants.NS,
                MODEL_VOLUME,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        /*DatastreamEditorType.PARENT, */DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_SUPPLEMENT, true, null,
                Arrays.asList(new ElementType("Old Print Supplement", "en"), new ElementType("Starý tisk - Příloha", "cs")),
                ModsConstants.NS,
                MODEL_SUPPLEMENT,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
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
        return Collections.emptyList();
    }

    @Override
    public MetadataHandler<ModsDefinition> createMetadataHandler(DigitalObjectHandler handler) {
        return new NdkMetadataHandler(handler, new OldPrintMapperFactory());
    }

}
