/*
 * Copyright (C) 2018 Martin Rumanek
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

package cz.cas.lib.proarc.common.object.ndk;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;

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
import cz.cas.lib.proarc.common.object.emods.BornDigitalDisseminationHandler;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.ElementType;


public class NdkEbornPlugin implements DigitalObjectPlugin {

    public static final String ID = "ndkEborn";

    public static final String MODEL_EMONOGRAPHVOLUME = "model:ndkemonographvolume";

    @Override
    public String getId() {
        return ID;
    }

    @Override
    public Collection<MetaModel> getModel() {
        List<MetaModel> models = new ArrayList<>();
        models.add(new MetaModel(
                MODEL_EMONOGRAPHVOLUME, true, null,
                Arrays.asList(new ElementType("NDK Svazek eBorn monografie", "cs")),
                ModsConstants.NS,
                MODEL_EMONOGRAPHVOLUME,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM, DatastreamEditorType.MEDIA)
        ));

        return Collections.unmodifiableList(models);
    }

    @Override
    public <T extends HasDataHandler> T getHandlerProvider(Class<T> type) {
        if (!type.equals(HasMetadataHandler.class)) {
            return null;
        } else if (type.equals(HasDisseminationHandler.class)) {
            return (T) new HasDisseminationHandler() {

                @Override
                public DisseminationHandler createDisseminationHandler(String dsId, DigitalObjectHandler handler) {
                    DefaultDisseminationHandler ddh = new DefaultDisseminationHandler(dsId, handler);
                    return (BinaryEditor.RAW_ID.equals(dsId)) ? new BornDigitalDisseminationHandler(ddh) : ddh;
                }
            };
        }

        return (T) new HasMetadataHandler() {

            @Override
            public MetadataHandler createMetadataHandler(DigitalObjectHandler handler) {
                return new NdkMetadataHandler(handler);
            }
        };
    }

    @Override
    public List<ValueMap> getValueMaps(ValueMap.Context context) {
        final ArrayList<ValueMap> maps = new ArrayList<>();
        maps.add(JsonValueMap.fromBundle(BundleName.MODS_ROLES, context.getLocale()));
        return maps;
    }
}