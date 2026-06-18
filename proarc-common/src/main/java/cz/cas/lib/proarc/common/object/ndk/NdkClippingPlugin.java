/*
 * Copyright (C) 2024 Lukas Sykora
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
package cz.cas.lib.proarc.common.object.ndk;

import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.i18n.JsonValueMap;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectPlugin;
import cz.cas.lib.proarc.common.object.HasDataHandler;
import cz.cas.lib.proarc.common.object.HasMetadataHandler;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.RelationCriteria;
import cz.cas.lib.proarc.common.object.ValueMap;
import cz.cas.lib.proarc.common.object.collectionOfClippings.CollectionOfClippingsPlugin;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;


/**
 * The plugin to support NDK Clipping digital objects.
 *
 * @author Lukas Sykora
 */
public class NdkClippingPlugin implements DigitalObjectPlugin, HasMetadataHandler<ModsDefinition> {

    /**
     * The plugin ID.
     */
    public static final String ID = "ndkClipping";

    public static final String MODEL_CLIPPING_COLLECTION = "model:ndkclippingcollection";
    public static final String MODEL_CLIPPING_DIRECTORY = "model:ndkclippingdirectory";
    public static final String MODEL_CLIPPING_UNIT = "model:ndkclippingunit";
    public static final String MODEL_PAGE = "model:ndkpage";

    private NdkPlugin.NdkSearchViewHandler searchViewHandler;

    @Override
    public String getId() {
        return ID;
    }

    @Override
    public Collection<MetaModel> getModel() {
        // for now it is read only repository
        List<MetaModel> models = new ArrayList<>();
        models.add(new MetaModel(MODEL_CLIPPING_COLLECTION, true, null,
                Arrays.asList(new ElementType("NDK Clipping collection", "en"), new ElementType("NDK Soubor výstřižků", "cs")),
                ModsConstants.NS, MODEL_CLIPPING_COLLECTION, this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM),
                new RelationCriteria[]{}
        ));
        models.add(new MetaModel(MODEL_CLIPPING_DIRECTORY, false, null,
                Arrays.asList(new ElementType("NDK Clipping directory", "en"), new ElementType("NDK Obálka výstřižku", "cs")),
                ModsConstants.NS, MODEL_CLIPPING_DIRECTORY, this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE, DatastreamEditorType.PARENT,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM),
                new RelationCriteria[]{new RelationCriteria(MODEL_CLIPPING_COLLECTION, RelationCriteria.Type.PID)}
        ));
        models.add(new MetaModel(MODEL_CLIPPING_UNIT, null, null,
                Arrays.asList(new ElementType("NDK Clipping Unit", "en"), new ElementType("NDK Výstřižek", "cs")),
                ModsConstants.NS, MODEL_CLIPPING_UNIT, this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM),
                new RelationCriteria[]{
                        new RelationCriteria(MODEL_CLIPPING_DIRECTORY, RelationCriteria.Type.PID),
                        new RelationCriteria(MODEL_CLIPPING_COLLECTION, RelationCriteria.Type.PID)
                }
        ));
        models.add(new MetaModel(
                MODEL_PAGE, null, true,
                Arrays.asList(new ElementType("NDK Page", "en"), new ElementType("NDK Strana", "cs")),
                ModsConstants.NS,
                MODEL_PAGE,
                this,
                EnumSet.complementOf(EnumSet.of(DatastreamEditorType.CHILDREN)),
                new RelationCriteria[]{
                        new RelationCriteria(NdkPlugin.MODEL_PERIODICALISSUE, RelationCriteria.Type.PID),
                        new RelationCriteria(NdkPlugin.MODEL_MONOGRAPHVOLUME, RelationCriteria.Type.PID),
                        new RelationCriteria(NdkPlugin.MODEL_MONOGRAPHUNIT, RelationCriteria.Type.PID),
                        new RelationCriteria(NdkPlugin.MODEL_CARTOGRAPHIC, RelationCriteria.Type.PID),
                        new RelationCriteria(NdkPlugin.MODEL_GRAPHIC, RelationCriteria.Type.PID),
                        new RelationCriteria(NdkPlugin.MODEL_SHEETMUSIC, RelationCriteria.Type.PID),
                        new RelationCriteria(NdkPlugin.MODEL_PERIODICALSUPPLEMENT, RelationCriteria.Type.PID),
                        new RelationCriteria(NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, RelationCriteria.Type.PID),
                        new RelationCriteria(CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_VOLUME, RelationCriteria.Type.PID),
                        new RelationCriteria(NdkAudioPlugin.MODEL_MUSICDOCUMENT, RelationCriteria.Type.PID),
                        new RelationCriteria(NdkAudioPlugin.MODEL_PHONOGRAPH, RelationCriteria.Type.PID),
                        new RelationCriteria(NdkAudioPlugin.MODEL_SONG, RelationCriteria.Type.PID),
                        new RelationCriteria(NdkAudioPlugin.MODEL_TRACK, RelationCriteria.Type.PID),
                        new RelationCriteria(NdkClippingPlugin.MODEL_CLIPPING_UNIT, RelationCriteria.Type.PID)
                }).setPriority(2)
        );


        return models;
    }

    @Override
    public <T extends HasDataHandler> T getHandlerProvider(Class<T> type) {
        return type.isInstance(this) ? type.cast(this) : null;
    }

    @Override
    public List<ValueMap> getValueMaps(ValueMap.Context context) {
        final ArrayList<ValueMap> maps = new ArrayList<ValueMap>();
        maps.add(JsonValueMap.fromBundle(BundleName.MODS_ROLES, context.getLocale()));
        return maps;
    }

    @Override
    public MetadataHandler<ModsDefinition> createMetadataHandler(DigitalObjectHandler handler) {
        return new NdkMetadataHandler(handler);
    }
}

