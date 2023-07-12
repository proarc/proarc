/*
 * Copyright (C) 2017 Lukas Sykora
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

import cz.cas.lib.proarc.common.fedora.BinaryEditor;
import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.PageView.PageViewItem;
import cz.cas.lib.proarc.common.fedora.SearchView.HasSearchViewHandler;
import cz.cas.lib.proarc.common.fedora.SearchView.SearchViewHandler;
import cz.cas.lib.proarc.common.fedora.SearchViewItem;
import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.i18n.JsonValueMap;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper.Context;
import cz.cas.lib.proarc.common.mods.ndk.NdkPageMapper.Page;
import cz.cas.lib.proarc.common.object.DefaultDisseminationHandler;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;


/**
 * The plugin to support NDK Audio digital objects.
 *
 * @author Lukas Sykora
 */
public class NdkAudioPlugin implements DigitalObjectPlugin, HasMetadataHandler<ModsDefinition>,
        HasDisseminationHandler, HasSearchViewHandler {

    /**
     * The plugin ID.
     */
    public static final String ID = "ndkAudio";

    public static final String MODEL_MUSICDOCUMENT = "model:ndkmusicdocument";
    public static final String MODEL_SONG = "model:ndksong";
    public static final String MODEL_TRACK = "model:ndktrack";
    public static final String MODEL_PAGE = "model:ndkaudiopage";
    public static final String MODEL_PHONOGRAPH = "model:ndkphonographcylinder";

    private SoundrecordingSearchViewHandler searchViewHandler;

    @Override
    public String getId() {
        return ID;
    }

    @Override
    public Collection<MetaModel> getModel() {
        // for now it is read only repository
        List<MetaModel> models = new ArrayList<>();
        models.add(new MetaModel(MODEL_PHONOGRAPH, true, null,
                Arrays.asList(new ElementType("NDK Phonograph cylinder", "en"), new ElementType("NDK Fonografický váleček", "cs")),
                ModsConstants.NS, MODEL_PHONOGRAPH, this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
        ));
        models.add(new MetaModel(MODEL_MUSICDOCUMENT, true, null,
                Arrays.asList(new ElementType("NDK Sound Collectiont", "en"), new ElementType("NDK Zvukový dokument", "cs")),
                ModsConstants.NS, MODEL_MUSICDOCUMENT, this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM)
        ));
        models.add(new MetaModel(MODEL_SONG, null, null,
                Arrays.asList(new ElementType("NDK Sound Recording", "en"), new ElementType("NDK Skladba", "cs")),
                ModsConstants.NS, MODEL_SONG, this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
        ));
        models.add(new MetaModel(MODEL_TRACK, null, null,
               Arrays.asList(new ElementType("NDK Sound Part", "en"), new ElementType("NDK Část skladby", "cs")),
               ModsConstants.NS, MODEL_TRACK, this,
               EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                       DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                       DatastreamEditorType.ATM)
        ));
        models.add(new MetaModel(
                NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, null, null,
                Arrays.asList(new ElementType("NDK Monograph Supplement", "en"), new ElementType("NDK Příloha monografie", "cs")),
                ModsConstants.NS,
                NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
        ));
        models.add(new MetaModel(
                MODEL_PAGE, null, true,
                Arrays.asList(new ElementType("Soundrecording Page", "en"), new ElementType("Zvukova nahravka", "cs")),
                ModsConstants.NS,
                MODEL_PAGE,
                this,
                EnumSet.complementOf(EnumSet.of(DatastreamEditorType.CHILDREN))
        ));


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
        return new NdkMetadataHandler(handler) {

            @Override
            public PageViewItem createPageViewItem(Locale locale) throws DigitalObjectException {
                String modelId = handler.relations().getModel();
                if (modelId.equals(MODEL_PAGE)) {
                    ModsDefinition mods = editor.read();
                    NdkAudioPageMapper mapper = new NdkAudioPageMapper();
                    Page page = mapper.toJsonObject(mods, new Context(handler));
                    PageViewItem item = new PageViewItem();
                    item.setPageIndex(page.getIndex());
                    item.setPageNumber(page.getNumber());
                    item.setPageType(page.getType());
                    item.setPageTypeLabel(page.getType());
                    return item;
                } else {
                    throw new DigitalObjectException(fobject.getPid(), "Unexpected model for oldprint page: " + modelId);
                }
            }

            @Override
            public void setPage(PageViewItem page, String message) throws DigitalObjectException {
                String modelId = handler.relations().getModel();
                if (modelId.equals(MODEL_PAGE)) {
                    DescriptionMetadata<ModsDefinition> metadata = new DescriptionMetadata<ModsDefinition>();
                    metadata.setTimestamp(editor.getLastModified());
                    NdkAudioPageMapper mapper = new NdkAudioPageMapper();
                    ModsDefinition mods = mapper.createPage(
                            page.getPageIndex(), page.getPageNumber(), page.getPageType(), new Context(handler));
                    metadata.setIgnoreValidation(true);
                    write(modelId, mods, metadata, message, NdkMetadataHandler.OPERATION_UPDATE);
                } else {
                    throw new DigitalObjectException(fobject.getPid(), "Unexpected model for oldprint page: " + modelId);
                }
            }
        };
    }

    public DisseminationHandler createDisseminationHandler(String dsId, DigitalObjectHandler handler) {
        final DefaultDisseminationHandler ddh = new DefaultDisseminationHandler(dsId, handler);
        if (BinaryEditor.RAW_ID.equals(dsId)) {
            return new BornDigitalDisseminationHandler(ddh);
        } else {
            return ddh;
        }
    }

    @Override
    public SearchViewHandler createSearchViewHandler() {
        if (searchViewHandler == null) {
            searchViewHandler = new SoundrecordingSearchViewHandler();
        }
        return searchViewHandler;
    }

    public static boolean isNdkAudioModel(String model) {
        return MODEL_PAGE.equals(model) || MODEL_MUSICDOCUMENT.equals(model) || MODEL_PHONOGRAPH.equals(model) || MODEL_SONG.equals(model) || MODEL_TRACK.equals(model);
    }

    private static class SoundrecordingSearchViewHandler implements SearchViewHandler {

        @Override
        public String getObjectLabel(SearchViewItem item, Locale locale) {
            if (MODEL_PAGE.equals(item.getModel())) {
                return item.getLabel();
            }
            return item.getLabel();
        }
    }
}

