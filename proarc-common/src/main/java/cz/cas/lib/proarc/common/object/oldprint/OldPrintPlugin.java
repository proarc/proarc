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

import cz.cas.lib.proarc.common.fedora.DigitalObjectException;
import cz.cas.lib.proarc.common.fedora.PageView;
import cz.cas.lib.proarc.common.fedora.PageView.PageViewItem;
import cz.cas.lib.proarc.common.fedora.SearchView.HasSearchViewHandler;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.common.fedora.SearchView.SearchViewHandler;
import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.i18n.BundleValue;
import cz.cas.lib.proarc.common.i18n.BundleValueMap;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper.Context;
import cz.cas.lib.proarc.common.mods.ndk.NdkPageMapper.Page;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
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
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;

/**
 * The plug-in to support old print digital objects.
 *
 * @author Jan Pokorsky
 */
public class OldPrintPlugin implements DigitalObjectPlugin, HasMetadataHandler<ModsDefinition>,
        HasSearchViewHandler {

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

    /**
     * The page of old prints.
     */
    public static final String MODEL_PAGE = "model:oldprintpage";

    /**
     * The multipart volume of old prints.
     */
    public static final String MODEL_MONOGRAPHTITLE = "model:oldprintmonographtitle";

    private OldPrintSearchViewHandler searchViewHandler;

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
                Arrays.asList(new ElementType("Old Print Volume", "en"), new ElementType("STT Svazek monografie", "cs")),
                ModsConstants.NS,
                MODEL_VOLUME,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        /*DatastreamEditorType.PARENT, */DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_SUPPLEMENT, true, null,
                Arrays.asList(new ElementType("Old Print Supplement", "en"), new ElementType("STT Příloha monografie", "cs")),
                ModsConstants.NS,
                MODEL_SUPPLEMENT,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_PAGE, null, true,
                Arrays.asList(new ElementType("Old Print Page", "en"), new ElementType("STT Strana", "cs")),
                ModsConstants.NS,
                MODEL_PAGE,
                this,
                EnumSet.complementOf(EnumSet.of(DatastreamEditorType.CHILDREN))
                ));
        models.add(new MetaModel(
                MODEL_MONOGRAPHTITLE, true, null,
                Arrays.asList(new ElementType("Old Print Multipart Monograph", "en"), new ElementType("STT Vícedílná monografie", "cs")),
                ModsConstants.NS,
                MODEL_MONOGRAPHTITLE,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM)
        ));
        return models;
    }

    @Override
    public <T extends HasDataHandler> T getHandlerProvider(Class<T> type) {
        return type.isInstance(this) ? type.cast(this): null;
    }

    @Override
    public List<ValueMap> getValueMaps(ValueMap.Context context) {
        Locale locale = context.getLocale();
        ArrayList<ValueMap> maps = new ArrayList<ValueMap>();
        maps.add(readPageTypes(locale));
        return maps;
    }

    private ValueMap<BundleValue> readPageTypes(Locale locale) {
        return BundleValueMap.fromBundle(BundleName.MODS_OLDPRINT_PAGE_TYPES, locale);
    }

    @Override
    public MetadataHandler<ModsDefinition> createMetadataHandler(DigitalObjectHandler handler) {
        return new NdkMetadataHandler(handler, new OldPrintMapperFactory()) {

            @Override
            protected ModsDefinition createDefault(String modelId) throws DigitalObjectException {
                ModsDefinition defaultMods = super.createDefault(modelId);
                DigitalObjectHandler parent = handler.getParameterParent();
                if (OldPrintPlugin.MODEL_SUPPLEMENT.equals(modelId)) {
                    // issue 329
                    DigitalObjectHandler title = findEnclosingObject(parent, OldPrintPlugin.MODEL_VOLUME);
                    if (title != null) {
                        ModsDefinition titleMods = title.<ModsDefinition>metadata().getMetadata().getData();
                        inheritSupplementTitleInfo(defaultMods, titleMods.getTitleInfo());
                        defaultMods.getLanguage().addAll(titleMods.getLanguage());
                        inheritIdentifier(defaultMods, titleMods.getIdentifier(), "ccnb", "isbn");
                        inheritOriginInfoDateIssued(defaultMods, titleMods.getOriginInfo());
                        inheritPhysicalDescriptionForm(defaultMods, titleMods.getPhysicalDescription());
                    }
                }
                return defaultMods;
            }

            @Override
            public PageViewItem createPageViewItem(Locale locale) throws DigitalObjectException {
                String modelId = handler.relations().getModel();
                if (modelId.equals(MODEL_PAGE)) {
                    ModsDefinition mods = editor.read();
                    OldPrintPageMapper mapper = new OldPrintPageMapper();
                    Page page = mapper.toJsonObject(mods, new Context(handler));
                    PageViewItem item = new PageViewItem();
                    item.setPageIndex(page.getIndex());
                    item.setPageNumber(page.getNumber());
                    item.setPageType(page.getType());
                    item.setPageTypeLabel(OldPrintPageMapper.getPageTypeLabel(item.getPageType(), locale));
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
                    OldPrintPageMapper mapper = new OldPrintPageMapper();
                    ModsDefinition mods = mapper.createPage(
                            page.getPageIndex(), page.getPageNumber(), page.getPageType(), new Context(handler));
                    metadata.setIgnoreValidation(true);
                    write(modelId, mods, metadata, message);
                } else {
                    throw new DigitalObjectException(fobject.getPid(), "Unexpected model for oldprint page: " + modelId);
                }
            }


        };
    }

    @Override
    public SearchViewHandler createSearchViewHandler() {
        if (searchViewHandler == null) {
            searchViewHandler = new OldPrintSearchViewHandler();
        }
        return searchViewHandler;
    }

    private static class OldPrintSearchViewHandler implements SearchViewHandler {

        @Override
        public String getObjectLabel(Item item, Locale locale) {
            if (MODEL_PAGE.equals(item.getModel())) {
                return PageView.resolveFedoraObjectLabel(
                        item.getLabel(), OldPrintPageMapper.getPageTypeLabels(locale));
            }
            return item.getLabel();
        }

    }

}
