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

import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.i18n.BundleValue;
import cz.cas.lib.proarc.common.i18n.BundleValueMap;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.ndk.NdkMapper.Context;
import cz.cas.lib.proarc.common.object.DescriptionMetadata;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectPlugin;
import cz.cas.lib.proarc.common.object.HasDataHandler;
import cz.cas.lib.proarc.common.object.HasMetadataHandler;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.RelationCriteria;
import cz.cas.lib.proarc.common.object.ValueMap;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler;
import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.common.storage.DigitalObjectException;
import cz.cas.lib.proarc.common.storage.PageView;
import cz.cas.lib.proarc.common.storage.PageView.PageViewItem;
import cz.cas.lib.proarc.common.storage.SearchView.HasSearchViewHandler;
import cz.cas.lib.proarc.common.storage.SearchView.SearchViewHandler;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static cz.cas.lib.proarc.common.process.export.mets.Const.FEDORAPREFIX;

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
    public static final String MODEL_MONOGRAPHVOLUME = "model:oldprintvolume";

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

    /**
     * The multipart unit of old prints.
     */
    public static final String MODEL_MONOGRAPHUNIT = "model:oldprintmonographunit";

    /**
     * The chapter of old prints.
     */
    public static final String MODEL_CHAPTER = "model:oldprintchapter";

    /**
     * The convolute of oldprints.
     */
    public static final String MODEL_CONVOLUTTE = "model:oldprintomnibusvolume";

    /**
     * The graphics of oldprints.
     */
    public static final String MODEL_GRAPHICS = "model:oldprintgraphics";

    /**
     * The map of oldprints.
     */
    public static final String MODEL_CARTOGRAPHIC = "model:oldprintmap";

    /**
     * The sheet music of oldprints.
     */
    public static final String MODEL_SHEETMUSIC = "model:oldprintsheetmusic";


    private OldPrintSearchViewHandler searchViewHandler;

    public static final Map<String, String> TYPE_MAP = Collections.unmodifiableMap(new HashMap<String, String>() {{
        put(FEDORAPREFIX + OldPrintPlugin.MODEL_MONOGRAPHVOLUME, Const.MONOGRAPH_UNIT);
        put(FEDORAPREFIX + OldPrintPlugin.MODEL_SUPPLEMENT, Const.SUPPLEMENT);
        put(FEDORAPREFIX + OldPrintPlugin.MODEL_MONOGRAPHTITLE, Const.MONOGRAPH_MULTIPART);
        put(FEDORAPREFIX + OldPrintPlugin.MODEL_MONOGRAPHUNIT, Const.MONOGRAPH_UNIT);
        put(FEDORAPREFIX + OldPrintPlugin.MODEL_CHAPTER, Const.CHAPTER);
        put(FEDORAPREFIX + OldPrintPlugin.MODEL_PAGE, Const.PAGE);
        put(FEDORAPREFIX + OldPrintPlugin.MODEL_CONVOLUTTE, Const.MONOGRAPH_MULTIPART);
        put(FEDORAPREFIX + OldPrintPlugin.MODEL_GRAPHICS, Const.MONOGRAPH_UNIT);
        put(FEDORAPREFIX + OldPrintPlugin.MODEL_CARTOGRAPHIC, Const.MONOGRAPH_UNIT);
        put(FEDORAPREFIX + OldPrintPlugin.MODEL_SHEETMUSIC, Const.MONOGRAPH_UNIT);
    }});

    @Override
    public String getId() {
        return ID;
    }

    @Override
    public Collection<MetaModel> getModel() {
        // for now it is read only repository
        List<MetaModel> models = new ArrayList<MetaModel>();
        models.add(new MetaModel(
                MODEL_MONOGRAPHTITLE, true, null,
                Arrays.asList(new ElementType("Old Print Multipart Monograph", "en"), new ElementType("STT Vícedílná monografie", "cs")),
                ModsConstants.NS,
                MODEL_MONOGRAPHTITLE,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM),
                new RelationCriteria[]{}
                ));
        models.add(new MetaModel(
                MODEL_MONOGRAPHUNIT, null, null,
                Arrays.asList(new ElementType("Old Print Monograph Unit", "en"), new ElementType("STT Svazek Vícedílné monografie", "cs")),
                ModsConstants.NS,
                MODEL_MONOGRAPHUNIT,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.PARENT, DatastreamEditorType.ATM),
                new RelationCriteria[]{new RelationCriteria(MODEL_MONOGRAPHTITLE, RelationCriteria.Type.PID)}
        ));
        models.add(new MetaModel(
                MODEL_MONOGRAPHVOLUME, true, null,
                Arrays.asList(new ElementType("Old Print Volume", "en"), new ElementType("STT Svazek monografie", "cs")),
                ModsConstants.NS,
                MODEL_MONOGRAPHVOLUME,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM),
                new RelationCriteria[]{
                        new RelationCriteria(MODEL_CONVOLUTTE, RelationCriteria.Type.PID)}
                ));
        models.add(new MetaModel(
                MODEL_SUPPLEMENT, true, null,
                Arrays.asList(new ElementType("Old Print Supplement", "en"), new ElementType("STT Příloha monografie", "cs")),
                ModsConstants.NS,
                MODEL_SUPPLEMENT,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM),
                new RelationCriteria[]{
                        new RelationCriteria(MODEL_MONOGRAPHVOLUME, RelationCriteria.Type.PID),
                        new RelationCriteria(MODEL_MONOGRAPHUNIT, RelationCriteria.Type.PID)}
                ));
        models.add(new MetaModel(
                MODEL_CHAPTER, null, null,
                Arrays.asList(new ElementType("Old Print Chapter", "en"), new ElementType("STT  Kapitola", "cs")),
                ModsConstants.NS,
                MODEL_CHAPTER,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM),
                new RelationCriteria[] {
                        new RelationCriteria(MODEL_MONOGRAPHVOLUME, RelationCriteria.Type.PID),
                        new RelationCriteria(MODEL_MONOGRAPHUNIT, RelationCriteria.Type.PID)}
                ));
        models.add(new MetaModel(
                MODEL_PAGE, null, true,
                Arrays.asList(new ElementType("Old Print Page", "en"), new ElementType("STT Strana", "cs")),
                ModsConstants.NS,
                MODEL_PAGE,
                this,
                EnumSet.complementOf(EnumSet.of(DatastreamEditorType.CHILDREN)),
                new RelationCriteria[]{
                        new RelationCriteria(MODEL_MONOGRAPHVOLUME, RelationCriteria.Type.PID),
                        new RelationCriteria(MODEL_MONOGRAPHUNIT, RelationCriteria.Type.PID),
                        new RelationCriteria(MODEL_SUPPLEMENT, RelationCriteria.Type.PID),
                        new RelationCriteria(MODEL_CONVOLUTTE, RelationCriteria.Type.PID),
                        new RelationCriteria(MODEL_CARTOGRAPHIC, RelationCriteria.Type.PID),
                        new RelationCriteria(MODEL_GRAPHICS, RelationCriteria.Type.PID),
                        new RelationCriteria(MODEL_SHEETMUSIC, RelationCriteria.Type.PID),
                }
                ));
        models.add(new MetaModel(
                MODEL_CONVOLUTTE, true, null,
                Arrays.asList(new ElementType("Old Print Omnibus volume", "en"), new ElementType("STT Konvolut", "cs")),
                ModsConstants.NS,
                MODEL_CONVOLUTTE,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM),
                new RelationCriteria[]{}
        ));
        models.add(new MetaModel(
                MODEL_GRAPHICS, true, null,
                Arrays.asList(new ElementType("Old Print Graphics", "en"), new ElementType("STT  Grafika", "cs")),
                ModsConstants.NS,
                MODEL_GRAPHICS,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM),
                new RelationCriteria[] {
                        new RelationCriteria(MODEL_MONOGRAPHVOLUME, RelationCriteria.Type.PID),
                        new RelationCriteria(MODEL_MONOGRAPHUNIT, RelationCriteria.Type.PID),
                        new RelationCriteria(MODEL_CONVOLUTTE, RelationCriteria.Type.PID)}
        ));
        models.add(new MetaModel(
                MODEL_CARTOGRAPHIC, true, null,
                Arrays.asList(new ElementType("Old print Cartographic Document", "en"), new ElementType("STT Kartografický dokument", "cs")),
                ModsConstants.NS,
                MODEL_CARTOGRAPHIC,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM),
                new RelationCriteria[]{
                        new RelationCriteria(MODEL_MONOGRAPHVOLUME, RelationCriteria.Type.PID),
                        new RelationCriteria(MODEL_MONOGRAPHUNIT, RelationCriteria.Type.PID),
                        new RelationCriteria(MODEL_CONVOLUTTE, RelationCriteria.Type.PID)}
        ));
        models.add(new MetaModel(
                MODEL_SHEETMUSIC, true, null,
                Arrays.asList(new ElementType("Old print Sheet Music", "en"), new ElementType("STT Hudebnina", "cs")),
                ModsConstants.NS,
                MODEL_SHEETMUSIC,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM),
                new RelationCriteria[]{
                        new RelationCriteria(MODEL_MONOGRAPHVOLUME, RelationCriteria.Type.PID),
                        new RelationCriteria(MODEL_MONOGRAPHUNIT, RelationCriteria.Type.PID),
                        new RelationCriteria(MODEL_CONVOLUTTE, RelationCriteria.Type.PID)}
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
        //return BundleValueMap.fromBundle(BundleName.MODS_OLDPRINT_PAGE_TYPES, locale);
        return BundleValueMap.fromBundle(BundleName.MODS_PAGE_TYPES, locale);
    }

    @Override
    public MetadataHandler<ModsDefinition> createMetadataHandler(DigitalObjectHandler handler) {
        return new NdkMetadataHandler(handler, new OldPrintMapperFactory()) {

            @Override
            public PageViewItem createPageViewItem(Locale locale) throws DigitalObjectException {
                String modelId = handler.relations().getModel();
                if (modelId.equals(MODEL_PAGE)) {
                    ModsDefinition mods = editor.read();
                    OldPrintPageMapper mapper = new OldPrintPageMapper();
//                    NdkNewPageMapper page = mapper.toJsonObject(mods, new Context(handler));
                    PageViewItem item = new PageViewItem();
                    item.setPageIndex(mapper.getIndex(mods));
                    item.setPageNumber(mapper.getNumber(mods));
                    item.setPageType(mapper.getType(mods));
                    item.setPagePosition(mapper.getPosition(mods));
                    item.setPageRepre(mapper.getPageRepre(mods));
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
                    write(modelId, mods, metadata, message, NdkMetadataHandler.OPERATION_UPDATE);
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
        public String getObjectLabel(SearchViewItem item, Locale locale) {
            if (MODEL_PAGE.equals(item.getModel())) {
                return PageView.resolveFedoraObjectLabel(
                        item.getLabel(), OldPrintPageMapper.getPageTypeLabels(locale));
            }
            return item.getLabel();
        }

    }

}
