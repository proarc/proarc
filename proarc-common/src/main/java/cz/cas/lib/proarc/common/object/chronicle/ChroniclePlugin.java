package cz.cas.lib.proarc.common.object.chronicle;

import cz.cas.lib.proarc.common.process.export.mets.Const;
import cz.cas.lib.proarc.common.storage.PageView;
import cz.cas.lib.proarc.common.storage.SearchView.HasSearchViewHandler;
import cz.cas.lib.proarc.common.storage.SearchView.SearchViewHandler;
import cz.cas.lib.proarc.common.storage.SearchViewItem;
import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.i18n.BundleValue;
import cz.cas.lib.proarc.common.i18n.BundleValueMap;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.custom.ModsCutomEditorType;
import cz.cas.lib.proarc.common.mods.ndk.NdkPageMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectPlugin;
import cz.cas.lib.proarc.common.object.HasDataHandler;
import cz.cas.lib.proarc.common.object.HasMetadataHandler;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.RelationCriteria;
import cz.cas.lib.proarc.common.object.ValueMap;
import cz.cas.lib.proarc.common.object.collectionOfClippings.CollectionOfClippingsPlugin;
import cz.cas.lib.proarc.common.object.graphic.GraphicPlugin;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.common.object.ndk.NdkAudioPlugin;
import cz.cas.lib.proarc.common.object.ndk.NdkMetadataHandler;
import cz.cas.lib.proarc.common.object.ndk.NdkPlugin;
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
 *
 * The plugin to support NDK digital objects.
 *
 * @author Lukas Sykora
 */
public class ChroniclePlugin implements DigitalObjectPlugin, HasMetadataHandler<ModsDefinition>,
        HasSearchViewHandler {

    /**
    * The plugin ID.
    */
    public static final String ID = "chronicle";
    public static final String MODEL_CHRONICLETITLE = "model:chronicletitle";
    public static final String MODEL_CHRONICLEVOLUME = "model:chroniclevolume";
    public static final String MODEL_CHRONICLESUPPLEMENT = "model:chroniclesupplement";
    public static final String MODEL_PAGE = "model:page";

    private ChronicleSearchViewHandler searchViewHandler;

    public static final Map<String, String> TYPE_MAP = Collections.unmodifiableMap(new HashMap<String, String>() {{
        put(FEDORAPREFIX + ChroniclePlugin.MODEL_CHRONICLETITLE, Const.MONOGRAPH_MULTIPART);
        put(FEDORAPREFIX + ChroniclePlugin.MODEL_CHRONICLESUPPLEMENT, Const.SUPPLEMENT);
        put(FEDORAPREFIX + ChroniclePlugin.MODEL_CHRONICLEVOLUME, Const.MONOGRAPH_UNIT);
        put(FEDORAPREFIX + ChroniclePlugin.MODEL_PAGE, Const.PAGE);
    }});

    @Override
    public String getId() {
        return ID;
    }

    @Override
    public <T extends HasDataHandler> T getHandlerProvider(Class<T> type) {
        return type.isInstance(this) ? type.cast(this): null;
    }

    @Override
    public Collection<MetaModel> getModel() {
            // for now it is read only repository
            List<MetaModel> models = new ArrayList<MetaModel>();
            models.add(new MetaModel(
                    MODEL_CHRONICLEVOLUME, true, null,
                    Arrays.asList(new ElementType("Chronicle Volume", "en"), new ElementType("Svazek kronika", "cs")),
                    ModsConstants.NS,
                    MODEL_CHRONICLEVOLUME,
                    this,
                    EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                            DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                            DatastreamEditorType.ATM),
                    new RelationCriteria[] {new RelationCriteria(MODEL_CHRONICLETITLE, RelationCriteria.Type.PID)}
            ));
            models.add(new MetaModel(
                    MODEL_CHRONICLETITLE, true, null,
                    Arrays.asList(new ElementType("Multipart Chronicle", "en"), new ElementType("Vícedílná kronika", "cs")),
                    ModsConstants.NS,
                    MODEL_CHRONICLETITLE,
                    this,
                    EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                            DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM),
                    new RelationCriteria[] {new RelationCriteria(MODEL_CHRONICLETITLE, RelationCriteria.Type.PID)}
            ));
            models.add(new MetaModel(
                    MODEL_CHRONICLESUPPLEMENT, null, null,
                    Arrays.asList(new ElementType("Chronicle Supplement", "en"), new ElementType("Příloha kroniky", "cs")),
                    ModsConstants.NS,
                    MODEL_CHRONICLESUPPLEMENT,
                    this,
                    EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                            DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                            DatastreamEditorType.ATM),
                    new RelationCriteria[] {new RelationCriteria(MODEL_CHRONICLEVOLUME, RelationCriteria.Type.PID)}
            ).setPriority(2));
            models.add(new MetaModel(
                    MODEL_PAGE, null, true,
                    Arrays.asList(new ElementType("Page", "en"), new ElementType("Strana", "cs")),
                    ModsConstants.NS,
                    ModsCutomEditorType.EDITOR_PAGE,
                    this,
                    EnumSet.complementOf(EnumSet.of(DatastreamEditorType.CHILDREN, DatastreamEditorType.TECHNICAL)),
                    new RelationCriteria[]{
                            new RelationCriteria(NdkPlugin.MODEL_PERIODICALISSUE, RelationCriteria.Type.PID),
                            new RelationCriteria(NdkPlugin.MODEL_MONOGRAPHVOLUME, RelationCriteria.Type.PID),
                            new RelationCriteria(NdkPlugin.MODEL_MONOGRAPHUNIT, RelationCriteria.Type.PID),
                            new RelationCriteria(NdkPlugin.MODEL_CARTOGRAPHIC, RelationCriteria.Type.PID),
                            new RelationCriteria(NdkPlugin.MODEL_GRAPHIC, RelationCriteria.Type.PID),
                            new RelationCriteria(NdkPlugin.MODEL_SHEETMUSIC, RelationCriteria.Type.PID),
                            new RelationCriteria(NdkPlugin.MODEL_PERIODICALSUPPLEMENT, RelationCriteria.Type.PID),
                            new RelationCriteria(NdkPlugin.MODEL_MONOGRAPHSUPPLEMENT, RelationCriteria.Type.PID),
                            new RelationCriteria(NdkAudioPlugin.MODEL_MUSICDOCUMENT, RelationCriteria.Type.PID),
                            new RelationCriteria(NdkAudioPlugin.MODEL_PHONOGRAPH, RelationCriteria.Type.PID),
                            new RelationCriteria(NdkAudioPlugin.MODEL_SONG, RelationCriteria.Type.PID),
                            new RelationCriteria(NdkAudioPlugin.MODEL_TRACK, RelationCriteria.Type.PID),
                            new RelationCriteria(CollectionOfClippingsPlugin.MODEL_COLLECTION_OF_CLIPPINGS_VOLUME, RelationCriteria.Type.PID),
                            new RelationCriteria(GraphicPlugin.MODEL_GRAPHIC, RelationCriteria.Type.PID),
                            new RelationCriteria(ChroniclePlugin.MODEL_CHRONICLEVOLUME, RelationCriteria.Type.PID),
                            new RelationCriteria(ChroniclePlugin.MODEL_CHRONICLESUPPLEMENT, RelationCriteria.Type.PID),
                    }
            ).setPriority(4)) // override K4 plugin
            ;
            return models;
        }

    @Override
    public MetadataHandler<ModsDefinition> createMetadataHandler(DigitalObjectHandler handler) {
        return new NdkMetadataHandler(handler, new ChronicleMapperFactory());
    }

    @Override
    public SearchViewHandler createSearchViewHandler() {
        if (searchViewHandler == null) {
            searchViewHandler = new ChronicleSearchViewHandler();
        }
        return searchViewHandler;
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

    private static class ChronicleSearchViewHandler implements SearchViewHandler {

        @Override
        public String getObjectLabel(SearchViewItem item, Locale locale) {
            if (MODEL_PAGE.equals(item.getModel())) {
                return PageView.resolveFedoraObjectLabel(
                        item.getLabel(), NdkPageMapper.getPageTypeLabels(locale));
            }
            return item.getLabel();
        }
    }
}
