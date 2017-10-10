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
package cz.cas.lib.proarc.common.object.ndk;

import cz.cas.lib.proarc.common.fedora.PageView;
import cz.cas.lib.proarc.common.fedora.SearchView.HasSearchViewHandler;
import cz.cas.lib.proarc.common.fedora.SearchView.Item;
import cz.cas.lib.proarc.common.fedora.SearchView.SearchViewHandler;
import cz.cas.lib.proarc.common.i18n.BundleName;
import cz.cas.lib.proarc.common.i18n.BundleValue;
import cz.cas.lib.proarc.common.i18n.BundleValueMap;
import cz.cas.lib.proarc.common.i18n.JsonValueMap;
import cz.cas.lib.proarc.common.mods.custom.ModsConstants;
import cz.cas.lib.proarc.common.mods.custom.ModsCutomEditorType;
import cz.cas.lib.proarc.common.mods.ndk.NdkPageMapper;
import cz.cas.lib.proarc.common.object.DigitalObjectHandler;
import cz.cas.lib.proarc.common.object.DigitalObjectPlugin;
import cz.cas.lib.proarc.common.object.HasDataHandler;
import cz.cas.lib.proarc.common.object.HasMetadataHandler;
import cz.cas.lib.proarc.common.object.MetadataHandler;
import cz.cas.lib.proarc.common.object.ValueMap;
import cz.cas.lib.proarc.common.object.model.DatastreamEditorType;
import cz.cas.lib.proarc.common.object.model.MetaModel;
import cz.cas.lib.proarc.mods.CodeOrText;
import cz.cas.lib.proarc.mods.LanguageTermDefinition;
import cz.cas.lib.proarc.mods.ModsDefinition;
import cz.cas.lib.proarc.oaidublincore.ElementType;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.ResourceBundle.Control;

/**
 * The plugin to support NDK digital objects.
 *
 * @author Jan Pokorsky
 */
public class NdkPlugin implements DigitalObjectPlugin, HasMetadataHandler<ModsDefinition>,
        HasSearchViewHandler {

    /**
     * The plugin ID.
     */
    public static final String ID = "ndk";
    public static final String MODEL_PERIODICAL = "model:ndkperiodical";
    public static final String MODEL_PERIODICALVOLUME = "model:ndkperiodicalvolume";
    public static final String MODEL_PERIODICALISSUE = "model:ndkperiodicalissue";
    public static final String MODEL_PERIODICALSUPPLEMENT = "model:ndkperiodicalsupplement";
    public static final String MODEL_MONOGRAPHTITLE = "model:ndkmonographtitle";
    public static final String MODEL_MONOGRAPHVOLUME = "model:ndkmonographvolume";
    public static final String MODEL_MONOGRAPHSUPPLEMENT = "model:ndkmonographsupplement";
    public static final String MODEL_CARTOGRAPHIC = "model:ndkmap";
    public static final String MODEL_SHEETMUSIC = "model:ndksheetmusic";
    public static final String MODEL_ARTICLE = "model:ndkarticle";
    public static final String MODEL_CHAPTER = "model:ndkchapter";
    public static final String MODEL_PICTURE = "model:ndkpicture";
    public static final String MODEL_PAGE = "model:page";

    private NdkSearchViewHandler searchViewHandler;

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
                MODEL_PERIODICAL, true, null,
                Arrays.asList(new ElementType("NDK Periodical", "en"), new ElementType("NDK Periodikum", "cs")),
                ModsConstants.NS,
                MODEL_PERIODICAL,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_PERIODICALVOLUME, null, null,
                Arrays.asList(new ElementType("NDK Periodical Volume", "en"), new ElementType("NDK Ročník", "cs")),
                ModsConstants.NS,
                MODEL_PERIODICALVOLUME,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_PERIODICALISSUE, null, null,
                Arrays.asList(new ElementType("NDK Periodical Issue", "en"), new ElementType("NDK Číslo", "cs")),
                ModsConstants.NS,
                MODEL_PERIODICALISSUE,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_PERIODICALSUPPLEMENT, null, null,
                Arrays.asList(new ElementType("NDK Periodical Supplement", "en"), new ElementType("NDK Příloha periodika", "cs")),
                ModsConstants.NS,
                MODEL_PERIODICALSUPPLEMENT,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_ARTICLE, null, null,
                Arrays.asList(new ElementType("NDK Article", "en"), new ElementType("NDK Článek", "cs")),
                ModsConstants.NS,
                MODEL_ARTICLE,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.OCR, DatastreamEditorType.MEDIA,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_PICTURE, null, null,
                Arrays.asList(new ElementType("NDK Picture/Map - internal part", "en"), new ElementType("NDK Obrázek/Mapa - vnitřní část", "cs")),
                ModsConstants.NS,
                MODEL_PICTURE,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.OCR, DatastreamEditorType.MEDIA,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_MONOGRAPHTITLE, true, null,
                Arrays.asList(new ElementType("NDK Multipart Monograph", "en"), new ElementType("NDK Vícedílná monografie", "cs")),
                ModsConstants.NS,
                MODEL_MONOGRAPHTITLE,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.CHILDREN, DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_MONOGRAPHVOLUME, true, null,
                Arrays.asList(new ElementType("NDK Monograph Volume", "en"), new ElementType("NDK Svazek monografie", "cs")),
                ModsConstants.NS,
                MODEL_MONOGRAPHVOLUME,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_MONOGRAPHSUPPLEMENT, null, null,
                Arrays.asList(new ElementType("NDK Monograph Supplement", "en"), new ElementType("NDK Příloha monografie", "cs")),
                ModsConstants.NS,
                MODEL_MONOGRAPHSUPPLEMENT,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_CHAPTER, null, null,
                Arrays.asList(new ElementType("NDK Chapter", "en"), new ElementType("NDK Kapitola", "cs")),
                ModsConstants.NS,
                MODEL_CHAPTER,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_CARTOGRAPHIC, true, null,
                Arrays.asList(new ElementType("NDK Cartographic Document", "en"), new ElementType("NDK Kartografický dokument", "cs")),
                ModsConstants.NS,
                MODEL_CARTOGRAPHIC,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_SHEETMUSIC, true, null,
                Arrays.asList(new ElementType("NDK Sheet Music", "en"), new ElementType("NDK Hudebnina", "cs")),
                ModsConstants.NS,
                MODEL_SHEETMUSIC,
                this,
                EnumSet.of(DatastreamEditorType.MODS, DatastreamEditorType.NOTE,
                        DatastreamEditorType.PARENT, DatastreamEditorType.CHILDREN,
                        DatastreamEditorType.ATM)
                ));
        models.add(new MetaModel(
                MODEL_PAGE, null, true,
                Arrays.asList(new ElementType("Page", "en"), new ElementType("Strana", "cs")),
                ModsConstants.NS,
                ModsCutomEditorType.EDITOR_PAGE,
                this,
                EnumSet.complementOf(EnumSet.of(DatastreamEditorType.CHILDREN))
                ).setPriority(2)) // override K4 plugin
                ;

        return models;
    }

    @Override
    public MetadataHandler<ModsDefinition> createMetadataHandler(DigitalObjectHandler handler) {
        return new NdkMetadataHandler(handler);
    }

    @Override
    public SearchViewHandler createSearchViewHandler() {
        if (searchViewHandler == null) {
            searchViewHandler = new NdkSearchViewHandler();
        }
        return searchViewHandler;
    }

    @Override
    public List<ValueMap> getValueMaps(ValueMap.Context context) {
        Locale locale = context.getLocale();
        ArrayList<ValueMap> maps = new ArrayList<ValueMap>();
        maps.add(readLangs(locale));
        maps.add(readPageTypes(locale));
        maps.add(JsonValueMap.fromBundle(BundleName.MODS_ROLES, locale));
        return maps;
    }

    private ValueMap<? extends LanguageTermDefinition> readLangs(Locale locale) {
        ArrayList<LangTermValue> langs = new ArrayList<LangTermValue>();
        // to read properties file in UTF-8 use PropertyResourceBundle(Reader)
        Control control = ResourceBundle.Control.getNoFallbackControl(ResourceBundle.Control.FORMAT_PROPERTIES);

        ResourceBundle rb = ResourceBundle.getBundle(BundleName.LANGUAGES_ISO639_2.toString(), locale, control);
        for (String key : rb.keySet()) {
            LangTermValue lt = new LangTermValue();
            lt.setAuthority("iso639-2b");
            lt.setType(CodeOrText.CODE);
            lt.setValue(key);
            lt.setTitle(rb.getString(key));
            langs.add(lt);
        }
        Collections.sort(langs, new LangComparator(locale));
        return new ValueMap<LangTermValue>(BundleName.LANGUAGES_ISO639_2.getValueMapId(), langs);
    }

    private ValueMap<BundleValue> readPageTypes(Locale locale) {
        return BundleValueMap.fromBundle(BundleName.MODS_PAGE_TYPES, locale);
    }

    public static class LangTermValue extends LanguageTermDefinition {

        private String title;

        public String getTitle() {
            return title;
        }

        public void setTitle(String title) {
            this.title = title;
        }
    }

    private static class LangComparator implements Comparator<LangTermValue> {

        private final Collator collator;

        public LangComparator(Locale locale) {
            collator = Collator.getInstance(locale);
        }

        @Override
        public int compare(LangTermValue o1, LangTermValue o2) {
            return collator.compare(o1.getTitle(), o2.getTitle());
        }

    }

    public static class NdkSearchViewHandler implements SearchViewHandler {

        @Override
        public String getObjectLabel(Item item, Locale locale) {
            if (MODEL_PAGE.equals(item.getModel())) {
                return PageView.resolveFedoraObjectLabel(
                        item.getLabel(), NdkPageMapper.getPageTypeLabels(locale));
            }
            return item.getLabel();
        }

    }

}
